"""Read the log stream produced by log-to-json.rkt, and upload each
entry to elasticsearch.

We're using elasticsearch's bulk ingestion API, which is documented
(not very well) at
https://www.elastic.co/guide/en/elasticsearch/reference/5.4/docs-bulk.html

"""

import concurrent.futures
import hashlib
import json
import os

import elasticsearch            # pip install elasticsearch
import elasticsearch.helpers
import progressbar              # pip install progressbar2

# To delete the whole mess in order to start over, paste this into https://wat/_plugin/kibana/app/kibana#/dev_tools/console
"""
POST messages/message/_delete_by_query?conflicts=proceed
{
  "query": {
    "match_all": {}
  }
}
"""

# TODO -- query elasticsearch before starting, to find the newest
# timestamp; only upload newer items.

# I don't want the actual URL here since the server's permissions are
# too lax, and anything in this file will wind up on github
ELASTICSEARCH_DOMAIN_ENDPOINT = os.getenv('ELASTICSEARCH_DOMAIN_ENDPOINT')


def short_hash(stuff):
    return hashlib.sha256(stuff).hexdigest()[0:8]


def ingest_one_batch(es, batch_of_lines):
    index_action_dicts = []
    for line in batch_of_lines:
        data = json.loads(line)
        data.update(
            {
                '_index': 'messages',
                '_type': 'message',
                '_id': short_hash(line.encode('utf-8'))
            })
        index_action_dicts.append(data)

    elasticsearch.helpers.bulk(es, index_action_dicts)

    return sum((len(l) for l in batch))


LINES_PER_BATCH = 50000
NUMBER_OF_WORKER_THREADS = 2

def line_batcher(inf, number_of_lines):
    batch = []
    for line in inf:
        batch.append(line)
        if len(batch) == number_of_lines:
            yield batch
            batch = []
    if batch:
        yield batch


if __name__ == "__main__":
    es = elasticsearch.Elasticsearch(hosts=[
        {
            'host': ELASTICSEARCH_DOMAIN_ENDPOINT,
            'use_ssl': True,
            'port': 443,
        }
    ])

    with open('big-log.json') as inf:
        with progressbar.ProgressBar(max_value=os.fstat(inf.fileno()).st_size) as progress:
            with concurrent.futures.ThreadPoolExecutor(max_workers=NUMBER_OF_WORKER_THREADS) as executor:
                futures = []
                for batch in line_batcher(inf, LINES_PER_BATCH):
                    f = executor.submit(ingest_one_batch, es, batch)
                    f.add_done_callback(lambda f: progress.update(progress.value + f.result()))
                    futures.append(f)

                    if len(futures) == NUMBER_OF_WORKER_THREADS:
                        concurrent.futures.wait(futures)
                        futures = []
