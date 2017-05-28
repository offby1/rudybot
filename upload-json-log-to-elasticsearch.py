"""Read the log stream produced by log-to-json.rkt, and upload each
entry to elasticsearch.

"""

# This is surprsingly slow; I suspect I want to use the "bulk
# ingestion" API
# (https://www.elastic.co/guide/en/elasticsearch/reference/5.4/docs-bulk.html)

# To delete the whole mess in order to start over, paste this into https://wat/_plugin/kibana/app/kibana#/dev_tools/console
# POST messages/message/_delete_by_query?conflicts=proceed
# {
#   "query": {
#     "match_all": {}
#   }
# }

import hashlib
import json
import os
import pprint
import queue
import threading

import line_batcher

import elasticsearch
import elasticsearch.helpers
import progressbar              # pip install progressbar2

# I don't want the actual URL here since its permissions are too lax,
# and anything in this file will wind up on github
ELASTICSEARCH_DOMAIN_ENDPOINT = os.getenv('ELASTICSEARCH_DOMAIN_ENDPOINT')

ELASTICSEARCH_DOMAIN_MESSAGE_CONTAINER = '/messages/message'


def short_hash(stuff):
    return hashlib.sha256(stuff).hexdigest()[0:8]


def compute_document_url(line):
    return 'https://{}/{}/{}'.format(
        ELASTICSEARCH_DOMAIN_ENDPOINT,
        ELASTICSEARCH_DOMAIN_MESSAGE_CONTAINER,
        short_hash(line))


def worker():
    es = elasticsearch.Elasticsearch(hosts=[
        {
            'host': ELASTICSEARCH_DOMAIN_ENDPOINT,
            'use_ssl': True,
            'port': 443,
        }
    ])
    while True:
        batch_of_lines = work_queue.get()
        if batch_of_lines is None:
            break

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

        work_queue.task_done()


if __name__ == "__main__":
    work_queue = queue.Queue(maxsize=100)
    threads = [threading.Thread(target=worker) for _ in range(10)]
    for t in threads:
        t.start()

    with open('big-log.json') as inf:
        progress = progressbar.ProgressBar(max_value=os.fstat(inf.fileno()).st_size)

        # Oddly, we can't use "for line in inf:" here, because that
        # causes the "tell" method to raise an exception.
        # https://stackoverflow.com/a/42150352/20146
        for batch in line_batcher.batch(inf, 50000):
            work_queue.put(batch)

            progress.update(inf.tell())

    work_queue.join()

    # stop workers
    for t in threads:
        work_queue.put(None)

    for t in threads:
        t.join()
