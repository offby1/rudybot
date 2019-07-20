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

import botocore.configloader    # pip install botocore
import elasticsearch            # pip install elasticsearch
import elasticsearch.helpers
import progressbar              # pip install progressbar2
from requests_aws4auth import AWS4Auth # pip install requests-aws4auth


def _delete_everything_and_start_over(es):
    es.delete_by_query(index='messages',
                       body={'query': {'match_all': {}}},
                       conflicts='proceed',
                       request_timeout=60)


# I don't want the actual URL here since the server's permissions are
# too lax, and anything in this file will wind up on github
# This should be the same as the value in ~/.racket/racket-prefs.rktd
ELASTICSEARCH_DOMAIN_ENDPOINT = os.getenv('ELASTICSEARCH_DOMAIN_ENDPOINT')


def short_hash(stuff):
    return hashlib.sha256(stuff).hexdigest()[0:8]


def ingest_one_batch(es, batch_of_lines, newest_already_uploaded_timestamp):
    if newest_already_uploaded_timestamp is None:
        newest_already_uploaded_timestamp = ''

    index_action_dicts = []
    for line in batch_of_lines:
        data = json.loads(line)
        timestamp = data['timestamp']
        if timestamp >= newest_already_uploaded_timestamp:
            data.update(
                {
                    '_index': 'messages',
                    '_type': 'message',
                    '_id': short_hash(line.encode('utf-8'))
                })
            index_action_dicts.append(data)

    if index_action_dicts:
        elasticsearch.helpers.bulk(es, index_action_dicts)

    return sum((len(l) for l in batch_of_lines))


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


def _get_hwm(es):
    """Return the timestamp of the newest message currently in elasticsearch.

    This lets us save gobs of time when bulk-uploading, by simply
    skipping documents that we've presumably already uploaded.

    """
    # e.g. {'hits': {'hits': [{'_source': {'timestamp': '2017-02-15T10:40:07Z'}}]}}
    result = es.search (index='messages',
                        size=1,
                        filter_path=['hits.hits._source.timestamp'],
                        sort='timestamp:desc')
    if not result:
        return None

    return result['hits']['hits'][0]['_source']['timestamp']


if __name__ == "__main__":
    es = elasticsearch.Elasticsearch(
        connection_class=elasticsearch.RequestsHttpConnection,
        hosts=[{'host': ELASTICSEARCH_DOMAIN_ENDPOINT, 'port': 443,}],
        http_auth=AWS4Auth(
            botocore.configloader.multi_file_load_config("~/.aws/credentials")['profiles']['default']['aws_access_key_id'],
            botocore.configloader.multi_file_load_config("~/.aws/credentials")['profiles']['default']['aws_secret_access_key'],
            botocore.configloader.multi_file_load_config("~/.aws/config")['profiles']['default']['region'],
            'es'),
        use_ssl=True,
        verify_certs=True,
    )

    newest_already_uploaded_timestamp = _get_hwm(es)

    print('Will only upload records more recent than {}'.format(newest_already_uploaded_timestamp))

    with open('big-log.json') as inf:
        with progressbar.ProgressBar(max_value=os.fstat(inf.fileno()).st_size) as progress:
            with concurrent.futures.ThreadPoolExecutor(max_workers=NUMBER_OF_WORKER_THREADS) as executor:
                futures = []
                for batch in line_batcher(inf, LINES_PER_BATCH):
                    f = executor.submit(ingest_one_batch, es, batch, newest_already_uploaded_timestamp)
                    f.add_done_callback(lambda f: progress.update(progress.value + f.result()))
                    futures.append(f)

                    if len(futures) == NUMBER_OF_WORKER_THREADS:
                        concurrent.futures.wait(futures)
                        futures = []
