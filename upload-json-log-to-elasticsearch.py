"""Read the log stream produced by log-to-json.rkt, and upload each
entry to elasticsearch.

"""

import hashlib
import os
import pprint
import progressbar              # pip install progressbar2
import requests

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


if __name__ == "__main__":
    with open('big-log.json', 'rb') as inf:
        progress = progressbar.ProgressBar(max_value=os.fstat(inf.fileno()).st_size)

        # Oddly, we can't use "for line in inf:" here, because that
        # causes the "tell" method to raise an exception.
        # https://stackoverflow.com/a/42150352/20146
        line = inf.readline()
        lines_read = 1
        while line:
            url = compute_document_url(line)
            response = requests.put(url=url, data=line).json()

            if response.get('error'):
                pprint.pprint(response)
                exit(1)

            if (lines_read % 100) == 0:
                progress.update(inf.tell())

            line = inf.readline()
            lines_read += 1
