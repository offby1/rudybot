"""Read the log stream produced by log-to-json.rkt, and upload each
entry to elasticsearch.

Obviously this is forehead-smackingly inefficient, but so far I've
been to lazy to break out boto3 and do it properly.

"""

import hashlib
import json
import os
import subprocess

# I don't want the actual URL here since its permissions are too lax,
# and anything in this file will wind up on github
ELASTICSEARCH_DOMAIN_ENDPOINT = os.getenv('ELASTICSEARCH_DOMAIN_ENDPOINT')

ELASTICSEARCH_DOMAIN_MESSAGE_CONTAINER = '/messages/message'


def short_hash(stuff):
    return hashlib.sha256(stuff).hexdigest()[0:4]


def compute_message_id(entry):
    return '{}-{}'.format(entry['timestamp'], short_hash(entry['text'].encode('utf-8')))


def compute_document_url(entry):
    return '{}/{}/{}'.format(
        ELASTICSEARCH_DOMAIN_ENDPOINT,
        ELASTICSEARCH_DOMAIN_MESSAGE_CONTAINER,
        compute_message_id(entry))


if __name__ == "__main__":
    with open('big-log.json') as inf:
        for line in inf:
            entry = json.loads(line)

            url = compute_document_url(entry)
            commandline = ['curl', '-XPUT', url, '-d', json.dumps(entry)]
            curl_process = subprocess.run(commandline, stdout=subprocess.PIPE)
            response = json.loads(curl_process.stdout)
            Message = response.get('Message')
            if Message and "not authorized" in Message:
                exit(1)
