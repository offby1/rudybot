import os

import botocore.configloader    # pip install botocore
import elasticsearch            # pip install elasticsearch
from requests_aws4auth import AWS4Auth # pip install requests-aws4auth


# I don't want the actual URL here since the server's permissions are
# too lax, and anything in this file will wind up on github
# This should be the same as the value in ~/.racket/racket-prefs.rktd
ELASTICSEARCH_DOMAIN_ENDPOINT = os.getenv('ELASTICSEARCH_DOMAIN_ENDPOINT')


def get_es_client():
    return elasticsearch.Elasticsearch(
        connection_class=elasticsearch.RequestsHttpConnection,
        hosts=[{'host': ELASTICSEARCH_DOMAIN_ENDPOINT, 'port': 443}],
        http_auth=AWS4Auth(
            botocore.configloader.multi_file_load_config("~/.aws/credentials")['profiles']['default']['aws_access_key_id'],
            botocore.configloader.multi_file_load_config("~/.aws/credentials")['profiles']['default']['aws_secret_access_key'],
            botocore.configloader.multi_file_load_config("~/.aws/config")['profiles']['default']['region'],
            'es'),
        use_ssl=True,
        verify_certs=True,
    )
