import os
import pprint
import random
import sys

import click
import elasticsearch


def _e(*args, **kwargs):
    print(*args, **dict(kwargs, file=sys.stderr))


def keep_looking_for_witticism(*args):
    for minimum in ("90%", "60%", "30%"):
        hits = find_witticism(*args, minimum_should_match=minimum)
        if hits:
            _e('Found {} hits with {}'.format(len(hits), minimum))
            return hits
        else:
            _e('No hits with {}'.format(minimum))
    _e('Nuts')


HITS_TO_FETCH = 10


def find_witticism(input_string, timestamp_of_input, minimum_should_match):
    # Not sure this is worthwhile:

    #    split intput string into words
    #    rank words by interesting-ness
    #    choose top few most interesting words

    # The results without it seem pretty decent.

    # construct query like the above
    # get top few highest-scoring hits from elasticsearch
    # eliminate any uttered within a few seconds before our input_string
    # perhaps eliminate any that entirely contain the input string as a substring
    # choose randomly from a few of the top-ranked of those left

    # https://www.elastic.co/guide/en/elasticsearch/reference/5.4/query-dsl-common-terms-query.html

    response = es.search(
        body={
            "query": {
                "bool": {
                    "must": {
                        "common": {
                            "text": {
                                "query": input_string,
                                "cutoff_frequency": 0.01,
                                "minimum_should_match": minimum_should_match
                            }
                        }
                    },
                    "filter": {
                        "range": {
                            "timestamp": {
                                "lt": timestamp_of_input + '||-2h'
                            }
                        }
                    }
                }
            }
        },
        size=HITS_TO_FETCH)
    _e(pprint.pformat(response))
    hits = response['hits']['hits']
    return hits


@click.command()
@click.argument('text', nargs=-1)
def main(text):
    wits_hits = keep_looking_for_witticism(' '.join(text), "2017-05-28T22:54:14Z")
    if wits_hits:
        print(random.choice(wits_hits[0:HITS_TO_FETCH])['_source']['text'])


if __name__ == "__main__":
    ELASTICSEARCH_DOMAIN_ENDPOINT = os.getenv('ELASTICSEARCH_DOMAIN_ENDPOINT')
    es = elasticsearch.Elasticsearch(hosts=[
        {
            'host': ELASTICSEARCH_DOMAIN_ENDPOINT,
            'use_ssl': True,
            'port': 443,
        }
    ])

    main()
