import datetime
import os
import pprint
import random
import re
import sys

import arrow                    # pip install arrow
import click                    # pip install click
import elasticsearch            # pip install elasticsearch


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


@click.group()
def main():
    pass


@click.command()
@click.argument('text', nargs=-1)
def get(text):
    iso_3339_now = datetime.datetime.utcnow().isoformat() + 'Z'
    wits_hits = keep_looking_for_witticism(' '.join(text), iso_3339_now)
    if wits_hits:
        print(random.choice(wits_hits[0:HITS_TO_FETCH])['_source']['text'])


def validate_timestamp(ctx, param, value):
    try:
        arrow.get(value)
        return (value)
    except Exception:
        raise click.BadParameter(repr(value))


def validate_speaker(ctx, param, value):
    pattern = r'.+!.+@.+'
    if not re.match(pattern, value):
        raise click.BadParameter("{!r} doesn't match regex {!r}".format(value, pattern))
    return value


@click.command()
@click.argument('speaker', required=True, callback=validate_speaker)
@click.argument('timestamp', required=True, callback=validate_timestamp)
@click.argument('target', required=True)
@click.argument('text', nargs=-1)
def save(speaker, timestamp, target, text):
    payload = {
        'speaker': speaker,
        'timestamp': timestamp,
        'target': target,
        'text': text
    }
    pprint.pprint(payload)


if __name__ == "__main__":
    main.add_command(get)
    main.add_command(save)

    ELASTICSEARCH_DOMAIN_ENDPOINT = os.getenv('ELASTICSEARCH_DOMAIN_ENDPOINT')
    es = elasticsearch.Elasticsearch(hosts=[
        {
            'host': ELASTICSEARCH_DOMAIN_ENDPOINT,
            'use_ssl': True,
            'port': 443,
        }
    ])

    main()
