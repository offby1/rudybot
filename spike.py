import collections
import sqlite3


def tokenize(sentence):
    with sqlite3.connect('corpus.db') as conn:
        hits_by_token = collections.defaultdict(set)
        for token in sentence.strip().split():

            for row in conn.execute("SELECT text FROM f_log WHERE text match ? LIMIT 10", (token,)):
                hits_by_token[token].add(row)

    return hits_by_token

import pprint
pprint.pprint(dict(tokenize("The rain in Spain falls mainly in the #emacs")))
