import sqlite3


def tokenize(sentence):
    with sqlite3.connect('corpus.db') as conn:
        hits_by_token = []
        for token in sentence.strip().split():

            random_offset = next(conn.execute("""select abs(random()) % (max(rowid)) from f_log"""))[0]

            # I suspect that sqlite interprets certain upper-case
            # words like AND as syntax
            token = token.lower()

            for text in conn.execute("""
            SELECT text
            FROM f_log
            WHERE text MATCH ?
            AND rowid > ?
            ORDER BY rowid ASC
            LIMIT 1
            """,
                                    (token, random_offset)):
                hits_by_token.append((token, text))

    return hits_by_token

import pprint
pprint.pprint(tokenize("The rain in Spain falls mainly in the #emacs"), width=250)
