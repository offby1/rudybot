import operator
import sqlite3


# This requires some preparation that I haven't yet coded anywhere:

# CREATE VIRTUAL TABLE f_log USING FTS4(text TEXT);
# INSERT INTO f_log(text) SELECT text FROM log;
# CREATE VIRTUAL TABLE ft_terms USING fts4aux(f_log);
# CREATE VIRTUAL TABLE tok1 USING fts3tokenize('simple');

def tokes_and_occurences(connection, sentence):
    for row in connection.execute("""
    SELECT tok1.token, ft_terms.occurrences
    FROM tok1
    JOIN ft_terms ON ft_terms.term = tok1.token
    WHERE INPUT=?
    AND ft_terms.col = '*'
    """, (sentence,)):
        yield row


def rarest_token(connection, sentence):
    toke_pops = tokes_and_occurences(connection, sentence)

    try:
        return min(toke_pops, key=operator.itemgetter(1))[0]
    except ValueError:
        return None


def something(sentence):
    with sqlite3.connect('corpus.db') as conn:
        rarest = rarest_token(conn, sentence)

        if rarest is None:
            return 'saywhat?'

        random_offset = next(conn.execute("""select abs(random()) % (max(rowid)) from f_log"""))[0]

        (text,) = next(conn.execute("""
        SELECT f_log.text
        FROM f_log
        WHERE f_log.text MATCH ?
        AND f_log.rowid > ?
        ORDER BY f_log.rowid ASC
        LIMIT 1
        """, (rarest, random_offset)))

        return text


for sentence in ("The rain in Spain falls mainly in the #emacs",
                 "I wonder if you really want to hurt me",
                 "Do you really want to make me cry?",
                 "aaackthppppt"):
    print('{} => {}'.format(sentence, something(sentence)))
