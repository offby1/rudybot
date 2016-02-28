#lang at-exp racket

(require
 (prefix-in db: db)
 (only-in mzlib/etc this-expression-source-directory)
 (only-in "utils.rkt" safely)
 (only-in "vars.rkt" *incubot-logger*))


(define *db-file-name*
  (make-parameter
   (build-path (this-expression-source-directory)
               "corpus.db")))

(define (add-sentence conn s)
   (safely
    (db:query-exec
     conn
     "INSERT INTO f_log VALUES (?)"
     s)))


(define (tokens-by-popularity conn s)
  (db:query-list conn
                 @string-append{
                                SELECT  DISTINCT(tok1.token)
                                FROM    tok1
                                JOIN    ft_terms ON ft_terms.term = tok1.token
                                WHERE   INPUT=?
                                AND     ft_terms.col = '*'
                                ORDER BY ft_terms.occurrences ASC
                                }
                 s))


(define (random-offset conn)
  (db:query-value conn
                  "select abs(random()) % (max(rowid)) from f_log"))

(define (safe-take lst pos)
  (take lst (min pos (length lst))))

(define (find-witticism conn s)
  (let ([tokes (tokens-by-popularity conn s)])
    (let loop ([ro (random-offset conn)]
               [tokes (safe-take tokes 4)])
      (define match-me (string-join tokes " "))
      (and (not (null? tokes))
           ((*incubot-logger*) (format "Matching ~s from offset ~a" match-me ro))
           (let ([texts (db:query-list conn
                                       @string-append{
                                                      SELECT  f_log.text
                                                      FROM    f_log
                                                      WHERE   f_log.text MATCH ?
                                                      AND     f_log.rowid > ?
                                                      ORDER   BY f_log.rowid ASC
                                                      LIMIT   1
                                                      }
                                       match-me ro)])
             ;; If we couldn't find anything, then try again, increasing
             ;; our odds in two different ways: 1) halve the random offset,
             ;; so that we're examining more rows; 2) remove a word from
             ;; the string we're passing to MATCH.
             (if (null? texts)
                 (begin
                   ((*incubot-logger*) (format "Nothing; trying again"))
                   (loop (quotient ro 2)
                         (drop-right tokes 1)))

                 (begin
                   ((*incubot-logger*) (format "W00t" ))
                   (car texts))))))))

(provide make-incubot-server)
(define (make-incubot-server)
  (define connection (db:sqlite3-connect
                      #:database (*db-file-name*)))
  (lambda (command-sym inp)
    (match command-sym
      ['put-string
       (add-sentence connection inp)]
      ['get
       (find-witticism connection (string-join inp " "))])
    ))

(module+ main
  (define (prep-db conn)
    (for ([command '("CREATE VIRTUAL TABLE f_log USING FTS4(text TEXT)"
                     "CREATE VIRTUAL TABLE ft_terms USING fts4aux(f_log)"
                     "CREATE VIRTUAL TABLE tok1 USING fts3tokenize('simple')")])
      (db:query-exec conn command)))
  (define test-db-connection (db:sqlite3-connect
                              #:database 'memory))
  (parameterize ([*incubot-logger* (lambda args   (let ([op (current-error-port)])
                                                    (apply fprintf op args)
                                                    (newline op)))])
    (prep-db test-db-connection)
    (for ([inp '( "Hello, world")])
      (add-sentence test-db-connection inp))
    (for ([probe '("What's happening?!"
                   "What in the world is going on?!")])
      (printf "~a => ~a~%" probe (find-witticism test-db-connection probe)))
    )
)
