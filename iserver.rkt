#lang at-exp racket

(require
 (prefix-in db: db)
 (only-in mzlib/etc this-expression-source-directory)
 (only-in "vars.rkt" *incubot-logger*)
 unstable/debug)

(define *db-file-name*
  (make-parameter
   (build-path (this-expression-source-directory)
               "corpus.db")))

(define (add-sentence conn s)
   (db:query-exec
    conn
    "INSERT INTO f_log VALUES (?)"
    s))


(define (rarest-token conn s)
  (for/fold ([token "wtf?"]
             [popularity +inf.0])
      ([(this-token this-popularity)
        (db:in-query conn
                     @string-append{
                                    SELECT  tok1.token, ft_terms.occurrences
                                    FROM    tok1
                                    JOIN    ft_terms ON ft_terms.term = tok1.token
                                    WHERE   INPUT=?
                                    AND     ft_terms.col = '*'
                                    }
                     s)])
    (if (< this-popularity popularity)
        (values this-token this-popularity)
        (values token popularity))))


(define (random-offset conn)
  (db:query-value conn
                  "select abs(random()) % (max(rowid)) from f_log"))

(define (find-witticism conn s)
  (let-values ([(rarest pop) (rarest-token conn s)])
    (and (integer? pop)
         (let ([ro (random-offset conn)])
           (db:query-value conn
            @string-append{
SELECT  f_log.text
FROM    f_log
WHERE   f_log.text MATCH ?
AND     f_log.rowid > ?
ORDER   BY f_log.rowid ASC
LIMIT   1
}
            s ro)))))

(provide make-incubot-server)
(define (make-incubot-server)
  (define connection (db:sqlite3-connect
                      #:database (*db-file-name*)))
  (dprintf "Connected to ~a: ~a" (*db-file-name*) connection)
  (lambda (command-sym inp)
    (match command-sym
      ['put-string
       (add-sentence connection inp)]
      ['get
       (find-witticism connection (string-join inp " "))])
    ))
