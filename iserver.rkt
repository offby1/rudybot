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
       (dprintf "Pretend I'm retrieving something from ~a" inp)
       "incubot is on vacation today"])
    ))
