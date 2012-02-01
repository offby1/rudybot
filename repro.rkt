;; rm -rfv /tmp/bug* ; racket ./repro.rkt

#lang racket

(require
 (only-in db
          query-exec
          query-value
          sqlite3-connect))

(define db
  (sqlite3-connect #:database "/tmp/buggissimo" #:mode 'create))

(query-exec
 db
 "CREATE TABLE IF NOT EXISTS
        log_word_map(word TEXT, log_id INTEGER,
            PRIMARY KEY (word, log_id)
            ON CONFLICT FAIL)")

(query-exec db "BEGIN TRANSACTION")

(for ([x (in-range 200000)])
  (query-exec
   db
   "insert into log_word_map values (?, ?)"
   (number->string x) x))

(query-exec db "COMMIT")
