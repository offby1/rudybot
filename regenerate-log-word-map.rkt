#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require "corpus.rkt"
         (prefix-in db: db)
         racket/date)

;; Before running this, prep the db by hand:
;; CREATE TABLE new_log_word_map(word TEXT, log_id INTEGER);

(define *batch-size* 10000)

(date-display-format 'iso-8601)

(provide main)
(define (main . args)
  (let ([conn (db:sqlite3-connect
               #:database "corpus.db"
               #:mode 'read/write)])
    (let ([rows-to-process (db:query-value conn "select count(*) from log")])
      (db:start-transaction conn)

      (for ([(id row) (db:in-query
                       conn
                       "select rowid, text from log"
                       #:fetch *batch-size*)])

        (for ([word (string->lowercased-words row)])
          (db:query-exec conn "insert into new_log_word_map values ($1, $2)"
                         word id))
        (set! rows-to-process (sub1 rows-to-process))

        (when (zero? (remainder rows-to-process *batch-size*))
          (db:commit-transaction conn)

          (printf "~a: ~a rows remaining~%"
                  (date->string (seconds->date (current-seconds)) #t)
                  rows-to-process)

          (db:start-transaction conn)))

      (db:commit-transaction conn)
      )))
