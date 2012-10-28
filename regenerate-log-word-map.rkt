#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require "corpus.rkt"
         (prefix-in db: db))

;; open the db
;; for each row in the "log" table ...
;;   split the string into words using wordlist->wordset
;;   for each of those words ...
;;      create a new entry in log-word-map consisting of this word, and the rowid from "log"

(provide main)
(define (main . args)
  (let ([conn (db:sqlite3-connect
               #:database "/tmp/corpus.db"
               #:mode 'read/write)])
    (let ([rows-to-process (db:query-value conn "select count(*) from log")])
      (for ([(id row) (db:in-query
                       conn
                       "select rowid, text from log"
                       #:fetch 1000)])
        (displayln rows-to-process)
        (for ([word (string->lowercased-words row)])
          (db:query-exec conn "insert into new_log_word_map values ($1, $2)"
                         word id))
        (set! rows-to-process (sub1 rows-to-process))))))
