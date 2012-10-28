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
    (for ([(id row) (db:in-query
                     conn
                     "select rowid, text from log"
                     #:fetch 10)])
      (for ([word (string->lowercased-words row)])
        (displayln (cons id word))))))
