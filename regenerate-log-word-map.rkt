#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang at-exp racket

;; This program recreates the "log_word_map" table, based on the
;; contents of the "log" table (and then recreates word_popularity
;; based on the contents of log_word_map :-).  I cannot remember why
;; I thought I needed it, but it seems to work, so there's no harm in
;; keeping it.

(require "corpus.rkt"
         (prefix-in db: db)
         racket/date)

(define *batch-size* 2000)

(date-display-format 'iso-8601)

(provide main)
(define (main . args)
  (let ([conn (db:sqlite3-connect
               #:database "temporary-corpus.db"
               #:mode 'read/write)])
    (fprintf (current-error-port) "Deleting from the log_word_map table.  This can take a surprisingly long itme ... ")
    (time (db:query-exec conn "DELETE FROM log_word_map"))
    (fprintf (current-error-port) "done.~%")

    (let ([rows-to-process (db:query-value conn "SELECT COUNT(*) FROM log")])
      (db:start-transaction conn)

      (for ([(id row) (db:in-query
                       conn
                       "SELECT rowid, text FROM log"
                       #:fetch *batch-size*)])

        (for ([word (string->lowercased-words row)])
          (db:query-exec conn "INSERT INTO log_word_map VALUES ($1, $2)"
                         word id))
        (set! rows-to-process (sub1 rows-to-process))

        (when (zero? (remainder rows-to-process *batch-size*))
          (db:commit-transaction conn)

          (printf "~a: ~a rows remaining~%"
                  (date->string (seconds->date (current-seconds)) #t)
                  rows-to-process)

          (db:start-transaction conn)))

      (db:commit-transaction conn))

    ;; Now backfill another table.  Amusingly: word_popularity is just
    ;; a summary of log_word_map, which is itself a summary of log.
    ;; Perhaps a relational database isn't the right tool after all
    ;; (maybe some sorta text-indexing system is).
    (db:query-exec conn "DELETE FROM word_popularity")
    (db:query-exec conn
                   @string-append{
                                  INSERT INTO word_popularity(word, occurrences)
                                       SELECT word, count(word)
                                         FROM log_word_map
                                     GROUP BY word
                                  })))
