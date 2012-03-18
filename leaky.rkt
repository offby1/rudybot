#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
echo "Watch my memory usage with 'htop' or similar"
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require
 (only-in db
          commit-transaction
          query-exec
          sqlite3-connect
          start-transaction
          )
 (only-in "corpus.rkt"
          add-sentence-to-corpus
          corpus-db
          make-corpus))

(define (pe fmt . args)
  (apply fprintf (current-error-port) fmt args))

(provide main)
(define (main . args)
  (let ([corpus (make-corpus)])

    (start-transaction (corpus-db corpus) )

    (for ([current-line (in-range 5000000)])
      (add-sentence-to-corpus  "text, shmext!!" corpus)

      (when (zero? (remainder current-line 10000))
        (commit-transaction (corpus-db corpus))
        (start-transaction (corpus-db corpus))
        (pe
         "Line ~a (~a megabytes in use) ~%"
         current-line
         (inexact->exact (round (/ (current-memory-use) 1024 1024.))))))
    (commit-transaction (corpus-db corpus) )

    (pe "done~%")))
