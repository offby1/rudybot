#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
PLTSTDERR=debug ; export PLTSTDERR
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require
 (only-in db
          query-exec
          query-value
          sqlite3-connect))

(define *db-file-name*  "/tmp/buggissimo")

(with-handlers ([exn:fail:filesystem?
                 (lambda (e)
                   (fprintf (current-error-port) "No ~s present; no problem.~%" *db-file-name*))])
  (delete-file *db-file-name*)
  (fprintf (current-error-port) "Nuked ~s~%" *db-file-name*))


(define db
  (sqlite3-connect
   #:database *db-file-name*
   #:mode 'create))

(query-exec
 db
 "CREATE TABLE IF NOT EXISTS
        log_word_map(word TEXT, log_id INTEGER,
            PRIMARY KEY (word, log_id)
            ON CONFLICT FAIL)")

(query-exec db "BEGIN TRANSACTION")

(for ([x (in-naturals)])
  (query-exec
   db
   "insert into log_word_map values (?, ?)"
   (number->string x) x))

(query-exec db "COMMIT")
