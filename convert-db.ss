#lang scheme

(require "sighting.ss")

(define *old-db-file-name* "old.db")

(for (((k v) (in-hash (call-with-input-file *old-db-file-name* read))))
  (printf "~a ...~%" k)
  (note-sighting v))
