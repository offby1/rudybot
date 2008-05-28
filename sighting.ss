#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(define *sightings-database-file-name* "sightings.db")

(define-struct sighting (who where when was-action? words) #:prefab)

(define *sightings* #f)

(define (maybe-load!)
  (unless *sightings*
    (set! *sightings*
          (with-handlers
              ([exn:fail:filesystem?
                (lambda (e)
                  (make-hash))])
            (hash-copy (call-with-input-file *sightings-database-file-name* read))))))

(define (lookup-sighting who)
  (maybe-load!)
  (hash-ref *sightings* who #f))

(define (note-sighting s)
  (maybe-load!)
  (hash-set! *sightings* (sighting-who s) s)
  (call-with-output-file *sightings-database-file-name*
    (lambda (op)
      (write *sightings* op))
    #:exists 'truncate/replace))

(provide/contract
 [struct sighting ((who string?)
                   (where string?)
                   (when natural-number/c)
                   (was-action? boolean?)
                   (words (listof string?)))]

 [lookup-sighting (-> string? (or/c sighting? not))]
 [note-sighting (-> sighting? void?)])

(define (main . args)
  '|golly gee!|)

(provide main)