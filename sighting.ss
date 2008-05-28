#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(define *sightings-database-file-name* "sightings.db")

(define-struct sighting (who where when was-action? words) #:prefab)

(define (lookup-sighting who)
  #s(sighting "1" "2" 3 #t ("hey" "you")))

(define (note-sighting s)
  (void))

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