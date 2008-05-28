#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#
#lang scheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" ))
         (except-in "sighting.ss" main))


(define sighting-tests

  (test-suite
   "sighting-test"
   (test-case
    "yow"
    (let ((s (make-sighting "1" "2" 3 #t (list "hey" "you"))))
      (note-sighting s)
      (check-equal? s (lookup-sighting "1"))
      (check-false (lookup-sighting "snorkuplexity"))))))

(define (main . args)
  ;; fill up the sightings table with silliness, just to see if we can
  (for ((num (in-range 10)))
    (let ((s (make-sighting (number->string (random 10000))
                            "silly"
                            num
                            (odd? num)
                            (list "ho" "hum"))))
      (note-sighting s)))
  (exit (test/text-ui sighting-tests 'verbose)))
(provide (all-defined-out))

