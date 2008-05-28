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
      (check-equal? s (lookup-sighting "1"))))))

(define (main . args)
  (exit (test/text-ui sighting-tests 'verbose)))
(provide (all-defined-out))

