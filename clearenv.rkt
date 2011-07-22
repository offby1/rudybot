#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         scheme/foreign)

(unsafe!)

(define (clearenv)
  (let ([func (get-ffi-obj 'clearenv #f (_fun  ->  _void))])
    (when (procedure? func)
      (func))))

(define hmm-tests

  (test-suite
   "loop"
   (test-case
    "dunno"
    (clearenv)
    (check-false (getenv "HOME")))
   ))
(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide clearenv main)
