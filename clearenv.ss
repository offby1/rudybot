#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         scheme/foreign)

(unsafe!)

(define (clearenv)
  (let* ((libc (ffi-lib #f))
         (func (get-ffi-obj 'clearenv #f (_fun  ->  _void))))
    (func)))

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
