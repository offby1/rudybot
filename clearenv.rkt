#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         ffi/unsafe)

;; Doesn't work on OS X, alas
(define clearenv
  (with-handlers ([exn? (lambda (e)
                          (fprintf (current-error-port)
                                   "~s: can't get an FFI object for clearenv; using dummy function instead~%" e)
                          (thunk
                           (fprintf (current-error-port)
                                    "Warning!  Not clearning environment~%")))
                        ])
  (get-ffi-obj 'clearenv #f (_fun -> _void))))

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
