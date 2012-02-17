#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         ffi/unsafe)

;; The 'clearenv' function doesn't exist on some systems (notably Mac
;; OS X), so we use 'unsetenv' in a loop instead.
(define (clearenv)
  (let ([unsetenv (get-ffi-obj 'unsetenv #f (_fun _bytes -> _int))])
    (let loop ()
      (let ([one-pair (ptr-ref (get-ffi-obj 'environ #f _pointer) _bytes)])
        (when one-pair
          (match-let ([(list k v ...) (regexp-split #rx"=" one-pair)])
            (unsetenv k))
          (loop))))))

(define hmm-tests

  (test-suite
   "loop"
   (test-case
    "dunno"
    (clearenv)
    (for ([v '( "HOME" "PATH" "EDITOR")])
      (check-false (getenv v))))))

(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide clearenv main)
