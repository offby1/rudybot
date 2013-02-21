#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
FOO=bar=baz exec  racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit
         rackunit/text-ui
         ffi/unsafe)

;; The 'clearenv' function doesn't exist on some systems (notably Mac
;; OS X), so we use 'unsetenv' in a loop instead.
(define (clearenv)
  (let ([unsetenv (get-ffi-obj 'unsetenv #f (_fun _bytes -> _int))])
    (let loop ()
      (match
          (ptr-ref (get-ffi-obj 'environ #f _pointer) _bytes)
        [(regexp #rx"^(.*?)=(.*)$" (list _ k v))
         (unsetenv k)
         (loop)]
        [#f (void)]))))

(define hmm-tests

  (test-suite
   "loop"
   (test-case
    "dunno"
    (clearenv)
    (for ([v '("FOO" "HOME" "PATH" "EDITOR" "SNICKERDOODLE")])
      (check-false (getenv v) v)))))

(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide clearenv main)
