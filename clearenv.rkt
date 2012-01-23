#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         ffi/unsafe)

;; The 'clearenv' function doesn't exist on some systems (notably Mac
;; OS X), so we use this instead.
(define (clobber-environment-by-hand)
  (let ([environment (get-ffi-obj 'environ #f _pointer)])
    (let loop ([offset 0])
      (let ([one-pair (ptr-ref environment _bytes offset)])
        (when one-pair
          (match-let
              ([(list k v) (regexp-split #rx"=" one-pair)])
            ;; TODO -- I can imagine all kinds of horror if an
            ;; environment variable's name isn't a UTF-8 encoded
            ;; string.
            (putenv (bytes->string/utf-8 k) ""))
          (loop (add1 offset)))))))

(define clearenv
  (get-ffi-obj 'clearenv #f (_fun -> _void)
               (thunk
                (fprintf (current-error-port)
                         "Can't get an FFI object for clearenv; setting existing values to the empty string instead~%")
                clobber-environment-by-hand)))

(define hmm-tests

  (test-suite
   "loop"
   (test-case
    "dunno"
    (clearenv)
    (check-true (or (not (getenv "HOME"))
                    (equal? "" (getenv "HOME")))))
   ))
(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide clearenv main)
