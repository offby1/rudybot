#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require srfi/26
         net/url
         (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(provide xlate main)
(define (xlate text from to)
  (call/input-url
   (make-url
    "http" #f "ajax.googleapis.com" #f #t
    (map (cut make-path/param <> '()) (list "ajax" "services" "language" "translate"))
    `((v . "1.0")
      (q . ,text)
      (langpair . ,(format "~a|~a" from to))) #f)
   get-pure-port
   port->string)
  )

(define-test-suite xlate-tests

  (check-equal? "whatever"(xlate "forty-five separate amendments" "en" "it")))

(define (main . args)
  (exit (run-tests xlate-tests 'verbose)))