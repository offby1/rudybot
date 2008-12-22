#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         "incubot.ss")

(require/expose "incubot.ss" (make-db db?))

(define hmm-tests

  (test-suite
   "loop"
   (check-false
    (lookup "snord" (port->db (open-input-string ""))))
   (check-equal?
    (lookup "snord" (port->db (open-input-string "The snord horde")))
    "The snord horde")

   (test-case
    "Returns longest of multiple matching strings"
    (check-equal?
     (lookup "snord" (port->db (open-input-string (string-join (list "snord" "The snord horde" "my snord") "\n"))))
     "The snord horde"))))

(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide  main)