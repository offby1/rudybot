#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         (except-in "echolalia.ss" main)
         "db.ss"
         "read-db.ss")

(define hmm-tests

  (test-suite
   "loop"
   (check-false
    (lookup "snord" (prefiltered-port->db (open-input-string ""))))
   (check-equal?
    (lookup "snord" (prefiltered-port->db (open-input-string "The snord horde")))
    (list "The snord horde"))

   (check-equal?
    (lookup "snord"
            (prefiltered-port->db (open-input-string
                                   (string-join
                                    (list "snord" "The snord horde" "my snord") "\n"))))
    (list "my snord" "The snord horde" "snord"))))

(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide  main)