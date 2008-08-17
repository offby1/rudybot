#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(require
 (planet schematics/schemeunit:3)
 (planet schematics/schemeunit:3/text-ui))

(define (process ip op)
  (for ((line (in-lines ip)))
    (fprintf op "Let's see if we can parse this line ... ")
    (let ((ip (open-input-string line)))
      (with-handlers
           ([exn:fail:read?
             (lambda (e)
               (fprintf op
                "Guess not: ~s~%" (exn-message e)))])

      (fprintf op "~s~%" (read ip)))
      (let ((leftovers (read-line ip)))
        (when (string? leftovers)
          (fprintf
           (current-error-port)
           "Leftover crud in ~a: ~s~%" ip leftovers))))))

(define (one-test inp expected)
  (let ((results (open-output-string)))
    (process (open-input-string inp) results)
    (check-equal? (get-output-string results) expected)))

(define (main . args)
  (one-test  "yo ho ho"  "Everything went peachy!!"))

(provide main)
