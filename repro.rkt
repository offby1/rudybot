#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require scheme/sandbox)

(provide main)
(define (main . args)
  (let ([sandbox
         (parameterize ([sandbox-output 'string])
           (make-evaluator '(begin (require scheme))))])
    (sandbox "(begin (display \"I think I've found a bug in rudybot\") 9)")
    (write (get-output sandbox))
    (newline)))
