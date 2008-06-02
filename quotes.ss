#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(define (one-quote)
  (let ((all (call-with-input-file "quotes" read)))
    (list-ref all
     (random (length all)))))

(define (main . args)
  (display (one-quote))
  (newline))
(provide (all-defined-out))

