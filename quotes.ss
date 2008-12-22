#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(require (planet offby1/offby1/fys))

(define *the-channel* (make-channel))

(define *dealer*
  (thread
   (lambda ()
     (let re-read ()
       (fprintf (current-error-port)
                "Reading quotes file~%")
       (let push-one ((all (fisher-yates-shuffle (call-with-input-file "quotes" read))))
         (if (null? all)
             (re-read)
             (begin
               (channel-put *the-channel* (car all))
               (push-one (cdr all)))))))))

(provide one-quote)
(define (one-quote)
  (channel-get *the-channel*))

(provide main)
(define (main . args)
  (display (one-quote))
  (newline))
