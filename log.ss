#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(define *receiver* (make-log-receiver (current-logger)
                                      'debug))

(define *log-file-name* "sandbox-log")

(define logging-thread
  (thread
   (lambda ()
     (let loop ()
       (match (sync *receiver*)
         [(vector level string marks)
          (call-with-output-file
              *log-file-name*
              (lambda (op)
                (fprintf
                 op
                 "~s: ~a: ~s~%"
                 level
                 string
                 (and marks (continuation-mark-set->context marks))))
              #:exists 'append)])
       (loop)))))

(provide main)

(define (main . args)
  (log-fatal "Oh noooo!!")
  (printf "Waiting for logging thread ...~%")
  (sleep 1)
  (printf "Now look at ~a and see if I wrote some stuff to it.~%" *log-file-name*))
