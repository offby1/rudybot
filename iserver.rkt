#lang racket

(require
 (only-in "vars.rkt" *incubot-logger*)
 unstable/debug)

(provide make-incubot-server)
(define (make-incubot-server)
  (lambda (command-sym inp)
    (match command-sym
      ['put-string
       (dprintf "Pretend I'm saving ~s somewhere" inp)]
      ['get
       (dprintf "Pretend I'm retrieving something from ~a" inp)
       "incubot is on vacation today"])
    ))
