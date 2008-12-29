#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(define (is-power-of-two? x)
  (and (integer? x)
       (positive? x)
       (exact? x)
       (or (= x 1)
           (and (even? x)
                (is-power-of-two? (/ x 2))))))

(define single-arg-proc/c
  (flat-named-contract "single-arg-proc" (lambda (thing) (and (procedure? thing) (equal? 1 (procedure-arity thing))))))
(define thunk/c
  (flat-named-contract "thunk"           (lambda (thing) (and (procedure? thing) (equal? 0 (procedure-arity thing))))))
(provide/contract (make-notifier [single-arg-proc/c . -> . thunk/c]))

(define (make-notifier single-arg-proc)
  (let ((times-called 0))
    (lambda ()
      (set! times-called (add1 times-called))
      (when (is-power-of-two? times-called)
        (single-arg-proc times-called)))))

(provide main)
(define (main . args)
  (define note-progress!
    (make-notifier
     (lambda (times-called)
       (fprintf
        (current-output-port)
        "Yo vinnie -- I been called ~a times already~%" times-called))))
  (for ([i (in-range 10)])
    (note-progress!))
  )
