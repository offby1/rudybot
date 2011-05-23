#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require (except-in "userinfo.rkt" main))

(define (snarf-userinfo)

  (define (unwrap thing)
    (match thing
      [(list (list 'sightings s ...) _ ...) s]))

  (if (directory-exists? (*userinfo-database-directory-name*))
      (sort
       (append-map (lambda (fn)
              (unwrap (call-with-input-file fn read)))
            (find-files file-exists? (*userinfo-database-directory-name*)))
       string<?
       #:key (lambda (s) (format "~a ~a" (sighting-who s) (sighting-when s))))
      '()))

(define (sighting->simpledb-attrs s [index 0])
  (cons
   (cons (format "ItemName") (sighting-who s))
   (append*
    (for/list ([(p i) (in-indexed
                       (sort
                        (for/fold ([result '()])
                            ([(k v) (in-dict (sighting->dict s))])
                            (cons (cons k v) result))
                        string<?
                        #:key (compose symbol->string car)))])
      (list
       (cons (format "Attribute.~a.Name"  (add1 i)) (format "~a.~a" index (car p)))
       (cons (format "Attribute.~a.Value" (add1 i)) (cdr p)))))))

(provide main)
(define (main . args)
  (call-with-output-file "/tmp/whee"
    (lambda (op)
      (let loop ([h (make-immutable-hash '())]
                 [things (snarf-userinfo)])
        (when (not (null? things))
          (let* ([key (sighting-who (car things))]
                 [h (hash-update h key add1 -1)])
            (write (sighting->simpledb-attrs (car things) (hash-ref h key)) op)
            (newline op)
            (loop h (cdr things))))))
    #:exists 'replace))
