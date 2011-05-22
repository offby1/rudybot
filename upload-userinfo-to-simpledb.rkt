#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require (except-in "userinfo.rkt" main))

(define (unwrap thing)
  (match thing
    [(list (list 'sightings s ...) _ ...) s]))

(define (contents p)
  (call-with-input-file p read))

(define (snarf-userinfo)
  (if (directory-exists? (*userinfo-database-directory-name*))
      (map (lambda (fn)
             (unwrap (contents fn)))
           (sort
            (find-files file-exists? (*userinfo-database-directory-name*))
            string<? #:key path->string))
      '()))

(define (sighting->simpledb-attrs s)
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
       (cons (format "Attribute.~a.Name"  (add1 i)) (symbol->string (car p)))
       (cons (format "Attribute.~a.Value" (add1 i)) (cdr p)))))))

(provide main)
(define (main . args)
  (call-with-output-file "/tmp/whee"
    (lambda (op)
      (for ([thing (reverse
                    (for/fold ([all '()])
                        ([sighting-list (snarf-userinfo)])
                        (append (map sighting->simpledb-attrs (sort sighting-list < #:key sighting-when)) all)
                      ))])
        (write thing op)
        (newline op)))
    #:exists 'replace))
