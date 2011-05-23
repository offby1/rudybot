#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require (only-in "userinfo.rkt"
                  *userinfo-database-directory-name*
                  sighting->dict
                  sighting-when
                  sighting-who
                  )

         ;; This is a symlink, created thus:

         #|
         ln -s /home/erich/doodles/plt-scheme/web/amazon/ ~/.racket/5.1.1/collects/
         |#
         (only-in amazon/simpledb simpledb-post)

         ;; Eventually I should make my amazon package presentable,
         ;; and upload it to PLaneT.
         )

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
  (define (stringify v)
    (cond
     ((string? v)
      v)
     ((boolean? v)
      (if v "true" "false"))
     ((number? v)
      (number->string v))
     ((list? v)
      (string-join v " "))))

  (cons
   (cons 'ItemName (sighting-who s))
   (append*
    (for/list ([(p i) (in-indexed
                       (sort
                        (for/fold ([result '()])
                            ([(k v) (in-dict (sighting->dict s))])
                            (cons (cons k (stringify v)) result))
                        string<?
                        #:key (compose symbol->string car)))])
      (list
       (cons (string->symbol (format "Attribute.~a.Name"  (add1 i))) (format "~a.~a" index (car p)))
       (cons (string->symbol (format "Attribute.~a.Value" (add1 i))) (cdr p)))))))

(provide main)
(define (main . args)
  (let loop ([h (make-immutable-hash '())]
             [things (snarf-userinfo)])
    (when (not (null? things))
      (let* ([key (sighting-who (car things))]
             [h (hash-update h key add1 -1)])
        (simpledb-post (append
                        '((DomainName . "frotz")
                          (Action . "PutAttributes"))
                        (sighting->simpledb-attrs
                         (car things)
                         (hash-ref h key))))
        (loop h (cdr things))))))
