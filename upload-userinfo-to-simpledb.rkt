#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require (only-in "userinfo.rkt"
                  *userinfo-database-directory-name*
                  sighting->dict
                  sighting-hash
                  sighting-when
                  sighting-who
                  )

         ;; This is a symlink, created thus:

         #|
         ln -s /home/erich/doodles/plt-scheme/web/amazon/ ~/.racket/5.1.1/collects/
         |#
         (only-in amazon/simpledb simpledb-post )
         (only-in amazon/group group)

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

(define (sighting->simpledb-attrs s)
  (define (stringify v)
    (cond
     ((string? v)
      v)
     ((boolean? v)
      (if v "true" "false"))
     ((number? v)
      (number->string v))
     ((list? v)
      (string-join v " "))
     ((symbol? v)
      (stringify (symbol->string v)))))

  (cons
   ;; As far as I can tell, ItemName is more or less the primary key
   ;; for the datum we're about to upload.  So it might as well be a
   ;; hash of the contents.  An autoincrementing integer or a
   ;; timestamp would probably work as well.
   (cons 'ItemName (stringify (sighting-hash s)))
   (append*
    (for/list ([(p i) (in-indexed
                       (sort
                        (for/fold ([result '()])
                            ([(k v) (in-dict (sighting->dict s))])
                            (cons (cons (stringify k)
                                        (stringify v)) result))
                        string<?
                        #:key car))])
      (list
       (cons (string->symbol (format "Attribute.~a.Name"  (add1 i))) (car p))
       (cons (string->symbol (format "Attribute.~a.Value" (add1 i))) (cdr p)))))))

(provide main)
(define (main . args)
  (for ([(things index) (in-indexed (group (snarf-userinfo) 25))])
    (fprintf (current-error-port) "Chunk #~a..." index)
    (simpledb-post
     (append*
      '((DomainName . "frotz")
        (Action     . "BatchPutAttributes"))
      (for/list ([(item i)
                  (in-indexed things)])
        (map
         (lambda (p)
           (cons
            (string->symbol (format "Item.~a.~a" i (symbol->string (car p))))
            (cdr p)))
         (sighting->simpledb-attrs item)
         ))
      ))
    (fprintf (current-error-port) " done~%")))
