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

(provide main)
(define (main . args)
  (printf "Hi~%")
  (call-with-output-file
      "/tmp/whee"
    (lambda (op)
      (for ([sighting-list (snarf-userinfo)])
        (for ([s (sort sighting-list < #:key sighting-when)])
          (write s op)
          (newline op))))
    #:exists 'replace))
