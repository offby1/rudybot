#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         mzlib/trace)

(define (create-logger . args)
  'nada)

(define (rotating-log . stuff)
  "dude, maybe you should write some tests")


(define (sorted-pathlist dir)
  (sort
   (pathlist-closure (list dir))
   string<? #:key path->string))

(define (fold-sorted-files proc init dir)
  (for/fold ([return-value init])
      ([fn (in-list (sorted-pathlist dir))])
      (if (file-exists? fn)
          (proc return-value fn)
          return-value)))

(define (all-file-content dir)
  (fold-sorted-files (lambda (accum fn)
                       (string-append accum (call-with-input-file fn port->string)))
                     ""
                     dir))

(trace all-file-content)
(define (file-sizes dir)
  (reverse
   (fold-sorted-files  (lambda (accum fn)
                         (cons (file-size fn) accum))
                       '()
                       dir)))

(trace file-sizes)
(define rotating-log-tests

  (let ((dir (make-temporary-file "rotating-log-tests~a" 'directory)))
    (test-suite
     "loop"
     #:after (lambda () (delete-directory dir))
     (test-begin
      (let* (
             (logger (create-logger dir 10))
             (data "Hey doodz!  Lookit me getting all logged and shit!!"))
        (log-info data)
        ;; concatenation of all files yields our input data
        (check-equal? (all-file-content dir) data)

        ;; no file is > 10 bytes
        (check-true (andmap (lambda (x)
                              (<= x 10))
                            (file-sizes dir)))

        ;; at most one file is < 10 bytes
        (check-true (<= (length (filter (lambda (x) (< x 10)) (file-sizes dir)))
                        1)))))))

(define (main . args)
  (exit (run-tests rotating-log-tests 'verbose)))
(provide rotating-log main)