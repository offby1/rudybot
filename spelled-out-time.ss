#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "numspell.ss" ("neil" "numspell.plt")))

(define (number->english/plural n unit-name)
  (string-append (number->english n)
                 " "
                 unit-name
                 (if (equal? 1 n)
                     ""
                   "s")))

(define (maybe n unit-name)
  (if (positive? n)
      (format ", ~a"
              (number->english/plural n unit-name))
    ""))

(define (spelled-out-time seconds)
  (let* ((minutes (floor (/ seconds 60)))
         (hours   (floor (/ minutes 60)))
         (days    (floor (/ hours 24))))
    (cond
     ((positive? days)
      (string-append
       (number->english/plural days "day")
       (maybe (remainder hours 24) "hour")))
     ((positive? hours)
      (string-append
       (number->english/plural (remainder hours 24) "hour")
       (maybe (remainder minutes 60) "minute")))
     ((positive? minutes)
      (string-append
       (number->english/plural (remainder minutes 60) "minute")
       (maybe (remainder seconds 60) "second")))
     (else
      (number->english/plural seconds "second")))))


(define spelled-out-time-tests

  (test-suite
   "spelled-out-time"
   (test-equal? "one second"          (spelled-out-time 1) "one second")
   (test-equal? "two seconds"         (spelled-out-time 2) "two seconds")
   (test-equal? "twenty-five seconds" (spelled-out-time 25) "twenty-five seconds")
   (test-equal? "two minutes, three seconds" (spelled-out-time 123) "two minutes, three seconds")
   (test-equal? "one hour"            (spelled-out-time 3611) "one hour")
   (test-equal? "two hours"           (spelled-out-time 7229) "two hours")
   (test-equal? "one day"             (spelled-out-time (+ 17 (* 24 3600))) "one day")))

(provide main spelled-out-time)
(define (main . args)
  (exit (test/text-ui spelled-out-time-tests 'verbose)))
