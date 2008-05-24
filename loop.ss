#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#
#lang scheme

(require (only-in "fake-server.ss" data->input-port)
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" )))

(define (all-lines-from-this-port ip proc)
  (for/list ((line (in-lines ip)))
    (proc line)))

(define (keep-trying ip line-proc (tries 0))
  (with-handlers ([exn:fail:network?
                   (lambda (exn)
                     (printf "Oh noes! ~s!~%" exn)
                     (sleep (expt 2 tries))
                     (keep-trying ip line-proc (add1 tries)))])
    (all-lines-from-this-port
     ip
     line-proc)))


(define-struct connection (events) #:transparent)

(define loop-tests

  (let ((minimal-processor (lambda (line)
                             (format "OK, I read ~s.  Now what?~%" line))))
    (define (check-bot data-from-server expected-responses)
      (let ((output-from-bot (open-output-string)))
        (for ((actual (in-list
                       (keep-trying
                        (data->input-port data-from-server)
                        minimal-processor)))
              (expected (in-list expected-responses)))
          (check-regexp-match
           (byte-regexp (regexp-quote expected))
           actual))))


    (test-suite
     "loop"

     (test-case
      "no problem"

      (check-bot
       (list #"Welcome to freenode, douchebag"
             #"Have a nice day")
       (list #"Welcome to freenode, douchebag"
             #"Have a nice day")))

     (test-case
      "deals with exception"
      (check-bot
       (list #"OK, this is the second, buggier, connection."
             make-exn:fail:network
             #"Time to go.")
       (list #"OK, this is the second, buggier, connection."
             #"Time to go."))))))

(define (main . args)
  (exit (test/text-ui loop-tests 'verbose)))

(provide (all-defined-out))

