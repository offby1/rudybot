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

(define (keep-trying ip line-proc)
  (let loop ((retries 0)
             (results '()))
    (with-handlers ([exn:fail:network?
                     (lambda (exn)
                       (printf "Oh noes! ~s!~%" exn)
                       (sleep (expt 2 retries))
                       (loop (add1 retries)
                             results))])
      (let ((line (read-line ip)))
        (if (eof-object? line)
            (reverse results)
            (loop 0
                  (cons (line-proc line) results)))))))



(define (check-bot data-from-server expected-responses)
  (let ((output-from-bot (open-output-string)))
    (for ((actual (in-list
                   (keep-trying
                    (data->input-port data-from-server)
                    (lambda (line)
                      (format "OK, I read ~s.  Now what?~%" line)))))
          (expected (in-list expected-responses)))
      (check-regexp-match
       (byte-regexp (regexp-quote expected))
       actual))))

(define loop-tests
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
     (list #"OK, this is the second, buggier, connection.\n"
           make-exn:fail:network
           #"Time to go.")
     (list #"OK, this is the second, buggier, connection."
           #"Time to go.")))))

(define (main . args)
  (exit (test/text-ui loop-tests 'verbose)))

(provide (all-defined-out))

