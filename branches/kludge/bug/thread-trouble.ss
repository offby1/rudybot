#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0"  -e "(main)"
|#
(module thread-trouble mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt"))
         )

;; An example of a problem that I don't know how to work around --
;; when a thread dies during a test, the test still passes.

(define (complex-buggy-function)
  (thread
   (lambda ()
     (car 'foo))))

(define (hard-working-function)
  (complex-buggy-function)
  (sleep 1/2)
  6)


(define exception-in-child-thread? #f)

(define thread-trouble-tests

  (test-suite
   "thread-trouble"
   (test-equal?
    "yow"
    (hard-working-function) 6)
   (test-false "No exceptions"
               exception-in-child-thread?)))

(provide (all-defined))
(define (main)
  (parameterize ((uncaught-exception-handler
                  (lambda args
                    (set! exception-in-child-thread? args)
                    )))
    (exit (test/text-ui thread-trouble-tests 'verbose))))
)
