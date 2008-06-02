#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(require scheme/sandbox
         (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" )))

(define-struct sandbox (evaluator
                        last-used-time) #:transparent #:mutable)
(define (public-make-sandbox)
  (make-sandbox
   (parameterize ((sandbox-output       'string)
                  (sandbox-error-output 'string)
                  (sandbox-eval-limits '(2 20)))

     (make-evaluator '(begin)))
   0))

(define (sandbox-eval sb string)
  (set-sandbox-last-used-time! sb (current-milliseconds))
  ((sandbox-evaluator sb) string))
;(trace sandbox-eval)

(define (get-sandbox-by-name ht name)
  (hash-ref ht name
    (lambda ()
      (when (>= (hash-count ht) (*max-sandboxes*))
        ;; evict the sandbox that has been unused the longest
        (let ([moldiest #f])
          (hash-for-each ht
            (lambda (name sb)
              (let ([t (sandbox-last-used-time sb)])
                (unless (and moldiest (> t (car moldiest)))
                  (set! moldiest (cons t name))))))
          (when (not moldiest)
            (error 'assertion-failure))
          (hash-remove! ht (cdr moldiest))))
      (let ([sb (public-make-sandbox)])
        (hash-set! ht name sb)
        sb))))

;(trace get-sandbox-by-name)

(define (sandbox-get-stdout s)
  (get-output (sandbox-evaluator s)))

(define (sandbox-get-stderr s)
  (get-error-output (sandbox-evaluator s)))

(define *max-sandboxes* (make-parameter 3))


(print-hash-table #t)
(define silly-tests
  (test-suite
   "silly"
   (test-case
    "very silly"
    (check-equal? (+ 2 2) 5))))

(define sandboxes-tests

  (let ((*sandboxes-by-nick* (make-hash)))
    (test-suite
     "sandboxes"
     (test-case
      "simple get"
      (let ((s  (get-sandbox-by-name *sandboxes-by-nick*"charlie")))
        (check-pred sandbox? s)
        (check-equal? (sandbox-eval s "3") 3)))

     (test-case
      "output"
      (let ((s  (get-sandbox-by-name *sandboxes-by-nick*"charlie")))
        (sandbox-eval s "(display \"You bet!\")")
        (check-equal? (sandbox-get-stdout s) "You bet!")
        (sandbox-eval s "(display \"Whatever\")")
        (check-equal? (sandbox-get-stdout s) "Whatever")))

     (test-suite
      "timeouts"
      (test-exn
       "sleeps too long"
       exn:fail?
       (lambda ()
         (sandbox-eval
          (get-sandbox-by-name *sandboxes-by-nick*"sleepy")
          "(sleep 10)")))

      (test-exn
       "gacks on incomplete input"
       exn:fail?
       (lambda ()
         (sandbox-eval
          (get-sandbox-by-name *sandboxes-by-nick*"oops")
          "("
          ))))

     (let ((charlies-sandbox #f)
           (keiths-sandbox   #f))

       (test-suite
        "distinct "
        '#:before
        (lambda ()
          (set! *sandboxes-by-nick* (make-hash))
          (set! charlies-sandbox (get-sandbox-by-name *sandboxes-by-nick* "charlie"))
          (set! keiths-sandbox   (get-sandbox-by-name *sandboxes-by-nick* "keith")))
        (test-false
         "keeps sandboxes distinct, by name"
         (eq? charlies-sandbox keiths-sandbox))
        (test-case
         "remembers state"
         (sandbox-eval charlies-sandbox "(define x 99)")
         (let ((this-better-still-be-charlies (get-sandbox-by-name *sandboxes-by-nick*"charlie")))
           (check-equal? (sandbox-eval this-better-still-be-charlies
                                       "x")
                         99))
         (check-exn
          exn:fail?
          (lambda () (sandbox-eval keiths-sandbox))
          "keith's sandbox didn't gack when I referenced 'x' -- even though we never defined it.")))))))

(provide get-sandbox-by-name
         sandbox-eval
         sandbox-get-stderr
         sandbox-get-stdout
         sandboxes-tests
         main
         (rename-out [public-make-sandbox make-sandbox]))

(define (main . args)
  (printf "Main running ...~%")
  (exit (test/text-ui sandboxes-tests 'verbose)))