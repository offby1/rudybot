#lang racket

(require racket/sandbox
         net/url)

(module+ test (require rackunit rackunit/text-ui))

(struct sandbox (evaluator last-used-time) #:transparent #:mutable)
(provide (rename-out [public-make-sandbox make-sandbox]))
(define (public-make-sandbox #:lang [lang '(begin (require racket))]
                             #:timeout-seconds [timeout-seconds 10])
  (sandbox
   (parameterize ([sandbox-output       'string]
                  [sandbox-error-output 'string]
                  [sandbox-eval-limits (list timeout-seconds 50)]
                  [sandbox-path-permissions '([exists "/"])])
     (call-with-limits 10 #f
       (lambda ()
         (let ([port (and (string? lang)
                          (regexp-match? #rx"^http://" lang)
                          (get-pure-port (string->url lang)))])
           (if port
             (make-module-evaluator port)
             (make-evaluator lang))))))
   0))

(define (sandbox-eval sb string)
  (set-sandbox-last-used-time! sb (current-inexact-milliseconds))
  ((sandbox-evaluator sb) string))

;; returns the sandbox, force/new? can be #t to force a new sandbox,
;; or a box which will be set to #t if it was just created
(define (get-sandbox-by-name ht name
                             #:lang [lang '(begin (require scheme))]
                             #:timeout-seconds [timeout-seconds 10]
                             #:force/new? [force/new? #f])
  (define sb (hash-ref ht name #f))
  (define (make)
    (let ([sb (public-make-sandbox #:lang lang #:timeout-seconds timeout-seconds)])
      (when (box? force/new?) (set-box! force/new? #t))
      (add-grabber name sb)
      (hash-set! ht name sb)
      sb))
  (cond
    [(not (and sb (evaluator-alive? (sandbox-evaluator sb))))
     (when (and (not sb) (>= (hash-count ht) (*max-sandboxes*)))
       ;; evict the sandbox that has been unused the longest, don't do this
       ;; if we have a dead sandbox -- since we'll just replace it.
       (let ([moldiest #f])
         (for ([(name sb) (in-hash ht)])
           (let ([t (sandbox-last-used-time sb)])
             (unless (and moldiest (> t (car moldiest)))
               (set! moldiest (list t name sb)))))
         (when (not moldiest)
           (error "assertion-failure"))
         (kill-evaluator (sandbox-evaluator (caddr moldiest)))
         (hash-remove! ht (cadr moldiest))))
     ;; (when sb ...inform user about reset...)
     (make)]
    [(and force/new? (not (box? force/new?)))
     (kill-evaluator (sandbox-evaluator sb))
     (make)]
    [else sb]))

(define (sandbox-get-stdout s)
  (get-output (sandbox-evaluator s)))

(define (sandbox-get-stderr s)
  (get-error-output (sandbox-evaluator s)))

(define *max-sandboxes* (make-parameter 3))

;; A subtle point here is memory that is accessible from the sandbox:
;; the value shouldn't be accessible outside the originating sandbox to
;; prevent this from being a security hole (use `give' to avoid being
;; charged for the allocated memory).  Solve this by registering the
;; value with a gensym handle in the sending sandbox's namespace, and
;; make the handle accessible in the other sandbox.  The handle is
;; available in the receiving sandbox and weakly held in the giving
;; sandbox, so if the receiver dies the handle can be GCed and with it
;; the value.
(define given-handles (gensym 'given-values))
(define (sandbox->given-registry sb)
  (call-in-sandbox-context (sandbox-evaluator sb)
    (lambda ()
      (namespace-variable-value given-handles #f
        (lambda ()
          (let ([t (make-weak-hasheq)])
            (namespace-set-variable-value! given-handles t)
            t))))
    #t))

(define name->grabber (make-hash))

;; give : Sandbox String Any -> Void
(define (sandbox-give from to val)
  ;; Evaluate the expression (all the usual things apply: should catch errors,
  ;; and require a single value too).  See above for an explanation for the
  ;; handle.
  (define handle (gensym 'given))
  (hash-set! (sandbox->given-registry from) handle val)
  ;; Note: removing registered values depends on the handle being released, so
  ;; (a) the following should be done only for existing nicks (otherwise
  ;; error), (b) when a nick leaves it should be removed from this table
  (hash-set!
   name->grabber to
   (lambda ()
     (if (evaluator-alive? (sandbox-evaluator from))
       ;; note: this could be replaced with `val' -- but then this
       ;; closure will keep a reference for the value, making it
       ;; available from the receiving thread!
       (hash-ref (sandbox->given-registry from) handle
                 (lambda ()
                   (error 'grab "internal error (the value disappeared)")))
       (error 'grab "the sending evaluator died")))))

;; adds the GRAB binding to a given sandbox
(define (add-grabber name sb)
  (call-in-sandbox-context (sandbox-evaluator sb)
    (lambda ()
      (define (GRAB) ((hash-ref name->grabber name (lambda () void))))
      (namespace-set-variable-value! 'GRAB GRAB))))

(print-hash-table #t)

(module+ test
 (define sandboxes-tests

   (let ([*sandboxes-by-nick* (make-hash)])
     (test-suite
      "sandboxes"

      (let ([s (get-sandbox-by-name *sandboxes-by-nick*"charlie")])
        (check-equal? (sandbox-eval s "(dict-update '((a . 9) (b . 2) (a . 1)) 'a add1 0)") '((a . 10) (b . 2) (a . 1))))

      (test-case
       "simple get"
       (let ([s (get-sandbox-by-name *sandboxes-by-nick*"charlie")])
         (check-pred sandbox? s)
         (check-equal? (sandbox-eval s "3") 3)))

      (test-case
       "command line args inaccessible"
       (let ([s (get-sandbox-by-name *sandboxes-by-nick* "charlie")])
         (check-pred zero? (vector-length (sandbox-eval s "(current-command-line-arguments)")))))

      (test-case
       "output"
       (let ([s (get-sandbox-by-name *sandboxes-by-nick*"charlie")])
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
           (get-sandbox-by-name *sandboxes-by-nick* "sleepy"
                                #:timeout-seconds 1)
           "(sleep 20)")))

       (test-exn
        "gacks on incomplete input"
        exn:fail?
        (lambda ()
          (sandbox-eval
           (get-sandbox-by-name *sandboxes-by-nick*"oops")
           "("
           ))))

      (let ([charlies-sandbox #f]
            [keiths-sandbox   #f])

        (test-suite
         "distinct "
         #:before
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
          (let ([this-better-still-be-charlies (get-sandbox-by-name *sandboxes-by-nick*"charlie")])
            (check-equal? (sandbox-eval this-better-still-be-charlies
                                        "x")
                          99))
          (check-exn
           exn:fail?
           (lambda () (sandbox-eval keiths-sandbox "x"))
           "keith's sandbox didn't gack when I referenced 'x' -- even though we never defined it."))))
      ;; I'm not sure what I want to do here.  On the one hand, I want
      ;; all calls to "getenv" to fail in the sandbox; on the other
      ;; hand, I cannot think of an elegant way to have the sandbox
      ;; itself ensure that (currently I'm counting on the bot's "main"
      ;; function to clear the environment).

;;;      (test-case
;;;       "environment"
;;;       (let ([s (get-sandbox-by-name *sandboxes-by-nick* "yow")])
;;;         (check-false (sandbox-eval s "(getenv \"HOME\")"))))

      (test-case
       "immediately recycles dead sandbox"
       (check-exn exn:fail:sandbox-terminated?
                  (lambda ()
                    (sandbox-eval
                     (get-sandbox-by-name *sandboxes-by-nick* "yow")
                     "(kill-thread (current-thread))")))
       (check-equal?
        (sandbox-eval
         (get-sandbox-by-name *sandboxes-by-nick* "yow")
         "3")
        3)
       )
      )))
 (run-tests sandboxes-tests))

(provide get-sandbox-by-name
         sandbox-evaluator
         sandbox-eval
         sandbox-get-stderr
         sandbox-get-stdout
         sandbox-give)

