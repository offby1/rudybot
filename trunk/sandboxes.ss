#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui sandboxes-tests 'verbose))"
|#
(module sandboxes mzscheme
(require (only (lib "misc.ss" "swindle") with-output-to-string)
         (lib "sandbox.ss")
         (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi") iota)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "assert.ss" ("offby1" "offby1.plt")) assert)
         "globals.ss"
         )
(register-version-string "$Id$")

(define-struct sandbox (evaluator
                        last-used-time) #f)
(define (public-make-sandbox)
  (make-sandbox
   (parameterize ((sandbox-output       'string)
                  (sandbox-error-output 'string)
                  (sandbox-eval-limits '(2 20)))

     (make-evaluator '(begin
                        ;; these functions are dangerous, so we
                        ;; disable them.
                        (set! getenv  (lambda args "Oh, no, you don't."))
                        (set! putenv getenv))
                     '()))
   0))

(define (sandbox-eval sb string)
  (set-sandbox-last-used-time! sb (current-milliseconds))
  ((sandbox-evaluator sb) string))
;(trace sandbox-eval)

(define (get-sandbox-by-name ht name)
  (hash-table-get ht name
    (lambda ()
      (when (>= (hash-table-count ht) (*max-sandboxes*))
        ;; evict the sandbox that has been unused the longest
        (let ([moldiest #f])
          (hash-table-for-each ht
            (lambda (name sb)
              (let ([t (sandbox-last-used-time sb)])
                (unless (and moldiest (> t (car moldiest)))
                  (set! moldiest (cons t name))))))
          (assert moldiest)
          (hash-table-remove! ht (cdr moldiest))))
      (let ([sb (public-make-sandbox)])
        (hash-table-put! ht name sb)
        sb))))

;(trace get-sandbox-by-name)

(define (sandbox-get-stdout s)
  (get-output (sandbox-evaluator s)))

(define (sandbox-get-stderr s)
  (get-error-output (sandbox-evaluator s)))

(define *max-sandboxes* (make-parameter 3))


(print-hash-table #t)
(define sandboxes-tests

  (let ((*sandboxes-by-nick* (make-hash-table 'equal)))
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
      #:before
      (lambda ()
        (set! *sandboxes-by-nick* (make-hash-table 'equal))
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
          "keith's sandbox didn't gack when I referenced 'x' -- even though we never defined it."))))

     (test-case
      "won't store too many"
      (let* ((names (iota (* 2 (*max-sandboxes*))))
             (boxes (map (lambda (name)
                           (get-sandbox-by-name *sandboxes-by-nick* name))
                         names)))
        (check-equal? (hash-table-count *sandboxes-by-nick*)
                      (*max-sandboxes*))))
     (test-case
      "evicts oldest"
      (before
       (set! *sandboxes-by-nick* (make-hash-table 'equal))
       ;; now I have to decide precisely when old sandboxes get
       ;; evicted.  Is it when we call get-sandbox-by-name?  And I have
       ;; to decide when the timestamp updates -- again, when we call
       ;; get-sandbox-by-name?  Or when we run its evaluator?  There
       ;; might not be any practical difference.
       (parameterize ((*max-sandboxes* 2))
         (sandbox-eval (get-sandbox-by-name *sandboxes-by-nick*"old"   ) "(define x 'old   )")
         (sleep 1/10)
         (sandbox-eval (get-sandbox-by-name *sandboxes-by-nick*"young" ) "(define x 'young )")
         (sleep 1/10)
         (sandbox-eval (get-sandbox-by-name *sandboxes-by-nick*"newest") "(define x 'newest)")

         (check-equal? (sandbox-eval (get-sandbox-by-name *sandboxes-by-nick*"young")
                                     "x")
                       'young)
         (check-equal? (sandbox-eval (get-sandbox-by-name *sandboxes-by-nick*"newest")
                                     "x")
                       'newest)
         (check-exn exn:fail? (lambda () (sandbox-eval (get-sandbox-by-name *sandboxes-by-nick*"old")
                                                       "x"))))))
     )))

(provide get-sandbox-by-name
         sandbox-eval
         sandbox-get-stderr
         sandbox-get-stdout
         sandboxes-tests
         (rename public-make-sandbox make-sandbox))
)
