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

                  ;; You'll note I'm setting the parameter
                  ;; sandbox-error-output to #f, which means
                  ;; "silently discard error output".  This
                  ;; isn't _quite_ as bad as it sounds:
                  ;; evaluation errors will raise an
                  ;; exception, and the exception message will
                  ;; contain a good error message.  However,
                  ;; if someone evals error-free code that
                  ;; nevertheless writes to
                  ;; current-error-port, I silently discard
                  ;; what they wrote.  This of course is a
                  ;; shortcoming, but nobody's yet complained
                  ;; :-|
                  (sandbox-error-output #f)
                  (sandbox-eval-limits '(2 20)))

     (make-evaluator '(begin) '()))
   0))

;; I no longer remember precisely why this is so complex -- it would
;; seem that the entire body of this procedure could be
;; ((sandbox-evaluator sb) string).  But I suspect that causes
;; weirdness when passed an empty string.  This weirdness may well be
;; fixed in PLT's r7445 (probably release 372).
(define (sandbox-eval sb string)
  (let ((first-sexp (read (open-input-string string))))
    (if (eof-object? first-sexp)
        (raise
         (make-exn:fail:read:eof
          "I ain't gonna argue scripture with no nun"
          (current-continuation-marks)
          '()))
     (begin
       (set-sandbox-last-used-time! sb (current-milliseconds))
       ((sandbox-evaluator sb)
        (with-output-to-string
         (lambda ()
           (write first-sexp)
           (flush-output (current-output-port)))))))))
;(trace sandbox-eval)

(define (get-sandbox-by-name name)
  (hash-table-get *sandboxes-by-nick* name
    (lambda ()
      (when (>= (hash-table-count *sandboxes-by-nick*) (*max-sandboxes*))
        ;; evict the sandbox that has been unused the longest
        (let ([moldiest #f])
          (hash-table-for-each *sandboxes-by-nick*
            (lambda (name sb)
              (let ([t (sandbox-last-used-time sb)])
                (unless (and moldiest (> t (car moldiest)))
                  (set! moldiest (cons t name))))))
          (assert moldiest)
          (hash-table-remove! *sandboxes-by-nick* (cdr moldiest))))
      (let ([sb (public-make-sandbox)])
        (hash-table-put! *sandboxes-by-nick* name sb)
        sb))))

;(trace get-sandbox-by-name)

(define (sandbox-get-stdout s)
  (get-output (sandbox-evaluator s)))

(define *sandboxes-by-nick* (make-hash-table 'equal))

(define *max-sandboxes* (make-parameter 3))


(print-hash-table #t)
(define sandboxes-tests

  (test-suite
   "sandboxes"
   (test-case
    "simple get"
    (let ((s  (get-sandbox-by-name "charlie")))
      (check-pred sandbox? s)
      (check-equal? (sandbox-eval s "3") 3)))

   (test-case
    "Gacks on empty input"
    (check-exn
     exn:fail:read:eof?
     (lambda ()
       (sandbox-eval
        (get-sandbox-by-name "huh?")
        ""))))

   (test-case
    "output"
    (let ((s  (get-sandbox-by-name "charlie")))
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
        (get-sandbox-by-name "sleepy")
        "(sleep 10)")))

    (test-exn
     "gacks on incomplete input"
     exn:fail?
     (lambda ()
       (sandbox-eval
        (get-sandbox-by-name "oops")
        "("
        ))))

   (let ((charlies-sandbox #f)
         (keiths-sandbox   #f))

     (test-suite
      "distinct "
      #:before
      (lambda ()
        (set! *sandboxes-by-nick* (make-hash-table 'equal))
        (set! charlies-sandbox (get-sandbox-by-name "charlie"))
        (set! keiths-sandbox   (get-sandbox-by-name "keith")))
      (test-false
       "keeps sandboxes distinct, by name"
       (eq? charlies-sandbox keiths-sandbox))

      (test-case
       "remembers state"
       (sandbox-eval charlies-sandbox "(define x 99)")
       (let ((this-better-still-be-charlies (get-sandbox-by-name "charlie")))
         (check-equal? (sandbox-eval this-better-still-be-charlies
                                     "x")
                       99))
       (check-exn
        exn:fail?
        (lambda () (sandbox-eval keiths-sandbox))
        "keith's sandbox didn't gack when I referenced 'x' -- even though we never defined it."))))

   (test-case
    "won't store too many"
    (before
     (set! *sandboxes-by-nick* (make-hash-table 'equal))
     (let* ((names (iota (* 2 (*max-sandboxes*))))
            (boxes (map get-sandbox-by-name names)))
       (check-equal? (hash-table-count *sandboxes-by-nick*)
                     (*max-sandboxes*)))))
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
       (sandbox-eval (get-sandbox-by-name "old"   ) "(define x 'old   )")
       (sleep 1/10)
       (sandbox-eval (get-sandbox-by-name "young" ) "(define x 'young )")
       (sleep 1/10)
       (sandbox-eval (get-sandbox-by-name "newest") "(define x 'newest)")

       (check-equal? (sandbox-eval (get-sandbox-by-name "young")
                                   "x")
                     'young)
       (check-equal? (sandbox-eval (get-sandbox-by-name "newest")
                                   "x")
                     'newest)
       (check-exn exn:fail? (lambda () (sandbox-eval (get-sandbox-by-name "old")
                                                     "x"))))))
   ))

(provide *max-sandboxes*
         get-sandbox-by-name
         sandbox-eval
         sandbox-get-stdout
         sandboxes-tests
         (rename public-make-sandbox make-sandbox))
)
