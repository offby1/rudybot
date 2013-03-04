#lang racket
(require racket/sandbox
         "sandboxes.rkt"
         "vars.rkt")

(module+ test (require rackunit rackunit/text-ui))

(provide roughly-evaluable?)
(define (roughly-evaluable? for-whom str)
  (if (regexp-match? #px"\\w'\\w|\\w,|[.!?,;]$" str)
    #f ; "obviously not"(TM) text => don't evaluate
    (let ([inp       ; try  to read it with the default sandbox reader
           (with-handlers ([void (lambda (_) #f)])
             (parameterize ([current-input-port (open-input-string str)])
               ((sandbox-reader) 'repl)))]
          [sb (cond [(hash-ref *sandboxes* for-whom #f) => sandbox-evaluator]
                    [else #f])])
      (and inp ;; it's readable
           (equal? 1 (length inp))      ;; just a single form
           (let ([lone-identifier (identifier? (car inp))])
             (or sb (not lone-identifier)))))))

(module+ test
  (define-test-suite without-sandbox-tests
    (check-false     (roughly-evaluable? "ted" "") "empty string")
    (check-false     (roughly-evaluable? "ted" "  ") "mostly empty string")
    (check-false     (roughly-evaluable? "ted" " \" ") "lone double-quote")

    (check-false     (roughly-evaluable? "ted" "singleword") "Single word")
    (check-false     (roughly-evaluable? "ted" "frotz plotz hotz totz") "Strings of words")
    (check-not-false (roughly-evaluable? "ted" "(frotz)")))
 
  (define-test-suite with-sandbox-tests
    (hash-set! *sandboxes* "ted" (make-sandbox))
    (check-not-false (roughly-evaluable? "ted" "(frotz)")) ;this gives ted a sandbox

    (check-not-false (roughly-evaluable? "ted" "singlewordwithsandbox") "Single word")
    (check-false (roughly-evaluable? "ted" "frotz plotz hotz totz") "Strings of words"))
  
  (define-test-suite all-tests
    without-sandbox-tests
    with-sandbox-tests)
  
  (parameterize ([*logger* (lambda (fmt . args) (apply printf fmt args) (newline))])
    (run-tests all-tests 'verbose)))

