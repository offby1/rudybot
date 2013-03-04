#lang racket

;; Run all the tests

(require racket/require-syntax)

(define-require-syntax test-in
  (syntax-rules ()
    [(_ spec) (submod spec test)]))

(require (test-in "analyze-quotes.rkt")
         (test-in "backfill.rkt")
         (test-in "clearenv.rkt")
         (test-in "incubot.rkt")
         (test-in "lexer.rkt")
         (test-in "re.rkt")
         (test-in "sandboxes.rkt") 
         (test-in "spelled-out-time.rkt")
         (test-in "tinyurl.rkt")
         (test-in "userinfo.rkt")
         (test-in "zdate.rkt")
         (test-in "xlate.rkt")
         "sighting-test.rkt")
