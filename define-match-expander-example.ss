#lang scheme

(define-match-expander =
    (syntax-rules ()
          [(_ expr) (? (lambda (x) (equal? x expr)))]))

(define x 123)
(match (list 123 456)
    [(list (= x) y) y])
