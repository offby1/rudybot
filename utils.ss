#lang scheme/base

(require scheme/match (for-syntax scheme/base))
(provide from-env defmatcher domatchers)

;; this is used when this module is loaded, before `clearenv' is called
(define (from-env var default [split #f])
  (let ([val (getenv var)])
    (if (and val (> (string-length val) 0))
      (if split (regexp-split split val) val)
      default)))

;; Allows defining matchers separately, easier to maintain code.
(define-for-syntax matcher-patterns '())
(define-syntax (defmatcher stx)
  (syntax-case stx ()
    [(_ pattern body ...)
     (begin (set! matcher-patterns (cons #'[pattern body ...] matcher-patterns))
            #'(begin))]))
(define-syntax (domatchers stx)
  (syntax-case stx ()
    [(_ val) #`(match val #,@(reverse matcher-patterns))]))
