#lang scheme/base

(require scheme/match (for-syntax scheme/base syntax/boundmap))
(provide from-env defmatcher domatchers)

;; this is used when this module is loaded, before `clearenv' is called
(define (from-env var default [split #f])
  (let ([val (getenv var)])
    (if (and val (> (string-length val) 0))
      (if split (regexp-split split val) val)
      default)))

;; Allows defining matchers separately, easier to maintain code.
(define-for-syntax matcher-patterns (make-free-identifier-mapping))
(define-syntax (defmatcher stx)
  (syntax-case stx ()
    [(_ name pattern body ...)
     (begin (free-identifier-mapping-put!
             matcher-patterns #'name
             (cons #'[pattern body ...]
                   (free-identifier-mapping-get matcher-patterns #'name
                                                (lambda () '()))))
            #'(begin))]))
(define-syntax (domatchers stx)
  (syntax-case stx ()
    [(_ name val)
     #`(match val #,@(reverse (free-identifier-mapping-get matcher-patterns
                                                           #'name)))]))
