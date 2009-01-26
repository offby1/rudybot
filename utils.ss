#lang scheme/base

(provide from-env)

;; this is used when this module is loaded, before `clearenv' is called
(define (from-env var default [split #f])
  (let ([val (getenv var)])
    (if (and val (> (string-length val) 0))
      (if split (regexp-split split val) val)
      default)))
