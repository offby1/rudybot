#!/usr/bin/env mzscheme
#lang scheme

(require scheme/system
         (planet dherman/memoize:3:1))

;; We can't use default arguments with define/memo, so we need a silly
;; wrapper.
(define (git-version [style 'short])
  (git-version-internal style))

(define/memo (git-version-internal style)
  (match-define
   (list stdout-ip stdin-op pid stderr-ip controller)
   (process (format
             "git log --pretty=format:%~a%n -1"
             (case style
               ((short) "h")
               (else "H"))
             )))

  (controller 'wait)

  (if (eq? 'done-ok (controller 'status))
      (read-line stdout-ip)
      "unknown"))

(provide/contract
 [git-version (->* () (symbol?) string?)])
