#!/usr/bin/env mzscheme
#lang scheme

(require scheme/system
         (planet "memoize.ss" ("dherman" "memoize.plt")))

(define/memo (git-version)
  (match-define
   (list stdout-ip stdin-op pid stderr-ip controller)
   (process "git log --pretty=format:%p%n -1"))

  (controller 'wait)

  (if (eq? 'done-ok (controller 'status))
      (read-line stdout-ip)
      "unknown"))

(provide/contract
 [git-version (-> string?)])