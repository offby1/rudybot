#!/usr/bin/env mzscheme
#lang scheme

(require scheme/system
         (planet dherman/memoize:3:1))

(define/memo (git-version)
  (match-define
   (list stdout-ip stdin-op pid stderr-ip controller)
   (process "git log --pretty=format:%h%n -1"))

  (controller 'wait)

  (if (eq? 'done-ok (controller 'status))
      (read-line stdout-ip)
      "unknown"))

(provide/contract
 [git-version (-> string?)])