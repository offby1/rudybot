#lang scheme/base

(require scheme/contract "utils.ss")

(define (git-version [style 'short])
  ;; TODO -- run "git diff-index --name-only HEAD --" (just as
  ;; /usr/local/src/git/GIT-VERSION-GEN does) to see if the working
  ;; tree is "dirty", and so indicate in our output.
  (run-command "git" "log"
               (format "--pretty=format:%~a"
                       (case style ((short) "h") (else "H")))
               "-1"))

(provide/contract
 [git-version (->* () (symbol?) string?)])
