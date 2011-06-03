#lang racket/base

(require scheme/contract "utils.rkt")

(define git-versions (make-hash))

(define (git-version [style 'short])
  (if (eq? style 'reset!)
    (set! git-versions (make-hash))
    ;; TODO -- run "git diff-index --name-only HEAD --" (just as
    ;; /usr/local/src/git/GIT-VERSION-GEN does) to see if the working
    ;; tree is "dirty", and so indicate in our output.
    (or (hash-ref git-versions style #f)
        (let ([r (run-command "git" "log"
                              (format "--pretty=format:%~a"
                                      (case style ((short) "h") (else "H")))
                              "-1")])
          (hash-set! git-versions style r)
          r))))

(provide/contract
 [git-version (->* () ((or/c 'short 'complete 'reset!)) any)])
