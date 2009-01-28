#lang scheme/base

(require scheme/match scheme/system (for-syntax scheme/base syntax/boundmap))
(provide from-env run-command defmatcher domatchers)

;; this is used when this module is loaded, before `clearenv' is called
(define (from-env var default [split #f])
  (let ([val (getenv var)])
    (if (and val (> (string-length val) 0))
      (if split (regexp-split split val) val)
      default)))

;; Conveniently running an external process (given its name and string args)
;; and return the stdout in a string (capture the initial PATH)
(define default-path (getenv "PATH"))
(define (run-command cmd . args)
  (define exe (begin (putenv "PATH" default-path)
                     (begin0 (find-executable-path cmd)
                       (putenv "PATH" "")))) ; no way to delete a var
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (if (and exe (apply system* exe args))
      (get-output-string out)
      "unknown")))

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

