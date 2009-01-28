#lang scheme/base

(require scheme/match scheme/system scheme/promise
         (for-syntax scheme/base syntax/boundmap))
(provide from-env run-command defmatcher domatchers defautoloads)

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

;; used to delay loading libraries
(define-syntax defautoloads
  (syntax-rules ()
    [(_ [lib var])
     (begin (define hidden (delay (begin (printf "loading ~a:~a\n" 'lib 'var)
                                         (dynamic-require 'lib 'var))))
            (define-syntax var
              (syntax-id-rules (set!)
                [(set! . _) (error 'var "cannot mutate")]
                [(x . xs) ((force hidden) . xs)]
                [_ (force hidden)])))]
    [(_ [lib var ...])
     (begin (defautoloads (lib var)) ...)]
    [(_ [lib var ...] ...)
     (begin (defautoloads (lib var ...)) ...)]))
