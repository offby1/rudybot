#lang racket/base

(require scheme/match scheme/system scheme/promise
         (for-syntax scheme/base syntax/boundmap))

(provide from-env run-command call-with-PATH defmatcher domatchers defautoloads)

;; this is used when this module is loaded, before `clearenv' is called
(define (from-env var default [split #f])
  (let ([val (getenv var)])
    (if (and val (> (string-length val) 0))
      (if split (regexp-split split val) val)
      default)))

;; Capture the initial path for all kinds of things that need it
(define default-path (getenv "PATH"))
(define (call-with-PATH thunk)
  (dynamic-wind
    (lambda () (putenv "PATH" default-path))
    thunk
    (lambda () (putenv "PATH" "")))) ; no way to actually delete a var

;; Conveniently running an external process (given its name and string args)
;; and return the stdout in a string
(define (run-command cmd . args)
  (define exe (call-with-PATH (lambda () (find-executable-path cmd))))
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
     (begin (define hidden (delay (dynamic-require 'lib 'var)))
            (define-syntax var
              (syntax-id-rules (set!)
                [(set! . _) (error 'var "cannot mutate")]
                [(x . xs) ((force hidden) . xs)]
                [_ (force hidden)])))]
    [(_ [lib var ...])
     (begin (defautoloads (lib var)) ...)]
    [(_ [lib var ...] ...)
     (begin (defautoloads (lib var ...)) ...)]))

;; In theory, we can have sqlite retry a bunch of times if the db is
;; locked.  In practice, it doesn't seem to work, so ... we just
;; ignore the exception :-|

;; TODO -- only ignore the "sqlite3-db-is-locked" exception.
(provide safely)
(define-syntax-rule (safely body ...)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (fprintf (current-error-port) "~a; ignoring~%" e))
                   ])
    body ...))
