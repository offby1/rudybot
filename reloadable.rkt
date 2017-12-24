#lang racket/base

;; based on handin-server/private/reloadable

(require syntax/moddep)

(provide reload-module)
(define (reload-module modspec path [notifier #f])
  ;; the path argument is not needed (could use resolve-module-path here), but
  ;; its always known when this function is called
  (let* ([name ((current-module-name-resolver) modspec #f #f #t)])
    (when notifier (notifier "(re)loading module from ~a" modspec))
    (parameterize ([current-module-declare-name name]
                   [compile-enforce-module-constants #f])
      ;; only notify, it's fine to reset the file timer, since there's no point
      ;; in attempting to reload it yet again until it is edited.
      (with-handlers ([exn?
                       (lambda (e)
                         (notifier "error, module not reloaded (~a)"
                                   (exn-message e))
                         (notifier "~a~%" (continuation-mark-set->context (exn-continuation-marks e))))])
        (namespace-require '(only scheme module #%top-interaction))
        (load/use-compiled path)))))

;; pulls out a value from a module, reloading the module if its source file was
;; modified
(provide auto-reload-value)
(define module-times (make-hash))
(define (auto-reload-value modspec valname)
  (let* ([path (resolve-module-path modspec #f)]
         [last (hash-ref module-times path #f)]
         [cur  (file-or-directory-modify-seconds path)])
    (unless (equal? cur last)
      (hash-set! module-times path cur)
      (reload-module modspec path))
    (dynamic-require modspec valname)))

(define poll-freq 2000.0) ; poll at most once every two seconds

;; pulls out a procedure from a module, and returns a wrapped procedure that
;; automatically reloads the module if the file was changed whenever the
;; procedure is used
(provide auto-reload-procedure)
(define (auto-reload-procedure
         modspec procname #:notifier [notifier #f] #:on-reload [on-reload #f])
  (let ([path (resolve-module-path modspec #f)] [date #f] [proc #f] [poll #f])
    (define (reload)
      (unless (and proc (< (- (current-inexact-milliseconds) poll) poll-freq))
        (set! poll (current-inexact-milliseconds))
        (let ([cur (file-or-directory-modify-seconds path)])
          (unless (equal? cur date)
            (when on-reload (on-reload))
            (set! date cur)
            (reload-module modspec path notifier)
            (set! proc (dynamic-require modspec procname))))))
    (reload)
    (lambda xs (reload) (apply proc xs))))
