#| Hey Emacs, this is -*-scheme-*- code!
|#
#lang racket

(require
 "git-version.rkt"
 "reloadable.rkt"
 "iserver.rkt"
 (except-in "vars.rkt" log)
 (only-in "lexer.rkt" parse-message)
 "zdate.rkt"
 srfi/19
 )

(define *log-ports* (make-parameter (list (current-error-port)
                                          (open-output-file
                                           "big-log"
                                           #:mode 'text
                                           #:exists 'append))))

(for ([op (in-list (*log-ports*))])
  (with-handlers ([exn:fail? values])
    (file-stream-buffer-mode op 'line)))

(define (log . args)
  (for ([op (in-list (*log-ports*))])
    (fprintf op "~a " (zdate #:offset 0))
    (apply fprintf op args)
    (newline op)))

(define irc-process-line
  (auto-reload-procedure "irc-process-line.rkt" 'irc-process-line
                         #:notifier log
                         #:on-reload (lambda () (git-version 'reset!))))

;; Given a line of input from the server, do something side-effecty.
;; Writes to OP get sent back to the server.
(define (slightly-more-sophisticated-line-proc line)
  (log "<= ~s" (parse-message line))
  (parameterize ([*logger* log])
    (irc-process-line line)))

(define (connect-and-run
         server-maker
         (consecutive-failed-connections 0)
         #:retry-on-hangup? (retry-on-hangup? #t))

  (set-box! *authentication-state* 'havent-even-tried)

  (when (positive? consecutive-failed-connections)
    (log "~a consecutive-failed-connections"
         consecutive-failed-connections)
    (sleep (expt 2 consecutive-failed-connections)))

  (with-handlers ([exn:fail:network?
                   (lambda (exn)
                     (printf "Oh noes! ~a!~%" (exn-message exn))
                     (connect-and-run server-maker (add1 consecutive-failed-connections)))])
    (let-values ([(ip op) (server-maker)])
      (*connection-start-time* (current-seconds))
      (log "Bot version ~a starting" (git-version))
      (let do-one-line ([cfc consecutive-failed-connections])
        (let ([ready-ip (sync/timeout (*bot-gives-up-after-this-many-silent-seconds*) ip)]
              [retry (lambda ()
                       (close-input-port ip)
                       (close-output-port op)
                       (connect-and-run server-maker (add1 cfc)))])

          (if (not ready-ip)
              (begin
                (log
                 "Bummer: ~a seconds passed with no news from the server"
                 (*bot-gives-up-after-this-many-silent-seconds*))
                (retry))
              (let ([line (read-line ready-ip 'return-linefeed)])
                (match line
                  [(? eof-object?)
                   (when retry-on-hangup?
                     (log
                      "Uh oh, server hung up on us")
                     (retry))]
                  [(regexp #rx"^ERROR :(.*)$" (list _ whine))
                   (log "Hmm, error: ~s" whine)
                   (retry)]
                  [_
                   (parameterize ([*irc-output* op])
                     (slightly-more-sophisticated-line-proc line))
                   (do-one-line 0)]))))))))

(provide/contract
 [connect-and-run
  (->* (procedure?) (natural-number/c #:retry-on-hangup? boolean?) void?)])
(provide
 log
 *my-nick*
 *nickserv-password*
 *bot-gives-up-after-this-many-silent-seconds*
 *log-ports*)
