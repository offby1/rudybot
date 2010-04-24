#| Hey Emacs, this is -*-scheme-*- code!
|#
#lang scheme

(require srfi/19
         "vars.ss"
         "git-version.ss"
         (except-in "iserver.ss" main)
         "reloadable.ss")

(define *log-ports* (make-parameter (list (current-error-port)
                                          (open-output-file
                                           "big-log"
                                           #:mode 'text
                                           #:exists 'append))))

(for ((op (in-list (*log-ports*))))
  (fprintf (current-error-port) "Whopping port ~a~%" op)
  (with-handlers ([exn:fail? values])
    (file-stream-buffer-mode op 'line)))

(define (log . args)
  (for ((op (in-list (*log-ports*))))
    (fprintf op "~a " (date->string (current-date) "~4"))
    (apply fprintf op args)
    (newline op)))

(define irc-process-line
  (auto-reload-procedure "irc-process-line.ss" 'irc-process-line
                         #:notifier log
                         #:on-reload (lambda () (git-version 'reset!))))

;; Given a line of input from the server, do something side-effecty.
;; Writes to OP get sent back to the server.
(define (slightly-more-sophisticated-line-proc line)
  (log "<= ~s" line)
  (parameterize ([*logger* log])
    (irc-process-line line)))

(define (connect-and-run
         server-maker
         (consecutive-failed-connections 0)
         #:retry-on-hangup? (retry-on-hangup? #t))

  (*connection-start-time* (current-seconds))
  (set-box! *authentication-state* 'havent-even-tried)

  (when (positive? consecutive-failed-connections)
    (log "~a consecutive-failed-connections"
         consecutive-failed-connections)
    (sleep (expt 2 consecutive-failed-connections)))

  (with-handlers ([exn:fail:network?
                   (lambda (exn)
                     (printf "Oh noes! ~a!~%" (exn-message exn))
                     (connect-and-run server-maker (add1 consecutive-failed-connections)))])
    (let-values (((ip op)
                  (server-maker)))
      (let ([ch (make-channel)])
        (log "Bot version ~a starting" (git-version))
        (let do-one-line ((cfc consecutive-failed-connections))
          (let ((ready-ip (sync/timeout (*bot-gives-up-after-this-many-silent-seconds*) ip))
                (retry (lambda ()
                         (close-input-port ip)
                         (close-output-port op)
                         (connect-and-run server-maker (add1 cfc)))))

            (if (not ready-ip)
                (begin
                  (log
                   "Bummer: ~a seconds passed with no news from the server"
                   (*bot-gives-up-after-this-many-silent-seconds*))
                  (retry))
                (let ((line (read-line ready-ip 'return-linefeed)))
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
                     (do-one-line 0)])))))))))

(provide/contract
 [connect-and-run
  (->* (procedure?) (natural-number/c #:retry-on-hangup? boolean?) void?)])
(provide
 log
 *my-nick*
 *nickserv-password*
 *bot-gives-up-after-this-many-silent-seconds*
 *log-ports*)
