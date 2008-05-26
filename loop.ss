#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#
#lang scheme

(require scheme/date
         scheme/port
         (lib "trace.ss")
         (lib "13.ss" "srfi")
         (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" )))

(define *bot-gives-up-after-this-many-silent-seconds* 30)

(define *log-port* (make-parameter (current-error-port)))
(port-count-lines! (*log-port*))
(file-stream-buffer-mode (*log-port*) 'line)
(define (log . args)
  (apply fprintf (*log-port*) args)
  (newline (*log-port*)))
(define (fresh-line op)
  (let-values ([(line column pos)
                (port-next-location op)])
    (unless (zero? column)
      (newline op))))

(define *state* 'start)

(define (slightly-more-sophisticated-line-proc line op)
  (define (out format-string . args)
    (let ((str (apply format format-string args)))
      (fresh-line (*log-port*))
      (fprintf (*log-port*) "~s~%" str)
      (fprintf op "~a~%" str)))
  (fprintf (*log-port*) "~s~%" line)
  (let ((toks (string-tokenize line)))
    (case (string->symbol (car toks))
      ((ERROR)  (fprintf (current-error-port) "Uh oh!~%"))
      ((NOTICE)
       (case *state*
         ((start)
          (out "NICK nubybot")
          (out "USER luser unknown-host localhost :duh, version 0")
          (set! *state* 'attempted-auth))))
      ((PING)   (fprintf op "PONG!!!~%"))
      (else
       ;; e.g. ":Chalain!n=chalain@216-74-233-198.res.logixcom.net"
       ;; but sometimes just ":kubrick.freenode.net"
       (match (car toks)
         [(regexp #rx"^:(.*)!(.*)@(.*)$" (list _ nick id host))
          (log "~a at ~a sez ~a" nick host (cdr toks))]
         [(regexp #rx"^:(.*)" (list _ host))
          (log "Host ~a sez ~s" host (cdr toks))
          (match (cdr toks)
            [(list digits mynick blather ...)
             (log "Oh, that's one o' those numeric coded things: ~a ~s" digits blather)
             (case (string->symbol digits)
               ((|001|)
                (log "Yay, we're in")
                (set! *state* 'authenticated)
                (out "JOIN #floozy")))])]
         [_ (log "Duh? ~s" toks)])
       ))))

(define (connect-and-run server-maker (consecutive-failed-connections 0))
  (when (positive? consecutive-failed-connections)
    (fprintf (current-error-port)
             "~a consecutive-failed-connections~%"
             consecutive-failed-connections)
    (sleep (expt 2 consecutive-failed-connections)))

  (with-handlers ([exn:fail:network?
                   (lambda (exn)
                     (printf "Oh noes! ~a!~%" (exn-message exn))
                     (connect-and-run server-maker (add1 consecutive-failed-connections)))])
    (let-values (((ip op)
                  (server-maker)))
      (let ((ch (make-channel)))
        (let do-one-line ((cfc consecutive-failed-connections))
          (let ((reader (thread (lambda ()
                                  (let ((line (read-line ip)))
                                    (channel-put ch line)))))
                (line (sync/timeout *bot-gives-up-after-this-many-silent-seconds* ch))
                (retry (lambda ()
                         (close-input-port ip)
                         (close-output-port op)
                         (connect-and-run server-maker (add1 cfc)))))

            (kill-thread reader)

            (cond
             ((not line)
              (fprintf (current-error-port)
                       "Bummer: ~a seconds passed with no news from the server~%" *bot-gives-up-after-this-many-silent-seconds*)
                                        ;(retry)
              )
             ((eof-object? line)
              (fprintf (current-error-port)
                       "Uh oh, server hung up on us~%")
              (retry))
             ((string? line)
              (slightly-more-sophisticated-line-proc line op)
              (do-one-line 0))
             (else
              (error 'do-the-bot-thing "I don't know what to do with ~s" line)))))))))


(define (make-flaky-server)
  (when (zero? (random 10))
    (raise (make-exn:fail:network
            "de network, she be broke"
            (current-continuation-marks))))

  (let-values (((ip op)
                (make-pipe)))
    (thread
     (lambda ()
       (when (not (port-closed? op))
         (call-with-input-file "../irc/example input"
           (lambda (ip)
             (let loop ()
               (let ((datum (read ip)))
                 (when (not (eof-object? datum))
                   (display datum op)
                   (newline op)
                   (loop))))))
         )))
    (values ip
            (relocate-output-port
             (current-output-port)
             #f #f 1 #f)
            )))


(define (main . args)
  (random-seed 0)
  (connect-and-run
   (lambda ()
     (let-values (((ip op) (tcp-connect "localhost" 6667)))
       (file-stream-buffer-mode op 'line)
       (values ip op)))))

(provide (all-defined-out))
