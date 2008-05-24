#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#
#lang scheme

(require scheme/date
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" )))

(define *bot-gives-up-after-this-many-silent-seconds* 1)

(define (retry-somehow server-maker (consecutive-failed-connections 0))
  (when (positive? consecutive-failed-connections)
    (fprintf (current-error-port)
             "~a consecutive-failed-connections~%"
             consecutive-failed-connections)
    (sleep (expt 2 consecutive-failed-connections)))

  (with-handlers ([exn:fail:network?
                   (lambda (exn)
                     (printf "Oh noes! ~a!~%" (exn-message exn))
                     (retry-somehow server-maker (add1 consecutive-failed-connections)))])
    (let-values (((ip op)
                  (server-maker)))
      (do-the-bot-thing
       ip
       op
       (lambda (line)
         (format "~a: ~a"
                 (parameterize ((date-display-format 'iso-8601))
                   (date->string (seconds->date (current-seconds)) #t))
                 line))
       *bot-gives-up-after-this-many-silent-seconds*
       server-maker
       consecutive-failed-connections))))

(define (do-the-bot-thing
         ip
         op
         line-proc
         timeout-seconds
         server-maker
         consecutive-failed-connections)
  (let ((ch (make-channel)))
    (let loop ((consecutive-failed-connections consecutive-failed-connections))
      (thread (lambda ()
                (let ((line (read-line ip)))
                  (channel-put ch line))))
      (let ((line (sync/timeout timeout-seconds ch)))
        (cond
         ((not line)
          (fprintf (current-error-port)
                   "Bummer: ~a seconds passed with no news from the server~%" timeout-seconds)
          (retry-somehow server-maker (add1 consecutive-failed-connections)))
         ((eof-object? line)
          (fprintf (current-error-port)
                   "Uh oh, server hung up on us~%")
          (retry-somehow server-maker (add1 consecutive-failed-connections)))
         ((string? line)
          (display (line-proc line) op)
          (newline op)
          (loop 0))
         (else
          (error 'do-the-bot-thing "I don't know what to do with ~s" line)))))))


(define (make-flaky-server)
  (when (zero? (random 2))
    (raise (make-exn:fail:network
            "de network, she be broke"
            (current-continuation-marks))))

  (let-values (((ip op)
                (make-pipe)))
    (thread
     (lambda ()
       (let loop ()
         (when (not (port-closed? op))
           (fprintf op "PING~%")
           (sleep (/ *bot-gives-up-after-this-many-silent-seconds* 2))
           (fprintf op "KAPOW~%")
           (sleep (/ *bot-gives-up-after-this-many-silent-seconds* 2))
           (fprintf op "SNORKULOSITY~%")
           (when (zero? (random 2)) (sleep (* *bot-gives-up-after-this-many-silent-seconds* 2)))
           (fprintf op "Thought I was a goner, eh?~%")
           (when (zero? (random 2)) (close-output-port op))))))
    (values ip
            (current-output-port))))


(define (main . args)
  (random-seed 0)
  (retry-somehow make-flaky-server))

(provide (all-defined-out))
