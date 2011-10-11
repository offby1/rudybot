#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
if [ "x$BOTDEBUG" != "xno" ]; then
  exec racket -l errortrace --require $0 --main -- ${1+"$@"}
else
  exec racket --require $0 --main -- ${1+"$@"}
fi
|#

#lang racket

(require "loop.rkt"
         (except-in "vars.rkt" log)
         "git-version.rkt"
         (except-in "quotes.rkt" main)
         (except-in "clearenv.rkt" main)
         (only-in "incubot.rkt" make-test-corpus)
         (only-in "iserver.rkt" make-incubot-server)
         scheme/port)

(define (real-server)
  (let-values ([(ip op) (tcp-connect (*irc-server-hostname*)
                                     (*irc-server-port*))])
    (file-stream-buffer-mode op 'line)
    (values ip op)))

(define (make-preloaded-server op)
  (lambda ()
    (values (let-values ([(ip op) (make-pipe)])
              (thread
               (lambda ()
                 (define (meh str)
                   (format ":n!n@n PRIVMSG #c :~a"
                           str))
                 (define (c str)
                   (format ":n!n@n PRIVMSG #c :~a: ~a"
                           (unbox *my-nick*)
                           str))
                 (define (p str)
                   (format ":n!n@n PRIVMSG ~a :~a"
                           (unbox *my-nick*)
                           str))
                 (for-each
                  (lambda (line)
                    (display line op)
                    (display "\r\n" op))
                  (list
                   (c "eval (require racket/place)")
                   (c "eval (define-values (a b) (place-channel))")
                   (c "eval (for ([i (in-range 999999999)]) (place-channel-put b (list 1 2 3 4)))")
                   ))
                 (close-output-port op)))
              ip)
            op)))

(define (make-log-replaying-ip-port log-file-name (max-lines 'all))
  (let-values ([(ip op) (make-pipe)])
    (thread
     (lambda ()
       (call-with-input-file log-file-name
         (lambda (ip)
           (let/ec return
             (for ([line (in-lines ip)]
                   [lines-handled (in-naturals)])
               (when (equal? lines-handled max-lines)
                 (return))
               (match line
                 [(regexp #px"<= (\".*\")" (list _ datum))
                  (display (read (open-input-string datum)) op)
                  (display #\return op)
                  (newline op)]
                 [_ #f])))
           (close-output-port op)))))
    ip))

(define (make-flaky-server log-file-name)
  (lambda ()
    (when (zero? (random 3))
      (raise (make-exn:fail:network
              "de network, she be broke"
              (current-continuation-marks))))

    (values (make-log-replaying-ip-port log-file-name 20)
            (open-output-nowhere))))

(define (make-log-replaying-server log-file-name)
  (lambda ()
    (values (make-log-replaying-ip-port log-file-name)
            (relocate-output-port
             (current-output-port)
             #f #f 1 #f))))

(define (make-random-server)

  (define (random-bytes [length 200])
    (let ([r (make-bytes length)])
      (for ([i (in-range length)])
        (let new-byte ()
          (let ([b (random 256)])
            (case b
              [(10 13) (new-byte)]
              [else    (bytes-set! r i b)]))))
      r))

  (let-values ([(ip op) (make-pipe)])
    (thread
     (lambda ()
       (let loop ([lines-emitted 0])
         (when (< lines-emitted 200)
           (display #":ow!ow@ow PRIVMSG #ow :" op)
           (display (random-bytes) op)
           (display #"\r\n" op)
           (loop (add1 lines-emitted))))
       (close-output-port op)))

    (values ip (open-output-nowhere))))

(define (make-hanging-up-server)
  (lambda ()
    (let-values ([(ip op) (make-pipe)])
      (thread
       (lambda ()
         (for ([line (in-list '("NOTICE AUTH :*** Looking up your hostname..."
                                "NOTICE AUTH :*** Found your hostname, welcome back"
                                "NOTICE AUTH :*** Checking ident"
                                "NOTICE AUTH :*** No identd (auth) response"
                                "ERROR :Closing Link: 127.0.0.1 (Connection Timed Out)"))])
           (fprintf op "~a\r~%" line))

         (sleep 1)
         (close-output-port op)))

      (values ip (open-output-nowhere)))))


(define (replay-main . args)
  (parameterize ([*bot-gives-up-after-this-many-silent-seconds* 1/4]
                 [*log-ports* (list (current-error-port))])
    (log "Main starting.")
    (connect-and-run
     (make-log-replaying-server "big-log")
     #:retry-on-hangup? #f)))

(define (preload-main . args)
  (log "Main starting.")
  (parameterize* ([*bot-gives-up-after-this-many-silent-seconds* 1/4]
                  [*log-ports* (list (current-error-port))]
                  [*incubot-logger* log]
                  [*incubot-server* (make-incubot-server (make-test-corpus))])
    (connect-and-run
     (make-preloaded-server (open-output-nowhere))
     #:retry-on-hangup? #f)))

(define (localhost-main . args)
  (log "Main starting: ~a" (git-version))
  (parameterize ([*irc-server-hostname* "localhost"])
    (connect-and-run real-server)))

(define (flaky-main . args)
  (parameterize ([*bot-gives-up-after-this-many-silent-seconds* 1/4]
                 [*log-ports* (list (current-error-port))])
    (random-seed 0)
    (connect-and-run
     (make-flaky-server "big-log")
     #:retry-on-hangup? #t)))

(define (random-main . args)
  (parameterize ([*bot-gives-up-after-this-many-silent-seconds* 1/4]
                 [*log-ports* (list (current-error-port))])
    (random-seed 0)
    (connect-and-run
     make-random-server
     #:retry-on-hangup? #f)))

(define (hanging-up-main . args)
  (parameterize ([*log-ports* (list (current-error-port))])
    (connect-and-run
     (make-hanging-up-server))))

(define (main . args)
  (fprintf (current-error-port) "Say goodbye to your environment ...")
  (clearenv)
  (fprintf (current-error-port) " poof~%")
;;  flaky-main
;;;   hanging-up-main
;;;   (localhost-main)
     (preload-main)
;;;   random-main
;;;   replay-main
  )
(provide (all-defined-out))
