#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(require "loop.ss"
         scheme/port)

(define (make-flaky-server)
  (when (zero? (random 3))
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
                 (cond
                  ((zero? (random 5))
                   (close-output-port op))
                  ((not (eof-object? datum))
                   (display datum op)
                   (newline op)
                   (loop))
                  (else
                   (close-output-port op))))))))))

    (values ip
            (open-output-nowhere))))

(define (real-server)
  (let-values (((ip op) (tcp-connect (*irc-server-hostname*) 6667)))
    (file-stream-buffer-mode op 'line)
    (values ip op)))

(define (make-preloaded-server op)
  (lambda ()
    (values (let-values (((ip op)
                          (make-pipe)))
              (thread
               (lambda ()
                 (for-each
                  (lambda (line)
                    (display line op)
                    (display "\r\n" op))
                  (list
                   ":freenode-connect!freenode@freenode/bot/connect PRIVMSG upstartbot :\u0001VERSION\u0001"
                   "foO!"
                   "PING :localhost."
                   ":sykopomp!n=user@host-70-45-40-165.onelinkpr.net PRIVMSG #emacs :\u0001ACTION is wondering if it's easy to save any logs from bitlbee to a different folder than all the irc logs.\u0001"
                   ":arcfide!n=arcfide@VPNBG165-7.umsl.edu PRIVMSG #scheme :\u0001ACTION sighs. \u0001"
                   (format
                    ":n!n=n@n PRIVMSG #scheme :~a: SOURCE"
                    *my-nick*)
                   ":niven.freenode.net 001 rudybot :Welcome to the freenode IRC Network rudybot"
                   (format
                    ":NickServ!NickServ@services. NOTICE ~a :If this is your nickname, type /msg NickServ \0002IDENTIFY\0002 <password>"
                    *my-nick*)
                   (format
                    ":n!n=n@n PRIVMSG #scheme :~a: quote"
                    *my-nick*)
                   ))

          (close-output-port op)))
       ip)
     op)))

(define (make-log-replaying-server log-file-name)
  (lambda ()
    (let-values (((ip op)
                  (make-pipe)))
      (thread
       (lambda ()
         (call-with-input-file log-file-name
           (lambda (ip)
             (for ((line (in-lines ip)))
               (match line
                 [(regexp #px"^<= (\".*\")" (list _ datum))
                  (display (read (open-input-string datum)) op)
                  (newline op)]
                 [_ #f]))
             (close-output-port op)))))

      (values ip
              (relocate-output-port
               (current-output-port)
               #f #f 1 #f)))))

(define (make-random-server)

  (define (random-bytes [length 200])
    (let ((r (make-bytes length)))
      (for ((i (in-range length)))
        (let new-byte ()
          (let ((b (random 256)))
            (case b
              ((10 13)
               (new-byte))
              (else
               (bytes-set! r i b))))))
      r))

  (let-values (((ip op)
                (make-pipe)))
    (thread
     (lambda ()
       (let loop ((lines-emitted 0))
         (when (< lines-emitted 200)
           (display #":ow!ow@ow PRIVMSG #ow :" op)
           (display (random-bytes) op)
           (display #"\r\n" op)
           (loop (add1 lines-emitted))))
       (close-output-port op)))

    (values ip (open-output-nowhere))))



(define (replay-main . args)
  (log "Main starting.")
  (parameterize ((*bot-gives-up-after-this-many-silent-seconds* 1/4)
                 (*log-ports* (list (current-error-port))))
    (connect-and-run
     (make-log-replaying-server "big-log")
     #:retry-on-hangup? #f)))

(define (preload-main . args)
  (log "Main starting.")
  (parameterize ((*bot-gives-up-after-this-many-silent-seconds* 1/4)
                 (*log-ports* (list (current-error-port))))
    (connect-and-run
     (make-preloaded-server (open-output-nowhere))
     #:retry-on-hangup? #f)))

(define (localhost-main . args)
  (log "Main starting.")
  (parameterize ((*irc-server-hostname* "localhost"))
    (connect-and-run real-server)))

(define (freenode-main . args)
  (log "Main starting.")
  (parameterize ((*irc-server-hostname* "irc.freenode.org")
                 (*mute-privmsgs?* #t))
    (connect-and-run real-server)))

(define (flaky-main . args)
  (parameterize ((*bot-gives-up-after-this-many-silent-seconds* 1/4)
                 (*log-ports* (list (current-error-port))))
    (random-seed 0)
    (connect-and-run
     make-flaky-server
     #:retry-on-hangup? #t)))

(define (random-main . args)
  (parameterize ((*bot-gives-up-after-this-many-silent-seconds* 1/4)
                 (*log-ports* (list (current-error-port))))
    (random-seed 0)
    (connect-and-run
     make-random-server
     #:retry-on-hangup? #f)))

(define main preload-main)
(provide (all-defined-out))
