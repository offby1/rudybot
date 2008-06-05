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
                  `(
                    ":freenode-connect!freenode@freenode/bot/connect PRIVMSG upstartbot :\u0001VERSION\u0001"
                    "foO!"
                    "PING :localhost."
                    ":sykopomp!n=user@host-70-45-40-165.onelinkpr.net PRIVMSG #emacs :\u0001ACTION is wondering if it's easy to save any logs from bitlbee to a different folder than all the irc logs.\u0001"
                    ":arcfide!n=arcfide@VPNBG165-7.umsl.edu PRIVMSG #scheme :\u0001ACTION sighs. \u0001"

                    ":action!n=No@unaffiliated/clue PRIVMSG #ch :\u0001ACTION does an action!\u0001"
                    ":invite!n=No@unaffiliated/clue INVITE upstartbot :##mircscripts"
                    ":join!n=Aaron@b415.adsl.ecomtel.com.au JOIN :#scheme"
                    ":kick!n=chandler@opendarwin/developer/chandler KICK #scheme lumon :http://www.penny-arcade.com/comic/2003/11/07/"
                    ":kick2!n=asc@pdpc/supporter/active/kensanata KICK #emacs jordanb :you too"
                    ":mode!ChanServ@services. MODE #emacs +o alephnull "
                    ":nick!n=Aaron@b415.adsl.ecomtel.com.au NICK :AshyIsMe"
                    ":nick2!n=Aaron@b415.adsl.ecomtel.com.au NICK :AshyIsMe"
                    ":notice!NickServ@services. NOTICE rudybot :This nickname is registered. Please choose a different nickname, or identify via \u0002/msg NickServ identify <password>\u0002."
                    ":notice2!i=christel@freenode/staff/exherbo.christel NOTICE $* :[Global Notice] Aaaaand we make contact! A small step for manki..oh wai-! Sorry about the delay there and thank you for your patience. Services are now back up!"
                    ":part!n=Akaleb@bl6-112-187.dsl.telepac.pt PART #emacs :\"Changed major mode\""
                    ":quit!n=adam@yax.org.uk PRIVMSG #ch :This is my last utterance before quitting."
                    ":quit!n=adam@yax.org.uk QUIT :Client Quit"
                    ":topic!n=javachat@cpe-74-71-143-65.twcny.res.rr.com TOPIC #emacs :-=[ www.WHAK.com ]=- Make Free/Fun Graphics Online At http://www.ImageGenerator.org =)"

                    ,(format ":n!n@n PRIVMSG #c :~a: quote"       *my-nick*)
                    ,(format ":jordanb!n@n PRIVMSG #c :~a: quote" *my-nick*)

                    ,@(for/list ((action (in-list (list "action" "invite" "join" "kick" "kick2" "mode" "nick" "nick2" "notice" "notice2" "part" "quit" "topic"))))
                        (format
                         ":n!n=n@n PRIVMSG #scheme :~a: seen ~a"
                         *my-nick*
                         action))

                    ,(format
                      ":n!n=n@n PRIVMSG #scheme :~a: SOURCE"
                      *my-nick*)
                    ":niven.freenode.net 001 rudybot :Welcome to the freenode IRC Network rudybot"
                    ,(format
                      ":NickServ!NickServ@services. NOTICE ~a :If this is your nickname, type /msg NickServ \0002IDENTIFY\0002 <password>"
                      *my-nick*)

                    ,(format ":n!n=n@n PRIVMSG #scheme :~a: eval ~a"
                             *my-nick*
                             '(+ 2 1))
                    ,(format ":n!n=n@n PRIVMSG #scheme :~a: eval ~a"
                             *my-nick*
                             '(begin (display (+ 2 1)) (newline)))
                    ,@(for/list ((cmd (in-list (list "quote" "uptime"))))
                        (format
                         ":n!n=n@n PRIVMSG #scheme :~a: ~a"
                         *my-nick*
                         cmd))))

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
                 (*mute-privmsgs?* #f))
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

(define main freenode-main)
(provide (all-defined-out))
