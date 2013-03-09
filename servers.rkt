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
         "clearenv.rkt"
         (only-in "corpus.rkt" make-corpus)
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
                  (cond
                   (#t
                    (list
                     (c "eval (require racket/date)")
                     (c "eval (date->string (seconds->date 1333210982))")))
                   (#f
                    (list
                     (meh "Hey everyone!  What's happening?")
                     (c "uptime")
                     (c "settle")
                     (meh "frotz: plotz.")
                     (c "everyone")
                     (c "plotz")
                     (meh "\1ACTION fred eats salami\1")
                     (c "salami")))
                   (#f
                    ;; Typical stuff from ircd-seven
                    `(":bartol.freenode.net NOTICE * :*** No Ident response"
                      ":notice!NickServ@services. NOTICE rudybot :This nickname is registered. Please choose a different nickname, or identify via \u0002/msg NickServ identify <password>\u0002.")
                    )
                   (#f
                    `(
                      ":t8!n=foo@bar PRIVMSG #ch :,t8"
                      ":t8!n=foo@bar PRIVMSG #ch :,t8 fr"
                      ":t8!n=foo@bar PRIVMSG #ch :,t8 fr de"
                      ,(format ":t8!n=foo@bar PRIVMSG #ch :~a: t8 en it kits, cats, sacks, wives: how many were going to St Ives?" (unbox *my-nick*))
                      ":t8!n=foo@bar PRIVMSG #ch :,t8 en hu I will not buy this record, it is scratched"
                      ":t8!n=foo@bar PRIVMSG #ch : ,t8 en hu I will not buy this translation; it contains leading whitespace"))
                   (else
                    `(
                      ,(c "(dict-update '((a . 9) (b . 2) (a . 1)) 'a add1 0)")
                      ,(c (format "eval (error \"foo\\r\\nQUIT bar\")"))
                      ":freenode-connect!freenode@freenode/bot/connect PRIVMSG upstartbot :\u0001VERSION\u0001"
                      "foO!"
                      "PING :localhost."
                      ":sykopomp!n=user@host-70-45-40-165.onelinkpr.net PRIVMSG #emacs :\u0001ACTION is wondering if it's easy to save any logs from bitlbee to a different folder than all the irc logs.\u0001"
                      ":arcfide!n=arcfide@VPNBG165-7.umsl.edu PRIVMSG #scheme :\u0001ACTION sighs. \u0001"

                      ":action!n=No@unaffiliated/clue PRIVMSG #ch :\u0001ACTION does an action!\u0001"
                      ":invite!n=No@unaffiliated/clue INVITE upstartbot :##mircscripts"
                      ":join!n=Aaron@b415.adsl.ecomtel.com.au JOIN :#scheme"
                      ":join!n=Aaron@b415.adsl.ecomtel.com.au JOIN #scheme" ; both flavors have been seen in the wild
                      ":duncanm!n=duncanm@b415.adsl.ecomtel.com.au JOIN :#scheme"
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

                      ,(c "version")
                      ,(c "SOURCE")
                      ,(c "quote")
                      ,(format ":t8!n=foo@bar PRIVMSG #ch :~a: t8 en it kits, cats, sacks, wives: how many were going to St Ives?" (unbox *my-nick*))
                      ":t8!n=foo@bar PRIVMSG #ch :,t8 en hu I will not buy this record, it is scratched"
                      ":t8!n=foo@bar PRIVMSG #ch : ,t8 en hu I will not buy this translation; it contains leading whitespace"

                      ,(format ":t8!n=foo@bar PRIVMSG #ch :~a: t8 snord horde" (unbox *my-nick*))

                      ,(format ":jordanb!n@n PRIVMSG #c :~a: quote" (unbox *my-nick*))
                      ,(format ":jordanb!n@n PRIVMSG #c :Let's say something memorable")
                      ,(format ":n!n@n PRIVMSG #emacs :,...")
                      ,(format ":n!n@n PRIVMSG #not-emacs :,...")
                      ,(format ":n!n@n PRIVMSG #c :~a:~a" (unbox *my-nick*) "lookboynospaces")
                      ,(format ":n!n@n PRIVMSG #c :~a:" (unbox *my-nick*) )
                      ,@(for/list ([action (in-list (list "action" "invite" "join" "kick" "kick2" "mode" "nick" "nick2" "notice" "notice2" "part" "quit" "topic"))])
                          (c (format "seen ~a" action)))

                      ":niven.freenode.net 001 rudybot :Welcome to the freenode IRC Network rudybot"
                      ,(format
                        ":NickServ!NickServ@services. NOTICE ~a :If this is your nickname, type /msg NickServ \0002IDENTIFY\0002 <password>"
                        (unbox *my-nick*))

                      ,@(apply
                         append
                         (for/list ([expr (in-list '((+ 2 1)
                                                     (begin (display (+ 2 1)) (newline))
                                                     (let loop ()
                                                       (printf "Yaa!!")
                                                       (loop))
                                                     (require srfi/1)
                                                     (make-list 100000)
                                                     (apply values (make-list 100000))))])
                           (list
                            (c (format "eval ~s" expr))
                            (p (format "eval ~s" expr)))))

                      ,@(map c (list "quote" "uptime"))
                      ,@(map p (list "This is a private utterance, and I certainly hope you don't divulge it!!"))
                      ,(c "seen n")
                      ;; This should work, if you set BOTMASTER in the
                      ;; environment before running this test.
                      ,(c "system ls /")

                      ;; This should yield an empty string.
                      ,(c "eval (getenv \"PATH\")")

                      ;; This should simply not blow up.
                      ,(p "eval (number->string #d10000000000000000000000000000000000000000000000000000000000 16)")
                      ))))
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
                  [*incubot-server* (make-incubot-server)])
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
