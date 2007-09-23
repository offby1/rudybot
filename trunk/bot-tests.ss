#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui bot-tests 'verbose))"
|#
(module bot-tests mzscheme
(require (only (lib "pregexp.ss") pregexp-quote)
         (lib "trace.ss")
         (only (lib "19.ss" "srfi")
               current-time)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "bot.ss"
         "cached-channel.ss"
         "globals.ss"
         "headline.ss"
         "session.ss"
         "test-utils.ss"
         (only "planet-emacsen.ss"
               make-cached-channel
               *planet-poll-interval*)
         (only "parse.ss"
               parse-irc-message)
         "vprintf.ss")
(require/expose "headline.ss" (reset-prefs-file-semaphore!))
(register-version-string "$Id$")

;; TODO -- write proper "check-response", so that it gives meaningful
;; spew when it fails
(define (got-response? sess ip input regexp)
  (respond (parse-irc-message input) sess)
  (expect/timeout ip regexp 3/2))

(define (fresh-session op)
  (let ((s (make-irc-session op)))
    (register-usual-services! s)
    s))

(define bot-tests

  (let-values (((ip op) (make-pipe #f "bot-tests")))
    (let ((test-cust (make-custodian)))
      (parameterize
          ((current-custodian test-cust))
        (let ((sess #f))

          (define (call-with-setup thunk)
            (around
             (begin
               (custodian-shutdown-all test-cust)
               (fprintf (current-error-port)
                        "Creating a fresh session~%")
               (*minimum-delay-for-periodic-spew* 1/10)
               (*planet-poll-interval* 2)
               (reliably-put-pref #f)
               (reset-prefs-file-semaphore!)
               (set! sess (fresh-session op)))
             (thunk)
             (begin
               (custodian-shutdown-all test-cust)
               (fprintf (current-error-port)
                        "Killed all bot threads~%"))))

          (define-syntax test-with-setup
            (syntax-rules ()
              [(test-with-setup name body ...)
               (test-case name (call-with-setup (lambda () body ...)))]))

          (test-suite
           "crap"
           #:before
           (lambda ()
             (*initial-channel-names* (list "#bots")))

           (test-suite
            "eval"
            (test-with-setup
             "return of simple value"
             (check-not-false (got-response?
                               sess
                               ip
                               (format ":a!b@c PRIVMSG #d :~a: eval (+ 1 2)" (irc-session-nick sess))
                               #rx"PRIVMSG #d :3:")))
            (test-with-setup
             "proper display of output"
             (check-not-false (got-response?
                               sess
                               ip
                               (format
                                ":a!b@c PRIVMSG #d :~a: eval (display \"fred\")"
                                (irc-session-nick sess))
                               #rx"PRIVMSG #d :.*:\"fred\"")))
            (test-with-setup
             "works in \"/QUERY\""
             (check-not-false (got-response?
                               sess
                               ip
                               (format ":a!b@c PRIVMSG ~a :eval (+ 1 2)" (irc-session-nick sess))
                               #rx"PRIVMSG a :3:"))))

           (test-with-setup
            "join"
            (respond (parse-irc-message ":server 001 :welcome") sess)
            (check-not-false
             (expect/timeout ip #rx"JOIN #bots" 1)
             "didn't join"))

           (test-with-setup
            "short semi-private message"
            (check-not-false
             (got-response?
              sess
              ip
              (format ":a!b@c PRIVMSG #d :~a: " (irc-session-nick sess))
              (pregexp-quote "PRIVMSG #d :Eh? Speak up"))
             ))

           (test-with-setup
            "replies go to the right place"
            (check-not-false
             (got-response?
              sess
              ip
              (format ":a!b@c PRIVMSG ~a :what are your plans this weekend?" (irc-session-nick sess))
              #rx"PRIVMSG a :")))

           (test-with-setup
            "backed-up idle events"

            (respond (parse-irc-message ":x 366 rudybot #emacs :backed-up idle events'") sess)

            ;; ensure there is no news available.
            (set-irc-session-async-for-news! sess (make-cached-channel #f))

            ;; let the channel go idle.
            (vtprintf "Test is sleeping~%")

            ;; have someone say something on the channel.
            (respond (parse-irc-message ":a!b@c PRIVMSG #emacs :yo") sess)

            ;; now QUICKLY provide some news.
            (cached-channel-put
             (irc-session-async-for-news sess)
             (make-entry (current-time)
                         "JAPS BOMB PERL HARBOR"
                         "http://ruby-lang.org/mua/ha/ha"))

            ;; we should not see the bot spew the news, because the channel is
            ;; no longer idle.

            (check-false
             (expect/timeout ip #rx"JAPS" (* 3/4 (*minimum-delay-for-periodic-spew*)))))

           (test-suite
            "news in general"

            (test-with-setup
             "keeps saying 'no news'"

             (respond (parse-irc-message ":x 366 rudybot #emacs :keeps saying 'no news'") sess)
             ;; ensure there is no news available.
             (set-irc-session-async-for-news! sess (make-cached-channel #f))

             ;; wait a smidge for the input-examiner proc to get
             ;; subscribed.
             (sleep 1/2)

             (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 1" (irc-session-nick sess)) #rx"no news"))
             (sleep (*minimum-delay-for-periodic-spew*))
             (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 2" (irc-session-nick sess)) #rx"no news"))
             (sleep (*minimum-delay-for-periodic-spew*))
             (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 3" (irc-session-nick sess)) #rx"no news"))
             (sleep (*minimum-delay-for-periodic-spew*))
             (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 4" (irc-session-nick sess)) #rx"no news"))
             (sleep (*minimum-delay-for-periodic-spew*))
             (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 5" (irc-session-nick sess)) #rx"no news")))

            (test-with-setup
             "items only appear once"

             (set-irc-session-async-for-news! sess (make-cached-channel #f))
             (respond (parse-irc-message ":x 366 rudybot #emacs :items only appear once") sess)

             ;; now put one item out there; expect to see it exactly once.
             (cached-channel-put
              (irc-session-async-for-news sess)
              (make-entry (current-time)
                          "SPACE ALIENS CAUSE GLOBAL WARMING"
                          "http://synasthesia.org"))

             (check-not-false
              (expect/timeout ip #rx"SPACE ALIENS" (* 4 (*minimum-delay-for-periodic-spew*)))
              "damn, it didn't appear the first time")
             (check-false
              (expect/timeout ip #rx"SPACE ALIENS" (* 8 (*minimum-delay-for-periodic-spew*)))
              "damn, it appeared a second time")))))))))

(provide (all-defined))
)
