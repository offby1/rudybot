#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require bot-tests.ss -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui bot-tests 'verbose))"
|#
(module bot mzscheme
(require (lib "sandbox.ss")
         (lib "kw.ss")
         (only (lib "etc.ss") this-expression-source-directory)
         (only (lib "1.ss" "srfi")
               any
               first second third
               filter)
         (only (lib "13.ss" "srfi")
               string-join)
         (only (lib "19.ss" "srfi")
               date->string
               current-date)
         (only (lib "url.ss" "net")
               get-pure-port
               string->url)
         (lib "trace.ss")
         (only (planet "delicious.ss" ("untyped" "delicious.plt" 1 1))
               exn:fail:delicious?)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "zdate.ss" ("offby1" "offby1.plt")) zdate)
         (planet "assert.ss" ("offby1" "offby1.plt"))
         "resettable-alarm.ss"
         "cached-channel.ss"
         "channel-events.ss"
         "del.ss"
         "globals.ss"
         "headline.ss"
         "parse.ss"
         "planet-emacsen.ss"
         "quotes.ss"
         "sandboxes.ss"
         "session.ss"
         "shuffle.ss"
         "spelled-out-time.ss"
         "thread.ss"
         "tinyurl.ss"
         "vprintf.ss"
         )
(register-version-string "$Id$")
(define *process-start-time* (current-seconds))

(define (on-channel? c m)
  (and (PRIVMSG? m)
       (member c (PRIVMSG-receivers m))))

(define out
  (lambda (s . args)
    ;; ensure the output doesn't exceed around 500 characters, lest
    ;; the IRC server kick us for flooding.
    (let* ((full-length (apply format args))
           (l (string-length full-length))
           (trimmed (substring full-length 0 (min 500 l))))
      (display trimmed (irc-session-op s))
      (vtprintf " => ~s~%" trimmed))))

(define pm
  (lambda (s target msg)
    (check-type 'pm string? msg)
    (check-type 'pm string? target)
    (out  s "PRIVMSG ~a :~a~%" target msg)))

(define notice
  (lambda (s target msg)
    (check-type 'pm string? msg)
    (check-type 'pm string? target)
    (out  s "NOTICE ~a :~a~%" target msg)))

(define reply
  (lambda/kw (s message response #:key [proc pm] )
    (check-type 'reply irc-session? s)
    (check-type 'reply message? message)
    (check-type 'reply string? response)
    (check-type 'reply procedure? proc)
    (for-each
     (lambda (r)
       (proc s r response))

     (if (PRIVMSG-is-for-channel? message)
         (PRIVMSG-receivers message)
       (list (PRIVMSG-speaker message))))))

(define (respond message s)

  (let* ((threads (filter
                   thread?
                   (custodian-managed-list
                    (irc-session-custodian s)
                    (current-custodian))))
         (corpses (filter thread-dead? threads)))
    (when (not (null? corpses))
      (error 'respond "Gaah! Some threads died: ~s"
             corpses)))

  (vtprintf " <= ~s~%" message)

  ;; notify each subscriber that we got a message.
  (let ((handled? #f))
    (hash-table-for-each
     (irc-session-message-subscriptions s)
     (lambda (proc ignored)
       (when (proc message)
         (vtprintf "Proc ~s has claimed message ~s~%"
                   proc message)
         (set! handled? #t))))

    (when (and
           (for-us? message s)
           (not handled?))

      (cond
       ((and (PRIVMSG? message)
             (regexp-match
              #rx"n=Hfuy"
              (prefix-user (message-prefix message)))
             (regexp-match
              #rx"^8[12]\\."
              (prefix-host (message-prefix message))))
        (reply s message
               (format
                "\u0001ACTION ~a\u0001"
                (random-choice (list "purrs"
                                     "half-closes his eyes"
                                     "yawns and stretches"
                                     "scratches his ear with his hind leg"
                                     "rubs his cheek against the chair leg"
                                     "rolls on his back"
                                     "jumps onto the counter"
                                     "pretends to stalk a bug"
                                     (format "twines around ~a's feet"
                                             (PRIVMSG-speaker message))
                                     (format "bats at ~a's shoelaces"
                                             (PRIVMSG-speaker message)))))))
       ((with-handlers
            ([(lambda (e)
                (or (exn:delicious:auth? e)
                    (exn:fail:network? e)
                    (exn:fail:delicious? e)))
              (lambda (e)
                #f)])

          (let* ((g (gist-for-us message s))
                 ;; unfortunately this only searches _my_ tags; it'd be
                 ;; far better if it searched everybody's.  Alas, I don't
                 ;; know if that's possible with the delicious PLaneT
                 ;; package.
                 (posts (and g (snarf-some-recent-posts #:tag g))))
            (and (not (null? posts))
                 posts)))
        =>
        (lambda (posts)
          (reply s message
                 (entry->string (random-choice posts)))))
       (else
        (reply s message
               (format "\u0001ACTION ~a\u0001"
                       (random-choice
                        (list "is at a loss for words, as usual"
                              "'s jaw slackens"
                              "stares vacantly"
                              "pretends to pay attention"
                              "mumbles incoherently")))))
       ))))


;;(trace respond)

(define *sess* #f)

(define (subscribe-proc-to-server-messages! proc s)
  (check-type 'subscribe-proc-to-server-messages! procedure? proc)
  (hash-table-put!
   (irc-session-message-subscriptions s)
   proc
   #t))

(define (unsubscribe-proc-to-server-messages! proc s)
  (when *sess*
    (hash-table-remove!
     (irc-session-message-subscriptions *sess*)
     proc)))

;(trace unsubscribe-proc-to-server-messages!)

;; TODO -- as usual, this should really be in the session, not a
;; global.
(define *sandboxes-by-nick* (make-hash-table 'equal))

(define-struct sighting (who where when was-action? words))

(define (register-usual-services! session)

  ;; so that these threads will be easily killable
  (parameterize ((current-custodian (irc-session-custodian session)))

    (define/kw (add!
                discriminator
                action
                #:key
                [responds? #f]
                [timeout #f]
                [descr "unknown"])
      (subscribe-proc-to-server-messages!
       (make-channel-action
        discriminator
        action
        #:responds? responds?
        #:timeout timeout
        #:descr descr)
       session))

    (when (*nickserv-password*)
      (add!
       (lambda (m)
         (and (eq? (message-command m)
                   'NOTICE)
              (message-prefix m)
              (equal? "NickServ"  (prefix-nick (message-prefix m)))
              (equal? "NickServ"  (prefix-user (message-prefix m)))
              (equal? "services." (prefix-host (message-prefix m)))
              (equal? (irc-session-nick session) (first (message-params m)))
              (regexp-match #rx"If this is your nickname, type /msg NickServ .*IDENTIFY"
                            (second (message-params m)))
              ))
       (lambda (m)
         (out session "PRIVMSG NickServ :identify ~a~%" (*nickserv-password*)))))

    (add!
     RPL_ENDOFNAMES?
     (lambda (366-message)
       (vtprintf "Got 366~%")
       (let ((ch (RPL_ENDOFNAMES-channel-name 366-message)))
         (define (chatter? m) (on-channel? ch m))
         (define (for-this-task? m)
           (or (member (irc-session-nick session) (PRIVMSG-receivers m))
               (member ch (PRIVMSG-receivers m))))
         (define (task-gist-equal? str m)
           (and (gist-equal? str m session)
                (for-this-task? m)))
         ;; (trace chatter?)

         (define (exponentially-backing-off-spewer proc descr)
           (thread-with-id
            (let* ((delay (*minimum-delay-for-periodic-spew*)))
              (lambda ()

                (add!
                 chatter?
                 (lambda (m)
                   (set! delay (*minimum-delay-for-periodic-spew*)))
                 #:responds? #f
                 #:descr descr)

                (let loop ()
                  (proc delay)
                  (set! delay (* 2 delay))
                  (loop))
                ))
            #:descr (format
                     "exponentially-backing-off-spewer for ~a for channel ~a"
                     descr ch)))

         (define/kw (consume-and-spew
                     news-source
                     headline-filter
                     headline-proc
                     #:key

                     ;; this is basically a chance to wrap both
                     ;; headline-filter and headline-proc in a
                     ;; call-with-semaphore, since in at least one
                     ;; case, those procedures need to read and write
                     ;; the PLT preferences file.
                     [wrapper (lambda ( t) ( t))]

                     [descr "unknown, damn it"])
           (exponentially-backing-off-spewer
            (lambda (delay)
              ;; wait for something to say.
              (let ((headline (sync news-source)))
                (wrapper
                 (lambda ()
                   (when (headline-filter headline)
                     ;; wait for a chance to say it.
                     (let ((cme (make-channel-message-event
                                 chatter?
                                 #:periodic? #f
                                 #:timeout delay)))
                       (subscribe-proc-to-server-messages!
                        (channel-idle-event-input-examiner cme)
                        session)

                       (sync cme)

                       (headline-proc headline)

                       (unsubscribe-proc-to-server-messages! cme session)))))))

            (format "delay resetter for ~a" descr)))

         (set-irc-session-joined-channels!
          session
          (cons ch (irc-session-joined-channels session)))

         ;; jordanb quotes

         ;; on-demand ...
         (add!
          (lambda (m) (task-gist-equal? "quote" m))
          (lambda (m)
            (reply session m (one-quote)))
          #:responds? #t)

         (when (member ch '("#emacs" "#bots" "#scheme-bots"))
           (let ((quote-channel (make-channel)))
             ;; producer of quotes
             (thread-with-id
              (lambda ()
                (let loop ()
                  (channel-put quote-channel (one-quote))
                  (loop)))
              #:descr "quote producer")

             (consume-and-spew
              quote-channel
              (lambda (quote) #t)
              (lambda (quote)
                (pm session ch quote))
              #:descr "periodic funny quotes")))

         ;; on-demand news spewage.
         (add!
          (lambda (m) (task-gist-equal? "news" m))
          (lambda (m)
            (let ((headline (cached-channel-cache (irc-session-async-for-news session))))
              (reply session m
                     (if headline
                         (entry->string headline)
                       "no news yet."))))
          #:responds? #t
          #:descr "on-demand news")

         (when (member ch '("#emacs" "#scheme-bots"))

           ;; periodic news spewage.
           (when (irc-session-async-for-news session)
             (consume-and-spew
              (irc-session-async-for-news session)
              (lambda (headline)
                (assert (entry? headline))
                (not (already-spewed? headline)))
              (lambda (headline)
                (assert (entry? headline))
                (pm session ch
                    (entry->string headline))
                (note-spewed! headline))
              #:wrapper (lambda (thunk)
                          (vtprintf "Waiting on prefs file semaphore ...~%")
                          (call-with-semaphore
                           *prefs-file-semaphore*
                           (lambda ()
                             (begin0
                               (thunk)
                               (vtprintf "Got through prefs file semaphore; posting to it.~%")))))
              #:descr "periodic news spewage")))

         ;; moviestowatchfor

         ;; TODO -- this thread should probably get created once per
         ;; session, as opposed to once per 366 message.

         ;; producer thread -- updates (irc-session-movies-queue session)
         (thread-with-id
          (lambda ()
            (with-handlers
                ([exn:delicious:auth?
                  (lambda (e)
                    (vtprintf
                     "wrong delicious password; won't snarf moviestowatchfor posts~%"))]
                 [exn:fail:network?
                  (lambda (e)
                    (vtprintf
                     "Can't seem to contact del.icio.us~%"))])

              (let loop ()
                (with-handlers
                    ([exn:fail:delicious?
                      (lambda (e)
                        (vtprintf
                         "huh ... ~a~%" (exn-message e)))])
                  (for-each
                   (lambda (post)
                     (cached-channel-put
                      (irc-session-movies-queue session)
                      (maybe-make-URL-tiny post)))
                   (shuffle-list (snarf-some-recent-posts))))
                (sleep 3600)
                (loop))))

          #:descr "moviestowatchfor")

         (add!
          (lambda (m) (task-gist-equal? "movie" m))
          (lambda (m)
            (reply session
                   m
                   (let ((post (cached-channel-cache (irc-session-movies-queue session))))
                     (if post
                         (entry->string post)
                       "hmm, no movie recommendations yet"))))
          #:responds? #t)

         (when (member ch '("##cinema" "#scheme-bots"))

           (consume-and-spew
            (irc-session-movies-queue session)
            (lambda (post) #t)
            (lambda (post)
              (pm session
                  ch
                  (entry->string post)))
            #:descr "periodic moviestowatchfor")))))

    (add!
     (lambda (m)
       (and (PRIVMSG? m)
            (PRIVMSG-is-for-channel? m)))
     (lambda (m)

       ;; note who did what, when, where, how, and wearing what kind
       ;; of skirt; so that later we can respond to "seen Ted?"
       (let ((who (PRIVMSG-speaker m)))
         (hash-table-put!
          (irc-session-appearances-by-nick session)
          who
          (make-sighting
           who
           (car (PRIVMSG-receivers m))
           (current-seconds)
           (ACTION? m)
           (PRIVMSG-text m)))))

     #:descr "fingerprint file")

    (add!
     (lambda (m)
       (and
        (PRIVMSG? m)
        (not (regexp-match #rx"bot$" (PRIVMSG-speaker m)))
        (regexp-match url-regexp (PRIVMSG-text m))))
     (lambda (m)
       ;; if someone other than a bot uttered a long URL, run it
       ;; through tinyurl.com and spew the result.

       ;; might be worth doing this in a separate thread, since it
       ;; can take a while.
       (let ((url (car (regexp-match url-regexp (PRIVMSG-text m)))))
         (when (< (*tinyurl-url-length-threshold*) (string-length url))

           ;; I used to send these out as NOTICEs, since the RFC says
           ;; to do so, but people complained.
           (reply session
                  m
                  (make-tiny-url url #:user-agent (long-version-string))
                  #:proc notice))))
     #:descr "tinyurl")

    (add!
     (lambda (m)
       (and
        (ACTION? m)
        (regexp-match #rx"glances around nervously" (PRIVMSG-text m))))
     (lambda (m)
       (reply session
              m
              "\u0001ACTION loosens his collar with his index finger\u0001"))
     #:descr "loosens collar")

    (add!
     (lambda (m)
       (and (gist-equal? "seen" m session)
            (< 2 (length (PRIVMSG-text-words m)))))
     (lambda (m)
       (let* ((who (regexp-replace #rx"\\?+$" (third (PRIVMSG-text-words m)) ""))
              (s (hash-table-get (irc-session-appearances-by-nick session) who #f)))
         (reply session
                m
                (if s
                    (format
                     "~a~a in ~a~a ~a ago~a (also try \"/msg nickserv info ~a\")"
                     who
                     (if (sighting-was-action? s ) "'s last action" " last spoke")

                     (sighting-where s)
                     (if (sighting-was-action? s)
                         " was at"
                       "")

                     (spelled-out-time (- (current-seconds)
                                          (sighting-when s)))

                     (if (sighting-was-action? s)
                         (format ": ~a ~a" who (sighting-words s))
                       (format ", saying \"~a\"" (sighting-words s)))

                     who)
                  (format "I haven't seen ~a" who))
                )))
     #:responds? #t
     #:descr "'seen' command")

     (add!
       (lambda (m)
         (and (VERSION? m)
              (message-prefix m)
              (equal? "freenode-connect"     (prefix-nick (message-prefix m)))
              (equal? "freenode"             (prefix-user (message-prefix m)))
              (equal? "freenode/bot/connect" (prefix-host (message-prefix m)))
              (equal? (list (irc-session-nick session)) (PRIVMSG-receivers m))))
       (lambda (m)
         (when (not (equal? (*desired-nick*)
                            (irc-session-nick session)))
           ;; TODO -- check for responses to these messages
           (out session "PRIVMSG NickServ :ghost ~a ~a~%"
                (*desired-nick*)
                (*nickserv-password*))
           (out session "NICK ~a~%"
                (*desired-nick*))
           (set-irc-session-nick! (*desired-nick*)))
         )
       #:responds? #t
       #:descr "'ghost' if needed when server asks our VERSION")

    (add!
     (lambda (m) (or (VERSION? m)
                     (gist-equal? "version" m session)))
     (lambda (m)
       (if (VERSION? m)
           (out session "NOTICE ~a :\u0001VERSION ~a\0001~%"
                (PRIVMSG-speaker m)
                (long-version-string))
         (reply session m (long-version-string))))
     #:responds? #t)

    (add!
     (lambda (m)
       (or (SOURCE? m)
           (gist-equal? "source" m session)))
     (lambda (m)
       (let ((source-host "offby1.ath.cx")
             (source-directory "/~erich/bot/")
             (source-file-names "rudybot.tar.gz"))
         (if (SOURCE? m)
             (out session "NOTICE ~a :\u0001SOURCE ~a:~a:~a\0001~%"
                  (PRIVMSG-speaker m)
                  source-host
                  source-directory
                  source-file-names)
           (reply session  m
                  (format "http://~a~a~a" source-host source-directory source-file-names)))))
     #:responds? #t)

    (add!
     (lambda (m)
       (equal? 001 (message-command m)))
     (lambda (m)
       (for-each (lambda (cn)
                   (vtprintf "Joining ~a~%" cn)
                   (out session "JOIN ~a~%" cn))
                 (*initial-channel-names*))))


    (add!
     (lambda (m)
       (equal? 'PING (message-command m)))
     (lambda (m)
       (out session "PONG :~a~%" (car (message-params m)))))

    (add!
     (lambda (m)
       (gist-equal? "eval" m session))

     (lambda (m)

       (with-handlers
           ((exn:fail? (lambda (e)
                         (reply session m (exn-message e)))))

         (let ((s (get-sandbox-by-name
                   (PRIVMSG-speaker m))))

           (reply session m
                  (let* ((value (sandbox-eval
                                 s
                                 (string-join
                                  (cdr (text-for-us m session))
                                  " ")))
                         (output (sandbox-get-stdout s)))
                    (format "~s:~s"
                            value output))))))

     #:responds? #t)
    (add!
     (lambda (m) (gist-equal? "uptime" m session))
     (lambda (m)
       (reply session m (format "OK, so I've been up ~a; this TCP/IP connection has been up ~a."
                                (spelled-out-time (- (current-seconds)
                                                     (irc-session-start-time-seconds session)))
                                (spelled-out-time (- (current-seconds)
                                                     *process-start-time*)))))
     #:responds? #t)
    (add!
     (lambda (m)
       (and (PRIVMSG? m)
            (for-us? m session)
            (not (gist-for-us m session))))
     (lambda (m)
       (reply session  m "Eh? Speak up, sonny."))
     #:responds? #t)))


(define (maybe-close-output-port thing)
  (when (and (port? thing)
             (not (port-closed? thing)))
    (close-output-port thing)))

(define/kw (start #:key [nick-suffix #f])

  (with-handlers
      ([exn:fail:network?
        (lambda (e)
          (vtprintf "exception (~s); reconnecting~%"
                    e)

          (sleep 10)
          (start))])

    (when *sess*
      (custodian-shutdown-all (irc-session-custodian *sess*))
      (when (not (terminal-port? (irc-session-op *sess*)))
        (maybe-close-output-port (irc-session-op *sess*))))

    ;; if we're gonna twiddle the nick, we need to do it before we
    ;; start any threads, so that the new threads get the same value
    ;; we have here.
    (when nick-suffix
      (set-irc-session-nick! (format "~a~a" (*desired-nick*) nick-suffix)))

    (let-values (((ip op)
                  (if (*irc-server-name*)
                      (tcp-connect (*irc-server-name*) 6667)
                    (values (current-input-port)
                            (current-output-port)))))

      (set! *sess* (make-irc-session
                    op
                    #:newsfeed
                    (queue-of-entries
                     #:whence
                     (if (*use-real-atom-feed?*)
                         (lambda ()
                           (get-pure-port
                            (string->url "http://planet.emacsen.org/atom.xml")
                            (list)))
                       fake-atom-feed)
                     #:filter (lambda (e)
                                (not (already-spewed? e))))))

      (when (*log-to-file*)
        (when (not (terminal-port? (*log-output-port*)))
          (maybe-close-output-port (*log-output-port*)))
        (*log-output-port*
         (open-output-file
          ;; BUGBUGs: 1) this isn't portable; 2) we'll croak if this
          ;; directory doesn't exist
          (format "/var/log/irc-bot/~a-~a"
                  (*irc-server-name*)
                  (date->string (current-date) "~Y-~m-~dT~X~z")))))

      (fprintf (current-error-port)
               "Logging to ~s~%" (object-name (*log-output-port*)))

      ;; so we don't have to call flush-output all the time
      (for-each (lambda (p)
                  (file-stream-buffer-mode p 'line))
                (list op (*log-output-port*)))

      (for-each (lambda (s)
                  (vprintf "~a~%" s))
                (version-strings))
      (vprintf "~a~%" (long-version-string))

      (register-usual-services! *sess*)

      (out *sess* "NICK ~a~%" (irc-session-nick *sess*))
      (out *sess* "USER ~a unknown-host ~a :~a, ~a~%"
           (or (getenv "USER") "unknown")
           (*irc-server-name*)
           *client-name*
           *svnversion-string*)

      (parameterize-break
       #t
       (with-handlers
           ([exn:break?
             (lambda (x)
               ;; I often see               rudybot [~erich@127.0.0.1] has quit [Client Quit]
               ;; rather than the expected  rudybot [~erich@127.0.0.1] has quit [Ah been shot!]

               ;; http://poe.perl.org/?POE_Cookbook/IRC_Bots suggests
               ;; this may be because the server ignores custom QUIT
               ;; messages from clients that haven't been connected for
               ;; very long.
               (out *sess* "QUIT :Ah been shot!~%")
               (flush-output op)
               (close-output-port op))]
            [exn:fail?
             (lambda (e)
               (let ((whine (format  "Caught an exception: ~s~%" e)))
                 (display whine (*log-output-port*))
                 (display whine (current-error-port)))

               (with-handlers
                   ([exn:fail?
                     (lambda (e)
                       (vtprintf "oh hell, I can't send a quit message~%"))])
                 (out *sess* "QUIT :unexpected failure~%")
                 (flush-output op)
                 (close-output-port op))

               (raise e))])

         (let get-one-line-impatiently ()
           ;; irc.freenode.org, at least, sends "PING" messages about
           ;; every 200 seconds.  So if we don't hear _anything_ from
           ;; the servr for, say, 240 seconds, it's probably safe to
           ;; assume that something is all wonky, and that we should
           ;; try again.
           (let ((port-or-false
                  (sync/timeout
                   240
                   ip)))
             (if port-or-false
                 (let ((line (read-line port-or-false 'return-linefeed)))
                   (if (eof-object? line)
                       (begin
                         (vtprintf "eof on server; reconnecting~%")
                         (sleep 10)
                         (start))
                     (begin
                       (with-handlers
                           ([exn:fail:irc-parse?
                             (lambda (e)
                               (vtprintf
                                "~a: ~s~%"
                                (exn-message e)
                                (exn:fail:irc-parse-string e)))])

                         (let  ((message (parse-irc-message line)))

                           (let try-a-new-nick ((suffix "0")
                                                (tries 0))
                             (if (ERR_NICKNAMEINUSE? message)
                                 ;; append a character to our nick,
                                 ;; and try again.
                                 (begin
                                   (vtprintf "Got a ERR_NICKNAMEINUSE message; reconnecting with a new nick~%")
                                   (start #:nick-suffix "_"))
                               (respond message *sess*)))))

                       (get-one-line-impatiently))))
               (begin
                 (vtprintf "No data from server for a while; reconnecting~%")
                 (start))))))))))
(provide
 *sess*
 notice
 out
 pm
 register-usual-services!
 respond
 start))

