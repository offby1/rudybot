#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
|#
#lang scheme

(require scheme/date
         scheme/port
         scheme/sandbox
         (except-in "sandboxes.ss" main)
         "sighting.ss"
         "spelled-out-time.ss"
         (except-in "quotes.ss" main)
         "tinyurl.ss"
         (lib "trace.ss")
         (lib "13.ss" "srfi")
         (lib "14.ss" "srfi")
         (planet "macro.ss" ("schematics" "macro.plt"))
         (planet "numspell.ss" ("neil" "numspell.plt"))
         (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" )))

;; This value depends on the server; this seems to work for freenode
(define *bot-gives-up-after-this-many-silent-seconds* (make-parameter 250))
(define *my-nick* "rudybot")
(define *nickserv-password* (make-parameter "eezahpaiohpubahb"))
(define *irc-server-hostname* (make-parameter "localhost"))

(define *start-time* (current-seconds))
(define *connection-start-time* (make-parameter #f))

(define *sandboxes* (make-hash))
(define *max-values-to-display* 5)
(define *log-ports* (make-parameter (list (current-error-port)
                                          (open-output-file
                                           "big-log"
                                           #:mode 'text
                                           #:exists 'append))))
(for ((op (in-list (*log-ports*))))
  (fprintf (current-error-port)
           "Whopping port ~a~%" op)
  (file-stream-buffer-mode op 'line))

(define (log . args)
  (for ((op (in-list (*log-ports*))))
    (apply fprintf op args)
    (newline op)))

;; Maybe I should use rnrs/enums-6 to guard against typos
(define *authentication-state* 'havent-even-tried)

;; Normally this would be #f, so that when we issue a PRIVMSG, it
;; really gets sent.  But during development, I have the bot lurking
;; silently in a couple of channels, and this is #t, so that it
;; doesn't say anything.
(define *mute-privmsgs?* (make-parameter #f))

(define-match-expander colon
  (syntax-rules ()
    [(colon w)
     (regexp #rx"^:(.*)" (list _ w))]))

;; Given a line of input from the server, do something side-effecty.
;; Writes to OP get sent back to the server.

(define (describe-since when)
  (spelled-out-time (- (current-seconds) when)))

(define (nick->sighting-string n)
  (let ((info (lookup-sighting n)))
    (if info
        (format "~a was last seen ~ain/on ~a ~a ago~a"
                (sighting-who   info)
                (aif it (sighting-action? info) (string-append it " ") "")
                (sighting-where info)
                (describe-since (sighting-when  info))
                (if (null? (sighting-words info))
                    ""
                    (format ", saying \"~a\""
                            (string-join (sighting-words info)))))
        (format "No sign of ~a" n))))

;; Lines much longer than this will cause the server to kick us for
;; flooding.
(define *max-output-line* 500)

(define (slightly-more-sophisticated-line-proc line op)
  (define (out #:for-real? [for-real? #t] format-string . args)
    (let ((str (apply format format-string args)))
      (let ((str (if (> (string-length str) *max-output-line*)
                     (string-append (substring str 0 (- *max-output-line* 4)) " ...")
                     str)))

        (log "=> ~s" str)
        (when for-real?
          (fprintf op "~a~%" str)))))

  (define (pm #:notice? [notice? #f] target fmt . args)
    (out #:for-real? (not (*mute-privmsgs?*))
         "~a" (format "~a ~a :~a"
                      (if notice? "NOTICE" "PRIVMSG")
                      target (apply format fmt args))))

  (define (do-cmd response-target for-whom words)
    (define (reply fmt . args)
      (let ((response-prefix (if (equal? response-target for-whom)
                                 ""
                                 (format "~a: " for-whom))))
        (pm response-target "~a" (string-append response-prefix (apply format fmt args)))))
    (log "Doing ~s" words)
    (case (string->symbol (string-downcase (first words)))
      [(quote)
       (let ((q (one-quote)))
         ;; special case: jordanb doesn't want quotes prefixed with
         ;; his nick.
         (match for-whom
           [(regexp #rx"^jordanb")
            (pm response-target "~a" q)]
           [_ (reply q)])
         )]
      [(source) (reply "$HeadURL$")]
      [(seen)
       (when (not (null? (cdr words)))
         (reply (nick->sighting-string (second words)))
         )]
      [(uptime)
       (reply "I've been up for ~a; this tcp/ip connection has been up for ~a"
              (describe-since *start-time*)
              (describe-since (*connection-start-time*)))]
      [(eval)
       (let ((s (get-sandbox-by-name *sandboxes* for-whom)))
         (with-handlers
             (
              ;; catch _all_ exceptions, to prevent "eval (raise 1)" from
              ;; killing this thread.
              [void
               (lambda (v)
                 (let ((whine (if (exn? v)
                                  (exn-message v)
                                  (format "~s" v))))
                   (reply
                    ;; make sure our error message begins with "error: ".
                    (if (regexp-match #rx"^error: " whine)
                        whine
                        (format "error: ~a" whine)))))])

           (call-with-values
               (lambda ()
                 (sandbox-eval
                  s
                  (string-join (cdr words))))
             (lambda values
               (let loop ((values values)
                          (displayed 0))
                 (when (not (null? values))

                   ;; prevent flooding
                   (if (= displayed *max-values-to-display*)
                       (reply
                        "~a values is enough for anybody; here's the rest in a list: ~s"
                        (number->english *max-values-to-display*)
                        values)

                       ;; Even though the sandbox runs with strict
                       ;; memory and time limits, we use
                       ;; call-with-limits here anyway, because it's
                       ;; possible that the sandbox can, without
                       ;; exceeding its limits, return a value that
                       ;; will require a lot of time and memory to
                       ;; convert into a string!  (make-list 100000)
                       ;; is an example.
                       (call-with-limits
                        2 20
                        (lambda ()
                          (when (not (void? (car values)))
                            (when (positive? displayed)
                              (sleep 1))
                            (reply
                             "; Value: ~s"
                             (car values)))
                          (loop (cdr values)
                                (add1 displayed)))))))))

           (let ((stdout (sandbox-get-stdout s))
                 (stderr (sandbox-get-stderr s)))
             (when (and (string? stdout)
                        (positive? (string-length stdout)))
               (reply "; stdout: ~s" stdout))
             (when (and (string? stderr)
                        (positive? (string-length stderr)))
               (reply  "; stderr: ~s" stderr)))))]

      [else #f]))

  (log "<= ~s" line)
  (let ((toks (string-tokenize line (char-set-adjoin char-set:graphic #\u0001))))
    (when (*nickserv-password*)
      (match (car toks)
        ["ERROR"
         (log "Uh oh!")]

        ["NOTICE"
         (when (eq? *authentication-state* 'havent-even-tried)
           (out "NICK ~a" *my-nick*)
           (out "USER luser unknown-host localhost :duh, version 0")
           (set! *authentication-state* 'tried))]

        ["PING"
         (out "PONG ~a" (cadr toks))]

        [(regexp #rx"^:(.*)!(.*)@(.*)$" (list _ nick id host))
         ;; update the "seen" database
         (if (equal? nick *my-nick*)
             (log "I seem to have said ~s" (cdr toks))
             (match (cdr toks)
               [(list
                 "NOTICE"
                 my-nick
                 ":This"  "nickname" "is" "registered."
                 ;; not sure where I got this from
                 ;;":If" "this" "is" "your" "nickname,"
                 yaddayaddayadda ...)
                (when (and (equal? nick "NickServ")
                           (equal? id   "NickServ")
                           (equal? host "services."))
                 (log "Gotta register my nick.")
                 (pm "NickServ" "identify ~a" (*nickserv-password*)))]
               [(list "KICK" target victim mumblage ...)
                (note-sighting
                 (make-sighting
                  nick
                  target
                  (current-seconds)
                  (format "kicking ~a" victim)
                  mumblage))]
               [(list "MODE" target mode-data ...)
                (note-sighting
                 (make-sighting
                  nick
                  target
                  (current-seconds)
                  (format "changing the mode to '~a'" mode-data) '()))]
               [(list "INVITE" lucky-recipient (colon party) further ...)
                (note-sighting
                 (make-sighting
                  nick
                  host
                  (current-seconds)
                  (format "inviting ~a to ~a" lucky-recipient party)
                  further))]
               [(list "NICK" (colon first-word) rest ...)
                (note-sighting
                 (make-sighting
                  nick
                  host
                  (current-seconds)
                  (format "changing their nick to ~a" first-word)
                  '()))]
               [(list "TOPIC" target (colon first-word) rest ...)
                (note-sighting
                 (make-sighting
                  nick
                  target
                  (current-seconds)
                  (format
                   "changing the channel's topic to '~a'"
                   (string-join (cons first-word rest)))
                  '()))]
               [(list "JOIN" target)
                (note-sighting
                 (make-sighting
                  nick
                  target
                  (current-seconds)
                  (format "joining")
                  '()))]
               [(list "NICK" (colon new-nick))
                (log "~a wants to be known as ~a" nick new-nick)]
               [(list "PART" target (colon first-word) rest ...)
                (note-sighting
                 (make-sighting
                  nick
                  target
                  (current-seconds)
                  "leaving the channel"
                  (cons first-word rest)))
                ]

               [(list "PRIVMSG"
                      target
                      (regexp #px"^:\u0001([[:alpha:]]+)" (list _ extended-data-word ))
                      inner-words ...
                      (regexp #px"(.*)\u0001$" (list _ trailing )))
                (note-sighting
                 (make-sighting
                  nick
                  target
                  (current-seconds)
                  (format "doing ~a: ~a" extended-data-word
                          (string-join
                           (append inner-words (list trailing))))
                  '()))]

               [(list "PRIVMSG"
                      target
                      (regexp #px"^:\u0001(.*)\u0001" (list _ request-word ))
                      rest ...)
                (log "request: ~s" request-word)
                (when (equal? "VERSION" request-word)
                  (pm #:notice? #t
                      nick
                      "\u0001VERSION ~a (offby1@blarg.net):v4.~a:PLT scheme version ~a on ~a\0001"
                      *my-nick*

                      ;; *sigh*.  The version string with
                      ;; which we reply to CTCP can't have a
                      ;; colon, but of course Subversion's
                      ;; keyword expansion inserted a colon
                      ;; into *client-version*, so we have to
                      ;; parse out the numbers.

                      (match "$Rev$"
                        [(regexp #rx"Revision: (.*)" (list _ rev))
                         rev]
                        [_ "?"])

                      (version)
                      (system-type 'os)))]

               [(list "PRIVMSG" target (colon first-word) rest ...)
                (note-sighting (make-sighting nick target (current-seconds) #f (cons first-word rest)))
                ;; look for long URLs to tiny-ify
                (for ((word (in-list (cons first-word rest))))
                  (match word
                    [(regexp url-regexp (list url _ _))
                     (when (<= 75 (string-length url))
                       (pm #:notice? #t
                           target
                           "~a"
                           (make-tiny-url url)))]
                    [_ #f]))
                (match nick
                  [(regexp "bot$")
                   (log "nick '~a' ends with 'bot', so I ain't gonna reply.  Bot wars, you know."
                        nick)]
                  [_
                   (if (equal? target *my-nick*)
                       (begin
                         (log "~a privately said ~a to me"
                              nick
                              (string-join (cons first-word rest)))

                         (do-cmd nick nick (cons first-word rest)))
                       (match first-word
                         [(regexp #px"^([[:alnum:]]+)[,:](.*)" (list _ addressee garbage))
                          (when (equal? addressee *my-nick*)
                            (do-cmd target nick (cons garbage rest)))]
                         [_ #f]))])
                ]

               [(list "QUIT" (colon first-word) rest ...)
                (note-sighting
                 (make-sighting
                  nick
                  host
                  (current-seconds)
                  "quitting; previously"
                  (append (cons first-word rest) (list (nick->sighting-string nick)))))
                ]
               [_
                (log "~a said ~s, which I don't understand" nick (cdr toks))]))]

        [(colon host)
         (match (cdr toks)
           [(list digits mynick blather ...)
            (case (string->symbol digits)
              ((|001|)
               (log "Yay, we're in")
               (set! *authentication-state* 'succeeded)
               (out "JOIN #scheme")
               (out "JOIN #emacs"))
              ((|366|)
               (log "I, ~a, seem to have joined channel ~a."
                    mynick
                    (car blather)))
              ((|433|)
               (log "Nuts, gotta try a different nick")
               (set! *my-nick* (string-append *my-nick* "_"))
               (out "NICK ~a" *my-nick*)))])]
        [_ (log "Duh?")]))
    ))

(define (connect-and-run
         server-maker
         (consecutive-failed-connections 0)
         #:retry-on-hangup? (retry-on-hangup? #t))

  (*connection-start-time* (current-seconds))

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
      (let ((ch (make-channel)))
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
                  (cond
                   ((eof-object? line)
                    (when retry-on-hangup?
                      (log
                       "Uh oh, server hung up on us")
                      (retry)))
                   (else
                    (slightly-more-sophisticated-line-proc line op)
                    (do-one-line 0)))))))))))
(provide/contract
 [connect-and-run
  (->* (procedure?) (natural-number/c #:retry-on-hangup? boolean?) void?)])
(provide
 log
 *irc-server-hostname*
 *my-nick*
 *bot-gives-up-after-this-many-silent-seconds*
 *log-ports*
 *mute-privmsgs?*)
