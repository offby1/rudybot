#| Hey Emacs, this is -*-scheme-*- code!
|#
#lang scheme

(require scheme/port
         scheme/sandbox
         srfi/19
         (except-in "sandboxes.ss" main)
         "git-version.ss"
         "sighting.ss"
         "spelled-out-time.ss"
         (except-in "quotes.ss" main)
         (except-in "tinyurl.ss" main)
         (lib "13.ss" "srfi")
         (lib "14.ss" "srfi")
         (planet "macro.ss" ("schematics" "macro.plt"))
         (planet "numspell.ss" ("neil" "numspell.plt")))

;; This value depends on the server; this seems to work for freenode
(define *bot-gives-up-after-this-many-silent-seconds* (make-parameter 250))
(define *my-nick* (make-parameter "rudybot"))
(define *initial-channels* (make-parameter '("#scheme" "#emacs")))
(define *nickserv-password* (make-parameter #f))

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
  (with-handlers
      ([exn:fail? values])
    (file-stream-buffer-mode op 'line)))

(define (log . args)
  (for ((op (in-list (*log-ports*))))
    (fprintf op "~a " (date->string (current-date) "~4"))
    (apply fprintf op args)
    (newline op)))

;; Maybe I should use rnrs/enums-6 to guard against typos
(define *authentication-state* 'havent-even-tried)

(define-match-expander colon
  (syntax-rules ()
    [(colon w)
     (regexp #rx"^:(.*)" (list _ w))]))

(define (describe-since when)
  (spelled-out-time (- (current-seconds) when)))

(define (nick->sighting-string n)
  (let ((ss (lookup-sightings n)))
    (if (null? ss)
        (format "No sign of ~a" n)
        (string-join
         (map (lambda (info)
                (format "~a was seen ~ain/on ~a ~a ago~a"
                        (sighting-who   info)
                        (aif it (sighting-action? info) (string-append it " ") "")
                        (sighting-where info)
                        (describe-since (sighting-when  info))
                        (let ((words (string-join (sighting-words info))))
                          (if (positive? (string-length words))
                              (format ", saying \"~a\"" words)
                              ""))))
              ss)
         ", and then "))))

;; Lines much longer than this will cause the server to kick us for
;; flooding.
(define *max-output-line* 500)

;; For rate limiting -- every time we respond to a direct request, we
;; save the time under the requstor's nick.  That way, we can later
;; check a request from the same nick to see if they've requested
;; something recently, and perhaps deny the request.
(define *action-times-by-nick* (make-hash))
(define (we-recently-did-something-for nick)
  (>= (hash-ref *action-times-by-nick* nick 0)
      (- (current-seconds) 10)))

(define (note-we-did-something-for! for-whom)
  (hash-set! *action-times-by-nick* for-whom (current-seconds)))

;; Cheap global bit to avoid nagging channels with grab instructions (doesn't
;; work when give is used in several channels at the same time, no care for
;; races)
(define last-give-instructions #f)

;; Given a line of input from the server, do something side-effecty.
;; Writes to OP get sent back to the server.
(define (slightly-more-sophisticated-line-proc line op)
  (define (out format-string . args)
    (let* ((str (apply format format-string args))
           (str (if (> (string-length str) *max-output-line*)
                    (string-append (substring str 0 (- *max-output-line* 4)) " ...")
                    str))
           ;; don't display newlines, so that Bad Guys won't be able
           ;; to inject IRC commands into our output.
           (str (regexp-replace* #rx"[\n\r]" str " <NEWLINE> ")))


      (log "=> ~s" str)
      (fprintf op "~a~%" str)))

  (define (pm #:notice? [notice? #f] target fmt . args)
    (out "~a" (format "~a ~a :~a"
                      (if notice? "NOTICE" "PRIVMSG")
                      target (apply format fmt args))))

  (define (do-cmd response-target for-whom words #:rate_limit? [rate_limit? #f])
    (define (reply fmt . args)
      (let ((response-prefix (if (equal? response-target for-whom)
                                 ""
                                 (format "~a: " for-whom))))
        (pm response-target "~a" (string-append response-prefix (apply format fmt args)))))
    (if (and rate_limit?
             (we-recently-did-something-for for-whom))
        (log "Not doing anything for ~a, since we recently did something for them." for-whom)
        (let ((verb (string->symbol (string-downcase (first words))))
              (words (cdr words)))
          (log "Doing ~a ~s" verb words)
          (case verb
            [(quote)
             (let ((q (one-quote)))
               ;; special case: jordanb doesn't want quotes prefixed with
               ;; his nick.
               (match for-whom
                 [(regexp #rx"^jordanb")
                  (pm response-target "~a" q)]
                 [_ (reply "~a" q)]))]
            [(source) (reply
                       "http://github.com/offby1/rudybot/tree/~a"
                       (git-version 'complete))]
            [(seen)
             (when (not (null? words))
               (reply "~a" (nick->sighting-string (car words))))]
            [(uptime)
             (reply "I've been up for ~a; this tcp/ip connection has been up for ~a"
                    (describe-since *start-time*)
                    (describe-since (*connection-start-time*)))]
            [(eval give)
             (with-handlers
                 ;; catch _all_ exceptions from the sandbox, to prevent "eval
                 ;; (raise 1)" or any other error from killing this thread.
                 ([void
                   (lambda (v)
                     (let ((whine (if (exn? v)
                                      (exn-message v)
                                      (format "~s" v))))
                       (apply reply
                              ;; make sure our error message begins with "error: ".
                              (if (regexp-match #rx"^error: " whine)
                                  (list "~a" whine)
                                  (list "error: ~a" whine)))))])

               ;; get-sandbox-by-name can raise an exception, so it's
               ;; important to have it inside the with-handlers.
               (define s (get-sandbox-by-name *sandboxes* for-whom))
               (define-values (give-to words*)
                 (cond ((or (eq? verb 'eval) (null? words))
                        (values #f words))
                       ((equal? for-whom (*my-nick*))
                        (error "I'm full, thanks."))
                       ((equal? for-whom (car words))
                        ;; allowing giving a value to yourself can lead to a
                        ;; nested call to `call-in-sandbox-context' which will
                        ;; deadlock.
                        (error "Talk to yourself much too?"))
                       (else (values (car words) (cdr words)))))
               (call-with-values
                   (lambda () (sandbox-eval s (string-join words*)))
                 (lambda values
                   ;; Even though the sandbox runs with strict memory and time
                   ;; limits, we use call-with-limits here anyway, because it's
                   ;; possible that the sandbox can, without exceeding its
                   ;; limits, return a value that will require a lot of time
                   ;; and memory to convert into a string!  (make-list 100000)
                   ;; is an example.
                   (call-with-limits 10 20 ; 15sec, 20mb
                     (lambda ()
                       (define (display-values values displayed)
                         (define (next)
                           (display-values (cdr values) (add1 displayed)))
                         (cond
                           ((null? values) (void))
                           ((void? (car values)) (next))
                           ;; prevent flooding
                           ((>= displayed *max-values-to-display*)
                            (reply
                             "; ~a values is enough for anybody; here's the rest in a list: ~s"
                             (number->english *max-values-to-display*)
                             (filter (lambda (x) (not (void? x))) values)))
                           (else
                            (reply "; Value~a: ~s"
                                   (if (positive? displayed)
                                     (format "#~a" (add1 displayed))
                                     "")
                                   (car values))
                            (sleep 1)
                            (next))))
                       (define (display-output name output-getter)
                         (let ((output (output-getter s)))
                           (when (and (string? output)
                                      (positive? (string-length output)))
                             (reply "; ~a: ~s" name output)
                             (sleep 1))))
                       (cond ((not give-to) (display-values values 0))
                             ((null? values)
                              (error "no value to give"))
                             ((not (null? (cdr values)))
                              (error "you can only give one value"))
                             (else
                              (sandbox-give s give-to (car values))
                              (let ((msg "has given you a value, use (GRAB) in an eval to get it (case sensitive)")
                                    (msg* "you got a value, use (GRAB)"))
                                (if (not (regexp-match? #rx"^#" response-target))
                                  ;; announce privately if given privately
                                  (pm give-to "~a ~a" for-whom msg)
                                  ;; cheap no-nag feature
                                  (let ((l last-give-instructions)
                                        (msg (if (and l
                                                      (equal? (car l) response-target)
                                                      (< (- (current-seconds) (cdr l))
                                                         120))
                                               msg*
                                               msg)))
                                    (set! last-give-instructions
                                          (cons response-target (current-seconds)))
                                    (pm response-target
                                        "~a: ~a ~a"
                                        give-to for-whom msg))))))
                       (display-output 'stdout sandbox-get-stdout)
                       (display-output 'stderr sandbox-get-stderr))))))]

            [(version)
             (reply "~a" (git-version))]
            [else #f])
          (note-we-did-something-for! for-whom))))

  (log "<= ~s" line)
  (let ((toks (string-tokenize line (char-set-adjoin char-set:graphic #\u0001))))
    (match (car toks)
      ["ERROR"
       (log "Uh oh!")]

      ;; Here we wait for a NOTICE before authenticating.  I suspect
      ;; this doesn't work for all servers; in particular,
      ;; ngircd-0.10.3 doesn't say anything when we connect.
      ["NOTICE"
       (when (eq? *authentication-state* 'havent-even-tried)
         (out "NICK ~a" (*my-nick*))
         ;; RFC 1459 suggests that most of this data is ignored.
         (out "USER luser unknown-host localhost :Eric Hanchrow's bot, version ~a"
              (git-version))
         (set! *authentication-state* 'tried))]

      ["PING"
       (out "PONG ~a" (cadr toks))]

      [(regexp #rx"^:(.*)!(.*)@(.*)$" (list _ nick id host))
       (define (espy target action words)
         (note-sighting
          (make-sighting
           nick
           target
           (current-seconds)
           action
           words)))
       (if (equal? nick (*my-nick*))
           (log "I seem to have said ~s" (cdr toks))
           (match (cdr toks)
             [(list
               "NOTICE"
               my-nick
               ":This"  "nickname" "is" "registered."
               yaddayaddayadda ...)
              (when (and (*nickserv-password*)
                         (equal? nick "NickServ")
                         (equal? id   "NickServ")
                         (equal? host "services."))
                (log "Gotta register my nick.")
                (pm "NickServ" "identify ~a" (*nickserv-password*)))]
             [(list "KICK" target victim mumblage ...)
              (espy target (format "kicking ~a" victim) mumblage)]
             [(list "MODE" target mode-data ...)
              (espy target (format "changing the mode to '~a'" mode-data) '())]
             [(list "INVITE" lucky-recipient (colon party) further ...)
              (espy host (format "inviting ~a to ~a" lucky-recipient party)
                    further)]
             [(list "NICK" (colon first-word) rest ...)
              (espy host (format "changing their nick to ~a" first-word)
                    '())]
             [(list "TOPIC" target (colon first-word) rest ...)
              (espy target
                    (format
                     "changing the channel's topic to '~a'"
                     (string-join (cons first-word rest)))
                    '())]
             [(list "JOIN" target)
              ;; Alas, this pretty much never triggers, since duncanm
              ;; keeps his client session around for ever
              (when (regexp-match #rx"^duncanm" nick)
                (pm target "la la la"))

              (espy target
                    (format "joining")
                    '())]
             [(list "NICK" (colon new-nick))
              ;; TODO -- call espy with the old nick, or the new one,
              ;; or both?
              (log "~a wants to be known as ~a" nick new-nick)]
             [(list "PART" target (colon first-word) rest ...)
              (espy target
                    "leaving the channel"
                    (cons first-word rest))]
             [(list "PRIVMSG"
                    target
                    (regexp #px"^:\u0001([[:alpha:]]+)" (list _ extended-data-word ))
                    inner-words ...
                    (regexp #px"(.*)\u0001$" (list _ trailing )))
              (espy target
                    (format "doing ~a: ~a" extended-data-word
                            (string-join
                             (append inner-words (list trailing))))
                    '())]
             ;; Hard to see how this will ever match, given that the
             ;; above clause would seem to match VERSION
             [(list "PRIVMSG"
                    target
                    (regexp #px"^:\u0001(.*)\u0001" (list _ request-word ))
                    rest ...)
              (log "request: ~s" request-word)
              (when (equal? "VERSION" request-word)
                (pm #:notice? #t
                    nick
                    "\u0001VERSION ~a (offby1@blarg.net):v4.~a:PLT scheme version ~a on ~a\0001"
                    (*my-nick*)
                    (git-version)
                    (version)
                    (system-type 'os)))]

             [(list "PRIVMSG" target (colon first-word) rest ...)

              ;; fledermaus points out that people may be surprised
              ;; to find "private" messages -- those where "target"
              ;; is (*my-nick*) -- recorded in the sightings log.
              (when (not (equal? target (*my-nick*)))
                (espy target #f (cons first-word rest)))
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
                 (if (equal? target (*my-nick*))
                     (begin
                       (log "~a privately said ~a to me"
                            nick
                            (string-join (cons first-word rest)))

                       (do-cmd nick nick (cons first-word rest) #:rate_limit? #f))
                     (begin

                       (match first-word
                         [(regexp #rx"^(?i:let(')?s)" (list x y))
                          (match nick
                            [(regexp #rx"^(?i:jordanb)")
                             (log "KOMEDY GOLD: ~s" (cons first-word rest))]
                            [_ #f])]
                         [_ #f])
                       (match first-word

                         ;; TODO -- use a regex that matches just
                         ;; those characters that are legal in
                         ;; nicknames, followed by _any_ non-white
                         ;; character -- that way people can use, say,
                         ;; a semicolon after our nick, rather than
                         ;; just the comma and colon I've hard-coded here.
                         [(regexp #px"^([[:alnum:]_]+)[,:](.*)" (list _ addressee garbage))
                          (when (equal? addressee (*my-nick*))
                            (let ((words  (if (positive? (string-length garbage))
                                              (cons garbage rest)
                                              rest)))
                              (when (not (null? words))
                                (do-cmd target nick words #:rate_limit?
                                        (and #f
                                             (not (regexp-match #rx"^offby1" nick))
                                             (equal? target "#emacs" ))))))]
                         [",..."
                          (when (equal? target "#emacs")
                            (pm target "Arooooooooooo"))]
                         [_ #f])))])
              ]

             [(list "QUIT" (colon first-word) rest ...)
              (espy host "quitting"
                (cons first-word rest))]
             [_
              (log "~a said ~s, which I don't understand" nick (cdr toks))]))]

      [(colon host)
       (match (cdr toks)
         [(list digits mynick blather ...)
          (case (string->symbol digits)
            ((|001|)
             (log "Yay, we're in")
             (set! *authentication-state* 'succeeded)
             (for ([c (*initial-channels*)]) (out "JOIN ~a" c)))
            ((|366|)
             (log "I, ~a, seem to have joined channel ~a."
                  mynick
                  (car blather)))
            ((|433|)
             (log "Nuts, gotta try a different nick")
             (*my-nick* (string-append (*my-nick*) "_"))
             (out "NICK ~a" (*my-nick*))))])]
      [_ (log "Duh?")])
    ))

(define (connect-and-run
         server-maker
         (consecutive-failed-connections 0)
         #:retry-on-hangup? (retry-on-hangup? #t))

  (*connection-start-time* (current-seconds))
  (set! *authentication-state* 'havent-even-tried)

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
                    (slightly-more-sophisticated-line-proc line op)
                    (do-one-line 0)])))))))))
(provide/contract
 [connect-and-run
  (->* (procedure?) (natural-number/c #:retry-on-hangup? boolean?) void?)])
(provide
 log
 *irc-server-hostname*
 *my-nick*
 *nickserv-password*
 *bot-gives-up-after-this-many-silent-seconds*
 *log-ports*)
