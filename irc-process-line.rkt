#lang racket

(provide irc-process-line)

(require racket/sandbox
         racket/system
         srfi/13
         srfi/14
         "sandboxes.rkt"
         "vars.rkt"
         "git-version.rkt"
         (only-in "http.rkt" exn:fail:http? exn:fail:http-code)
         "userinfo.rkt"
         "utils.rkt"
         "xlate.rkt"
         "spelled-out-time.rkt"
         (except-in "quotes.rkt" main)
         "re.rkt"
         "tinyurl.rkt"
         (planet neil/numspell/numspell))

(define (is-master?)
  (let ([mm (unbox *my-master*)] [id (*full-id*)])
    (cond [(regexp? mm) (regexp-match? mm id)]
          [(string? mm) (equal? mm id)]
          [else #f])))

;; (colon w) is a pattern that matches coloned words, and registers
;; their position
(define (starts-with-colon str)
  (cond [(regexp-match-positions #rx"^:(.*)" str)
         => (lambda (m) (substring+posn str (caadr m)))]
        [else #f]))
(define-match-expander colon
  (syntax-rules ()
    [(colon w) (app starts-with-colon w)]))

(define (describe-since when)
  (spelled-out-time (- (current-seconds) when)))

(define (nick->sighting-string n more)
  ;; We might have accidentally stored a bunch of sightings for this
  ;; nick.  If we were to display them all, they might get truncated,
  ;; due to the 500-character output limit.  So userinfo always gives
  ;; us at most two of the recent ones.
  (let ([ss (lookup-sightings n)])
    (if (null? ss)
        (format "I've never seen ~a before"  (string-join (cons n more) " "))
        (string-join
         (map (lambda (info)
                (string-join
                 (list
                  (format "~a was seen" (sighting-who   info))
                  (cond [(not (string? (sighting-action? info)))
                         (format "in ~a" (sighting-where info))]
                        [(equal? "quitting" (sighting-action? info))
                         (format "~a" (sighting-action? info))]
                        [else
                         (format "~a in ~a" (sighting-action? info) (sighting-where info))])
                  (format "~a ago~a" (describe-since (sighting-when  info))
                          (let ([words (string-join (sighting-words info))])
                            (if (positive? (string-length words))
                                (format ", saying \"~a\"" words)
                                ""))))
                 " "))
              ss)

         ", and then "))))

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

(define (out format-string . args)
  (let* ([str (apply format format-string args)]
         [str (if (> (string-length str) *max-output-line*)
                (string-append (substring str 0 (- *max-output-line* 4)) " ...")
                str)]
         ;; don't display newlines, so that Bad Guys won't be able
         ;; to inject IRC commands into our output.
         [str (regexp-replace* #rx"[\n\r]" str " <NEWLINE> ")])
    (log "=> ~a" str)
    (fprintf (*irc-output*) "~a~%" str)))

(define (pm #:notice? [notice? #f] target fmt . args)
  (out "~a" (format "~a ~a :~a"
                    (if notice? "NOTICE" "PRIVMSG")
                    target (apply format fmt args))))

;; ----------------------------------------------------------------------------
;; General IRC protocol matchers

(defmatcher IRC-COMMAND "ERROR"
  (log "Uh oh!"))

(define (send-NICK-and-USER)
  (when (eq? (unbox *authentication-state*) 'havent-even-tried)
    (out "NICK ~a" (unbox *my-nick*))
    ;; RFC 1459 suggests that most of this data is ignored.
    (out "USER luser unknown-host localhost :Eric Hanchrow's bot, version ~a"
         (git-version))
    (if (*nickserv-password*)
        ;; BUGBUG -- keep the password out of the log.
        (pm "NickServ" "identify ~a" (*nickserv-password*))
        (log "I'd register my nick, if I had a password."))
    (set-box! *authentication-state* 'tried)))

;; This message doesn't contain much information; it really just means
;; we've connected.  And not all servers emit this anyway.  The server
;; on freenode did up to about January 2010
(defmatcher IRC-COMMAND "NOTICE"
  (send-NICK-and-USER))

(defmatcher IRC-COMMAND "PING"
  (out "PONG ~a" (car (*current-words*))))

(defmatcher IRC-COMMAND (regexp #rx"^:((.*)!(.*)@(.*))$"
                                (list _ full-id nick id host))
  (define (espy target action words)
    (note-sighting (make-sighting nick target (current-seconds) action words)))
  (if (equal? nick (unbox *my-nick*))
      (match (*current-words*)
        [(list "NICK" (colon new-nick))
         (log "I seem to be called ~s now" new-nick)
         (set-box! *my-nick* new-nick)]
        [_ (log "I seem to have said ~s" (*current-words*))])
      (match (*current-words*)
        [(list "KICK" target victim mumblage ...)
         (espy target (format "kicking ~a" victim) mumblage)]
        [(list "MODE" target mode-data ...)
         (espy target (format "changing the mode to '~a'" mode-data) '())]
        [(list "INVITE" lucky-recipient (colon party) further ...)
         (espy host (format "inviting ~a to ~a" lucky-recipient party)
               further)]
        [(list "NICK" (colon first-word) rest ...)
         (espy host (format "changing their nick to ~a" first-word) '())]
        [(list "TOPIC" target (colon first-word) rest ...)
         (espy target
               (format "changing the channel's topic to '~a'"
                       (string-join (cons first-word rest))) '())]
        [(list "JOIN" (or target (colon target)))
         ;; Alas, this pretty much never triggers, since duncanm keeps his client
         ;; session around for ever
         (when (regexp-match #rx"^duncanm" nick)
           (pm target "la la la"))
         (when (regexp-match #rx"^klutometis" nick)
           (pm target "\1ACTION bows deeply before his master, inventor of incubot\1"))
         (espy target
               "joining"
               '())]
        [(list "NICK" (colon new-nick))
         ;; TODO -- call espy with the old nick, or the new one, or both?
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
         ((*incubot-server*) 'put-string (string-join (append inner-words (list trailing)) " "))
         (espy target
               (format "doing ~a: ~a" extended-data-word
                       (string-join
                        (append inner-words (list trailing))))
               '())]
        ;; Hard to see how this will ever match, given that the above clause
        ;; would seem to match VERSION
        [(list "PRIVMSG"
               target
               (regexp #px"^:\u0001(.*)\u0001" (list _ request-word ))
               rest ...)
         (log "request: ~s" request-word)
         (when (equal? "VERSION" request-word)
           (pm #:notice? #t
               nick
               "\u0001VERSION ~a (eric.hanchrow@gmail.com):v4.~a:Racket scheme version ~a on ~a\0001"
               (unbox *my-nick*)
               (git-version)
               (version)
               (system-type 'os)))]

        [(list "PRIVMSG" target (colon first-word) rest ...)
         ;; Unspeakable hack -- "irc-process-line" is way too dumb, and
         ;; merely hands us whitespace-delimited tokens; it should
         ;; really have some knowledge of what IRC lines look like, and
         ;; split the line into semantically-meaningful units.  But
         ;; until I get off my ass and do that properly ...

         ;; If first-word is just whitespace, then skip it.  This
         ;; happens when someone types a line to their IRC client that
         ;; begins with whitespace.
         (when (and (not (null? rest))
                    (regexp-match #px"^\\s*$" first-word))
           (set! first-word (car rest))
           (set! rest (cdr rest)))

         ;; fledermaus points out that people may be surprised
         ;; to find "private" messages -- those where "target"
         ;; is (unbox *my-nick*) -- recorded in the sightings log.
         (when (not (equal? target (unbox *my-nick*)))
           (espy target #f (cons first-word rest)))

         (cond
          [(regexp-match? #rx"[Bb]ot$" (canonicalize-nick nick))
           (log "nick '~a' ends with 'bot', so I ain't gonna reply.  Bot wars, you know."
                nick)]
          [(equal? target (unbox *my-nick*))
           (log "~a privately said ~a to me"
                nick (string-join (cons first-word rest)))
           (parameterize ([*full-id* full-id])
             (do-cmd nick nick (cons first-word rest) #:rate_limit? #f))]
          [else
           ;; look for long URLs to tiny-ify, but only if we're The
           ;; Original Rudybot, so avoid annoying duplicates from multiple
           ;; bots
           (when (regexp-match? #rx"^rudybot" (unbox *my-nick*))
             (for ([word (in-list (cons first-word rest))])
               (match word
                 [(regexp url-regexp (list url _ _))
                  (when (<= 75 (string-length url))
                    ;; TODO -- calling this synchronously seems like a
                    ;; bad idea -- what if the call takes forever?
                    (with-handlers ([exn:fail:http?
                                     (lambda (e)
                                       (pm target "tinyurl is feeling poorly today: ~a (~a)"
                                           (exn:fail:http-code e)
                                           (exn-message e)))])
                      (pm target "~a" (make-tiny-url url))))
                  ]
                 [_ #f])))
           (when (and (regexp-match? #rx"^(?i:let(')?s)" first-word)
                      (regexp-match? #rx"^(?i:jordanb)" nick))
             (log "KOMEDY GOLD: ~s" (cons first-word rest)))
           (match first-word
             ;; TODO -- use a regex that matches just those characters that
             ;; are legal in nicknames, followed by _any_ non-white
             ;; character -- that way people can use, say, a semicolon after
             ;; our nick, rather than just the comma and colon I've
             ;; hard-coded here.
             [(regexp #px"^([[:alnum:]_-]+)[,:](.*)" (list _ addressee garbage))
              (let ([words (if (positive? (string-length garbage))
                               (cons garbage rest)
                               rest)])
                (if (and (not (null? words))
                         (equal? addressee (unbox *my-nick*)))
                    (parameterize ([*full-id* full-id])
                      (do-cmd target nick words #:rate_limit?
                              (and #f
                                   (not (regexp-match #rx"^offby1" nick))
                                   (equal? target "#emacs" ))))
                    ((*incubot-server*) 'put-string (string-join rest " "))))]
             [",..."
              (when (equal? target "#emacs")
                (pm target "Woof."))]

             [",…"
              (when (equal? target "#emacs")
                (pm target "\1ACTION makes like a tree, and leaves\1"))]

             [_
              ((*incubot-server*)
               'put-string
               (string-join
                (if (string=? "ACTION" first-word)
                    rest
                    (cons first-word rest))
                " "))
              ])])]

        [(list "QUIT" (colon first-word) rest ...)
         (espy host "quitting"
               (cons first-word rest))]
        [_ (log "~a said ~s, which I don't understand" nick
                (text-from-word (*current-words*)))])))

(defmatcher IRC-COMMAND (colon host)
  (match (*current-words*)

    ;; ircd-seven (http://freenode.net/seven.shtml) emits this as soon
    ;; as we connect
    [(list "NOTICE" blather ...)
     (send-NICK-and-USER)]

    [(list digits mynick blather ...)
     (case (string->number digits)
       [(1)
        (log "Yay, we're in")
        (set-box! *authentication-state* 'succeeded)
        ;; BUGBUG --this appears to be to soon to join.  _Most_
        ;; channels will let us join now; but some require that we
        ;; authenticate with nickserv first, and at this point, even
        ;; though we've already sent our password via
        ;; send-NICK-and-USER, Nickserv hasn't processed it.  We thus
        ;; need to wait until Nickserv has acknowledged our attempt to
        ;; authenticate..

        ;; ":NickServ!NickServ@services. NOTICE rudybot :You are now identified for \u0002rudebot\u0002."
        (for ([c (*initial-channels*)]) (out "JOIN ~a" c))]
       [(366)
        (log "I, ~a, seem to have joined channel ~a."
             mynick
             (car blather))]
       [(433)
        (log "Nuts, gotta try a different nick")
        (set-box! *my-nick* (string-append (unbox *my-nick*) "_"))
        (out "NICK ~a" (unbox *my-nick*))])]
    [(list)
     (log "Completely unparseable line from the server.  current-words ~s; host ~s"
          (*current-words*)
          host)]))

(defmatcher IRC-COMMAND _ (log "Duh?"))

;; ----------------------------------------------------------------------------
;; User verbs

(define verbs        (make-hasheq))
(define master-verbs (make-hasheq))
(define verb-lines        '())
(define master-verb-lines '())
(define hidden-verb-lines '())

(require (for-syntax (only-in scheme last drop-right)))
(define-syntax (*defverb stx)
  (define (id->str id) (and (identifier? id) (symbol->string (syntax-e id))))
  (syntax-case stx ()
    [(_ verbs verb-lines (verb arg ...) desc body ...)
     (and (identifier? #'verb)
          (andmap (lambda (s) (or (identifier? s) (string? (syntax-e s))))
                  (syntax->list #'(arg ...)))
          (string? (syntax-e #'desc)))
     (let* ([raw-args (syntax->list #'(arg ...))]
            [args (map id->str raw-args)]
            [formstr
             (apply string-append
                    (id->str #'verb)
                    (map (lambda (a r)
                           (cond [(not a) (format " ~s" (syntax-e r))]
                                 [(equal? a "...") " ..."]
                                 [(regexp-match? #rx"^[?]" a)
                                  (string-append " [<" (substring a 1) ">]")]
                                 [else (string-append " <" a ">")]))
                         args raw-args))])
       (define clause
         (if (and (pair? args) (last args) (regexp-match? #rx"^[?]" (last args)))
             (let* ([opt  (last raw-args)]
                    [raw-args (drop-right raw-args 1)])
               #`[(list* #,@raw-args (and #,opt (or (list _) '())))
                  (let ([#,opt (and (pair? #,opt) (car #,opt))]) body ...)])
             #'[(list arg ...) body ...]))
       #`(begin (hash-set! verbs 'verb
                           (match-lambda #,clause
                                         [_ (reply "expecting: ~a" #,formstr)]))
                (set! verb-lines (cons '(verb #,formstr desc) verb-lines))))]))
;; defverb defines a new verb:
;;   (defverb (verb arg ...) "description" body ...)
;; where `arg ...' can use `...' to get a "rest" argument, or one
;; `?id' for an optional argument.
;; In addition, it can have up to two flags:
;; - #:whine can appear, which will wrap the verb action in a call/whine
;; - #:master means that this is a master-only verb, or
;;   #:hidden which makes it available for everyone, but not included in the
;;   `help' output for a non-master user.
(define-syntax (defverb stx)
  (let*-values ([(whine rest)
                 (syntax-case stx ()
                   [(_ #:whine . rest)
                    (values #t #'rest)]
                   [(_ kwd #:whine . rest)
                    (keyword? (syntax-e #'kwd))
                    (values #t #'(kwd . rest))]
                   [(_ . rest) (values #f #'rest)])]
                [(verbs verb-lines rest)
                 (syntax-case rest ()
                   [(#:master . rest)
                    (values #'master-verbs #'master-verb-lines #'rest)]
                   [(#:hidden . rest)
                    (values #'verbs #'hidden-verb-lines #'rest)]
                   [_ (values #'verbs #'verb-lines rest)])])
    (syntax-case rest ()
      [((verb arg ...) desc body ...)
       (and (identifier? #'verb)
            (andmap (lambda (s) (or (identifier? s) (string? (syntax-e s))))
                    (syntax->list #'(arg ...)))
            (string? (syntax-e #'desc)))
       #`(*defverb #,verbs #,verb-lines (verb arg ...) desc
           #,@(if whine #'((call/whine (lambda () body ...))) #'(body ...)))]
      [_ (raise-syntax-error 'defverb "malformed defverb" stx)])))

(define (reply fmt . args)
  (let* ([response-target (*response-target*)]
         [for-whom        (*for-whom*)]
         [response-prefix (if (equal? response-target for-whom)
                            (if (is-master?) "* " "")
                            (format (if (is-master?) "*~a: " "~a: ")
                                    for-whom))])
    (pm response-target "~a~a" response-prefix (apply format fmt args))))

;; ----------------------------------------------------------------------------
;; Misc utilities

(defverb (bug more ...) "You suck."
  (reply "Yes, I know I suck: https://github.com/offby1/rudybot/issues"))

(defverb (fom more ...) "Emit the URL for Greg Hendershott's 'Fear of Macros'"
  (reply "http://www.greghendershott.com/fear-of-macros/ ~a" (string-join more)))

(defverb (hopeless more ...) "Emit the URL that explains why the top level is hopeless"
  (reply "https://gist.github.com/samth/3083053"))

(defverb (bad-eval more ...) "Emit the URL that explains why using 'eval' is generally a mistake"
  (reply "http://blog.racket-lang.org/2011/10/on-eval-in-dynamic-languages-generally.html"))

(defverb (later "tell" recipient message ...) "backwards-compatible \"tell\""
  (let* ([response-target "memoserv"]
         [for-whom        (*for-whom*)])
    (pm response-target  "send ~a ~a says: ~a" recipient for-whom (string-join message " "))
    (reply "I asked `MemoServ' to forward the message to ~a." recipient)))

(defverb (help ?what) "what tricks can I do?"
  (let ([what (and ?what (string->symbol ?what))]
        [master? (is-master?)])
    (cond
      [(or (assq what verb-lines) (and master? (assq what master-verb-lines)))
       => (lambda (v) (reply "~a: ~a" (cadr v) (caddr v)))]
      [else (reply "~a"
                   (string-join
                    (map cadr (reverse `(,@(if master? master-verb-lines '())
                                         ,@verb-lines)))
                    ", "))])))

(defverb (version) "my source code version"
  (reply "~a" (git-version)))

(defverb (quote) "words of wisdom"
  (let ([q (one-quote)])
    ;; special case: jordanb doesn't want quotes prefixed with his nick.
    (match (*for-whom*)
      [(regexp #rx"^jordanb") (pm (*response-target*) "~a" q)]
      [_ (reply "~a" q)])))

(defverb (source) "my source location"
  (reply "git clone git://github.com/offby1/rudybot.git"))

(defverb (url) "my web page"
  (reply "https://github.com/offby1/rudybot"))

(defverb (seen nick more ...) "did I see someone?"
  ;; TODO -- if "nick" is fsbot, and the last time we saw 'em was
  ;; quite a while ago, suggest /msg fledermaus (fsbot's owner).
  ;; Thanks to jlf for the idea.
  (reply "~a" (nick->sighting-string nick more)))

(defverb (uptime) "how long was I awake"
  (reply "I've been up for ~a; this tcp/ip connection has been up for ~a"
         (describe-since *start-time*)
         (describe-since (*connection-start-time*))))

(defverb #:whine (t8 from to text ...) "translate TEXT from FROM to TO"
  (reply "~a" (xlate from to (string-join text " "))))

(defverb #:hidden (ping) "am I alive?"
  (reply "pong"))

(defverb (snotback) "another treat"
  (pm (*response-target*) "Yum! Oysters."))

;; TODO -- the "botsnack" and "quote" verbs are, for all intents,
;; identical; merge them?
(defverb (botsnack) "a treat"
  (pm (*response-target*) "~a" (random-botsnack)))

(define (random-botsnack)
  (define len (vector-length botsnack-responses))
  (vector-ref botsnack-responses (random len)))

(define botsnack-responses
  #(;; fsbot "standard" responses
    "yay!"
    ":)"
    "\1ACTION dances happily\1"
    "thank you!"
    "\1ACTION beams\1"
    "my favourite snack!"

    ;; rudybot proprietary extensions

    ;;; ungrateful
    "yech, generic brand"
    "barely even a mouthful"
    "mmm ... cheesesteak ..."
    "do I look like I eat vegan botsnacks?"

    ;;; grateful
    "you, sir, are a gent of the highest calibre"
    "thank you and one day I will return the favour"
    "OMG that's just what I needed LOL"

    ;;; other
    "yow!"
    "this is going straight to my thighs"
    "come on man, one more. I need my fix!"
    "Mighty tasty cereal flakes, Mrs. McDonough."
    "A légpárnás tele van angolnák."
    "\1ACTION wiggles his eyebrows at his cousin gabot\1"
    ))

;; ----------------------------------------------------------------------------
;; Evaluation related stuffs

(define *default-sandbox-language* 'racket)

(define (call/whine f . args)
  (define (on-error e)
    (let ([whine (if (exn? e) (exn-message e) (format "~s" e))])
      (reply ;; make sure our error message begins with "error: ".
             (if (regexp-match? #rx"^error: " whine) "~a" "error: ~a")
             whine)))
  (with-handlers ([void on-error]) (apply f args)))

(define (get-sandbox [force? #f])
  (let* ([for-whom (*for-whom*)]
         [lang (userinfo-ref for-whom 'sandbox-lang *default-sandbox-language*)]
         [lang-to-report (and (not (equal? lang *default-sandbox-language*))
                              lang)]
         [lang (case lang
                 [(r5rs) '(special r5rs)]
                 [else lang])]
         [force/new? (or force? (box #f))]
         [sb (get-sandbox-by-name *sandboxes* for-whom
                                  #:lang lang
                                  #:force/new? force/new?)])
    (when (or force? (unbox force/new?))
      (if lang-to-report
        (reply "your ~s sandbox is ready" lang-to-report)
        (reply "your sandbox is ready")))
    sb))

(define (some seq)
  (ormap values seq))

(define (do-eval words give-to)
  (define for-whom (*for-whom*))
  (define response-target (*response-target*))
  ;; catch _all_ exceptions from the sandbox, to prevent "eval (raise 1)" or
  ;; any other error from killing this thread (including creating the sandbox).
  (when give-to
    (cond [(equal? give-to (unbox *my-nick*)) (error "I'm full, thanks.")]
          [(equal? give-to for-whom)
           ;; allowing giving a value to yourself can lead to a nested call
           ;; to `call-in-sandbox-context' which will deadlock.
           (error "Talk to yourself much too?")]))
  (let ([s (get-sandbox)])
    (call-with-values (lambda () (sandbox-eval s (text-from-word words)))
      (lambda values
        ;; Even though the sandbox runs with strict memory and time limits, we
        ;; use call-with-limits here anyway, because it's possible that the
        ;; sandbox can, without exceeding its limits, return a value that will
        ;; require a lot of time and memory to convert into a string!
        ;; (make-list 100000) is an example.
        (call-with-limits 10 20 ; 10sec, 20mb
          (lambda ()
            (define (display-values values displayed shown?)
              (define (next s?)
                (display-values (cdr values) (add1 displayed) s?))
              (cond [(null? values) shown?]
                    [(void? (car values)) (next shown?)]
                    ;; prevent flooding
                    [(>= displayed *max-values-to-display*)
                     (reply
                      "; ~a values is enough for anybody; here's the rest in a list: ~v"
                      (number->english *max-values-to-display*)
                      (filter (lambda (x) (not (void? x))) values))
                     #t]
                    [else (reply "; Value~a: ~v"
                                 (if (positive? displayed)
                                   (format "#~a" (add1 displayed))
                                   "")
                                 (car values))
                          ;; another precaution against flooding.
                          (sleep 1)
                          (next #t)]))
            (define (display-values/give)
              (cond [(not give-to) (display-values values 0 #f)]
                    [(null? values)
                     (error "no value to give")]
                    [(not (null? (cdr values)))
                     (error "you can only give one value")]
                    [else
                     (sandbox-give s give-to (car values))
                     ;; BUGBUG -- we shouldn't put "my-nick" in the
                     ;; string if we're talking to a nick, as opposed to
                     ;; a channel.
                     (let* ((msg* (format "has given you a value, say \"~a: eval (GRAB)\""
                                          (unbox *my-nick*)))
                            (msg  (string-append msg* " to get it (case sensitive)")))
                       (if (not (regexp-match? #rx"^#" response-target))
                         ;; announce privately if given privately
                         (pm give-to "~a ~a" for-whom msg)
                         ;; cheap no-nag feature
                         (let* ((l last-give-instructions)
                                (msg (if (and l
                                              (equal? (car l) response-target)
                                              (< (- (current-seconds) (cdr l)) 120))
                                       msg*
                                       msg)))
                           (set! last-give-instructions
                                 (cons response-target (current-seconds)))
                           (pm response-target
                               "~a: ~a ~a" give-to for-whom msg))))
                     #t])) ; said something
            (define (display-output name output-getter)
              (let ([output (output-getter s)])
                (and (string? output) (positive? (string-length output))
                     (begin (reply "; ~a: ~s" name output) (sleep 1) #t))))
            (unless (some
                     (list
                      (display-values/give)
                      (display-output 'stdout sandbox-get-stdout)
                      (display-output 'stderr sandbox-get-stderr)))
              (reply "Done."))))))))

(defverb #:whine (init ?lang)
  "initialize a sandbox, <lang> can be 'r5rs, 'scheme, 'scheme/base, etc"
  (when ?lang
    (userinfo-set! (*for-whom*) 'sandbox-lang
                   (if (regexp-match? #rx"^http://" ?lang)
                     ?lang (string->symbol ?lang))))
  (get-sandbox #t))
(defverb #:whine (eval expr ...) "evaluate an expression(s)"
  (do-eval expr #f))
(defverb #:whine (give to expr ...) "evaluate and give someone the result"
  (do-eval expr to))

(defautoloads
  [net/uri-codec uri-encode]
  [setup/private/path-utils path->name]
  [setup/xref load-collections-xref]
  [setup/dirs find-doc-dir]
  [scribble/xref xref-binding->definition-tag xref-tag->path+anchor
                 xref-index entry-desc])
;; these cannot be autoloaded, since it leads to some problem with errortrace
(require (only-in scribble/manual-struct
                  exported-index-desc? exported-index-desc-name
                  exported-index-desc-from-libs))

(define (binding-info id-str)
  (call-in-sandbox-context (sandbox-evaluator (get-sandbox))
    (lambda ()
      (let* ([sym (string->symbol id-str)]
             [id  (namespace-symbol->identifier sym)])
        (values sym id (identifier-binding id))))))

;; Based on Eli's interactive library
(defverb #:whine (apropos str ...) "look for a binding"
  (if (null? str)
    (reply "give me something to look for")
    (let* ([arg (map (compose regexp regexp-quote) str)]
           [arg (lambda (str)
                  (andmap (lambda (rx) (regexp-match? rx str)) arg))]
           [syms (namespace-mapped-symbols
                  (call-in-sandbox-context (sandbox-evaluator (get-sandbox))
                                           current-namespace))]
           [syms (filter-map (lambda (sym)
                               (let ([str (symbol->string sym)])
                                 (and (arg str) str)))
                             syms)]
           [syms (sort syms string<?)])
      (if (null? syms)
        (reply "no matches found")
        (reply "matches: ~a." (string-join syms ", "))))))

;; Based on Eli's interactive library
(defverb #:whine (desc id) "describe an identifier"
  (define-values (sym identifier info) (binding-info id))
  (cond
    [(not info) (reply "`~s' is a toplevel (or unbound) identifier" sym)]
    [(eq? info 'lexical) (reply "`~s' is a lexical identifier" sym)]
    [(or (not (list? info)) (not (= 7 (length info))))
     (error "internal error, racket changed on me")]
    [else
     (let-values ([(source-mod source-id
                    nominal-source-mod nominal-source-id
                    source-phase import-phase
                    nominal-export-phase)
                   (apply values info)])
       (define (mpi* mpi)
         (let ([p (resolved-module-path-name
                   (module-path-index-resolve mpi))])
           (if (path? p) (path->name p) p)))
       (let ([source-mod (mpi* source-mod)]
             [nominal-source-mod (mpi* nominal-source-mod)])
         (reply
          "~a"
          (string-append*
           `("`",id"' is a bound identifier,"
             " defined"
             ,(case source-phase
                [(0) ""] [(1) "-for-syntax"] [else (error "internal error")])
             " in \"",(format "~a" source-mod)"\""
             ,(if (not (eq? sym source-id))
                (format " as `~s'" source-id)
                "")
             " required"
             ,(case import-phase
                [(0) ""] [(1) "-for-syntax"] [else (error "internal error")])
             " "
             ,(if (equal? source-mod nominal-source-mod)
                "directly"
                (format "through \"~a\"~a"
                        nominal-source-mod
                        (if (not (eq? sym nominal-source-id))
                          (format " where it is defined as `~s'"
                                  nominal-source-id)
                          "")))
             ,(case nominal-export-phase
                [(0) ""] [(1) (format ", (exported-for-syntax)")]
                [else (error "internal error")]))))))]))

;; Based on help/help-utils

(define remove-doc-dir
  (regexp (string-append "^" (regexp-quote (path->string (find-doc-dir))) "/")))
(define doc-url "http://docs.racket-lang.org/")
(defverb #:whine (doc id) "find documentation for a binding"
  (define-values (sym identifier info) (binding-info id))
  (define xref
    (load-collections-xref (lambda () (log "Loading help index..."))))
  (if info
    (let ([tag (xref-binding->definition-tag xref info 0)])
      (if tag
        (let*-values ([(file anchor) (xref-tag->path+anchor xref tag)]
                      [(file) (path->string file)]
                      [(m) (regexp-match-positions remove-doc-dir file)]
                      [(url) (and m (string-append
                                     doc-url
                                     (substring file (cdar m))))]
                      [(url) (cond [(and anchor url)
                                    (string-append url "#" (uri-encode anchor))]
                                   [url url]
                                   [else "??hidden??"])])
          (reply "~a" url))
        (error 'help
               "no documentation found for: ~e provided by: ~a"
               sym
               (module-path-index-resolve (caddr info)))))
    (search-for-exports xref sym)))

(define (search-for-exports xref sym)
  (let ([idx (xref-index xref)]
        [libs null])
    (for ([entry (in-list idx)])
      (when (and (exported-index-desc? (entry-desc entry))
                 (eq? sym (exported-index-desc-name (entry-desc entry))))
        (set! libs (append libs (exported-index-desc-from-libs
                                 (entry-desc entry))))))
    (if (null? libs)
      (reply "not found in any library's documentation: ~a" sym)
      (reply "no docs for a current binding, but provided by: ~a"
             (string-join (map symbol->string (remove-duplicates libs))
                          ", ")))))

;; ----------------------------------------------------------------------------
;; Master tools

(define *master-password* #f)
(defverb #:hidden (authenticate ?passwd) "request a passwd, or use one"
  (cond [(not ?passwd)
         (let ([passwd (random 1000000000)])
           (set! *master-password* (cons (current-seconds) passwd))
           (log "--->>> Temporary password: ~a <<<---" passwd)
           (pm (*for-whom*) "Check the logs."))]
        [(not *master-password*)
         (reply "No password set")]
        [(> (- (current-seconds) (car *master-password*)) 120)
         (reply "Too late, generate a new password")]
        [(not (equal? (string->number ?passwd) (cdr *master-password*)))
         ;; avoid brute force attacks however unlikely
         (set! *master-password* #f)
         (log "Bad authentication attempt!")
         (reply "Bad password, generate a new one now")]
        [else
         (set-box! *my-master* (*full-id*))
         ;; in case the password was uttered publicly, avoid hijacks it
         (set! *master-password* #f)
         (log "I am a mindless puppet")
         (reply "[bows deeply] Welcome, oh great master!")]))

(defverb #:master (join channel) "ask me to join a channel"
  (if (regexp-match? #rx"^#" channel)
    (begin (out "JOIN ~a" channel) (reply "OK"))
    (reply "not a proper channel name")))

(defverb #:master (part channel) "ask me to part from a channel"
  (if (regexp-match? #rx"^#" channel)
    (begin (out "PART ~a" channel) (reply "OK"))
    (reply "not a proper channel name")))

(defverb #:master (tell who stuff ...) "tell me to tell someone something"
  (pm (*response-target*) "~a: ~a" who (string-join stuff)))

(defverb #:master (emote stuff ...) "tell me to do something"
  (pm (*response-target*) "\1ACTION ~a\1" (string-join stuff)))

(defverb #:master (ghost victim) "kill an errant client that's using my favorite nick"
  (pm "NickServ" (format "ghost ~a ~a" victim (*nickserv-password*))))

(defverb #:master (nick new-nick) "tell me to rename myself"
  (out "NICK ~a" new-nick))

(defverb #:master (system command ...) "run something"
  (let ([s (open-output-string)])
    (parameterize ([current-output-port s] [current-error-port s])
      (call-with-PATH (lambda () (call/whine system (string-join command)))))
    (let* ([s (get-output-string s)]
           [s (regexp-replace #rx"^[ \r\n]+" s "")]
           [s (regexp-replace #rx"[ \r\n]+$" s "")]
           [s (regexp-replace* #rx" *[\r\n] *" s " <NL> ")])
      (reply "~a" (if (equal? s "") "OK" s)))))

(define-namespace-anchor anchor)
(define my-namespace (namespace-anchor->namespace anchor))
(defverb #:master (top-eval expr ...) "evaluate something in the sandbox"
  (call/whine
   (lambda ()
     (reply "~s"
            (eval (read (open-input-string
                         (string-append "(begin " (string-join expr) ")")))
                  my-namespace)))))

;; Handy for when I discover the bot has somehow lost its auth --
;; perhaps due to a netsplit
(defverb #:master (reauth) "Resend password to NickServ"
  ;; BUGBUG -- keep the password out of the log.
  (pm "NickServ" "identify ~a" (*nickserv-password*)))

;; ----------------------------------------------------------------------------
;; Incubot-like

(define (get-incubot-witticism words)
  (define incubot-witticism ((*incubot-server*) 'get (map string-downcase words)))
  (define (strip-just-one rx) (curryr (curry regexp-replace rx) ""))

  ;; Ideally we'd prevent ACTION from getting into the corpus in the
  ;; first place.  See https://github.com/offby1/rudybot/issues/14
  (define (trim-ACTION str)
    (regexp-replace #rx"\1ACTION (.*)\1" str "\\1"))

  (define (trim-leading-nick str)
    (if (regexp-match #px"^http(s)?://" str)
        str
        ((strip-just-one #px"^\\w+?: *") str)))

  (and (string? incubot-witticism)
       (lambda ()
         (reply "~a"
                (trim-ACTION
                 (trim-leading-nick incubot-witticism))))))

;; ----------------------------------------------------------------------------
;; Main dispatchers

(define (do-cmd response-target for-whom words #:rate_limit? [rate_limit? #f])
  (parameterize ([*for-whom* for-whom]
                 [*response-target* response-target])
    (if (and rate_limit? (we-recently-did-something-for for-whom))
      (log "Not doing anything for ~a, since we recently did something for them."
           for-whom)
      (let loop ([words words]
                 [verb (string->symbol (string-downcase (car words)))])
        (cond [(or (hash-ref verbs verb #f)
                   (and (is-master?) (hash-ref master-verbs verb #f)))
               => (lambda (p)
                    (log "Doing for ~a: ~a ~s" for-whom verb (cdr words))
                    (p (cdr words)))]
              [(roughly-evaluable? for-whom (text-from-word words))
               (loop (cons "eval" words) 'eval)]
              [(get-incubot-witticism words)
               => (lambda (p) (p))]
              [else (log "Not doing for ~a: ~a" for-whom (text-from-word words))
                    (reply "eh?  Try \"~a: help\"." (unbox *my-nick*))])
        (note-we-did-something-for! for-whom)))))

;; Maps a word to a (text . (start . end)) position in the original text
;; (store the text to avoid worrying about a global `*current-line*'
;; thing that can disappear)
(define word-posns (make-weak-hasheq))
;; Utility to get text from a word to the end (also accepts a list, and
;; will use the first word); `emergency' is some value to use in case we
;; don't find the text
(define (text-from-word w [emergency w])
  (cond [(pair? w) (text-from-word (car w) emergency)]
        [(not (string? w))
         (error 'text-from-word "Bad code, I have a bug -- got: ~s" w)]
        [else (cond [(hash-ref word-posns w #f)
                     => (lambda (p) (substring (car p) (cadr p)))]
                    [(string? emergency) emergency]
                    [(list? emergency) (string-join emergency " ")]
                    [else (format "~a" emergency)])]))
;; Sometimes we need to take a substring of a word -- so use this to
;; register its info too
(define (substring+posn str b [e (string-length str)])
  (let ([p (hash-ref word-posns str #f)]
        [r (substring str b e)])
    (when p
      (hash-set! word-posns r
                 (cons (car p) (cons (+ (cadr p) b) (+ (cadr p) e)))))
    r))

(define rx:word #px"(?:\\p{L}+|\\p{N}+|\\p{S}|\\p{P})+")
(define (irc-process-line line)
  (let* ([posns (regexp-match-positions* rx:word line)]
         [words (map (lambda (p)
                       (let ([s (substring line (car p) (cdr p))])
                         (hash-set! word-posns s (cons line p))
                         s))
                     posns)])
    (when (null? words) (log "BAD IRC LINE: ~a" line))
    (parameterize ([*current-words* (cdr words)])
      (domatchers IRC-COMMAND (car words)))))
