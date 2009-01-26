#| Hey Emacs, this is -*-scheme-*- code!
|#
#lang scheme

(require scheme/port
         scheme/sandbox
         scheme/system
         srfi/19
         (except-in "sandboxes.ss" main)
         "utils.ss"
         "git-version.ss"
         "sighting.ss"
         "spelled-out-time.ss"
         (except-in "quotes.ss" main)
         (except-in "tinyurl.ss" main)
         srfi/13
         srfi/14
         (planet schematics/macro/macro)
         (planet neil/numspell/numspell))

;; This value depends on the server; this seems to work for freenode
(define *my-nick*
  (make-parameter (from-env "BOTNICK" "rudybot")))
(define *my-master*
  #f
  #; ; use authentication by default
  (regexp (string-append
           "^" (from-env "BOTMASTER" "offby1!n=.*\\.avvanta\\.com") "$")))
(define *initial-channels* ; env var can be "#foo,#bar"
  (make-parameter (from-env "BOTCHANNELS" '("#scheme" "#emacs") #rx",")))
(define *nickserv-password*
  (make-parameter (from-env "BOTPASSWD" #f)))
(define *bot-gives-up-after-this-many-silent-seconds* (make-parameter 250))

(define *irc-server-hostname* (make-parameter "localhost"))

(define *start-time*            (current-seconds))
(define *connection-start-time* (make-parameter #f))

;; some state is put globally, to be able to separate functions conveniently
(define *irc-output*      (make-parameter #f))
(define *current-words*   (make-parameter #f))
(define *response-target* (make-parameter #f))
(define *for-whom*        (make-parameter #f))
(define *full-id*         (make-parameter #f))

(define *sandboxes* (make-hash))
(define *max-values-to-display* 5)
(define *log-ports* (make-parameter (list (current-error-port)
                                          (open-output-file
                                           "big-log"
                                           #:mode 'text
                                           #:exists 'append))))

(for ((op (in-list (*log-ports*))))
  (fprintf (current-error-port) "Whopping port ~a~%" op)
  (with-handlers ([exn:fail? values])
    (file-stream-buffer-mode op 'line)))

(define (log . args)
  (for ((op (in-list (*log-ports*))))
    (fprintf op "~a " (date->string (current-date) "~4"))
    (apply fprintf op args)
    (newline op)))

(define (is-master?)
  (let ([mm *my-master*] [id (*full-id*)])
    (cond [(regexp? mm) (regexp-match? mm id)]
          [(string? mm) (equal? mm id)]
          [else #f])))

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

(define (out format-string . args)
  (let* ((str (apply format format-string args))
         (str (if (> (string-length str) *max-output-line*)
                (string-append (substring str 0 (- *max-output-line* 4)) " ...")
                str))
         ;; don't display newlines, so that Bad Guys won't be able
         ;; to inject IRC commands into our output.
         (str (regexp-replace* #rx"[\n\r]" str " <NEWLINE> ")))
    (log "=> ~s" str)
    (fprintf (*irc-output*) "~a~%" str)))

(define (pm #:notice? [notice? #f] target fmt . args)
  (out "~a" (format "~a ~a :~a"
                    (if notice? "NOTICE" "PRIVMSG")
                    target (apply format fmt args))))

;; patterns for `slightly-more-sophisticated-line-proc'

(defmatcher IRC-COMMAND "ERROR"
  (log "Uh oh!"))

;; Here we wait for a NOTICE before authenticating.  I suspect this doesn't
;; work for all servers; in particular, ngircd-0.10.3 doesn't say anything when
;; we connect.
(defmatcher IRC-COMMAND "NOTICE"
  (when (eq? *authentication-state* 'havent-even-tried)
    (out "NICK ~a" (*my-nick*))
    ;; RFC 1459 suggests that most of this data is ignored.
    (out "USER luser unknown-host localhost :Eric Hanchrow's bot, version ~a"
         (git-version))
    (set! *authentication-state* 'tried)))

(defmatcher IRC-COMMAND "PING"
  (out "PONG ~a" (car (*current-words*))))

(defmatcher IRC-COMMAND (regexp #rx"^:(.*)!(.*)@(.*)$"
                                (list full-id nick id host))
  (define (espy target action words)
    (note-sighting (make-sighting nick target (current-seconds) action words)))
  (if (equal? nick (*my-nick*))
    (log "I seem to have said ~s" (*current-words*))
    (match (*current-words*)
      [(list "NOTICE" my-nick ":This"  "nickname" "is" "registered."
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
       (espy host (format "changing their nick to ~a" first-word) '())]
      [(list "TOPIC" target (colon first-word) rest ...)
       (espy target
             (format "changing the channel's topic to '~a'"
                     (string-join (cons first-word rest))) '())]
      [(list "JOIN" target)
       ;; Alas, this pretty much never triggers, since duncanm keeps his client
       ;; session around for ever
       (when (regexp-match #rx"^duncanm" nick)
         (pm target "la la la"))
       (espy target
             (format "joining")
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
       (cond
         [(regexp-match? #rx"bot$" nick)
          (log "nick '~a' ends with 'bot', so I ain't gonna reply.  Bot wars, you know."
               nick)]
         [(equal? target (*my-nick*))
          (log "~a privately said ~a to me"
               nick (string-join (cons first-word rest)))
          (parameterize ([*full-id* full-id])
            (do-cmd nick nick (cons first-word rest) #:rate_limit? #f))]
         [else
          (when (and (regexp-match? #rx"^(?i:let(')?s)" first-word)
                     (regexp-match? #rx"^(?i:jordanb)" nick))
            (log "KOMEDY GOLD: ~s" (cons first-word rest)))
          (match first-word
            ;; TODO -- use a regex that matches just those characters that
            ;; are legal in nicknames, followed by _any_ non-white
            ;; character -- that way people can use, say, a semicolon after
            ;; our nick, rather than just the comma and colon I've
            ;; hard-coded here.
            [(regexp #px"^([[:alnum:]_]+)[,:](.*)" (list _ addressee garbage))
             (when (equal? addressee (*my-nick*))
               (let ((words (if (positive? (string-length garbage))
                              (cons garbage rest)
                              rest)))
                 (when (not (null? words))
                   (parameterize ([*full-id* full-id])
                     (do-cmd target nick words #:rate_limit?
                             (and #f
                                  (not (regexp-match #rx"^offby1" nick))
                                  (equal? target "#emacs" )))))))]
            [",..."
             (when (equal? target "#emacs")
               (pm target "Arooooooooooo"))]
            [_ #f])])]

      [(list "QUIT" (colon first-word) rest ...)
       (espy host "quitting"
             (cons first-word rest))]
      [_ (log "~a said ~s, which I don't understand" nick (*current-words*))])))

(defmatcher IRC-COMMAND (colon host)
  (match (*current-words*)
    [(list digits mynick blather ...)
     (case (string->number digits)
       ((1)
        (log "Yay, we're in")
        (set! *authentication-state* 'succeeded)
        (for ([c (*initial-channels*)]) (out "JOIN ~a" c)))
       ((366)
        (log "I, ~a, seem to have joined channel ~a."
             mynick
             (car blather)))
       ((433)
        (log "Nuts, gotta try a different nick")
        (*my-nick* (string-append (*my-nick*) "_"))
        (out "NICK ~a" (*my-nick*))))]))

(defmatcher IRC-COMMAND _ (log "Duh?"))

(define verbs        (make-hasheq))
(define master-verbs (make-hasheq))
(define verb-lines        '())
(define master-verb-lines '())
(require (for-syntax (only-in scheme last drop-right)))
(define-syntax (*defverb stx)
  (define (id->str id) (symbol->string (syntax-e id)))
  (syntax-case stx ()
    [(_ verbs verb-lines (verb arg ...) desc body ...)
     (and (andmap identifier? (syntax->list #'(verb arg ...)))
          (string? (syntax-e #'desc)))
     (let* ([args (map id->str (syntax->list #'(arg ...)))]
            [formstr
             (apply string-append
                    (id->str #'verb)
                    (map (lambda (a)
                           (cond [(equal? a "...") " ..."]
                                 [(regexp-match? #rx"^[?]" a)
                                  (string-append " [<" (substring a 1) ">]")]
                                 [else (string-append " <" a ">")]))
                         args))])
       (define clause
         (if (and (pair? args) (regexp-match? #rx"^[?]" (last args)))
           (let* ([args (syntax->list #'(arg ...))]
                  [opt  (last args)]
                  [args (drop-right args 1)])
             #`[(list* #,@args (and #,opt (or (list _) '())))
                (let ([#,opt (and (pair? #,opt) (car #,opt))]) body ...)])
           #'[(list arg ...) body ...]))
       #`(begin (hash-set! verbs 'verb
                           (match-lambda #,clause
                                         [_ (reply "Expecting: ~a" #,formstr)]))
                (set! verb-lines (cons '(verb #,formstr desc) verb-lines))))]))
(define-syntax defverb
  (syntax-rules ()
    [(_ #:master (verb arg ...) desc body ...)
     (*defverb master-verbs master-verb-lines (verb arg ...) desc body ...)]
    [(_ #:hidden (verb arg ...) desc body ...)
     (*defverb verbs master-verb-lines (verb arg ...) desc body ...)]
    [(_ (verb arg ...) desc body ...)
     (*defverb verbs verb-lines (verb arg ...) desc body ...)]))

(define (reply fmt . args)
  (let* ((response-target (*response-target*))
         (for-whom        (*for-whom*))
         (response-prefix (if (equal? response-target for-whom)
                            ""
                            (format "~a: " for-whom))))
    (pm response-target "~a~a" response-prefix (apply format fmt args))))

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
  (let ((q (one-quote)))
    ;; special case: jordanb doesn't want quotes prefixed with his nick.
    (match (*for-whom*)
      [(regexp #rx"^jordanb") (pm (*response-target*) "~a" q)]
      [_ (reply "~a" q)])))

(defverb (source) "my source location"
  (reply "http://github.com/offby1/rudybot/tree/~a" (git-version 'complete)))

(defverb (seen nick) "did I see someone?"
  (reply "~a" (nick->sighting-string nick)))

(defverb (uptime) "how long was I awake"
  (reply "I've been up for ~a; this tcp/ip connection has been up for ~a"
         (describe-since *start-time*)
         (describe-since (*connection-start-time*))))

(define default-sandbox-language '(begin (require scheme)))

(define (call/whine f . args)
  (define (on-error e)
    (let ((whine (if (exn? e) (exn-message e) (format "~s" e))))
      (apply reply
             ;; make sure our error message begins with "error: ".
             (if (regexp-match #rx"^error: " whine)
               (list "~a" whine)
               (list "error: ~a" whine)))))
  (with-handlers ([void on-error]) (apply f args)))

(define (do-init lang force?)
  (let* ([lang (if lang (string->symbol lang) default-sandbox-language)]
         [lang (case lang
                 [(r5rs) '(special r5rs)]
                 [else lang])]
         [sb (get-sandbox-by-name *sandboxes* (*for-whom*) lang force?)])
    (when force? (reply "your sandbox is ready"))
    sb))

(define (do-eval words give-to)
  (define for-whom (*for-whom*))
  (define response-target (*response-target*))
  ;; catch _all_ exceptions from the sandbox, to prevent "eval (raise 1)" or
  ;; any other error from killing this thread (including creating the sandbox).
  (when give-to
    (cond ((equal? give-to (*my-nick*)) (error "I'm full, thanks."))
          ((equal? give-to for-whom)
           ;; allowing giving a value to yourself can lead to a nested call
           ;; to `call-in-sandbox-context' which will deadlock.
           (error "Talk to yourself much too?"))))
  (let ((s (do-init #f #f)))
    (call-with-values (lambda () (sandbox-eval s (string-join words)))
      (lambda values
        ;; Even though the sandbox runs with strict memory and time limits, we
        ;; use call-with-limits here anyway, because it's possible that the
        ;; sandbox can, without exceeding its limits, return a value that will
        ;; require a lot of time and memory to convert into a string!
        ;; (make-list 100000) is an example.
        (call-with-limits 10 20 ; 10sec, 20mb
          (lambda ()
            (define (display-values values displayed)
              (define (next) (display-values (cdr values) (add1 displayed)))
              (cond ((null? values) (void))
                    ((void? (car values)) (next))
                    ;; prevent flooding
                    ((>= displayed *max-values-to-display*)
                     (reply
                      "; ~a values is enough for anybody; here's the rest in a list: ~s"
                      (number->english *max-values-to-display*)
                      (filter (lambda (x) (not (void? x))) values)))
                    (else (reply "; Value~a: ~s"
                                 (if (positive? displayed)
                                   (format "#~a" (add1 displayed))
                                   "")
                                 (car values))
                          (sleep 1)
                          (next))))
            (define (display-output name output-getter)
              (let ((output (output-getter s)))
                (when (and (string? output) (positive? (string-length output)))
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
                         (msg* "has given you a value, use (GRAB)"))
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
                             "~a: ~a ~a" give-to for-whom msg))))))
            (display-output 'stdout sandbox-get-stdout)
            (display-output 'stderr sandbox-get-stderr)))))))

(defverb (init ?lang)
  "initialize a sandbox, <lang> can be 'r5rs, 'scheme, 'scheme/base, etc"
  (call/whine do-init ?lang #t))
(defverb (eval expr ...) "evaluate an expression(s)"
  (call/whine do-eval expr #f))
(defverb (give to expr ...) "evaluate and give someone the result"
  (call/whine do-eval expr to))

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
         (set! *my-master* (*full-id*))
         (log "I am a mindless puppet")
         (reply "[bows deeply] Welcome, oh great master!")]))

(defverb #:master (join channel) "ask me to join a channel"
  (if (regexp-match? #rx"^#" channel)
    (begin (out "JOIN ~a" channel) (reply "OK"))
    (reply "not a proper channel name")))

(defverb #:master (system command ...) "run something"
  (let ([s (open-output-string)])
    (parameterize ([current-output-port s] [current-error-port s])
      (call/whine system (string-join command)))
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

(define (do-cmd response-target for-whom words #:rate_limit? [rate_limit? #f])
  (parameterize ([*for-whom* for-whom]
                 [*response-target* response-target])
    (if (and rate_limit? (we-recently-did-something-for for-whom))
      (log "Not doing anything for ~a, since we recently did something for them."
           for-whom)
      (let* ((verb (string->symbol (string-downcase (car words))))
             (proc (or (hash-ref verbs verb #f)
                       (and (is-master?) (hash-ref master-verbs verb #f)))))
        (log "~a ~a ~s" (if proc "Doing" "Not doing") verb (cdr words))
        (when proc (proc (cdr words)))
        (note-we-did-something-for! for-whom)))))

;; Given a line of input from the server, do something side-effecty.
;; Writes to OP get sent back to the server.
(define (slightly-more-sophisticated-line-proc line)
  (log "<= ~s" line)
  (let ((toks (string-tokenize line (char-set-adjoin char-set:graphic #\u0001))))
    (parameterize ([*current-words* (cdr toks)])
      (domatchers IRC-COMMAND (car toks)))))

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
                    (parameterize ([*irc-output* op])
                      (slightly-more-sophisticated-line-proc line))
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
