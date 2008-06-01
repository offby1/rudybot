#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
|#
#lang scheme

(require scheme/date
         scheme/port
         "sighting.ss"
         "spelled-out-time.ss"
         (except-in "tinyurl.ss" main)
         (lib "trace.ss")
         (lib "13.ss" "srfi")
         (lib "14.ss" "srfi")
         (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" )))

;; This value depends on the server; this seems to work for freenode
(define *bot-gives-up-after-this-many-silent-seconds* (make-parameter 250))
(define *my-nick* "upstartbot")
(define *nickserv-password* (make-parameter "SEKRIT PASSWIRD"))
(define *irc-server-hostname* (make-parameter "localhost"))

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
(define (slightly-more-sophisticated-line-proc line op)
  (define (out #:for-real? [for-real? #t] format-string . args)
    (let ((str (apply format format-string args)))
      (log "=> ~s" str)
      (when for-real?
        (fprintf op "~a~%" str))))

  (define (pm #:notice? [notice? #f] target fmt . args)
    (out #:for-real? (not (*mute-privmsgs?*))
         "~a" (format "~a ~a :~a"
                      (if notice? "NOTICE" "PRIVMSG")
                      target (apply format fmt args))))

  (define (do-cmd response-target response-prefix words)
    (define (reply fmt . args)
      (pm response-target "~a" (string-append response-prefix (apply format fmt args))))
    (log "Doing ~s" words)
    (case (string->symbol (string-downcase (first words)))
      [(quote)  (reply "No quotes yet; I'm workin' on it though")]
      [(source) (reply "$HeadURL$")]
      [(seen)
       (when (not (null? (cdr words)))
         (let ((info (lookup-sighting (second words))))
           (if info
               (reply "~a was last seen in channel ~a ~a ago, saying \"~a\""
                      (sighting-who   info)
                      (sighting-where info)
                      (spelled-out-time (- (current-seconds) (sighting-when  info)))
                      (string-join (sighting-words info)))
               (reply "No sign of ~a" (second words)))))]
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
                 ":If" "this" "is" "your" "nickname," yaddayaddayadda ...)
                (when (and (equal? nick "NickServ")
                           (equal? id   "NickServ")
                           (equal? host "services."))
                  (log "Gotta register my nick.")
                  (pm "NickServ" "identify ~a" (*nickserv-password*)))]
               [(list "JOIN" target)
                (note-sighting (make-sighting nick target (current-seconds) "JOIN" '()))
                (log "~a joined ~a" nick target)]
               [(list "NICK" (colon new-nick))
                (log "~a wants to be known as ~a" nick new-nick)]
               [(list "PART" target (colon first-word) rest ...)
                (note-sighting (make-sighting nick target (current-seconds) "PART" (cons first-word rest)))
                (log "~a left ~a~a"
                     nick target
                     (if (zero? (string-length first-word))
                         ""
                         (format ", saying ~a" (string-join (cons first-word rest)))))]

               [(list "PRIVMSG"
                      target
                      (regexp #px"^:\u0001([[:alpha:]]+)" (list _ extended-data-word ))
                      inner-words ...
                      (regexp #px"(.*)\u0001$" (list _ trailing )))
                (log "extended data: ~s ~s"
                     extended-data-word
                     (append inner-words
                             (if (positive? (string-length trailing))
                                 (list trailing)
                                 '())))]

               [(list "PRIVMSG"
                      target
                      (regexp #px"^:\u0001(.*)\u0001" (list _ request-word ))
                      rest ...)
                (log "request: ~s" request-word)]

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
                (if (equal? target *my-nick*)
                    (begin
                      (log "~a privately said ~a to me"
                           nick
                           (string-join (cons first-word rest)))

                      (do-cmd nick "" (cons first-word rest)))
                    (match first-word
                      [(regexp #px"^([[:alnum:]]+)[,:]" (list _ addressee))
                       (log "~a spake unto ~a in ~a, saying ~a"
                            nick
                            addressee
                            target
                            (string-join rest))
                       (when (equal? addressee *my-nick*)
                         (do-cmd target (format "~a: " nick) rest))]
                      [_
                       (log "~a mumbled something uninteresting in ~a"
                            nick
                            target)]))]

               [(list "QUIT" (colon first-word) rest ...)
                (note-sighting (make-sighting nick host (current-seconds) "QUIT" (cons first-word rest)))
                (log "~a quit~a"
                     nick
                     (if (zero? (string-length first-word))
                         ""
                         (format
                          ", mumbling \"~a\""
                          (string-join (cons first-word rest)))))]
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
