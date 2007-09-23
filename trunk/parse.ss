#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
  exec  mzscheme                                        \
  -M errortrace                                         \
  --no-init-file                                        \
  --mute-banner                                         \
  --version                                             \
  --require "$0"                                        \
  -p "text-ui.ss" "schematics" "schemeunit.plt"         \
  -e "(exit (or #f (test/text-ui parse-tests 'verbose)))"
|#
(module parse mzscheme
(require (lib "trace.ss")
         (only (lib "string.ss") read-from-string)
         (only (lib "1.ss" "srfi")
               any
               first second third fourth fifth)
         (only (lib "13.ss" "srfi")
               string-tokenize
               )
         (only (lib "14.ss" "srfi")
               char-set
               char-set-complement
               char-set:whitespace
               )
         (only (lib "misc.ss""swindle") regexp-case)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only "globals.ss"
               register-version-string)
         "session.ss"
         (only (planet "assert.ss" ("offby1" "offby1.plt")) check-type))

(register-version-string "$Id$")

(define (make-sub-struct x sub-constructor . more-args)
  (apply sub-constructor
         (append (cdr (vector->list (struct->vector x)))
                 more-args)))

(define-struct (exn:fail:irc-parse exn:fail) (string))
(define-struct message (prefix command params) #f)
(define-struct prefix (nick user host) #f)
(define (parse-prefix str)
  (regexp-case
   str
   ((pregexp "^(.*?)(?:!(.*?))?(?:@(.*?))?$")
    => (lambda args (apply make-prefix (cdr args))))))

;; turn "NOTICE", e.g., into 'NOTICE
(define (public-message-command x)
  (read-from-string (message-command x)))

(define-struct (PRIVMSG message)
  (speaker receivers approximate-recipient text text-words)
  #f)
;(trace PRIVMSG-approximate-recipient)
;(trace PRIVMSG-text-words)

;; aka 366
(define-struct (RPL_ENDOFNAMES message) (channel-name) #f)
;(trace RPL_ENDOFNAMES?)

;; aka 433
(define-struct (ERR_NICKNAMEINUSE message) (channel-name) #f)

;; http://www.irchelp.org/irchelp/rfc/ctcpspec.html
(define-struct (CTCP PRIVMSG) (req/extended-data) #f)
(define-struct (ACTION CTCP) (text) #f)
(define-struct (VERSION CTCP) () #f)
(define-struct (SOURCE CTCP) () #f)
;; (trace make-message)
;; (trace make-PRIVMSG)
;; (trace make-CTCP)
(define (is-channel-name? x)
   (regexp-match #rx"^#" x))

(define (PRIVMSG-is-for-channel? m)
  (and (PRIVMSG? m)
       (any is-channel-name?
            (PRIVMSG-receivers m))))

;; (trace PRIVMSG-is-for-channel?)
;; (trace PRIVMSG-receivers)

(define (parse-irc-message string)
  (let ((m ;; http://tools.ietf.org/html/rfc1459#page-8
         (regexp-case
          string
          ((#rx"^(?::(.*?) )?(.*)$" prefix sans-prefix)
           (regexp-case
            sans-prefix
            ((#rx"^(.*?)( .*)$" command param-string)

             (let loop ((params '())
                        (param-string param-string))
               (if (zero? (string-length param-string))
                   (make-message (and prefix (parse-prefix prefix)) command (reverse params))
                 (regexp-case
                  param-string
                  ((#rx"^ :(.*)" trail)
                   (make-message (and prefix (parse-prefix prefix)) command (reverse (cons trail params))))
                  (((pregexp "^ ([^[:space:]]+)") one)
                   (loop (cons one params)
                         (substring param-string (string-length match)))))))))))))

    (when (not (message? m))
      (raise (make-exn:fail:irc-parse
              "Can't parse string from server"
              (current-continuation-marks)
              string)))
    (with-handlers
        (;; [exn:fail:contract?
;;           (lambda (e)
;;             (raise (make-exn:fail:irc-parse
;;                     "Can't parse string from server"
;;                     (current-continuation-marks)
;;                     string)))]
         )
      (or (maybe-make-RPL_ENDOFNAMES m)
          (maybe-make-ERR_NICKNAMEINUSE m)
          (maybe-make-PRIVMSG m)
          m))))

;(trace parse-irc-message)

(define (maybe-make-RPL_ENDOFNAMES m)
  (and (string=? "366" (message-command m))
       (make-sub-struct
        m
        make-RPL_ENDOFNAMES
        (second (message-params m)))))
(define (maybe-make-ERR_NICKNAMEINUSE m)
  (and (string=? "433" (message-command m))
       (make-sub-struct
        m
        make-ERR_NICKNAMEINUSE
        (second (message-params m)))))

(define (maybe-make-PRIVMSG m)
  (and (string=? "PRIVMSG" (message-command m))
       (let ((receivers (string-tokenize
                         (first (message-params m))
                         (char-set-complement (char-set #\,))))
             (text (second (message-params m))))
         (let ((text-tokens
                (string-tokenize
                 text
                 (char-set-complement char-set:whitespace))))
           (let ((p (make-sub-struct
                     m
                     make-PRIVMSG
                     (prefix-nick (message-prefix m))
                     receivers
                     (and (not (null? text-tokens))
                          (regexp-case
                           (car text-tokens)
                           ((pregexp "^([[:alnum:]]+)[:,]")
                            => (lambda args (second args)))
                           (else #f)))
                     text
                     text-tokens)))
             (or (maybe-make-CTCP p)
                 p))))))

;(trace maybe-make-PRIVMSG)

(define (maybe-make-CTCP p)
  (and (PRIVMSG? p)
       (regexp-case
        (PRIVMSG-text p)
        (((pregexp "\u0001([[:alpha:]]+)(?: (.*))?\u0001$") req/extended-data rest)
         (case (string->symbol req/extended-data)
           ((ACTION ) (make-sub-struct p make-ACTION  req/extended-data rest))
           ((VERSION) (make-sub-struct p make-VERSION req/extended-data     ))
           ((SOURCE ) (make-sub-struct p make-SOURCE  req/extended-data     ))
           (else (make-sub-struct p make-CTCP req/extended-data))))
        (else #f))))
;(trace maybe-make-CTCP)

(define (for-us? message session)
  (check-type 'for-us? message? message)
  (and
   (PRIVMSG? message)
   (any (lambda (r)
          (if (is-channel-name? r)
              (equal? (PRIVMSG-approximate-recipient message)
                      (irc-session-nick session))
            (equal? (irc-session-nick session)
                    r)))
        (PRIVMSG-receivers message))))

(define (text-for-us message sess)
  (check-type 'text-for-us message? message)
  (and (for-us? message sess)
       (cond
        ((and (PRIVMSG-is-for-channel? message)
              (< 1 (length (PRIVMSG-text-words message))))
         (cdr (PRIVMSG-text-words message)))
        ((and (not (PRIVMSG-is-for-channel? message))
              (< 0 (length (PRIVMSG-text-words message))))
         (PRIVMSG-text-words message))
        (else
         #f))))

(define (gist-for-us message sess)
  (check-type 'gist-for-us message? message)
  (let ((t (text-for-us message sess)))
    ;; trim trailing punctuation
    (and t
         (regexp-replace (pregexp "[^[:alpha:]]+$") (car t) ""))))

(define (gist-equal? str message sess)
  (check-type 'gist-equal? message? message)
  (equal? str (gist-for-us message sess)))
;(trace gist-equal?)
;(trace for-us?)
;(trace gist-for-us)

;(trace parse-irc-message)


(define-shortcut (test-parse input pref cmd params)
  (let ((m (parse-irc-message input)))
    (check-equal? (message-prefix  m) pref   (format "prefix of ~s"  input))
    (check-equal? (message-command m) cmd    (format "command of ~s" input))
    (check-equal? (message-params  m) params (format "params of ~s"  input))))

(define-shortcut (test-prefix-pieces input expected-pieces)
  (let ((p (message-prefix (parse-irc-message input))))
    (check-equal? (prefix-nick p) (first  expected-pieces) (format "nick of ~s" input))
    (check-equal? (prefix-user p) (second expected-pieces) (format "user of ~s" input))
    (check-equal? (prefix-host p) (third  expected-pieces) (format "host of ~s" input))))

(define parse-tests

  (test-suite
   "parsing"
   (test-case
    "barfs on malformed data from server"
    (check-exn
     exn:fail:irc-parse? (lambda () (parse-irc-message ":foo ")))
    (check-exn
     exn:fail:irc-parse? (lambda () (parse-irc-message ":foo :"))))

   (test-parse
    "empty trailing"
    ":foo bar baz :" (parse-prefix "foo") "bar" '("baz" ""))
   (test-parse
    "trailing"
    ":foo bar baz :params go here" (parse-prefix "foo") "bar" '("baz" "params go here"))
   (test-parse
    "NOTICE"
    ":localhost. NOTICE you :all suck"
    (parse-prefix "localhost.")
    "NOTICE"
    '("you" "all suck"))
   (test-parse
    "PRIVMSG"
    ":foo!foo@localhost. PRIVMSG #emacs :e1f: you all suck"
    (parse-prefix "foo!foo@localhost.")
    "PRIVMSG"
    '("#emacs" "e1f: you all suck"))
   (test-parse
    "MODE"
    ":ChanServ!ChanServ@services. MODE #cinema +tc"
    (parse-prefix "ChanServ!ChanServ@services.")
    "MODE"
    '("#cinema" "+tc"))
   (test-equal?
    "prefix"
    (message-prefix (parse-irc-message ":zip zap zop :snot"))
    (parse-prefix "zip"))
   (test-false
    "missing prefix"
    (message-prefix (parse-irc-message "NOTICE All Apple fanbois will be taken out back")))
   (test-equal?
    "command"
    (message-command (parse-irc-message "NOTICE All Apple fanbois will be taken out back"))
    "NOTICE")
   (test-equal?
    "trailing params (not ust trailing)"
    (message-params (parse-irc-message "COMMAND poo poo :platter puss"))
    (list "poo" "poo" "platter puss"))
   (test-suite
    "PRIVMSGs"
    (test-not-exn
     "No puke on a single-space"
     (lambda ()
       (parse-irc-message ":fledermaus!n=vivek@pdpc/supporter/active/fledermaus PRIVMSG #emacs : ")))
    (test-false
     "average command isn't a PRIVMSG"
     (PRIVMSG? (parse-irc-message "COMMAND poo poo :platter puss")))
    (test-pred
     "PRIVMSGs are indeed PRIVMSGs"
     PRIVMSG?
     (parse-irc-message ":X!X@Y PRIVMSG poo poo :platter puss"))
    (test-case
     "PRIVMSGs get properly parsed"
     (check-equal? (PRIVMSG-receivers (parse-irc-message ":X!X@Y PRIVMSG poo poo :platter puss"))
                   '("poo"))
     (check-equal? (PRIVMSG-text (parse-irc-message ":X!X@Y PRIVMSG poopoo :platter puss"))
                   "platter puss")
     (check-equal? (PRIVMSG-speaker (parse-irc-message ":fsbot!n=user@batfish.pepperfish.net PRIVMSG #emacs :yow!"))
                   "fsbot"))

    (test-suite
     "CTCP"
     (test-false
      "rejects non-actions"
      (ACTION?
       (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001UNDERWEAR eats cornflakes\u0001")))
     (test-case
      "recognizes and parses ACTION"
      (let ((m (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001ACTION eats cornflakes\u0001")))
        (check-pred PRIVMSG? m)
        (check-pred CTCP? m)
        (check-equal? (ACTION-text m) "eats cornflakes")))
     (test-case
      "recognizes VERSION"
      (check-pred
       VERSION?
       (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001VERSION\u0001")))

     (test-case
      "recognizes SOURCE"
      (check-pred
       SOURCE?
       (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001SOURCE\u0001"))))

    (test-case
     "channel versus truly private message"
     (check-pred
      PRIVMSG-is-for-channel?
      (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001ACTION eats cornflakes\u0001"))
     (check-false
      (PRIVMSG-is-for-channel?
       (parse-irc-message ":X!X@Y PRIVMSG sam :\u0001ACTION eats cornflakes\u0001"))))

    (test-case
     "approximate recipient"
     (check-false
      (PRIVMSG-approximate-recipient
       (parse-irc-message ":X!X@Y PRIVMSG sam :\u0001ACTION eats cornflakes\u0001")))
     (check-false
      (PRIVMSG-approximate-recipient
       (parse-irc-message ":X!X@Y PRIVMSG sam :well I think you smell")))
     (check-equal?
      (PRIVMSG-approximate-recipient
       (parse-irc-message ":X!X@Y PRIVMSG sam :well, I think you smell"))
      "well"))

    )
   (let* ((sess (make-irc-session
                 (open-output-string)))
          (nick (irc-session-nick sess)))
     (test-suite
      "gists"
      (test-not-false "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG ~a :yow" nick))                sess))
      (test-not-false "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG #some-chan :~a: yow" nick))    sess))
      (test-false     "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG x~ax :yow" nick))              sess))
      (test-false     "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG #some-chan :x~ax: yow" nick))  sess))

      (test-not-false
       "gist-equal?"
       (gist-equal?
        "yow"
        (parse-irc-message (format ":x!y@z PRIVMSG ~a :yow" nick))
        sess))

      (test-not-false
       "gist-equal?"
       (gist-equal?
        "yow"
        (parse-irc-message (format ":x!y@z PRIVMSG #ch-ch-ch-changes :~a, yow" nick))
        sess))))
   (test-suite
    "prefix"
    (test-prefix-pieces "nick only"        ":nick foo bar baz"             '("nick" #f #f))
    (test-prefix-pieces "nick and host"    ":nick@night foo bar baz"       '("nick" #f "night"))
    (test-prefix-pieces "nick, host, user" ":nick!knack foo bar baz"       '("nick" "knack" #f))
    (test-prefix-pieces "nick and user"    ":nick!knack@night foo bar baz" '("nick" "knack" "night"))
    (test-prefix-pieces "goofy"            ":fsbot!n=user@batfish.pepperfish.net a b c"
                        '("fsbot" "n=user" "batfish.pepperfish.net")))))

(provide (all-defined-except message-command)
         (rename public-message-command message-command)))
