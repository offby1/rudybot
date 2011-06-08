#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require
 racket/trace
 rackunit
 rackunit/text-ui
 (only-in amazon/aws-common run-tests/maybe-exit)
 )

(define (eat-ws inp)
 (regexp-try-match #px"^[[:space:]]+" inp))

(define (parse-prefix inp)
  (regexp-match #px"[[:graph:]]+" inp))

(define (parse-command inp)
  (regexp-match #px"[0-9]{3}|[[:alpha:]]+" inp))

(define (parse-params inp)
  (let loop ([result '()])
    (eat-ws inp)
    (cond
     ((eof-object? (peek-char inp))
      (reverse result))
     ((char=? #\: (peek-char inp))
      (begin
        (read-char inp)
        (if (eof-object? (peek-char inp))
            result
            (loop (cons `(param . ,(regexp-match #px"[^\u0000\r\n]+"  inp)) result)))))
     (else
      (loop   (cons `(param . ,(regexp-match #px"[^\u0000\r\n ]+" inp)) result))))))

(check-equal? (parse-params (open-input-string ":"))
              '())

;; I have the feeling I'm reinventing the wheel here.
(define-check (check-dicts-equal? d1 d2)
  (with-check-info (['actual d1]
                    ['expected d2])
      (when (not (equal?  (make-immutable-hash (dict->list d1))
                          (make-immutable-hash (dict->list d2))))
        (fail-check))))

(define (parse-crlf inp )
  (regexp-match #px"\r\n" inp))

(define (canonicalize-nick n)
  (string->bytes/utf-8
   (string-downcase (regexp-replace #rx"(?<=.)([`_]*)$" (bytes->string/utf-8 n) ""))))

(define (prefix->canonical-nick prefix)
  (match prefix
    [(regexp #rx"(.*)!(.*)@(.*)" (list _ nick id host)) (canonicalize-nick nick)]
    [_ "wtf"]))

(provide parse-message)
(define/contract parse-message
  ((or/c string? input-port?) . -> . list?)
  (match-lambda
   [(? string? message)
    (parse-message (open-input-string message))]
   [(? input-port? message)
    (append
     (if (char=? #\: (peek-char message))
         (begin
           (read-char message)
           (let* ([prefix (first (parse-prefix message))]
                  [nick (prefix->canonical-nick prefix)])
             `((prefix ,prefix)
               (nick ,nick))))
         '((prefix) (nick)))
     (begin
       (eat-ws message)
       (begin0
           `((command . ,(parse-command message))
             (params . ,(parse-params message)))
         (parse-crlf message))))]))

(define-test-suite all-tests
  (check-dicts-equal?
   (parse-message
    ":nick!knack@frotz 123 #channel :some stuff")
   '((prefix #"nick!knack@frotz")
     (command #"123")
     (nick #"nick")
     (params (param #"#channel")
             (param #"some stuff"))))

  (check-dicts-equal?
   (parse-message
    ":offby1!n=user@pdpc/supporter/monthlybyte/offby1 PRIVMSG ##cinema :rudybot:   uptime")
   '((prefix #"offby1!n=user@pdpc/supporter/monthlybyte/offby1")
     (command #"PRIVMSG")
     (nick #"offby1")

     ;; Ahh! It honors multiple consecutive spaces!  The old way didn't.
     (params (param #"##cinema") (param #"rudybot:   uptime"))))

  ;; As above, but with subtly different nick
  (check-dicts-equal?
   (parse-message
    ":offby1`!n=user@pdpc/supporter/monthlybyte/offby1 PRIVMSG ##cinema :rudybot:   uptime")
   '((prefix #"offby1`!n=user@pdpc/supporter/monthlybyte/offby1")
     (nick #"offby1")
     (command #"PRIVMSG")

     ;; Ahh! It honors multiple consecutive spaces!  The old way didn't.
     (params (param #"##cinema") (param #"rudybot:   uptime"))))

  (check-dicts-equal?
   (parse-message
    "COMMAND target :text shmext")
   '((prefix)
     (nick)
     (command #"COMMAND")
     (params (param #"target")
             (param #"text shmext"))))

  (check-equal? (parse-params (open-input-string ":"))
                '()))

(provide main)
(define (main)
  (run-tests/maybe-exit all-tests)
  (if #t
      (call-with-input-file
          "incoming"
        (lambda (ip)
          (for ([message (in-port read ip)])
            (write (parse-message message))
            (newline))))

      (parse-message  ":anthony.freenode.net 004 rudybot anthony.freenode.net ircd-seven-1.0.1 DOQRSZaghilopswz CFILMPQbcefgijklmnopqrstvz bkloveqjfI")))
