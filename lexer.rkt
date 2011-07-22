#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require
 rackunit
 rackunit/text-ui
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
     [(eof-object? (peek-char inp))
      (reverse result)]
     [(char=? #\: (peek-char inp))
      (begin
        (read-char inp)
        (if (eof-object? (peek-char inp))
            result
            (loop (cons `(param . ,(regexp-match #px"[^\u0000\r\n]+"  inp)) result))))]
     [else
      (loop   (cons `(param . ,(regexp-match #px"[^\u0000\r\n ]+" inp)) result))])))

(check-equal? (parse-params (open-input-string ":"))
              '())

(define (parse-crlf inp )
  (regexp-match #px"\r\n" inp))

(provide parse-message)
(define/contract parse-message
  ((or/c string? input-port?) . -> . list?)
  (match-lambda
   [(? string? message)
    (parse-message (open-input-string message))]
   [(? input-port? message)
    (begin0
        (cons
         (if (char=? #\: (peek-char message))
             (begin
               (read-char message)
               (cons 'prefix (parse-prefix message)))
             '(prefix))
         (begin
           (eat-ws message)
           (begin0
               `([command . ,(parse-command message)]
                 [params . ,(parse-params message)])
             (parse-crlf message)))))]))

(check-equal?
 (parse-message
  ":offby1!n=user@pdpc/supporter/monthlybyte/offby1 PRIVMSG ##cinema :rudybot:   uptime")
 '((prefix #"offby1!n=user@pdpc/supporter/monthlybyte/offby1")
   (command #"PRIVMSG")

   ;; Ahh! It honors multiple consecutive spaces!  The old way didn't.
   (params (param #"##cinema") (param #"rudybot:   uptime"))))

(check-equal?
 (parse-message
  ":nick!knack@frotz 123 #channel :some stuff")
 '((prefix #"nick!knack@frotz")
   (command #"123")
   (params (param #"#channel")
           (param #"some stuff"))))

(provide main)
(define (main)
  (if #t
      (call-with-input-file
          "incoming"
        (lambda (ip)
          (for ([message (in-port read ip)])
            (write (parse-message message))
            (newline))))

      (parse-message  ":anthony.freenode.net 004 rudybot anthony.freenode.net ircd-seven-1.0.1 DOQRSZaghilopswz CFILMPQbcefgijklmnopqrstvz bkloveqjfI")))
