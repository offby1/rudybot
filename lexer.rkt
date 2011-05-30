#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require
 racket/trace
 rackunit
 rackunit/text-ui
 )

(define (eat-ws inp)
 (regexp-try-match #px"^[[:space:]]+" inp))
;(trace eat-ws)

(define (parse-prefix inp )
  (regexp-match #px"[[:graph:]]+" inp))
;(trace parse-prefix)

(define (parse-command inp)
  (if (char-numeric? (peek-char inp))
      (regexp-match #px"[0-9]{3}" inp)
      (regexp-match #px"[[:alpha:]]+" inp)))
;(trace parse-command)

(define (parse-params inp)
  (let loop ([result '()])
    (eat-ws inp)
    (cond
     ((eof-object? (peek-char inp))
      (reverse result))
     ((char=? #\: (peek-char inp))
      (begin
        (read-char inp)
        (loop (cons `(param . ,(regexp-match #px"[^\u0000\r\n]+" inp)) result))))
     (else
      (loop (cons `(param . ,(regexp-match #px"[^\u0000\r\n ]+" inp)) result))))))

;(trace parse-params)

(define (parse-crlf inp )
  (regexp-match #px"\r\n" inp))
;(trace parse-crlf)

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
               `((command . ,(parse-command message))
                 (params . ,(parse-params message)))
             (parse-crlf message)))))]))

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
