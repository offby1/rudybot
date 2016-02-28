#lang racket
; Hey Emacs, this is -*-scheme-*- code!

;; Run my tests with ``raco test racket-script-template.rkt''.
;; Invoke my "main" with ``racket racket-script-template.rkt''.

(module+ test
  (require rackunit rackunit/text-ui))

(require
 (only-in net/url post-impure-port))

(provide (struct-out exn:fail:http))
(struct exn:fail:http exn:fail (code))

(provide post-pure-port/gack)
;; Like post-pure-port, but raises an exception if the HTTP response
;; is anything other than 200.
(define (post-pure-port/gack url post [header null])
  (let ([inp (post-impure-port url post header)])
    (check-and-discard-status-and-header-lines inp)
    inp))

(define (check-and-discard-status-and-header-lines inp)
  (define status-line (read-bytes-line inp 'return-linefeed))

  (match-define
   (list _ http-version status-code explanation)
   (regexp-match #px"(HTTP/.*?) ([0-9]{3}) (.*)" status-line))

  (when (not (bytes=? status-code #"200"))
    (raise (exn:fail:http (bytes->string/utf-8 explanation)
                          (current-continuation-marks)
                          (string->number (bytes->string/utf-8 status-code)) )))

  (for ([l (in-bytes-lines inp 'return-linefeed)])
    #:break (bytes=? l #"")
    ;; It's a header line, so drop it on the floor
    'kerplunk)

  inp)

;; BUGBUG -- doesn't handle "folded" header lines -- those that begin
;; with a space or a tab.  See
;; http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2.2
(module+ test
  (struct http-response (code message seconds mime headers body) #:transparent)
  (define (split-headers inp)
    (match (port->string inp)
      [(regexp #px"(.*?)\r\n\r\n(.*)" (list _ header-string body-string))
       (list header-string body-string)]))


  (define (header-dict)
    (make-immutable-custom-hash string-ci=?
                                (compose equal-hash-code string-downcase)))

  (define (lines->hash lines)
    (apply dict-set* (header-dict)
           (flatten (map (lambda (l)
                           (match l
                             [(regexp "(.*?): *(.*)" (list _ key value)) (list key value)]))
                         lines))))
  (define (port->http-response inp)
    (match-define (list header-string body)
                  (split-headers inp))
    (match-define
     (list status-line header-lines ...)
     (regexp-split "\r\n" header-string))

    (match-define
     (list _ http-version status-code explanation)
     (regexp-match #px"(HTTP/.*?) ([0-9]{3}) (.*)" status-line))

    (http-response (string->number status-code) explanation 0 "mime? wozzat?"
                   (lines->hash header-lines)
                   body))
  (define 404-response-string "HTTP/1.0 404 Not Found\r\nContent-Type: text/html\r\nFrotz: plotz\r\nDate: Sun, 04 Aug 2013 21:29:18 GMT\r\n\r\nHey You!!")
  (define r (port->http-response (open-input-string 404-response-string)))
  (check-equal? (http-response-code r)    404)
  (check-equal? (http-response-message r) "Not Found")
  (let ([headers (http-response-headers r)])
    (check-equal? (dict-ref headers "Content-Type") "text/html")
    (check-equal? (dict-ref headers "frotz"       ) "plotz")
    (check-equal? (dict-ref headers "date"        ) "Sun, 04 Aug 2013 21:29:18 GMT")
    )
  (check-equal? (http-response-body r) "Hey You!!")
  (check-equal? (split-headers (open-input-string "frotz\r\n\r\nplotz\nmore\r\neven more"))
                '("frotz" "plotz\nmore\r\neven more"))
  (check-equal? (port->string (check-and-discard-status-and-header-lines (open-input-bytes #"HTTP/2.3 200 Eva thang funky\r\nNow what\r\nFoo: Bar\r\nLong Line\r\n\tcontinuation\r\n\r\nBody!\nMore Breck Body!!\r\n")))
                "Body!\nMore Breck Body!!\r\n")
  (check-exn
   (lambda (e)
     (and (exn:fail:http? e)
          (= 201 (exn:fail:http-code e))
          (string=? "Outlook not so good" (exn-message e))))
   (thunk (check-and-discard-status-and-header-lines (open-input-bytes #"HTTP/2.3 201 Outlook not so good")))))
