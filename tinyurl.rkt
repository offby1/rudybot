#lang racket

(require
 (only-in net/uri-codec current-alist-separator-mode alist->form-urlencoded)
 (only-in net/url call/input-url string->url)
 (only-in net/url-structs path/param url)
 (only-in "http.rkt" get-pure-port/gack)
 )

(module+ test (require rackunit rackunit/text-ui))

;; stolen from erc-button.el in Emacs 22
(provide url-regexp)
;; TODO -- compare with https://tools.ietf.org/html/rfc3986#appendix-B
(define url-regexp (pregexp "http(s)?(//[-a-zA-Z0-9_.]+:[0-9]*)?[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,]+[-a-zA-Z0-9_=#$@~`%&*+\\/]"))

(provide/contract [make-tiny-url (string? . -> . string?)])
(define (make-tiny-url long-url)
  (call/input-url
   (url "http"
        #f
        "teensy.info"
        #f
        #t
        `(
          ,(path/param "shorten-" '())
          ,(path/param "" '()))
        `((input_url . ,long-url))
        #f)
   get-pure-port/gack
   port->string))

;; *groan* Now that I've added spam protection to teensy.info, these
;; tests fail with HTTP 401!  Dunno what to do about that.
(module+ test
  ;; TODO -- skip these tests if they can't possibly succeed, such as
  ;; when our host isn't the ec2 instance on which rudybot runs
  ;; (teensy.info requires a "Captcha" thing in order to accept
  ;; requests from other hosts)
 (define tinyurl-tests

   (test-suite
    "tinyurl"
    (test-case
     "absurdly long"
     (check-equal?
      (make-tiny-url "http://www.badastronomy.com/bablog/2008/05/26/best-image-ever/whoa/baby/surely-this-URL-is-long-enough-to-make-tiny")
      "http://teensy.info/dloXC4cxoW"))
    (test-case
     "photo.net"
     (with-handlers
         ([exn:fail:network?
           (lambda (e)
             (fprintf (current-error-port)
                      "Can't contact the URL shortener; skipping the test~%"))])
       (check-equal?
        (make-tiny-url "http://photo.net")
        "http://teensy.info/do55JLwjk5")))))
 (run-tests tinyurl-tests 'verbose))
