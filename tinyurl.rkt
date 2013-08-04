#lang racket

(require
 (only-in net/uri-codec current-alist-separator-mode alist->form-urlencoded)
 (only-in net/url call/input-url string->url)
 (only-in "http.rkt" post-pure-port/gack)
 )

(module+ test (require rackunit rackunit/text-ui))

;; stolen from erc-button.el in Emacs 22
(provide url-regexp)
(define url-regexp (pregexp "http(s)?(//[-a-zA-Z0-9_.]+:[0-9]*)?[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,]+[-a-zA-Z0-9_=#$@~`%&*+\\/]"))

(provide/contract [make-tiny-url (string? . -> . string?)])
(define (make-tiny-url url)
  (call/input-url
   (string->url "http://tinyurl.com/api-create.php")
   (lambda (create-url)
     (post-pure-port/gack
      create-url
      (string->bytes/utf-8
       (parameterize ([current-alist-separator-mode 'amp])
         (alist->form-urlencoded `([url . ,url]))))

      (list "Content-Type: application/x-www-form-urlencoded")))
   port->string))

(module+ test
 (define tinyurl-tests

   (test-suite
    "tinyurl"
    (test-case
     "absurdly long"
     (check-equal?
      (make-tiny-url "http://www.badastronomy.com/bablog/2008/05/26/best-image-ever/whoa/baby/surely-this-URL-is-long-enough-to-make-tiny")
      "http://tinyurl.com/3l4lw7"))
    (test-case
     "photo.net"
     (with-handlers
         ([exn:fail:network?
           (lambda (e)
             (fprintf (current-error-port)
                      "Can't contact tinyurl; skipping the test~%"))])
       (check-equal?
        (make-tiny-url "http://photo.net")
        "http://tinyurl.com/uecfh")))))
 (run-tests tinyurl-tests 'verbose))
