#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require
 net/uri-codec
 net/url
 (planet schematics/schemeunit:3)
 (planet schematics/schemeunit:3/text-ui))

;; stolen from erc-button.el in Emacs 22
(provide url-regexp)
(define url-regexp (pregexp "http(s)?(//[-a-zA-Z0-9_.]+:[0-9]*)?[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,]+[-a-zA-Z0-9_=#$@~`%&*+\\/]"))

(define (url->tinyurl-body url reader)
  (call/input-url
   (string->url "http://tinyurl.com/api-create.php")
   (lambda (create-url)
     (post-pure-port
      create-url
      (string->bytes/utf-8
       (parameterize ((current-alist-separator-mode 'amp))
         (alist->form-urlencoded `((url . ,url)))))

      (list "Content-Type: application/x-www-form-urlencoded")))
   reader))

(provide/contract [make-tiny-url (string? . -> . string?)])
(define (make-tiny-url url)
  (match  (url->tinyurl-body url port->string)
    [(regexp #rx"http://.*" (list url))
     url]
    [_ "??"]))


(define tinyurl-tests

  (test-suite
   "tinyurl"
   (test-case
    "absurdly long"
    (check-regexp-match
     #px"^http://tinyurl.com/.{5,6}$"
     (make-tiny-url "http://www.badastronomy.com/bablog/2008/05/26/best-image-ever/whoa/baby/surely-this-URL-is-long-enough-to-make-tiny")))
   (test-case
    "photo.net"
    (with-handlers
        ([exn:fail:network?
          (lambda (e)
            (fprintf (current-error-port)
                     "Can't contact tinyurl; skipping the test~%"))])
      (check-regexp-match
       #px"^http://tinyurl.com/.{5,6}$"
       (make-tiny-url "http://photo.net"))))
   (test-case
    "including a user agent"
    (with-handlers
        ([exn:fail:network?
          (lambda (e)
            (fprintf (current-error-port)
                     "Can't contact tinyurl; skipping the test~%"))])
      (check-regexp-match
       #px"^http://tinyurl.com/.{5,6}$"
       (make-tiny-url "http://photo.net"))))))

(define (main . args)
  (exit (run-tests tinyurl-tests 'verbose)))

(provide main)
