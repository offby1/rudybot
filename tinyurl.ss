#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require html
         xml
         mzlib/trace
         net/uri-codec
         net/url
         (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define *tinyurl-url-length-threshold* (make-parameter 75))

;; stolen from erc-button.el in Emacs 22
(define url-regexp (pregexp "http(s)?(//[-a-zA-Z0-9_.]+:[0-9]*)?[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,]+[-a-zA-Z0-9_=#$@~`%&*+\\/]"))

;; string? -> (listof string?)
(define (snag-urls-from-bytes bytes)
  (let ((ip (open-input-bytes bytes)))
    (let loop ((result '()))
      (if (eof-object? (peek-char ip))
          (reverse result)
        (let ((match (regexp-match url-regexp ip)))
          (if match
              (loop
               (cons (car match)
                     result))
            (reverse result)))))))

;; Yes, we're screen-scraping, in direct defiance of tinyurl's wishes.
;; I've asked them for API info as of June 2010, and will use their
;; API if it's convenient.  If not, there are plenty of other
;; URL-shortening services out there, at least some of which also have
;; APIs.  From the list at
;; http://www.shareaholic.com/account/services:


;; | bit.ly    | free API; requires registration                |
;; | clicky.me | ditto                                          |
;; | digg      | unclear.  Anyway, feh.                         |
;; | hub.tm    | seems dead                                     |
;; | ls.gd     | apparently no API                              |
;; | j.mp      | redirects to bit.ly                            |
;; | su.pr     | web site blares "get more traffic" -- bad sign |
;; | tinyurl   | supposedly has API; enquired for details       |
;; | goo.gl    | for google products only                       |

(define (url->tinyrul-html url reader)
  (call/input-url
   (string->url "http://tinyurl.com/create.php")
   (lambda (create-url)
     (post-pure-port
      create-url
      (string->bytes/utf-8
       (parameterize ((current-alist-separator-mode 'amp))
         (alist->form-urlencoded `((url . ,url)))))

      (cons "Content-Type: application/x-www-form-urlencoded"
            '())))
   reader))

;; string? -> string?
(define (make-tiny-url url)
  (match (url->tinyrul-html
          url
          (lambda (ip) (regexp-match #px"\\[<a href=\"(http://tinyurl.com/(.*?))\"" ip)))
    [(list whole result last-bit)
     (bytes->string/utf-8 result)]
    [_ "??"]))

(trace make-tiny-url)

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
       (make-tiny-url "http://photo.net"))))
   (test-equal?
    "empty snagging"
    (snag-urls-from-bytes
     #"I'm telling ya, photo.net is rilly cool")
    (list))
   (test-equal?
    "snagging"
    (snag-urls-from-bytes
     #"I'm telling ya, http://photo.net/foo?bar=baz, is, like rilly cool; http://microsoft.com is not")
    (list #"http://photo.net/foo?bar=baz" #"http://microsoft.com"))))

(define (main . args)
  (exit (run-tests tinyurl-tests 'verbose)))
(provide/contract [make-tiny-url (string? . -> . string?)])
(provide url-regexp main)
