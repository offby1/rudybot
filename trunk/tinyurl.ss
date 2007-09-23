#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e '(test/text-ui tinyurl-tests)'
|#

(module tinyurl mzscheme
(require (lib "kw.ss")
         (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
         (lib "uri-codec.ss" "net")
         (only (lib "url.ss" "net")
               post-pure-port
               string->url)
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (lib "trace.ss")
         (only "port.ss" port->string/close)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "globals.ss")

(register-version-string "$Id$")

;; stolen from erc-button.el in Emacs 22
(define url-regexp (pregexp "http(s)?(//[-a-zA-Z0-9_.]+:[0-9]*)?[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,]+[-a-zA-Z0-9_=#$@~`%&*+\\/]"))

(define long-url
  (let loop ((kinda-long "http://foo.bar/baz/i/hope/this/is/long/enough"))
    (if (< (string-length kinda-long) (*tinyurl-url-length-threshold*))
        (loop (string-append kinda-long (format "/geez-louise~a" (string-length kinda-long))))
      kinda-long)))

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

;; string? -> string?
(define/kw (make-tiny-url url #:key [user-agent #f])
  (car
   ((sxpath '(html body (table 2) tr (td 2) p blockquote small a @ href *text*))
    (html->shtml
     (port->string/close
      (post-pure-port
       (string->url
        "http://tinyurl.com/create.php")
       (string->bytes/utf-8
        (parameterize ((current-alist-separator-mode 'amp))
          (alist->form-urlencoded `((url . ,url)))))

       ;; this works as is, but let us note for the record that the
       ;; "tinyurl creator" extension for Firefox
       ;; (https://addons.mozilla.org/en-US/firefox/addon/126) passes
       ;; a buttload more headers, namely

       ;; ("User-Agent", navigator.userAgent);
       ;; ("Accept", "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,video/x-mng,image/png,image/jpeg,image/gif;q=0.2,*/*;q=0.1");
       ;; ("Accept-Language", navigator.language);
       ;; ("Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.7");
       ;; ("Referer", "http://tinyurl.com/");

       (cons "Content-Type: application/x-www-form-urlencoded"
             (if user-agent
                 (list (format "User-Agent: ~a" user-agent ))
               '()))
       ))))))

(define tinyurl-tests

  (test-suite
   "tinyurl"
   (test-case
    "photo.net"
    (with-handlers
        ([exn:fail:network?
          (lambda (e)
            (fprintf (current-error-port)
                     "Can't contact tinyurl; skipping the test~%"))])
      (check-regexp-match
       #rx"^http://tinyurl.com/.....$"
       (make-tiny-url "http://photo.net"))))
   (test-case
    "including a user agent"
    (with-handlers
        ([exn:fail:network?
          (lambda (e)
            (fprintf (current-error-port)
                     "Can't contact tinyurl; skipping the test~%"))])
      (check-regexp-match
       #rx"^http://tinyurl.com/.....$"
       (make-tiny-url "http://photo.net" #:user-agent "test code for Eric's bot"))))
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

(provide (all-defined))
)