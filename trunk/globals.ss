#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module globals mzscheme
(require (lib "kw.ss")
         (lib "include.ss")
         (only (lib "etc.ss") this-expression-source-directory)
         (only (lib "1.ss" "srfi")
               second))

(include "version.ss")

(define *desired-nick* (make-parameter "rudybot"))

(define *client-name* "Eric Hanchrow (aka offby1)'s bot")

(define version-strings #f)
(define register-version-string #f)

(let ((version-string-registry '("$Id$")))

  (set! version-strings
        (lambda () version-string-registry))
  (set! register-version-string
        (lambda ( str)
          (set! version-string-registry (cons str version-string-registry)))))

(define (long-version-string) (format
                               "~a (offby1@blarg.net):v3.~a:PLT scheme version ~a on ~a"
                               *client-name*

                               ;; *sigh*.  The version string with
                               ;; which we reply to CTCP can't have a
                               ;; colon, but of course Subversion's
                               ;; keyword expansion inserted a colon
                               ;; into *client-version*, so we have to
                               ;; parse out the numbers.

                               (regexp-replace #rx":" *svnversion-string* "-")
                               (version)
                               (system-type 'os)))

;; #f means read from stdin, write to stdout
(define *irc-server-name* (make-parameter #f))

(define *initial-channel-names* (make-parameter '()))
(define *random?* (make-parameter #t))
(define *minimum-delay-for-periodic-spew* (make-parameter (* 20 60)))
(define *quotes-file-name* (make-parameter
                                    (build-path
                                     (this-expression-source-directory)
                                     "quotes")))

(define *use-real-atom-feed?* (make-parameter #f))
(define *nickserv-password* (make-parameter #f))
(define (*atom-timestamp-preference-name*)
  (if (*use-real-atom-feed?*)
      'rudybot-planet-emacs-last-headlines
    'rudybot-test-planet-emacs-last-headlines))

;; The bot will "tiny-ify" URLs longer than this.  Tiny URLs are about
;; 25 characters, so it seems reasonable to ignore URLs that are
;; shorter than triple that.
(define *tinyurl-url-length-threshold* (make-parameter 75))

(define *del.icio.us-password* (make-parameter #f))
(define *log-output-port* (make-parameter (current-error-port)))
(define *log-to-file* (make-parameter #f))
(provide (all-defined))
)