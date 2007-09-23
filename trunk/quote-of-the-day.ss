#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module quote-of-the-day mzscheme
(require (lib "trace.ss")
         (only (lib "uri-codec.ss" "net")
               current-alist-separator-mode)
         (only (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
               sxpath)
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only "port.ss" port->string/close)
         (only (lib "pretty.ss")
               pretty-display
               pretty-print)
         (only (lib "url.ss" "net")
               make-url
               make-path/param
               get-pure-port
               string->url
               url->string)
         (only (lib "1.ss" "srfi")
               append-map
               second))
(provide quotes-of-the-day)
(define (trim str)
  (regexp-replace*
   (pregexp "(\r|\n)+")
   str
   ""))
;;(trace trim)
;; TODO -- handle exceptions, in particular those which come from the
;; web site being down.
(define (quotes-of-the-day)
  (parameterize ((current-alist-separator-mode 'amp))
                (let* ((url (string->url "http://feeds.feedburner.com/quotationspage/qotd")))
                  (let ((stuff
                         (html->shtml
                          (port->string/close
                           (get-pure-port
                            url
                            (list))))
                         ))
                    ;; I suspect there's a way to do this with nothing
                    ;; more than a single call to sxpath -- no maps, no
                    ;; cars, no seconds.  Hmph.
                    (map (lambda (quote author)
                           (cons (trim quote)
                                 (trim author)))
                         (map second ((sxpath '(rss channel item description ) ) stuff))
                         ((sxpath '(rss channel item title *text*) ) stuff))))))
)
