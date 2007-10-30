#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0"
|#
(module import-wayita-factoids mzscheme
(require (only (lib "1.ss" "srfi") first second)
         (only (lib "19.ss" "srfi")
               date->string
               current-date)(lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "delicious.ss" ("untyped" "delicious.plt" 1 3))
         (only (lib "url.ss" "net")
               combine-url/relative
               get-pure-port
               string->url
               url->string))

(define *factoids*
  (let ((ip (get-pure-port (string->url "http://svn.borg.ch/mirror/factoids-200710.svn"))))
    (begin0
        (read ip)
      (close-input-port ip))))

(dump-request-urls? #t)
(dump-sxml-responses? #t)

(parameterize ((current-username "tucumcari")
               (current-password (or (getenv "DELICIOUS_PASSWORD") "no delicious password specified.")))
  (for-each
   (lambda (factoid)
     (let ((key (car (first factoid)))
           (value (car (second factoid))))
      (printf "~a ..." key)
      ;; TODO -- if the description is just a URL, use that instead of
      ;; a bogus URL.
     (add-post/raw!
      (url->string (combine-url/relative (string->url "http://bogus.url/") key))
      key
      value
      (list "svnfaq" key)
      (current-date)
      #t
      #t)
     (printf "~%")))
   *factoids*))

)
