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
         (planet "delicious.ss" ("untyped" "delicious.plt" 1))
         (lib "url.ss" "net")
         (all-except "../del.ss" exn:delicious:auth?))

(define *factoids*
  (let ((ip (get-pure-port (string->url "http://svn.borg.ch/mirror/factoids-200710.svn"))))
    (begin0
        (read ip)
      (close-input-port ip))))

(dump-request-urls? #t)
(dump-sxml-responses? #t)

(parameterize ((current-username "tucumcari")
               (current-password
                ;; We can't use DELICIOUS_PASSWORD for the environment
                ;; variable here, because globals.ss will probably
                ;; have clobbered it, to prevent it from leaking into
                ;; the sandboxes.
                (or (getenv "PASSWORD")
                    "no delicious password specified.")))
  (for-each
   (lambda (factoid)
     (let ((key (car (first factoid)))
           (value (car (second factoid))))
       (printf "~a ..." key)
       (add-post/raw!
        (key->url-string key)
        key
        value
        (list "svnfaq")
        (current-date)
        #t
        #t)
       (printf "~%")))
   *factoids*))

)
