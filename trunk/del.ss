#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui del.icio.us-tests 'verbose))"
|#

(module del mzscheme
(require (lib "kw.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "memoize.ss" ("dherman" "memoize.plt" )) define/memo*)
         (lib "cmdline.ss")
         (all-except (planet "delicious.ss" ("untyped" "delicious.plt" 1 1))
                     exn:delicious:auth?)
         (rename (planet "delicious.ss" ("untyped" "delicious.plt" 1 1))
                 auth-exn exn:delicious:auth?)
         (only (lib "19.ss" "srfi")
               date->string
               date->time-utc)
         (only (lib "1.ss" "srfi")
               every
               take)
         (lib "pretty.ss")
         (lib "trace.ss")
         "globals.ss"
         "headline.ss"
         "vprintf.ss")
(register-version-string "$Id$")

;; return all items with the tag "moviestowatchfor"

;;(dump-sxml-responses? #t)

;; this is less pointless than it looks -- it lets me export the
;; identifier without having to say (provide all-from (planet
;; "delicious.ss" ...)), which I don't wanna do, because who knows
;; what all is in there.
(define exn:delicious:auth? auth-exn)

;; it'd sure be nice if I could use keywords with a memoized function,
;; but ...
(define/kw (snarf-some-recent-posts
            #:key [tag "moviestowatchfor"])

  (parameterize
      ((current-password
        (or (*del.icio.us-password*)

            ;; more convenient for testing
            (getenv "DELICIOUS_PASSWORD")

            "unknown"))

       (current-username "tucumcari"))


    (map (lambda (post)
           (make-entry (date->time-utc (post-date post))
                       (post-description post)
                       (post-url post)))
         (recent-posts tag))))


(define del.icio.us-tests

  (test-suite
   "del.icio.us"

   (test-not-false
    "gets some movies, and they're all entries"
    (with-handlers ([(lambda (e)
                       (or (exn:delicious:auth? e)
                           (exn:fail:network? e)))
                     (lambda (e)
                       (fprintf
                        (current-error-port)
                        "wrong delicious password OR can't contact del.icio.us; skipping the test~%")
                       #t)])
      (let ((snarfage (snarf-some-recent-posts)))
        (check-false     (null? snarfage) "didn't return any entries")
        (check-not-false (every entry? snarfage) "They're not all entries")))
    )))
(provide (all-defined)))
