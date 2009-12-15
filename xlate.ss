#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require srfi/26
         net/url
         (planet dherman/json:3:0)
         (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(provide xlate main)
;; TODO -- the returned string sometimes has HTML entities in it:
;; <offby1> ,t8 en fr fledermaus: have I rubbed this in your face yet?
;; <rudybot> Fledermaus: j&#39;ai frotté dans votre visage encore inscrit?
(define (xlate text from to)
  (hash-ref
   (hash-ref
    (call/input-url
     (make-url
      "http" #f "ajax.googleapis.com" #f #t
      (map (cut make-path/param <> '()) (list "ajax" "services" "language" "translate"))
      `((v . "1.0")
        (q . ,text)
        (langpair . ,(format "~a|~a" from to))) #f)
     get-pure-port
     read-json)
    'responseData)
   'translatedText))

(define-test-suite xlate-tests

  (check-equal?
   "quarantacinque emendamenti separati"
   (xlate "forty-five separate amendments" "en" "it")))

(define (main . args)
  (exit (run-tests xlate-tests 'verbose)))