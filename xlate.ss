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

;; The returned string sometimes has HTML entities in it; this
;; translates those to regular characters.
(define (replace-html-entities str)
  (regexp-replace*
   #px"&#([0-9]+);"
   str
   (lambda (whole-match digits)
     (string (integer->char (string->number digits))))))

(define-test-suite replace-tests
  (check-equal?
   ""
   (replace-html-entities ""))

  (check-equal?
   "frotz"
   (replace-html-entities "frotz"))

  (check-equal?
   "frotzM"
   (replace-html-entities "frotz&#77;"))

  (check-equal?
   "frotz{why notA"
   (replace-html-entities "frotz&#123;why not&#65;")))

(define (snag text from to)
  (call/input-url
   (make-url
    "http" #f "ajax.googleapis.com" #f #t
    (map (cut make-path/param <> '()) (list "ajax" "services" "language" "translate"))
    `((v . "1.0")
      (q . ,text)
      (langpair . ,(format "~a|~a" from to))) #f)
   get-pure-port
   read-json))

(define (xlate text from to)
  (let* ([stuff (snag text from to)]
         [responseStatus (hash-ref stuff 'responseStatus)])
    (if (equal? 200 responseStatus)
        (replace-html-entities
         (hash-ref
          (hash-ref
           stuff
           'responseData)
          'translatedText))
        (hash-ref stuff 'responseDetails))))

(define-test-suite xlate-tests

  (check-equal?
   (xlate "forty-five separate amendments" "en" "it")
   "quarantacinque emendamenti separati")

  (check-equal?
   (hash-ref
    (snag "print \"hello, world\\n\"" "perl" "java")
    'responseDetails)
   "invalid translation language pair")

  (check-equal?
   (xlate "fledermaus: have I rubbed this in your face yet?" "en" "fr")
   "Fledermaus: j'ai frotté dans votre visage encore inscrit?")
  )

(define-test-suite all-tests
  replace-tests
  xlate-tests)

(define (main . args)
  (exit (run-tests all-tests 'verbose)))
