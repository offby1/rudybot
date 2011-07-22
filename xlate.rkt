#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require net/url
         (planet dherman/json:3:0)
         (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(provide xlate t8 main)

;; Translate text using Google's translation APIs.

;; The returned string sometimes has HTML entities in it; the
;; functions on this page translate those to regular characters.

;; From http://htmlhelp.com/reference/html40/entities/special.html
(define entity-integers-by-name
  #hash(
        ("quot"   . 34)
        ("amp"    . 38)
        ("lt"     . 60)
        ("gt"     . 62)
        ("OElig"  . 338)
        ("oelig"  . 339)
        ("Scaron" . 352)
        ("scaron" . 353)
        ("Yuml"   . 376)
        ("circ"   . 710)
        ("tilde"  . 732)
        ("ensp"   . 8194)
        ("emsp"   . 8195)
        ("thinsp" . 8201)
        ("zwnj"   . 8204)
        ("zwj"    . 8205)
        ("lrm"    . 8206)
        ("rlm"    . 8207)
        ("ndash"  . 8211)
        ("mdash"  . 8212)
        ("lsquo"  . 8216)
        ("rsquo"  . 8217)
        ("sbquo"  . 8218)
        ("ldquo"  . 8220)
        ("rdquo"  . 8221)
        ("bdquo"  . 8222)
        ("dagger" . 8224)
        ("Dagger" . 8225)
        ("permil" . 8240)
        ("lsaquo" . 8249)
        ("rsaquo" . 8250)
        ("euro"   . 8364)
        ))

(define (replace-html-entities str)
  (define (numeric str)
    (regexp-replace*
     #px"&#([0-9]+);"
     str
     (lambda (whole-match digits)
       (string (integer->char (string->number digits))))))
  (define (named str)
    (regexp-replace*
     #px"&([a-z]+);"
     str
     (lambda (whole-match word)
       (let ([replacement (hash-ref entity-integers-by-name word #f)])
         (if replacement
             (format "&#~a;" replacement)
             str)))))
  (numeric (named str)))

(define-test-suite replace-tests
  (check-equal?
   (replace-html-entities "")
   "")

  (check-equal?
   (replace-html-entities "frotz")
   "frotz")

  (check-equal?
   (replace-html-entities "&frotz;")
   "&frotz;")

  (check-equal?
   (replace-html-entities "&amp;")
   "&")

  (check-equal?
   (replace-html-entities "&quot;plonk&quot;")
   "\"plonk\"")

  (check-equal?
   (replace-html-entities "frotz&#77;")
   "frotzM")

  (check-equal?
   (replace-html-entities "frotz&#123;why not&#65;")
   "frotz{why notA"))


(define (snag text from to)
  (call/input-url
   (make-url
    "http" #f "ajax.googleapis.com" #f #t
    (map ((curryr make-path/param) '()) (list "ajax" "services" "language" "translate"))
    `([v . "1.0"]
      [q . ,text]
      [langpair . ,(format "~a|~a" from to)]) #f)
   get-pure-port
   read-json))

(define-test-suite snag-tests
  (check-equal?
   (hash-ref
    (snag "print \"hello, world\\n\"" "perl" "java")
    'responseDetails)
   "invalid translation language pair"))

;; List of language codes, to "from" and "to":
;; http://code.google.com/apis/ajaxlanguage/documentation/reference.html#LangNameArray

(define (xlate from to text)
  (let* ([stuff (snag text from to)]
         [responseStatus (hash-ref stuff 'responseStatus)])
    (cond
     [(equal? responseStatus 200)
      (replace-html-entities
       (hash-ref
        (hash-ref
         stuff
         'responseData)
        'translatedText))]
     [(and (equal? responseStatus 400)
           (regexp-match #rx"invalid.*pair" (hash-ref stuff 'responseDetails)))
      (format "~a: see http://code.google.com/apis/language/translate/v1/reference.html#LangNameArray"
              (hash-ref stuff 'responseDetails))]
     [else
      (hash-ref stuff 'responseDetails)])))

(define t8 xlate)

(define-test-suite xlate-tests

  (check-equal?
   (xlate "en" "it" "forty-five separate amendments")
   "45 modifiche a parte")

  (check-equal?
   (xlate "en" "fr" "fledermaus: have I rubbed this in your face yet?")
   "Fledermaus: j'ai frotté présent dans votre visage encore?")

  (check-regexp-match
   #rx"invalid.*http://"
   (xlate "frotz" "plotz" "I doubt this will get translated properly")))

(define-test-suite all-tests
  replace-tests
  snag-tests
  xlate-tests)

(define (main . args)
  (exit (run-tests all-tests 'verbose)))
