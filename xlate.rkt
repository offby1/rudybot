#lang racket
(require net/url
         json)

(module+ test (require rackunit rackunit/text-ui))

(provide xlate t8)

;; The returned data sometimes has HTML entities in it; the functions
;; on this page translate those to regular characters.

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
       (cond
        ((hash-ref entity-integers-by-name word #f)
         => (curry format "&#~a;"))
        (else
         str)))))
  (numeric (named str)))

(module+ test
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
     "frotz{why notA")))

;; Translate text using Google's translation API v2.

(define (snag text from to)

  (call/input-url
   (make-url
    "https"               ;; scheme
    #f                    ;; user
    "www.googleapis.com"  ;; host
    #f                    ;; port
    #t                    ;; path-absolute?

    ;; path
    (map ((curryr make-path/param) '()) (list "language" "translate" "v2"))

    ;; query
    `([key . ,(bytes->string/utf-8 (get-preference 'google-API-key))]
      [q . ,text]
      [source . ,from]
      [target . ,to])
    #f ;; fragment
    )
   get-pure-port
   read-json))

(module+ test
  (define-test-suite snag-tests
    (check-equal?
     (hash-ref
      (hash-ref
       (snag "print \"hello, world\\n\"" "perl" "java")
       'error)
      'message)
     "Invalid Value")))

;; List of language codes, to "from" and "to":
;; https://developers.google.com/translate/v2/using_rest#language-params

(define (xlate from to text)
  (let* ([stuff (snag text from to)]
         [data (hash-ref stuff 'data #f)])
    (cond
     [data
      (replace-html-entities
       (hash-ref
        (first
         (hash-ref
          (hash-ref
           stuff
           'data)
          'translations))
        'translatedText))]
     [else
      (hash-ref (hash-ref stuff 'error) 'message)])))

(define t8 xlate)

(module+ test
  (define-test-suite xlate-tests

    ;; TODO -- wrap these in an exception handler, as well as a
    ;; timeout thingy, so that we can run tests without an Internet
    ;; connection.

    (displayln "")
    (displayln (xlate "en" "it" "forty-five separate amendments"))
    (displayln "45 emendamenti separati")

    (displayln "")
    (displayln (xlate "en" "fr" "fledermaus: have I rubbed this in your face yet?"))
    (displayln "fledermaus: je n'ai frotté dans votre visage encore?")

    (displayln "")
    (displayln (xlate "frotz" "plotz" "I doubt this will get translated properly"))
    (displayln "Invalid Value"))

  (define-test-suite all-tests
    replace-tests
    snag-tests
    xlate-tests)
  (run-tests all-tests 'verbose))
