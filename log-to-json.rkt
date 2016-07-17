#lang debug racket

;; read the big-log that rudybot emits, and convert the stupid
;; sexp-like entries to JSON, so that other tools can do something
;; with them.

(require (only-in json write-json jsexpr?))

(define/contract (maybe-parse-line l)
  (string? . -> . (or/c (cons/c string? any/c) false/c))
  (match l
    [(regexp #px"^(.{19}Z) <= (.*)" (list _ timestamp sexp))
     (cons timestamp (flatten-params (read  (open-input-string sexp))))
     ]
    [_ #f]))

(define (flatten-params alist)
  (let ([params (dict-ref alist 'params #f)])
    (if params
        (dict-set alist 'params (map second params))
        alist)))


(define/contract (to-jsexpr value)
  (any/c . -> . jsexpr?)
  (match value
    [(? bytes? v)
     (bytes->string/utf-8 v)]
    [(? list? v)
     #:when (dict? v)
     (make-immutable-hasheq (map (lambda (p) (cons (first p)
                                                   (to-jsexpr (second p))))
                       v))]
    [(? list? v)
     #:when (and (symbol? (first v))
                 (string? (second v)))
     (list (symbol->string (first v))
           (second v))
     ]
    [(? list? v)
     (map to-jsexpr v)]
    [(? symbol? v)
     v]
    [(? string? v)
     v]
    ))

(module+ test
  (require rackunit)
  (check-false (maybe-parse-line "fred"))
  (check-false (maybe-parse-line "2016-07-13T04:24:14Z Main starting."))
  (check-false (maybe-parse-line "2015-08-23T20:55:35Z => (left-pointing-arrow-only)"))
  (check-equal?
   (maybe-parse-line "2015-08-23T20:55:35Z <= ((prefix #\"weber.freenode.net\") (command #\"NOTICE\") (params (param #\"*\") (param #\"*** Looking up your hostname...\")))")
   (cons "2015-08-23T20:55:35Z"
         '((prefix #"weber.freenode.net") (command #"NOTICE") (params  #"*"  #"*** Looking up your hostname..."))))

  (check-equal? (to-jsexpr #"a byte string") "a byte string")
  (check-equal? (to-jsexpr '(symbol "string"))  '("symbol" "string"))
  (check-equal? #hasheq((a . "a")
                        (b . "b"))
                #hasheq((b . "b")
                        (a . "a")))
  (check-true (hash-eq? (to-jsexpr '((k1 "v1")))))
  (check-equal? (to-jsexpr '((k1 "v1")))
                #hasheq((k1 . "v1")))

  (check-equal?
   (flatten-params '((prefix #"nick!knack@frotz") (command #"123") (params (param #"#channel") (param #"some stuff"))))
   '((prefix #"nick!knack@frotz")
     (command #"123")
     (params #"#channel" #"some stuff")))
  (check-equal? (flatten-params #hasheq((k1 . "v1")
                                        (params . ((param "foo")(param "bar")))))
                #hasheq((k1 . "v1")
                        (params .  ("foo" "bar"))))
  (match (maybe-parse-line "2015-08-23T20:55:35Z <= ((prefix #\"weber.freenode.net\"))")
    [(cons timestamp sexp)
     (check-equal?
      (to-jsexpr sexp)
      #hasheq((prefix . "weber.freenode.net")))]
  ))


(module+ main
  (call-with-input-file "big-log"
    (lambda (inf)
      (for ([line (in-lines inf)])
        (match (maybe-parse-line line)
          [(cons timestamp sexp )
           (printf "~a~%" (list timestamp (to-jsexpr sexp)))]
          [_ #f])
        ))
    ))
