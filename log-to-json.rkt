#lang debug racket

;; read the big-log that rudybot emits, and convert the stupid
;; sexp-like entries to JSON, so that other tools can do something
;; with them.

(require (only-in json write-json jsexpr?)
         (only-in "lexer.rkt" parse-message))

(define/contract (maybe-parse-line l)
  (string? . -> . (or/c (and/c immutable? hash?) false/c))
  (match l
    ;; The log has two different varieties of entry: one is a sort of
    ;; sexp, which we recognize because it begins with a left paren;
    ;; the other is an unparsed string, which we parse via lexer.rkt).
    [(regexp #px"^(.{19}Z) <= (\\(.*\\))" (list _ timestamp sexp))
     (make-immutable-hasheq
      `(
        (timestamp . ,timestamp)
        ,@(simplify-log (read  (open-input-string sexp)))
        )
      )]
    [(regexp #px"^(.{19}Z) <= (\".*\")" (list _ timestamp string))
     (make-immutable-hasheq
      `(
        (timestamp . ,timestamp)
        ,@(simplify-log (parse-message (read (open-input-string string))))
        )
      )]
    [_ #f]))

(define (simplify-log alist)
  (append-map
   (match-lambda
     ;; I have no idea why the log writes out "params" this
     ;; verbosely.
     [(list 'params (list 'param (? bytes? v)) ...)
      (list (cons 'params (map bytes->string/utf-8 v)))]

     [(list symbol (? bytes? v))
      (list (cons symbol (bytes->string/utf-8 v)))]

     [_ '()])
   alist))


(module+ test
  (require rackunit)
  (check-false (maybe-parse-line "fred"))
  (check-false (maybe-parse-line "2016-07-13T04:24:14Z Main starting."))
  (check-false (maybe-parse-line "2015-08-23T20:55:35Z => (left-pointing-arrow-only)"))
  (check-equal? (maybe-parse-line "2011-06-03T14:11:54Z <= \":brx!~brx@p4FE11B58.dip.t-dialin.net JOIN :#emacs\"")
                #hasheq((timestamp . "2011-06-03T14:11:54Z")
                        (params . ("#emacs"))
                        (command . "JOIN")
                        (prefix . "brx!~brx@p4FE11B58.dip.t-dialin.net")))
  (check-equal? (simplify-log '((prefix #"niven.freenode.net")
                                (command #"001")
                                (params
                                 (param #"rudybot")
                                 (param #"Welcome to the freenode Internet Relay Chat Network rudybot"))))
                '((prefix . "niven.freenode.net")
                  (command . "001")
                  (params . ("rudybot" "Welcome to the freenode Internet Relay Chat Network rudybot"))))

  (check-equal?
   (maybe-parse-line
    "2015-08-23T20:55:35Z <= ((prefix #\"weber.freenode.net\") (command #\"NOTICE\") (params (param #\"*\") (param #\"*** Looking up your hostname...\")))")
   #hasheq((timestamp . "2015-08-23T20:55:35Z")
           (prefix . "weber.freenode.net")
           (command . "NOTICE")
           (params . ("*"  "*** Looking up your hostname..."))))
  (check-equal?
   (maybe-parse-line
    "2010-01-19T03:01:07Z <= \"NOTICE AUTH :*** Checking ident\"")
   #hasheq((timestamp . "2010-01-19T03:01:07Z")
           (command . "NOTICE")
           (params . ("AUTH" "*** Checking ident"))))
  (check-equal?
   (maybe-parse-line
    "2011-06-03T17:43:54Z <= ((params (param . #f)))")
   #hasheq((timestamp . "2011-06-03T17:43:54Z")))
  )


(module+ main
  (call-with-input-file "big-log"
    (lambda (inf)
      (call-with-output-file "big-log.json" #:exists 'append
        (lambda (outf)
          (for ([(line index) (in-indexed (in-lines inf))])
            (when (zero? (remainder index 5000))
              (printf "~a~%" index))
            (with-handlers ([exn:fail?
                             (lambda (e)
                               (printf "~s => ~a~%" line e))]
                            )
              (let ([maybe-json (maybe-parse-line line)])
                (when maybe-json
                  (write-json maybe-json outf)
                  (newline outf))))
            ))))
    ))
