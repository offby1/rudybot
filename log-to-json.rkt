#lang debug racket

;; read the big-log that rudybot emits, and convert the stupid
;; sexp-like entries to JSON, so that other tools can do something
;; with them.

(require (only-in json write-json jsexpr?)
         (only-in "lexer.rkt" parse-message))

(define/contract (maybe-parse-line l)
  (string? . -> . (or/c (cons/c string? any/c) false/c))
  (match l
    [(regexp #px"^(.{19}Z) <= (.*)" (list _ timestamp meat))

     ;; The log has two different varieties of entry: one is a sort of
     ;; sexp, which we recognize because it begins with a left paren;
     ;; the other is an unparsed string, which we parse via
     ;; lexer.rkt).

     (match (string-ref meat 0)
       [#\(
        (cons timestamp (to-jsexpr (read  (open-input-string meat))))]
       [#\"
        (cons timestamp (to-jsexpr (parse-message (read (open-input-string meat)))))]
       [_ #f])
     ]
    [_ #f]))

(define/contract (to-jsexpr alist)
  (dict? . -> . jsexpr?)
  (make-immutable-hasheq
   (list
    (cons 'command (bytes->string/utf-8 (car (dict-ref alist 'command))))
    (cons 'prefix  (bytes->string/utf-8 (car (dict-ref alist 'prefix))))
    (cons 'params  (map (compose bytes->string/utf-8 second) (dict-ref alist 'params))))))

(module+ test
  (require rackunit)
  (check-false (maybe-parse-line "fred"))
  (check-false (maybe-parse-line "2016-07-13T04:24:14Z Main starting."))
  (check-false (maybe-parse-line "2015-08-23T20:55:35Z => (left-pointing-arrow-only)"))
  (check-equal? (maybe-parse-line "2011-06-03T14:11:54Z <= \":brx!~brx@p4FE11B58.dip.t-dialin.net JOIN :#emacs\"")
                (cons "2011-06-03T14:11:54Z"
                      #hasheq((params . ("#emacs"))
                              (command . "JOIN")
                              (prefix . "brx!~brx@p4FE11B58.dip.t-dialin.net"))
                      ))
  (check-equal? (to-jsexpr '((prefix #"niven.freenode.net")
                             (command #"001")
                             (params
                              (param #"rudybot")
                              (param #"Welcome to the freenode Internet Relay Chat Network rudybot"))))
                #hasheq((prefix . "niven.freenode.net")
                        (command . "001")
                        (params . ("rudybot" "Welcome to the freenode Internet Relay Chat Network rudybot"))))

  (check-equal?
   (maybe-parse-line
    "2015-08-23T20:55:35Z <= ((prefix #\"weber.freenode.net\") (command #\"NOTICE\") (params (param #\"*\") (param #\"*** Looking up your hostname...\")))")
   (cons "2015-08-23T20:55:35Z"
         #hasheq((prefix . "weber.freenode.net")
                 (command . "NOTICE")
                 (params . ("*"  "*** Looking up your hostname...")))))
  )


(module+ main
  (call-with-input-file "big-log"
    (lambda (inf)
      (for ([line (in-lines inf)])
        (match (maybe-parse-line line)
          [(cons timestamp sexp )
           (write-json (list timestamp sexp))
           (newline)]
          [_ #f])
        ))
    ))
