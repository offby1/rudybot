#lang racket

;; read the big-log that rudybot emits, and convert the stupid
;; sexp-like entries to JSON, so that other tools can do something
;; with them.

(define/contract (maybe-parse-line l)
  (string? . -> . (or/c (cons/c string? any/c) false/c))
  (match l
    [(regexp #px"^(.{19}Z) <= (.*)" (list _ timestamp sexp))
     (cons timestamp (read  (open-input-string sexp)))
     ]
    [_ #f]))

(module+ test
  (require rackunit)
  (check-false (maybe-parse-line "fred"))
  (check-false (maybe-parse-line "2016-07-13T04:24:14Z Main starting."))
  (check-equal?
   (maybe-parse-line "2015-08-23T20:55:35Z <= ((prefix #\"weber.freenode.net\") (command #\"NOTICE\") (params (param #\"*\") (param #\"*** Looking up your hostname...\")))")
   (cons "2015-08-23T20:55:35Z"
         '((prefix #"weber.freenode.net") (command #"NOTICE") (params (param #"*") (param #"*** Looking up your hostname..."))))))

(module+ main
  (call-with-input-file "big-log"
    (lambda (inf)
      (for ([line (in-lines inf)])
        (match (maybe-parse-line line)
          [(cons timestamp sexp )
           (printf "JSONify ~a: ~a~%" timestamp sexp)]
          [_ #f])
        ))
    ))
