#lang racket

(require
 "corpus.rkt"
 "incubot.rkt"
 )

(define *c* (make-corpus '()))

(module+ main
  (for ([sentence
         '("I wonder if you could be bothered to get me a Grant's"
           "If everybody looked the same, we'd get tired of looking at each other"
           "the i to a is it that you and in of for but not with have be on emacs if"
           "do what it's just like are so can or my was this use at an don't there as no how"
           )])
    (for ([_ (in-range 2)])
      (printf "~a => ~a~%"
              sentence
              (time (incubot-sentence (string->lowercased-words sentence) *c*))))))
