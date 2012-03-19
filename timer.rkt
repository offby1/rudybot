#lang racket

(require
 "corpus.rkt"
 "incubot.rkt"
 )

(define/contract (omit-needless-words ws)
  ((set/c string?) . -> . (set/c string?))
  ;; I got this list by querying a reasonbly-current copy of the
  ;; corpus like so:

  ;; select distinct(word), count(word) c from log_word_map group by word having c> 20000 order by c desc limit 20;
  (define noise (set "a" "and" "be" "but" "emacs" "for" "have" "i" "if" "in" "is" "it" "not" "of" "on" "that" "the" "to" "with" "you"))
  (set-subtract ws noise))

(define *c* (make-corpus '()))
(define *sentence* "I wonder if you could be bothered to get me a Grant's")
(displayln (time (incubot-sentence (values              (string->lowercased-words *sentence*)) *c*)))
(displayln (time (incubot-sentence (omit-needless-words (string->lowercased-words *sentence*)) *c*)))
