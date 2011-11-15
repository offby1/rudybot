#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; Some code to reply in an alarmingly-human-like way.  Idea, but not
;; the code, utterly stolen from Peter Danenberg (aka "klutometis"),
;; to whom all credit is due.

;; The basic idea: someone says something to the bot.  The bot doesn't
;; recognize that input as one of its built-in commands, so this code
;; runs.  This code breaks the input into words, and ranks each word
;; based on how frequently it appears in the "corpus" (just a log of
;; all the input the bot has seen).  It picks the "most interesting"
;; word -- i.e., the one that appears the least -- and then finds all
;; the utterances in the corpus that contain it.  It then returns an
;; utterance chosen at random from that set, favoring the longer
;; (presumably more-interesting) ones.

#lang racket
(require
 scheme/set
 scheme/include
 "corpus.rkt"
 "utterance.rkt"
 (only-in "vars.rkt" *incubot-logger*))

(include "incubot-tests.rkt")

(provide  incubot-sentence)
(define incubot-sentence
  (match-lambda*
   [(list (? list? s) (? corpus? c))
    (incubot-sentence (wordlist->wordset s) c)]
   [(list (? string? s) (? corpus? c))
    (incubot-sentence (string->words s) c)]
   [(list (? set? ws) (? corpus? c))
    (let ([rare (rarest ws c)])
      ((*incubot-logger*) "incubot corpus has ~a entries" (corpus-size c))
      (and rare
           ((*incubot-logger*) "incubot chose ~s" rare)
           (random-choose-string-containing-word rare c)))]))

(define (setof pred)
  (lambda (thing)
    (and (set? thing)
         (for/and ([item (in-set thing)])
                  (pred item)))))

(define/contract (wordlist->wordset ws)
  ((listof string?) . -> . (setof string?))
  (define (strip rx) (curryr (curry regexp-replace* rx) ""))
  (apply
   set
   (filter (compose positive? string-length)
           (map (compose
                 (strip #px"^'+")
                 (strip #px"'+$")
                 (strip #px"[^'[:alpha:]]+"))
                ws))))

(provide string->words)
(define/contract (string->words s)
  (string? . -> . set?)
  (wordlist->wordset (regexp-split #rx" " (string-downcase s))))

(define/contract (rarest ws c)
  (-> set? corpus? (or/c string? #f))
  (let-values ([(_ tied-for-rarest)
                (for/fold ([smallest-ranking +inf.0]
                           [rarest-words-so-far (set)])
                    ([word (in-set ws)])
                    (let ([p (word-popularity word c)])
                      (if (and (positive? p)
                               (<= p smallest-ranking))
                          (values p (set-add rarest-words-so-far word))
                          (values smallest-ranking rarest-words-so-far))))])
    (and (positive? (set-count tied-for-rarest))
         (random-choose (set-map tied-for-rarest values)))))
