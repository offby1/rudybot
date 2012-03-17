#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
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

(provide log)
(define (log fmt . args)
  (apply (or (*incubot-logger*)
             (curry fprintf (current-error-port)))
         (string-append "incubot-server:" fmt) args))

(provide  incubot-sentence)
(define incubot-sentence
  (match-lambda*
   [(list (? list? s) (? corpus? c))
    (incubot-sentence (wordlist->wordset s) c)]
   [(list (? string? s) (? corpus? c))
    (incubot-sentence (string->words s) c)]
   [(list (? set? ws) (? corpus? c))
    (let ([rare (time (rarest ws c))])
      (and rare
           (random-choose-string-containing-word rare c)))]
   [bogon
    (log "incubot-sentence invoked with bogus arglist: ~s" bogon)
    #f]))

(define/contract (rarest ws c)
  (-> set? corpus? (or/c string? #f))
  (let ([ranked (corpus-rank-by-popularity c ws)])
    (log "~a" ranked)
    (let ([chosen-pair (car ranked)])
      (log "incubot chose ~s, which appears ~a times"
           (vector-ref chosen-pair 0)
           (vector-ref chosen-pair 1))
      (vector-ref chosen-pair 0))))
