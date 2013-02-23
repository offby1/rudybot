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
             (lambda (fmt . args)
               (apply fprintf (current-error-port) fmt args)
               (newline (current-error-port))))
         (string-append "incubot-server:" fmt) args))

(define (omit-needless words)
  ;; Finding the popularity of a word takes time proportional to that
  ;; popularity.  For example, to find the popularity of the word
  ;; "it", we must scan through the log_word_map index, counting the
  ;; number of occurrences.  For a common word like "it", that's slow!

  ;; Since we're ultimately only interested in _unpopular_ words, we
  ;; can simply filter out a bunch of the more popular words from our
  ;; input set ``words'', and rank only the remaining words.  An
  ;; example I tried showed that without such filtering, it took about
  ;; 8 seconds to return a witticism; with filtering, it was .4
  ;; seconds.  Seems worthwhile!

  ;; I got this list by querying a reasonbly-current copy of the
  ;; corpus like so:

  ;; select distinct(word), count(word) c from log_word_map group
  ;; by word having c> 20000 order by c desc limit 20;

  ;; I strongly suspect that I can just nuke this code, since I am
  ;; already keeping track of each word's popularity in the
  ;; "word_popularity" table.
  (define noise (set "a" "and" "be" "but" "emacs" "for" "have" "i" "if" "in" "is" "it" "not" "of" "on" "that" "the" "to" "with" "you"))
  (set-subtract words noise))

(provide  incubot-sentence)
(define incubot-sentence
  (match-lambda*
   [(list (? list? s) (? corpus? c))
    (incubot-sentence  (wordlist->wordset s) c)]

   ;; only tests use this
   [(list (? string? s) (? corpus? c))
    (incubot-sentence (string->lowercased-words s) c)]

   [(list (? set? ws) (? corpus? c))
    (let ([rare (rarest (omit-needless ws) c)])
      (and rare
           (random-choose-string-containing-word rare c)))]
   [bogon
    (log "incubot-sentence invoked with bogus arglist: ~s" bogon)
    #f]))

(define/contract (rarest ws c)
  (-> set? corpus? (or/c string? #f))
  (let ([ranked (corpus-rank-by-popularity c ws)])
    (log "~a" ranked)
    (and (not (null? ranked) )
         (let ([chosen-pair (car ranked)])
           ;; TODO -- instead of (or in addition to) logging, perhaps
           ;; send a PRIVMSG to the boss (i.e., me).  Since I'm always
           ;; curious to know what the word was ...
           (log "incubot chose ~s, which appears ~a times"
                (vector-ref chosen-pair 0)
                (vector-ref chosen-pair 1))
           (vector-ref chosen-pair 0)))))
