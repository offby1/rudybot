#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

;; a bunch of hash tables in which we'll keep track of interesting
;; stuff we've parsed
(define *tables*
  (make-immutable-hash
   (map
    (lambda (name) (cons name (make-hash)))
    '(
      lone-verbs
      notices
      numeric-verbs
      oddball-speakers
      randomness
      servers
      speaker-nicks
      speaker-hosts
      targets
      texts
      verbs
      ))))

(define (inc! dict-name key) (dict-update! (hash-ref *tables* dict-name) key add1 0))

(define (note-speaker! s)
  (match s
    [(pregexp #px"^(.*)!(.*)@(.*)" (list _ nick attrs host))
     (inc! 'speaker-nicks nick)
     (inc! 'speaker-hosts host)]
    [_ (inc! 'oddball-speakers s)]))

(provide (all-defined-out))
