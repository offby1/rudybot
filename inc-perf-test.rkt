#lang racket
; Hey Emacs, this is -*-scheme-*- code!

(require "corpus.rkt"
         "incubot.rkt"
         profile)

(module+ main
  (random-seed 0)
  (let ([c (make-corpus '())])
    (displayln (incubot-sentence "at least" c))))
