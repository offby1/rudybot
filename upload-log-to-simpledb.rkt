#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require racket/trace
         rackunit rackunit/text-ui)

(struct sighting (who when description) #:prefab)

;; grovel log, pulling out new-style sexps (and associated timestamps)
;;   actually, only get those whose command is JOIN, PART, QUIT, or
;;   PRIVMSG
;; come up with a struct sighting

(define (trim-action utterance)
  (match utterance
    [(regexp #px"^\1ACTION (.*)\1$" (list _ action))
     ;; actions are generally in the present tense, e.g. "backs away
     ;; slowly".  I wish I could convert to past tense here, but alas.
     (format ": ~a" (bytes->string/utf-8 action))]
    [_ (format "saying ~s" (bytes->string/utf-8 utterance))]))

(define (timestamp+message->description m)
  (match m
    [(list timestamp
           (list 'prefix prefix)
           (list 'command command)
           (list 'params params ...))

     (let* ([command-str (bytes->string/utf-8 command)])
       (case (string->symbol command-str)
         ((JOIN)
          (format "~aing ~a"      (string-downcase command-str) (second (first params))))
         ((PART)
          (format "~aing from ~a" (string-downcase command-str) (second (first params))))
         ((QUIT)
          (let ([parting-words (second (first params))])
            (if (bytes=? #"" parting-words)
                (format "quitting")
                (format "quitting, saying ~s" (bytes->string/utf-8 parting-words)))))
         ((PRIVMSG)
          (format "~a ~a" (trim-action
                           (second (second params)))
                  (second (first params)) ))))]
    [_ 'wtf]))

(define-test-suite tests
  (check-equal? (trim-action #"frotz")
                "saying \"frotz\"")
  (check-equal? (trim-action #"\1ACTION you said it\1")
                ": you said it")
  (check-equal? (timestamp+message->description
                 '("123.456"
                   (prefix #"colonel Mustard")
                   (command #"PRIVMSG")
                   (params (param #"in the study")
                           (param #"\1ACTION whuppin' hisself on de haid wit de candlestick\1"))))
                ": whuppin' hisself on de haid wit de candlestick in the study")
  (check-equal? (timestamp+message->description '("123.456"
                                                  (prefix #"colonel Mustard")
                                                  (command #"PRIVMSG")
                                                  (params (param #"in the study")
                                                          (param #"gimme seltzer"))))
                "saying \"gimme seltzer\" in the study")

  (check-equal? (timestamp+message->description '("123.456"
                                                  (prefix #"rudybot!~luser@ec2-204-236-167-175.us-west-1.compute.amazonaws.com")
                                                  (command #"PART")
                                                  (params (param #"#scheme"))))
                "parting from #scheme")

  (check-equal? (timestamp+message->description '("123.456"
                                                  (prefix #"monqy")
                                                  (command #"QUIT")
                                                  (params (param #"Quit: can't see a thing"))))
                "quitting, saying \"Quit: can't see a thing\"")

  (check-equal? (timestamp+message->description
                 '("123.456"
                   (prefix #"monqy")
                   (command #"QUIT")
                   (params (param #""))))
                "quitting"))

(when (positive?  (run-tests tests))
  (exit 1))

(provide main)
(define (main . args)
  "a-ok")