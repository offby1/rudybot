#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require racket/trace
         rackunit rackunit/text-ui
         (except-in "userinfo.rkt" main))

;; grovel log, pulling out new-style sexps (and associated timestamps)
;;   actually, only get those whose command is JOIN, PART, QUIT, or
;;   PRIVMSG
;; come up with a struct sighting: who where when action? words

(define (trim-action utterance)
  (match utterance
   [(regexp #px"^\1ACTION (.*)\1$" (list _ action))
    action]
   [_ utterance]))

(check-equal? (trim-action "frotz")
              "frotz")
(check-equal? (trim-action "\1ACTION you said it\1")
              "you said it")

(define (message->sighting m)
  (match m
    [(list timestamp
           (list 'prefix prefix)
           (list 'command command)
           (list 'params params ...))

     (let* ([str (bytes->string/utf-8 command)]
            [sym (string->symbol str)])
       (case sym
         ((JOIN PART)
          (sighting (bytes->string/utf-8 prefix)                  ;who
                    (bytes->string/utf-8 (second (first params))) ;where
                    (string->number timestamp) ;when
                    #f                         ;action?
                    str                        ;words
                    ))
         ((QUIT)
          (sighting (bytes->string/utf-8 prefix)
                    ""
                    (string->number timestamp)
                    #f
                    (bytes->string/utf-8 (second (first params)))))
         ((PRIVMSG)
          (sighting (bytes->string/utf-8 prefix)
                    (bytes->string/utf-8 (second (first params)))
                    (string->number timestamp)
                    (not (not (regexp-match "^\1ACTION " (second (second params)))))
                    (bytes->string/utf-8 (trim-action (second (second params))))))))]
    [_ 'wtf]))

(check-equal? (message->sighting '("123.456"
                                   (prefix #"colonel Mustard")
                                   (command #"PRIVMSG")
                                   (params (param #"in the study")
                                           (param #"\1ACTION whuppin' hisself on de haid wit de candlestick\1"))))
              (sighting "colonel Mustard"
                        "in the study"
                        123.456
                        #t
                        "whuppin' hisself on de haid wit de candlestick"))

(check-equal? (message->sighting '("123.456"
                                   (prefix #"colonel Mustard")
                                   (command #"PRIVMSG")
                                   (params (param #"in the study")
                                           (param #"gimme seltzer"))))
              (sighting "colonel Mustard"
                        "in the study"
                        123.456
                        #f
                        "gimme seltzer"))

(check-equal? (message->sighting '("123.456"
                                   (prefix #"rudybot!~luser@ec2-204-236-167-175.us-west-1.compute.amazonaws.com")
                                   (command #"PART")
                                   (params (param #"#scheme"))))
              (sighting "rudybot!~luser@ec2-204-236-167-175.us-west-1.compute.amazonaws.com"
                        "#scheme"
                        123.456
                        #f
                        "PART"))

(check-equal? (message->sighting '("123.456"
                                   (prefix #"monqy")
                                   (command #"QUIT")
                                   (params (param #"Quit: can't see a thing"))))
              (sighting "monqy"
                        ""
                        123.456
                        #f
                        "Quit: can't see a thing"))
(provide main)
(define (main . args)
  "a-ok")