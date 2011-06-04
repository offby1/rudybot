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
  (second (regexp-match #px"^\1ACTION (.*)\1$" utterance)))

(define (message->sighting m)
  (match m
    [(list timestamp
           (list 'prefix prefix)
           (list 'command command)
           (list 'params params ...))

     (case (string->symbol (bytes->string/utf-8 command))
       ((JOIN) 'yow)
       ((PART)
        (sighting (bytes->string/utf-8 prefix)
                  (bytes->string/utf-8 (second (first params)))
                  (string->number timestamp)
                  #f
                  ""))
       ((QUIT) "throw a fit")
       ((PRIVMSG)
        (sighting (bytes->string/utf-8 prefix)
                  (bytes->string/utf-8 (second (first params)))
                  (string->number timestamp)
                  (not (not (regexp-match "^\1ACTION " (second (second params)))))
                  (bytes->string/utf-8 (trim-action (second (second params))))))
       (else
        'nada)
       )]
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
                                   (prefix #"rudybot!~luser@ec2-204-236-167-175.us-west-1.compute.amazonaws.com")
                                   (command #"PART")
                                   (params (param #"#scheme"))))
              (sighting "rudybot!~luser@ec2-204-236-167-175.us-west-1.compute.amazonaws.com"
                        "#scheme"
                        123.456
                        #f
                        ""))

(provide main)
(define (main . args)
  "a-ok")