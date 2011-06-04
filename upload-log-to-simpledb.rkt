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
    (format "doing ~a" action)]
   [_ (format "saying ~a" utterance)]))

(check-equal? (trim-action "frotz")
              "frotz")
(check-equal? (trim-action "\1ACTION you said it\1")
              "you said it")

(define (message->description m)
  (match m
    [(list timestamp
           (list 'prefix prefix)
           (list 'command command)
           (list 'params params ...))

     (let* ([command-str (bytes->string/utf-8 command)])
       (case (string->symbol command-str)
         ((JOIN PART)
          (format "~aing ~a" (string-downcase command-str) (second (first params))))
         ((QUIT)
          (format "quitting, mumbling ~s" (bytes->string/utf-8 (second (first params)))))
         ((PRIVMSG)
          (format "~a in ~a" (second (second params))
                  (trim-action
                   (second (first params))) ))))]
    [_ 'wtf]))

(define (log-entry->sighting s)
  ;; stuff to pull a complete log entry -- including timestamp and
  ;; "<=" -- apart
  )

(check-equal? (message->description '("123.456"
                                   (prefix #"colonel Mustard")
                                   (command #"PRIVMSG")
                                   (params (param #"in the study")
                                           (param #"\1ACTION whuppin' hisself on de haid wit de candlestick\1"))))
              (sighting "colonel Mustard"
                        "in the study"
                        123.456
                        #t
                        "whuppin' hisself on de haid wit de candlestick"))

(check-equal? (message->description '("123.456"
                                   (prefix #"colonel Mustard")
                                   (command #"PRIVMSG")
                                   (params (param #"in the study")
                                           (param #"gimme seltzer"))))
              (sighting "colonel Mustard"
                        "in the study"
                        123.456
                        #f
                        "gimme seltzer"))

(check-equal? (message->description '("123.456"
                                   (prefix #"rudybot!~luser@ec2-204-236-167-175.us-west-1.compute.amazonaws.com")
                                   (command #"PART")
                                   (params (param #"#scheme"))))
              (sighting "rudybot!~luser@ec2-204-236-167-175.us-west-1.compute.amazonaws.com"
                        "#scheme"
                        123.456
                        #f
                        "PART"))

(check-equal? (message->description '("123.456"
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