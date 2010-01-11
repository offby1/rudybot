#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6182 2009-11-10 04:59:27Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require
 (except-in (planet offby1/offby1/zdate) main)
 "side-effects.ss")

(define (do-328 channel URL)
  (printf "Channel ~a; URL ~a~%"
          channel URL))

(define (do-332 channel topic)
  (printf "Channel ~a; topic ~a~%"
          channel topic))

(define (do-333 channel creator topic-set-time)
  (printf "Channel ~a; topic set by ~a at ~a~%"
                       channel creator (zdate (string->number topic-set-time))))

(define (do-353 channel users)
  (printf "Channel ~a; users ~a~%" channel users))

(define (do-usual-stuff speaker verb target text)
  (note-speaker! speaker)
  (case (string->symbol verb)
    ((PRIVMSG)
     (inc! 'verbs verb)
     (if (string=? target "rudybot")
         (printf "~s said ~s to me~%" speaker text)
         (match text
           [(pregexp #px"^\\s*(\\S+)[:,] (.*)$" (list _ ostensible-target text))
            (printf "~s said ~s to ~a in channel ~a~%" speaker text ostensible-target target)

            (inc! 'targets ostensible-target)
            (inc! 'texts text)]

          [_
           (printf "~s said ~s to channel ~a~%" speaker text target)

           (inc! 'targets target)
           (inc! 'texts text)])))))

(define (do-notice verb text)
  (inc! 'lone-verbs verb)
  (when (equal? "NOTICE" verb)
    (inc! 'notices text)))

(provide (all-defined-out))
