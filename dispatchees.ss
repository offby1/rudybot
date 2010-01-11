#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6182 2009-11-10 04:59:27Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require
 mzlib/trace
 (planet schematics/schemeunit:3)
 (planet schematics/schemeunit:3/text-ui)
 (except-in (planet offby1/offby1/zdate) main)
 "side-effects.ss")

;; #t if and only if reading STR yields exactly one s-expression, and
;; no errors.
(define just-one-sexp
  (match-lambda
   [(? string? str)
    (just-one-sexp (open-input-string str))]
   [(? input-port? ip)
    (let loop ([sexps '()])
      (with-handlers
          ([exn:fail:read? (lambda (e) #f)])
        (let ([datum (read ip)])
          (cond

           ((eof-object? datum)
            (= 1 (length sexps)))

           ((not (null? sexps)) #f)

           (else
            (loop (cons datum sexps)))))))]))

(define-test-suite just-one-sexp-tests

  (check-false (just-one-sexp "")              "empty string")
  (check-true  (just-one-sexp "frotz")         "single atom")
  (check-false (just-one-sexp " frotz plotz")  "two atoms")
  (check-true  (just-one-sexp " (frotz) ")     "single list")
  (check-false (just-one-sexp " (frotz plotz) (") "trailing open")
  (check-false (just-one-sexp " (frotz plotz) )") "trailing close")
  (check-false (just-one-sexp " (frotz plotz) '") "trailing quote")
  )

(define (main . args)
  (exit (run-tests just-one-sexp-tests 'verbose)))
(provide main)

(define (do-328 channel URL)
  (inc! '|328s| (list channel URL)))

(define (do-332 channel topic)
  (inc! '|332s| (list channel topic) ))

(define (do-333 channel creator topic-set-time)
  (inc! '|333s| (list channel creator (zdate (string->number topic-set-time)))))

(define (do-353 channel users)
  (inc! '|353s| (list channel users)))

(define *my-nick* (make-parameter "rudybot"))

(define (do-a-direct-order speaker channel text)
  (define for-whom (if channel (format "in channel ~a" channel) "privately"))
  (define (do-eval text)
    (inc! 'evals (list text speaker for-whom)))

  (match text
    [(pregexp #px"\1VERSION\1")
     (inc! 'versions (list speaker for-whom))]
    [(pregexp #px"^ *(\\S+)(?= +(.*))?" (list _ command command-args))
     (let ([command (string->symbol (string-downcase command))])
       (case  command
         ((uptime)
          (inc! 'uptimes (list speaker for-whom)))
         ((eval)
          (do-eval command-args))
         (else
          (if (just-one-sexp text)
              (do-eval text)
              (inc! 'unknown-commands command))
          )))]
    [_
     ;; empty string, presumably
     (printf "Weird direct order: ~s~%" text)
     ]))

(define (do-usual-stuff speaker verb target text)
  (note-speaker! speaker)
  (case (string->symbol verb)
    ((PRIVMSG)
     (inc! 'verbs verb)
     (if (string=? target (*my-nick*))
         (do-a-direct-order speaker #f text)
         (match text
           [(pregexp #px"^\\s*(\\S+)[:,] (.*)$" (list _ ostensible-target text))
            (if (string=? ostensible-target (*my-nick*))
                (do-a-direct-order speaker target text)
                (and #f (printf "~s said ~s to ~a in channel ~a~%" speaker text ostensible-target target))
                )

            (inc! 'targets ostensible-target)
            (inc! 'texts text)]

          [_
           (and #f (printf "~s said ~s to channel ~a~%" speaker text target))

           (inc! 'targets target)
           (inc! 'texts text)])))))

(define (do-notice verb text)
  (inc! 'lone-verbs verb)
  (when (equal? "NOTICE" verb)
    (inc! 'notices text)))

(provide (all-defined-out))
