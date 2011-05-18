#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5887 2008-12-30 18:12:50Z erich $
exec  racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require
 (only-in profile profile-thunk)
 (only-in mzlib/etc this-expression-source-directory)
 (planet schematics/schemeunit:3)
 (planet schematics/schemeunit:3/text-ui) )

(provide (struct-out utterance))
(struct utterance (timestamp speaker target text) #:prefab)

;; 18 seconds
(define eli1 #px"^ *([^ ]*) <= +(\".*\")")
(define eli2 #px"^:([^!]*)!([^@]*)@([^ ]*) PRIVMSG ([^:]+) :(.*)")

;; 21 seconds
(define me1 #px"^ *([[:print:]]*?) <= +(\".*\")")
(define me2 #px"^:(.*?)!(.*?)@(.*?) PRIVMSG ([[:print:]]+?) :(.*)")

(define (string->utterance s [me? #f])
  (match s
    [(regexp (if me? me1 eli1) (list _ timestamp raw-string))
     (let ([parsed-string (read (open-input-string raw-string))])
       (match parsed-string
         [(regexp (if me? me2 eli2)
                  (list _ nick id host target text))
          (utterance timestamp nick target text)]
         [_ #f]))]
    [_ #f]))

(define-test-suite tests
  (let ([line "2010-01-19T03:01:31Z <= \":offby1!n=user@pdpc/supporter/monthlybyte/offby1 PRIVMSG ##cinema :rudybot: uptime\""])
    (check-equal? (string->utterance line)
                  #s(utterance "2010-01-19T03:01:31Z"
                               "offby1"
                               "##cinema"
                               "rudybot: uptime"))))

(define (pe fmt . args)
  (apply fprintf (current-error-port) fmt args))

(provide main)
(define (main . args)
  (let ([test-failures (run-tests tests)])
    (when (zero? test-failures)
      (let ([input-file-name (build-path (this-expression-source-directory) "big-log")]
            [output-file-name "parsed-log"])
        (call-with-input-file
            input-file-name
          (lambda (ip)
            (pe "Reading from ~a; writing to ~a..." input-file-name output-file-name)
            (port-count-lines! ip)
            (profile-thunk
             (lambda ()
               (call-with-output-file output-file-name
                 (lambda (op)
                   (for ([line (in-lines ip)])
                     (let ([utz (string->utterance line)])
                       (when utz (pretty-print utz op)))))
                 #:exists 'truncate)))))
        (pe "done~%")))))
