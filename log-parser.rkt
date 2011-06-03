#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
exec racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require
 (only-in profile profile-thunk)
 (only-in mzlib/etc this-expression-source-directory)
 (planet schematics/schemeunit:3)
 (planet schematics/schemeunit:3/text-ui) )

(provide (struct-out utterance))
(struct utterance (timestamp speaker target text) #:prefab)

(define (string->utterance s)
  (define (ensure-string x)
    (cond
     ((string? x)
      x)
     ((bytes? x)
      (bytes->string/utf-8 x))))
  (match s
    [(regexp #px"^([^ ]+) <= \":([^!]*)!([^@]*)@([^ ]*) PRIVMSG ([^:]+) :(.*)\"$"
          (list _ timestamp   nick    id      host            target   text))
     (utterance timestamp nick target text)]

    [(regexp #px"^([[:print:]]+) <= +(\\(.*\\))" (list _ timestamp raw-string))
     (match  (read (open-input-string raw-string))
       [(list (list 'prefix (regexp #rx"(.*)!(.*)@(.*)" (list _ nick _ _)))
              (list 'command #"PRIVMSG")
              (list 'params
                    (list 'param target)
                    (list 'param text)))
        (apply utterance (map ensure-string (list timestamp nick target text)))])
     ]
    [_ #f]))

(define-test-suite tests
  (for ([line '("2010-01-19T03:01:31Z <= \":offby1!n=user@pdpc/supporter/monthlybyte/offby1 PRIVMSG ##cinema :rudybot: uptime\""
                "2010-01-19T03:01:31Z <= ((prefix #\"offby1!n=user@pdpc/supporter/monthlybyte/offby1\") (command #\"PRIVMSG\") (params (param #\"##cinema\") (param #\"rudybot: uptime\")))")])

    (check-equal? (string->utterance line)
                  #s(utterance "2010-01-19T03:01:31Z"
                               "offby1"
                               "##cinema"
                               "rudybot: uptime")))
  )

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
                       (when utz
                         (write utz op)
                         (newline op)))))
                 #:exists 'truncate)))))
        (pe "done~%")))))
