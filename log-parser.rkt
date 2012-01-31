#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require
 (only-in mzlib/etc this-expression-source-directory)
 (only-in db
          commit-transaction
          query-exec
          sqlite3-connect
          start-transaction
          )
 (only-in "corpus.rkt"
          add-utterance-to-corpus
          corpus-db
          make-corpus-from-sequence)
 "utterance.rkt"
 rackunit
 rackunit/text-ui )

(define (log-file-string->utterance s)
  (define (ensure-string x)
    (cond
     [(string? x)
      x]
     [(bytes? x)
      (bytes->string/utf-8 x)]))
  (match s
    ;; old style: the guts are an unparsed scheme string
    [(regexp #px"^([[:print:]]+) <= \":([^!]*)!([^@]*)@([^ ]*) PRIVMSG ([^:]+) :(.*)\"$"
          (list _ timestamp   nick    id      host            target   text))
     (utterance timestamp nick target text)]

    ;; new style: the guts are an s-expression
    [(regexp #px"^([[:print:]]+) <= +(\\(.*\\))" (list _ timestamp raw-string))
     (match (read (open-input-string raw-string))
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

    (check-equal? (log-file-string->utterance line)
                  #s(utterance "2010-01-19T03:01:31Z"
                               "offby1"
                               "##cinema"
                               "rudybot: uptime")))
  )

(define (pe fmt . args)
  (apply fprintf (current-error-port) fmt args))

(define (current-line ip)
  (call-with-values
      (thunk (port-next-location ip))
    (lambda (line col pos) line)))

(provide main)
(define (main . args)
  (define-values (ip op)
    (make-pipe))
  (define dummy-writer
    (thread
     (thunk
      (let loop ()
        (displayln "2010-08-14T11:05:50Z <= \":bpalmer!~user@unaffiliated/bpalmer PRIVMSG ##cinema :T: S was good ; but would have been much better with the original ending.\"" op)
        (loop)))))
  (define corpus (make-corpus-from-sequence '()))
  (port-count-lines! ip)

  (start-transaction (corpus-db corpus) )
  (for ([line (in-lines ip)])
    (cond
     ((log-file-string->utterance line)
      =>
      (curryr add-utterance-to-corpus corpus)))

    (when (zero? (remainder (current-line ip) 2000))
      (commit-transaction (corpus-db corpus))
      (start-transaction (corpus-db corpus))
      (fprintf (current-error-port)
               "Line ~a~%"
               (current-line ip))))
  (commit-transaction (corpus-db corpus) )
  (fprintf (current-error-port)
           "Line ~a~%"
           (current-line ip)))
