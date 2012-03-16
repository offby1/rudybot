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
        (apply utterance (map ensure-string (list timestamp nick target text)))]
       [_ #f])
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
  (define input-file-names
    (command-line
     #:program "log-parser"
     #:args input-file-names
     input-file-names))
  (cond
   [(null? input-file-names)
    (displayln "You didn't specify any input files; running unit tests instead of parsing" (current-error-port))
    (exit (if (positive?  (run-tests tests)) 1 0))]
   [(< 1 (length input-file-names))
    (error 'log-parser "I want at most one input file name; instead you gave me ~s" input-file-names)]
   [else
    (let ([input-file-name (car input-file-names)]
          [corpus (make-corpus-from-sequence '())])
      (when (not (absolute-path? input-file-name))
        (set! input-file-name
              (build-path (this-expression-source-directory) input-file-name)))
      (call-with-input-file
          input-file-name
        (lambda (ip)
          (port-count-lines! ip)

          (start-transaction (corpus-db corpus) )
          (for ([line (in-lines ip)])
            ;; TODO -- ignore exceptions here, since I once saw
            ;; add-utterance-to-corpus die because the DB was locked
            ;; (I had a rudybot running at the time)
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
                   (current-line ip))))
      (pe "done~%"))]))
