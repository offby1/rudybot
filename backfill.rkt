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
          add-sentence-to-corpus
          corpus-db
          corpus-sentence-count
          make-corpus)
 (only-in "utils.rkt" safely)
 "utterance.rkt"
 unstable/debug)

(module+ test (require rackunit rackunit/text-ui))

(define (add-utterance-to-corpus u c)
  (add-sentence-to-corpus (utterance-text u) c))

(define (log-file-string->utterance s)
  (define (ensure-string x)
    (cond
     [(string? x)
      x]
     [(bytes? x)
      (bytes->string/utf-8 x)]))
  (match s
    ;; old style: the guts are an unparsed scheme string
    [(regexp #px"^([[:print:]]+) <= (\".*\")$"
             (list _ timestamp unparsed))
     (match (with-input-from-string unparsed read)
       [(regexp #px":([^!]*)!([^@]*)@([^ ]*) PRIVMSG ([^:]+) :(.*)"
                (list _ nick id host target text))
        (utterance timestamp nick target text)]
       [_ #f])
     ]

    ;; new style: the guts are an s-expression
    [(regexp #px"^([[:print:]]+) <= +(\\(.*\\))" (list _ timestamp raw-string))
     (match (with-input-from-string raw-string read)
       [(list (list 'prefix (regexp #rx"(.*)!(.*)@(.*)" (list _ nick _ _)))
              (list 'command #"PRIVMSG")
              (list 'params
                    (list 'param target)
                    (list 'param text)))
        (apply utterance (map ensure-string (list timestamp nick target text)))]
       [_ #f])
     ]
    [_ #f]))

(module+ test
 (define-test-suite tests
   (for ([line '("2010-01-19T03:01:31Z <= \":offby1!n=user@pdpc/supporter/monthlybyte/offby1 PRIVMSG ##cinema :rudybot: uptime\""
                 "2010-01-19T03:01:31Z <= ((prefix #\"offby1!n=user@pdpc/supporter/monthlybyte/offby1\") (command #\"PRIVMSG\") (params (param #\"##cinema\") (param #\"rudybot: uptime\")))")])

     (check-equal? (log-file-string->utterance line)
                   #s(utterance "2010-01-19T03:01:31Z"
                                "offby1"
                                "##cinema"
                                "rudybot: uptime")))
   (let ()
     (define tricky
#<<TRICKY
2010-02-27T17:43:52Z <= ":jcowan!~jcowan@cpe-98-14-172-204.nyc.res.rr.com PRIVMSG #scheme :I see.  \"Degenerate\" is the word.  Or even \"skanky\"."
TRICKY
       )
     (check-equal? (log-file-string->utterance tricky)
                   #s(utterance "2010-02-27T17:43:52Z"
                                "jcowan"
                                "#scheme"
                                "I see.  \"Degenerate\" is the word.  Or even \"skanky\"."))))
 (run-tests tests))


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
     #:program "backfill"
     #:args input-file-names
     input-file-names))
  (cond
   [(null? input-file-names)
    (displayln "You didn't specify any input files; use raco test to run unit tests" (current-error-port))
    (exit 0)]
   [(< 1 (length input-file-names))
    (error 'backfill "I want at most one input file name; instead you gave me ~s" input-file-names)]
   [else
    (let ([input-file-name (car input-file-names)]
          [corpus (make-corpus '() #:create-tables? #t)])
      (when (not (absolute-path? input-file-name))
        (set! input-file-name
              (build-path (this-expression-source-directory) input-file-name)))
      (call-with-input-file
          input-file-name
        (lambda (ip)
          (port-count-lines! ip)

          (define inserts-to-omit (corpus-sentence-count corpus))

          (start-transaction (corpus-db corpus) )
          (for ([line (in-lines ip)])
            (cond
             ((log-file-string->utterance line)
              =>
              (lambda (u)
                (if (positive? inserts-to-omit)
                    (begin
                      (when (zero? (remainder inserts-to-omit 2000))
                        (fprintf
                         (current-error-port)
                         "~a inserts remaining to skip~%"
                         inserts-to-omit))
                      (set! inserts-to-omit (sub1 inserts-to-omit)))
                    (add-utterance-to-corpus u corpus)))))

            (when (zero? (remainder (current-line ip) 100000))
              (safely (commit-transaction (corpus-db corpus)))
              (start-transaction (corpus-db corpus))
              (fprintf (current-error-port)
                       "Line ~a (~a megabytes in use) ~%"
                       (current-line ip)
                       (inexact->exact (round (/ (current-memory-use) 1024 1024.))))))
          (safely (commit-transaction (corpus-db corpus) ))
          (fprintf (current-error-port)
                   "Line ~a~%"
                   (current-line ip))))
      (pe "done~%"))]))
