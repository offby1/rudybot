#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
exec racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require
 (only-in mzlib/etc this-expression-source-directory)
 (only-in db
          query-exec
          query-rows
          query-value
          sqlite3-connect
          )
 (only-in "incubot.rkt" string->words)
 "utterance.rkt"
 (planet schematics/schemeunit:3)
 (planet schematics/schemeunit:3/text-ui) )

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

;; Our own little ORM
(define (create-db!)
  (let ([connection
         (sqlite3-connect
          #:database "/tmp/parsed-log.db"
          #:mode 'create)])

    ;; The primary key is to prevent duplicates when we're
    ;; bulk-loading the table.  But it's imperfect, since there may
    ;; occasionally be two utterances from the same speaker, to the
    ;; same target, at the same time.  So ON CONFLICT discards all but
    ;; the newest.  Hopefully this won't happen too often.

    ;; I imagine that FAIL, REPLACE, and IGNORE would all work equally
    ;; well here.
    (query-exec
     connection
     "CREATE TABLE IF NOT EXISTS
        log(timestamp TEXT, speaker TEXT, target TEXT, text TEXT,
            PRIMARY KEY (timestamp, speaker, target)
            ON CONFLICT FAIL)")

    (query-exec
     connection
     "CREATE TABLE IF NOT EXISTS
        log_word_map(word TEXT, log_id INTEGER,
            PRIMARY KEY (word, log_id)
            ON CONFLICT FAIL)")
    connection))

(define (log-utterance! db u)
  (query-exec
   db
   "insert into log values (?, ?, ?, ?)"
   (utterance-timestamp u)
   (utterance-speaker   u)
   (utterance-target    u)
   (utterance-text      u)))

(define (get-last-row-id db)
  (query-value db "SELECT last_insert_rowid()"))

(define (log-word! db w log-id)
  (query-exec
   db
   "insert into log_word_map values (?, ?)"
   w log-id))

(define (begin-transaction db)
  (query-exec db "BEGIN TRANSACTION"))

(define (end-transaction db)
  (query-exec db "COMMIT"))

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
    (let ([input-file-name (build-path (this-expression-source-directory) (car input-file-names))]
          [db (create-db!)])
      (call-with-input-file
          input-file-name
        (lambda (ip)
          (port-count-lines! ip)

          (begin-transaction db)
          (for ([line (in-lines ip)])
            (cond
             ((log-file-string->utterance line)
              =>
              (lambda (ut)
                (log-utterance! db ut)
                (let ([log-id (get-last-row-id db)])
                  (for ([w (string->words (utterance-text ut))])
                    (log-word! db w log-id))))))

            (when (zero? (remainder (current-line ip) 2000))
              (end-transaction db)
              (begin-transaction db)
              (fprintf (current-error-port)
                       "Line ~a~%"
                       (current-line ip))))
          (end-transaction db)))
      (pe "done~%"))]))
