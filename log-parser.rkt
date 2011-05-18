#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5887 2008-12-30 18:12:50Z erich $
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

;; 4.4 seconds
(define me1 #px"^ *([[:print:]]*?) <= +(.*)")
(define me2 #px"^:(.*?)!(.*?)@(.*?) PRIVMSG ([[:print:]]+?) :(.*)")

;; 4.4 seconds
(define eli1 #px"^ *([^ ]*) <= +(.*)")
(define eli2 #px"^:([^!]*)!([^@]*)@([^ ]*) PRIVMSG ([^:]+) :(.*)")

(define me? #f)

(printf "Using ~a regexes~%" (if me? "my" "eli's"))

(define (string->utterance s)
  (match s
    [(regexp (if me? me1 eli1) (list _ timestamp right-hand-side))

     ;; compatibility hack: remove this once 1) we switch from ~s to
     ;; ~a when writing out the log; and 2) I've converted the
     ;; existing log
     (when (char=? #\" (string-ref right-hand-side 0))
       (set! right-hand-side (read (open-input-string right-hand-side))))

     (match right-hand-side
       [(regexp (if me? me2 eli2)
                (list _ nick id host target text))
        (utterance timestamp nick target text)]
       [_ #f])]
    [_ #f]))

(define-test-suite tests
  (let* ([timestamp "2010-01-19T03:01:31Z"]
         [right-hand-side ":offby1!n=user@pdpc/supporter/monthlybyte/offby1 PRIVMSG ##cinema :rudybot: uptime"]
         [sans-quotes (format "~a <= ~a" timestamp right-hand-side)]
         [with-quotes (format "~a <= ~s" timestamp right-hand-side)])
    (for ([candidate (list sans-quotes with-quotes)])
      (check-equal? (string->utterance candidate)
                    #s(utterance "2010-01-19T03:01:31Z"
                                 "offby1"
                                 "##cinema"
                                 "rudybot: uptime")))))

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
