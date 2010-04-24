#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5887 2008-12-30 18:12:50Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require mzlib/etc)

(define-struct utterance (timestamp speaker target text) #:prefab)

(define (string->utterance s)
  (match s
    [(regexp #px"^ *([[:print:]]*?) <= +\"(.*)\"" (list _ timestamp string))
     (match string
       [(regexp #px"^:(.*?)!(.*?)@(.*?) PRIVMSG ([[:print:]]+) :(.*)"
                (list _ nick id host target text))
        (make-utterance timestamp nick target text)]
       [_ #f])]
    [_ #f]))

(provide main)
(define (main . args)

  (call-with-input-file
      (build-path (this-expression-source-directory) "big-log")
    (lambda (ip)
      (port-count-lines! ip)
      (call-with-output-file "parsed-log"
        (lambda (op)
          (for ([line (in-lines ip)])
             (let ([utz (string->utterance line)])
               (when utz (pretty-print utz op)))))
        #:exists 'truncate))))
