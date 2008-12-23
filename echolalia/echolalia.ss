#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (lib "thread.ss")
         (lib "etc.ss")
         "read-db.ss"
         "db.ss")

(provide main)
(define (main . args)
  (let ((db (irc-lines->db
             (build-path
              (this-expression-source-directory)
              'up "big-log"))))
    (fprintf
     (current-error-port)
     "Server starting!~%")
    (for ([word (in-lines (current-input-port))])
      (display (lookup word db))
      (newline))))
