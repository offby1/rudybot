#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5748 2008-11-17 01:57:34Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require "loop.ss"
         scheme/port)

(log "Main starting.")
(parameterize ((*bot-gives-up-after-this-many-silent-seconds* 1/4)
               (*log-ports* (list (current-error-port))))
  (let ((devnull (open-output-nowhere)))
    (connect-and-run
     (lambda ()
       (values
        (let-values (((ip op)
                      (make-pipe)))
          (thread
           (lambda ()
             (define (c str)
               (format ":n!n@n PRIVMSG #c :~a: ~a"
                       (*my-nick*)
                       str))
             (define (p str)
               (format ":n!n@n PRIVMSG ~a :~a"
                       (*my-nick*)
                       str))
             (for-each
              (lambda (line)
                (display line op)
                (display "\r\n" op))
              `(
                ,(c (format "eval (error \"foo\\r\\nQUIT bar\")"))

                ,@(apply
                   append
                   (for/list ((expr (in-list '(
                                               (let loop ()
                                                 (printf "Yaa!!")
                                                 (loop))))))
                     (list
                      (c (format "eval ~s" expr))
                      (p (format "eval ~s" expr)))))))

             (close-output-port op)))
          ip)
        devnull))
     #:retry-on-hangup? #f)))
