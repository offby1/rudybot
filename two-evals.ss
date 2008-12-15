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
               (fprintf op ":n!n@n PRIVMSG #c :~a: ~a\r\n"
                        (*my-nick*)
                        str))
             (define (p str)
               (fprintf op ":n!n@n PRIVMSG ~a :~a\r\n"
                        (*my-nick*)
                        str))
             (c (format "eval ~s" '(let loop ()
                                     (printf "Yaa!!")
                                     (loop))))
             (p (format "eval ~s" '(let loop ()
                                     (printf "Yaa!!")
                                     (loop))))

             (close-output-port op)))
          ip)
        devnull))
     #:retry-on-hangup? #f)))
