#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(require "loop.rkt"
         "vars.rkt"
         "git-version.rkt"
         (except-in "clearenv.rkt" main)
         (only-in "main.rkt" real-server)
         (only-in "userinfo.rkt" *userinfo-database-directory-name*)
         (only-in "iserver.rkt" make-incubot-server))

(require mzlib/trace)
(define (main . args)
  (clearenv)
  (command-line
   #:program "rudybot"
   #:once-each)

  (log "Main starting: ~a" (git-version))
  (parameterize ([*irc-server-hostname* "irc.freenode.org"]
                 [*irc-server-port* 6667]
                 [*userinfo-database-directory-name* "userinfo.db"]
                 [current-trace-notify (lambda (string) (log-debug string))]
                 [*incubot-logger* log]
                 [*incubot-server* (make-incubot-server "parsed-log")]
                 [*nickserv-password* (get-preference '|rudybot-freenode-nickserv-password|)])

    (if (*nickserv-password*)
        (connect-and-run real-server)
        (error 'freenode-main "You didn't specify a NickServ password"))))

(provide (all-defined-out))
