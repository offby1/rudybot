#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require "loop.rkt"
         (except-in "vars.rkt" log)
         "git-version.rkt"
         (except-in "clearenv.rkt" main)
         (only-in "servers.rkt" real-server)
         (only-in "userinfo.rkt" *userinfo-database-directory-name*)
         (only-in "iserver.rkt" make-incubot-server))

(define (main . args)
  (clearenv)
  (command-line
   #:program "rudybot"
   #:once-each)

  (log "Main starting: ~a" (git-version))
  (parameterize* ([*irc-server-hostname* "irc.freenode.org"]
                  [*irc-server-port* 6667]
                  [*userinfo-database-directory-name* "userinfo.db"]
                  [*incubot-logger* log]
                  [*incubot-server* (make-incubot-server "parsed-log")]
                  [*nickserv-password* (get-preference '|rudybot-freenode-nickserv-password|)])

    (if (*nickserv-password*)
        (connect-and-run real-server)
        (error 'freenode-main "You didn't specify a NickServ password"))))

(provide (all-defined-out))
