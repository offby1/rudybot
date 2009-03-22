#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
if [ "x$BOTDEBUG" != "xno" ]; then
  exec mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
else
  exec mzscheme --require $0 --main -- ${1+"$@"}
fi
|#

#lang scheme

(require "loop.ss"
         "git-version.ss"
         (except-in "clearenv.ss" main)
         (only-in "main.ss" real-server)
         (only-in "userinfo.ss" *userinfo-database-directory-name*))

(require mzlib/trace)
(define (main . args)
  (clearenv)
  (log "Main starting: ~a" (git-version))
  (parameterize ((*irc-server-hostname* "irc.freenode.org")
                 (*userinfo-database-directory-name* "userinfo.db")
                 (current-trace-notify (lambda (string) (log-debug string))))
    (command-line
     #:program "rudybot"
     #:once-each

     ;; BUGBUG -- pass this via the environment, rather than the
     ;; command line.  We can retrieve the value before calling
     ;; "clearenv".
     [("-p" "--nickserv-password") pw
      "Password to identify with NickServ"
      (*nickserv-password* pw)]

                               )
    (if (*nickserv-password*)
        (connect-and-run real-server)
        (error 'freenode-main "You didn't specify a NickServ password"))))

(provide (all-defined-out))
