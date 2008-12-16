#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(require "loop.ss"
         "git-version.ss"
         (only-in "main.ss" real-server)
         (only-in "sighting.ss" *sightings-database-directory-name*))

(require mzlib/trace)
(define (main . args)
  (log "Main starting: ~a" (git-version))
  (parameterize ((*irc-server-hostname* "irc.freenode.org")
                 (*sightings-database-directory-name* "sightings.db")
                 (current-trace-notify (lambda (string) (log-debug string))))
    (command-line
     #:program "rudybot"
     #:once-each
     [("-p" "--nickserv-password") pw
      "Password to identify with NickServ"
      (*nickserv-password* pw)])
    (if (*nickserv-password*)
        (connect-and-run real-server)
        (error 'freenode-main "You didn't specify a NickServ password"))))

(provide (all-defined-out))