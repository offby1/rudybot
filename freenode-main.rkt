#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require "loop.rkt"
         (except-in "vars.rkt" log)
         "git-version.rkt"
         "clearenv.rkt"
         (only-in "servers.rkt" real-server)
         (only-in "userinfo.rkt" *userinfo-database-directory-name*)
         (only-in "iserver.rkt" make-incubot-server)

         version/utils)

(define (main . args)
  (let ([required-version "5.2.900.1"])
    (when (not (>= (version->integer (version))
                   (version->integer required-version)))
      (error 'freenode-main "You need at least version ~a of racket, to avoid a memory leak in sqlite" required-version)))

  (clearenv)
  (command-line
   #:program "rudybot"
   #:once-each)

  ;; TODO -- the equivalent of "echo -17 > /proc/self/oom_adj", to
  ;; immunize this process from being killed by the dreaded oom_killer
  ;; (it happened once).  See http://linux-mm.org/OOM_Killer

  (log "Main starting: ~a" (git-version))

  ;; (parameterize ([current-namespace (module->namespace "freenode-main.rkt")])
  ;;   (read-eval-print-loop))

  (parameterize* ([*irc-server-hostname* "chat.freenode.org"]
                  [*irc-server-port* 6667]
                  [*userinfo-database-directory-name* "userinfo.db"]
                  [*incubot-logger* log]
                  [*incubot-server* (make-incubot-server)])

    (if (*nickserv-password*)
        (connect-and-run real-server)
        (error 'freenode-main "You didn't specify a NickServ password"))))

(provide (all-defined-out))
