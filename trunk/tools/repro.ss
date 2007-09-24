#!/bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qr "$0" ${1+"$@"}
|#

;; This helped me track down a bug involving the "quote" command
;; (namely -- it responded more than once).  I checked out various
;; revisions from Subversion, and ran this on each; and used a binary
;; search to narrow down the guilty revision.

;; I've since fixed the bug, but suspect this could be handy in the
;; future, so ... here it remains.

(define *channels*  (list "#squank" "#wank" "#hippy" "#hoppy" "#flippy" "#floppy" "#silly" "#sally"))

(define *cmdline-words*
  (cons
   "run-bot.ss"
   (apply append
          (map (lambda (ch)
                 (list "-c" ch))
               *channels*))))

;; (printf "~s~%" *cmdline-words*)
;; (exit 0)

(define-values (proc stdout stdin stderr)
  (apply
   subprocess
   (current-output-port)
   #f
   (current-error-port)
   *cmdline-words*
   ) )

(file-stream-buffer-mode stdin 'line)
(define (->bot str) (display str stdin ) (display #\return stdin) (newline stdin))
(->bot ":localhost. 001 rudybot :Welcome to the Debian dancer-ircd Network rudybot")
(for-each (lambda (c)
           (->bot (format ":localhost 366 rudybot ~a :what's up, homes?" c)))
         *channels*)
(->bot (format ":a!b@c PRIVMSG ~a :rudybot: quote" (car *channels*)))
(close-output-port stdin) (set! stdin #f)
(sync/timeout 40 proc)
