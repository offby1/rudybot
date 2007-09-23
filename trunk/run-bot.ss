#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module run-bot mzscheme
(require  (lib "cmdline.ss")
          (only (lib "etc.ss")
                this-expression-source-directory)
          "bot.ss"
          "globals.ss"
          (only "headline.ss" reliably-put-pref)
          (only "planet-emacsen.ss" *planet-poll-interval*)
          "quotes.ss"
          "repl.ss"
          "vprintf.ss"
          )
(register-version-string "$Id$")

(command-line
 "bot"
 (current-command-line-arguments)
 (once-each

  (("-r" "--rng") vec
   "pseudo-random-generator vector like #6(1888977131 3014825601 3849035281 163056249 698545751 4293483726)"
   (current-pseudo-random-generator
    (vector->pseudo-random-generator (read (open-input-string vec)))))
  (("-s" "--host") host "Name of the IRC server to connect to"
   (*irc-server-name* host))
  (("--delicious") del "del.icio.us password"
   (*del.icio.us-password* del))
  (("-n" "--nick") nick "The nick I _want to_ be known by; but we might need to append characters to it"
   (*desired-nick* nick))
  (("--nickserv")
   pw "Password for NICKSERV"
   (*nickserv-password* pw))
  (("--planet") "Actually hit planet.emacsen.org, rather than using test data"
   (*use-real-atom-feed?* #t))
  (("-l" "--log") "Spew to log file as opposed to stderr"
   (*log-to-file* #t))
  )

 (multi
  (("-c" "--channel") channel "A channel to join when starting"
   (when (not (member channel (*initial-channel-names*)))
     (*initial-channel-names* (cons channel (*initial-channel-names*))))
   )))

(fprintf
 (current-error-port)
 "rng state: ~s~%"
 (pseudo-random-generator->vector (current-pseudo-random-generator)))

(let ((remote-irc? (and (*irc-server-name*)
                   (not (equal? "localhost" (*irc-server-name*)))))
      (feed-description (if (*use-real-atom-feed?*) "real" "fake")))

  (fprintf
   (current-error-port)
   "irc server name: ~s; using ~a Atom feed~%"
   (*irc-server-name*)
   feed-description)
  ;; if we're talking to something other than localhost, we should
  ;; probably be hitting planet.emacsen for real
  (when (not (equal? (*use-real-atom-feed?*) remote-irc?))
    (fprintf (current-error-port)
             "WARNING: you're connecting to IRC server ~s but using a ~a Atom feed~%"
             (*irc-server-name*)
             feed-description))

  ;; I generally do this by hand when testing, so ... why not do it
  ;; automatically.
  (when (not (*use-real-atom-feed?*))
    (reliably-put-pref #f))

  (vtprintf "Our version string is ~s~%" *svnversion-string*))

(print-hash-table #t)

;; run a repl, but only if we're not already trying to read from stdin
(when (*irc-server-name*)
  (thread
   (lambda ()
     (parameterize
         ((current-namespace
           (module->namespace "repl.ss")))
       (let loop ()
         (when (not *sess*)
           (fprintf (current-error-port)
                    "Waiting for the session, before starting the repl~%")
           (sleep 10)
           (loop))
         )
       (with-handlers
           ([exn:fail:filesystem?
             (lambda (e)
               (fprintf
                (current-error-port)
                "Can't load readline.  Bummer.~%"))])
         (dynamic-require '(lib "rep.ss" "readline") #f)
         (fprintf
          (current-error-port)
          "Welcome to the ~a namespace.  Use your power only for good.~%"
          (object-name (current-namespace))))
       (run-repl)))))

(start)
)
