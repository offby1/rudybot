#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui repl-tests 'verbose))"
|#
(module repl mzscheme
(require (only (lib "1.ss" "srfi") delete)
         (lib "kw.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (prefix bot: "bot.ss")
         (planet "zdate.ss"   ("offby1" "offby1.plt"))
         (lib "modresolve.ss" "syntax")
         "session.ss")

(define/kw (make-mru #:optional [initial '()])
  initial)

(define (mru-add mru item)
  (cons item (delete item mru)))

(define (mru-remove mru item)
  (delete item mru))

(define mru-member member)

(define/kw (pm text #:key [destination (car (irc-session-joined-channels bot:*sess*))])
  (bot:pm bot:*sess* destination text))

(define/kw (notice text #:key [destination (car (irc-session-joined-channels bot:*sess*))])
  (bot:notice bot:*sess* destination text))

;; I wonder if join, select, and part should be defined in session.ss
;; instead of here ...
(define (join channel)
  (bot:out bot:*sess* "JOIN ~a" channel)
  (set-irc-session-joined-channels!
   bot:*sess*
   (mru-add (irc-session-joined-channels bot:*sess*) channel))
  (select channel))

(define (select channel)
  (when (not (member channel (irc-session-joined-channels bot:*sess*)))
    (error 'select "channel ~s is not in ~s" channel (irc-session-joined-channels bot:*sess*)))
  (set-irc-session-joined-channels! bot:*sess* (mru-add (irc-session-joined-channels bot:*sess*) channel)))

(define/kw (me text #:key [channel (car (irc-session-joined-channels bot:*sess*))])
  (bot:pm bot:*sess* channel (format "\u0001ACTION ~a\u0001" text)))

(define/kw (part #:key [channel (car (irc-session-joined-channels bot:*sess*))])
  (bot:out bot:*sess* "PART ~a" channel)
  (set-irc-session-joined-channels! bot:*sess* (mru-remove (irc-session-joined-channels bot:*sess*) channel)))

(define (quit message)
  (bot:out bot:*sess* "QUIT :~a" message)
  (exit))

(define (run-repl)
  (set-irc-session-joined-channels! bot:*sess* (make-mru (irc-session-joined-channels bot:*sess*)))
  (read-eval-print-loop))

;; copied from .../collects/handin-server/private/reloadable.ss
(define (reload-module modspec)
  (let* ([name ((current-module-name-resolver) modspec #f #f)]
         [name (symbol->string name)]
         [name (if (eq? #\, (string-ref name 0))
                   (substring name 1)
                 (error 'reload-module
                        "unexpected module name for ~e: ~e" modspec name))]
         [prefix (let-values ([(base name dir?) (split-path name)])
                   (string->symbol (format ",~a" base)))])
    (fprintf (current-error-port) "(re)loading module from ~a~%" modspec)
    (parameterize ([current-module-name-prefix prefix]
                   [compile-enforce-module-constants #f])
      (load/use-compiled (resolve-module-path modspec #f)))))


(provide run-repl)

)
