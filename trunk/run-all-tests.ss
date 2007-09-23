#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qtmv "$0" -e "(run-and-exit)"
|#
(module run-all-tests mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss"    ("schematics" "schemeunit.plt" 2))
         "resettable-alarm.ss"
         "bot-tests.ss"
         "channel-events.ss"
         "del.ss"
         (only "globals.ss" register-version-string)
         "headline.ss"
         "parse.ss"
         "planet-emacsen.ss"
         "sandboxes.ss"
         "spelled-out-time.ss"
         "tinyurl.ss"
         )
(register-version-string "$Id$")
(define eva-thang (test-suite
                   "eva thang"
                   resettable-alarm-tests
                   bot-tests
                   channel-events-tests
                   del.icio.us-tests
                   headline-tests
                   parse-tests
                   planet-tests
                   sandboxes-tests
                   spelled-out-time-tests
                   tinyurl-tests))

(define (run-and-exit)

;;   (current-pseudo-random-generator
;;    (vector->pseudo-random-generator #6(1218415049 165534965 434768707 2141531185 950682075 2396563251)))

  (fprintf
   (current-error-port)
   "rng state: ~s~%"
   (pseudo-random-generator->vector (current-pseudo-random-generator)))

  (exit
   (if (positive? (test/text-ui eva-thang))
       1
     0)))

(provide (all-defined))
)
