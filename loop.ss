#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#
(module loop scheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" )))

(define (loop ip op proc)
  (display (proc (read-line ip)) op))

(define loop-tests

  (test-suite
   "loop"
   (test-case
    "yow"
    (let ((from-server (list "Welcome to freenode, douchebag"))
          (minimal-processor (lambda (line)
                               (format "OK, I read ~s.  Now what?~%" line))))
      (let ((magical-input-port (open-input-string (string-join from-server (string #\return))))
            (output-from-bot (open-output-string)))
        (loop magical-input-port output-from-bot minimal-processor)
        (check-regexp-match
         #rx"OK, I read .*Welcome to freenode.*.  Now what?"
         (get-output-string output-from-bot)))
      )
    )))

(define (main . args)
  (exit (test/text-ui loop-tests 'verbose)))

(provide (all-defined-out))
)
