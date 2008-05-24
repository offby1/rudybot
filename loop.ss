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

(define (all-lines-from-this-port ip proc)
  (for/list ((line (in-lines ip)))
    (proc line)))



(define loop-tests

  (test-suite
   "loop"
   (test-case
    "yow"

    (let ((minimal-processor (lambda (line)
                               (format "OK, I read ~s.  Now what?~%" line))))
      (let next-connection ((connections (list
                                          (list "Welcome to freenode, douchebag"
                                                "Have a nice day")
                                          (list "OK, this is the second connection."
                                                "Time to go."))))
        (when (not (null? connections))
          (let ((magical-input-port
                 (open-input-string
                  (string-join
                   (car connections)
                   (string #\return))))
                (output-from-bot (open-output-string)))
            (for ((actual (in-list
                           (all-lines-from-this-port
                            magical-input-port
                            minimal-processor)))
                  (expected (in-list (car connections))))
              (printf "Actual: ~s; expected: ~s~%"
                      actual
                      expected)
              (check-regexp-match
               (regexp (regexp-quote expected))
               actual)))

          (next-connection (cdr connections))))))))

(define (main . args)
  (exit (test/text-ui loop-tests 'verbose)))

(provide (all-defined-out))
)
