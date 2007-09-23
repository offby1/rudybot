#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
echo Nothing to see here\; move along; exit 0
|#
(module thread mzscheme

;; this is just so that vprintf can display a meaningful "id" for each
;; thread.  Thanks to Eli Barzilay for the suggestion.

(define threads-created 0)
(require (lib "kw.ss")
         (only "globals.ss" register-version-string)
         (only (planet "assert.ss" ("offby1" "offby1.plt")) check-type))
(register-version-string "$Id$")
(define *current-thread-id* (make-parameter 0))
(define/kw (thread-with-id thunk #:key [descr])
  (set! threads-created (add1 threads-created))
  (parameterize ((*current-thread-id*
                  (format "~a:~a"
                          threads-created
                          descr)))
    (thread thunk)))



(provide (all-defined-except threads-created))
)
