#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require (lib "errortrace.ss" "errortrace")
         (lib "etc.ss")
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
         )

(coverage-counts-enabled #t)
(execute-counts-enabled #t)
(profile-paths-enabled #t)
(profiling-enabled #t)
(profiling-record-enabled #t)

(require "../run-all-tests.ss")
(test/text-ui eva-thang)

(define *file-of-interest*
  (simplify-path (build-path
                  (this-expression-source-directory)
                  'up
                  "bot.ss")))
(define *coverage-output-fn* "coverage-data.txt")
(with-output-to-file *coverage-output-fn*
  (lambda ()
    (printf "-*-fundamental-*-~%")      ; for emacs
    (annotate-covered-file  *file-of-interest* #t)
    (fprintf (current-error-port)
             "Spewed coverage stuff to ~s~%" *coverage-output-fn*))

  'truncate/replace)
