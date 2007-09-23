#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require (lib "errortrace.ss" "errortrace")
         (lib "etc.ss"))

(profile-paths-enabled #t)
(profiling-enabled #t)
(profiling-record-enabled #t)

(require "jordanb.ss")
(parameterize ((*cache-file-name* #f))
(one-jordanb-quote))
(define *profile-output-fn* "profile-data.txt")
(with-output-to-file *profile-output-fn*
  (lambda ()
    (printf "-*-fundamental-*-~%")      ; for emacs
    (output-profile-results #t #t)
    (fprintf (current-error-port)
             "Spewed profile stuff to ~s~%" *profile-output-fn*))

  'truncate/replace)
