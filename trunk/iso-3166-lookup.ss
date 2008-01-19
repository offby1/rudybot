#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module iso-3166-lookup mzscheme
(require (lib "match.ss")
         (lib "string.ss"))

(define country-name-by-iso-3166-code (make-hash-table 'equal))

(call-with-input-file "/usr/share/iso-codes/iso_3166.tab"
  (lambda (ip)
    (fprintf (current-error-port)
             "Snarfing input file ...")
    (flush-output (current-error-port))
    (let loop ()
      (let ((datum (read-line ip)))
        (when (not (eof-object? datum))
          (match-let (((code name)
                       (regexp-split "\t" datum)))
            (hash-table-put! country-name-by-iso-3166-code code name))
          (loop))))
    (fprintf (current-error-port)
             " done.~%")))

(define (iso-3166-lookup string)
  (hash-table-get
   country-name-by-iso-3166-code
   (string-upcase string)
   #f))

(provide iso-3166-lookup)
)
