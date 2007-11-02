#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module web-server-idea mzscheme
(require (planet "zdate.ss"   ("offby1" "offby1.plt"))
         (lib "etc.ss")
         (lib "servlet.ss" "web-server")
         (lib "serialize.ss")
         "../sighting.ss")

(provide interface-version timeout start)

(define interface-version 'v1)
(define timeout +inf.0)

;; a module for the PLT web server.  It reads "sightings.db", and
;; nicely formats the data for display, as a table -- one entry per
;; row.  The table has buttons on top of the "who" and "when" columns;
;; if you click one of those buttons, it sorts the table by that
;; column.

(define *sightings-file-path*
  (build-path
   (this-expression-source-directory)
   'up
   "sightings.db"))

;; this might break, if it gets called at the same time as some other
;; process (namely, the IRC bot) is writing the file.
(define *sightings*
  (map (lambda (p )
         (cons (car p)
               (deserialize (cdr p))))
  (with-input-from-file *sightings-file-path* read)))

(define (start initial-request)

  (with-errors-to-browser
   send/finish
   (lambda ()
     `(html
       (body
        (h3
         ,(format
           "Sightings as of ~a"
           (zdate (file-or-directory-modify-seconds *sightings-file-path*))))
        (table ((rules "all"))
         (tr
          (th "who")
          (th "where")
          (th "when")
          (th "what"))

         ,@(map
            (lambda (p)
              `(tr
                (td ,(format "~a" (car p)))
                (td ,(format "~a" (sighting-where (cdr p))))
                (td ,(format "~a" (zdate  (sighting-when (cdr p)))))
                (td ,(format "~a" (sighting-words (cdr p))))))
            *sightings*)))))))

)
