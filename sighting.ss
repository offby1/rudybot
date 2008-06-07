#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: sighting.ss 5642 2008-06-05 03:26:22Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(define *sightings-database-directory-name* (make-parameter "sightings.db"))

(define-struct sighting (who where when action? words) #:prefab)

(define (canonicalize-nick n)
  (match n
    [(regexp #px"(.*?)([`_]*)$" (list _ base suffixes))
     base]
    [_ n]))

(define (nick->dirpath n)
  (build-path
   (*sightings-database-directory-name*)
   (canonicalize-nick n)))

(define (lookup-sightings who)
  (let ((dirname (nick->dirpath who)))
    (if (directory-exists? dirname)
        (map (lambda (fn)
               (call-with-input-file (build-path dirname fn) read))
             (directory-list dirname))
        '())))

(define (note-sighting s)
  (let ((dirname (nick->dirpath (sighting-who s))))

    (make-directory* dirname)

    (let ( ;; not thread-safe
          (new-name (build-path dirname (number->string (length (directory-list dirname))))))

      (call-with-output-file
          new-name
        (lambda (op)
          (write s op)
          (newline op))))))

(provide/contract
 [struct sighting ((who string?)
                   (where string?)
                   (when natural-number/c)
                   (action? (or/c string? not))
                   (words (listof string?)))]

 [lookup-sightings (-> string? (listof sighting?))]
 [note-sighting (-> sighting? void?)]
 [canonicalize-nick (-> string? string?)])
