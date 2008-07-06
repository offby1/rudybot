#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
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

(define (safe-take lst pos)
  (let ((pos (min pos (length lst))))
    (take lst pos)))

(define (lookup-sightings who)
  (let ((dirname (nick->dirpath who)))
    (if (directory-exists? dirname)
        (safe-take
         (sort
          (map (lambda (fn)
                 (call-with-input-file (build-path dirname fn) read))
               (directory-list dirname))
          (lambda (s1 s2)
            (< (sighting-when s1)
               (sighting-when s2))))
         2)
        '())))

(define (note-sighting s)
  (let ((dirname (nick->dirpath (sighting-who s))))

    (define-struct both (path number) #:transparent)

    (make-directory* dirname)

    ;; first, prune old existing sightings.
    (let* ((existing-increasing-order
            (sort
             (filter (lambda (p)
                       (both-number p))
                     (map (lambda (entry)
                            (make-both (build-path dirname entry)
                                       (string->number (path->string entry))))
                          (directory-list dirname)))
             <
             #:key both-number))

           (new-name
            (build-path
             dirname
             (number->string
              (if (null? existing-increasing-order)
                  0
                  (add1
                   (both-number (last  existing-increasing-order))))))))

      (define max-to-keep 2)
      (let nuke ((e  existing-increasing-order)
                 (num-files (length existing-increasing-order)))
        (when (< max-to-keep num-files)
          (delete-file (both-path (car e)))
          (nuke (cdr e)
                (sub1 num-files))))

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
