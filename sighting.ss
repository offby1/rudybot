#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(define *sightings-database-file-name* (make-parameter "sightings.db"))

(define-struct sighting (who where when was-action? words) #:prefab)

(define *sightings* #f)

(define (maybe-load!)
  (unless *sightings*
    (set! *sightings*
          (with-handlers
              ([exn:fail:filesystem?
                (lambda (e)
                  (make-hash))])
            (hash-copy (call-with-input-file (*sightings-database-file-name*) read))))))

(define (lookup-sighting who)
  (maybe-load!)
  (hash-ref *sightings* who #f))

(define (note-sighting s)
  (maybe-load!)
  (hash-set! *sightings* (sighting-who s) s)
  ;; Do the writing in two steps -- first, write the hash to a string,
  ;; and _then_ write the string to the file.  This seems pointless,
  ;; but it lets us spend far less time with an open file handle, thus
  ;; reducing (but not eliminating) the risk of corrupting the file if
  ;; the process dies in the middle of writing it.
  (let ((string-port (open-output-string)))
    (write *sightings* string-port)
    (let ((the-string (get-output-string string-port)))
      (call-with-output-file (*sightings-database-file-name*)
        (lambda (op)
          (display the-string op))
        #:exists 'truncate/replace))))

(provide/contract
 [struct sighting ((who string?)
                   (where string?)
                   (when natural-number/c)
                   (was-action? boolean?)
                   (words (listof string?)))]

 [lookup-sighting (-> string? (or/c sighting? not))]
 [note-sighting (-> sighting? void?)])

(define (main . args)
  '|golly gee!|)

(provide main)