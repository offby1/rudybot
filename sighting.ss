#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme  --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet "macro.ss" ("schematics" "macro.plt"))
         (lib "1.ss" "srfi"))

(define *sightings-database-directory-name* (make-parameter "test-sightings.db"))
;; This is just for testing.
(define *downcase-nicks* (make-parameter #t))

(define-struct sighting (who where when action? words) #:prefab)

(define (canonicalize-nick n)
  ;; TODO -- consider nixing _leading_ underscores as well; I've seen
  ;; those in the wild.
  (let ((sans-trailing-crap
         ((if (*downcase-nicks*) string-downcase values)
          (match n
            [(regexp #px"(.*?)([`_]*)$" (list _ base suffixes))
             base]
            [_ n]))))
    (if (string=? sans-trailing-crap "")
        "_"
        sans-trailing-crap)))

(define (nick->dirpath n)
  (let ((cn (canonicalize-nick n)))
  (build-path
   (*sightings-database-directory-name*)
   (substring cn 0 1)
   cn)))

(define (safe-take-right lst pos)
  (let ((pos (min pos (length lst))))
    (take-right lst pos)))

(define (false-if-null x)
  (and (not (null? x))
       x))

(define (newest files)
  (if (null? files)
      '()
      (for/fold ([newest (car files)])
                ([f (in-list files)])
                (if (< (file-or-directory-modify-seconds newest)
                       (file-or-directory-modify-seconds f))
                    f
                    newest))))

(define (siblings path)
  (let ((parent (apply build-path (drop-right (explode-path (simple-form-path path)) 1))))
    (map (lambda (p)
           (build-path parent p))
         (directory-list parent))))

(define (directory-exists/ci? name)
  (let ((name (build-path name)))
    (false-if-null
     (newest
      (filter (lambda (s)
                (equal? (simple-form-path (build-path (string-downcase (path->string s))))
                        (simple-form-path (build-path (string-downcase (path->string name))))))
              (siblings name))))))

(define (lookup-sightings who)
  (aif dirname (directory-exists/ci? (nick->dirpath who))
       (safe-take-right
        (sort
         (map (lambda (fn)
                (call-with-input-file (build-path dirname fn) read))
              (directory-list dirname))
         (lambda (s1 s2)
           (< (sighting-when s1)
              (sighting-when s2))))
        2)
       '()))

(define (note-sighting s)
  (let ((dirname (nick->dirpath (sighting-who s))))

    (define-struct both (path number) #:transparent)

    (make-directory* dirname)

    ;; first, prune old existing sightings.
    (let* ((existing-increasing-order
            (sort
             (filter both-number
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

(provide *sightings-database-directory-name*)
(provide/contract
 [struct sighting ((who string?)
                   (where string?)
                   (when natural-number/c)
                   (action? (or/c string? not))
                   (words (listof string?)))]

 [lookup-sightings (-> string? (listof sighting?))]
 [note-sighting (-> sighting? void?)]
 [canonicalize-nick (-> string? string?)])
