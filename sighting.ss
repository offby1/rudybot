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
  ((if (*downcase-nicks*) string-downcase values)
   (match n
     [(regexp #px"(.*?)([`_]*)$" (list _ base suffixes))
      base]
     [_ n])))

(define (nick->dirpath n)
  (build-path
   (*sightings-database-directory-name*)
   (canonicalize-nick n)))

(define (safe-take lst pos)
  (let ((pos (min pos (length lst))))
    (take lst pos)))

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

;; This is typically slow, because PATH is a nick, and we've noted a
;; couple of thousand other nicks.  Can't cache it, though, since new
;; people join all the time :-|
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
  (map (lambda (p)
         (call-with-input-file p read))
       (let ((dirname (nick->dirpath who)))
         (if (directory-exists? dirname)
             (safe-take
              (dirlist-newest-first dirname)
              2)
             '()))))

;; files whose names are numbers are sorted numerically; all other
;; files are sorted as if their numbers were 0 (i.e., at the end).
(define (dirlist-newest-first [path (current-directory)])
  (map
   (lambda (entry)
     (build-path path entry))
   (sort
    (directory-list path)
    >
    #:key
    (lambda (p)
      (or (string->number (path->string p)) 0)))))

(define (note-sighting s)
  (let ((dirname (nick->dirpath (sighting-who s))))

    (make-directory* dirname)

    ;; first, prune old existing sightings.
    (let ((max-to-keep 2))
      (for ([i (in-naturals)]
            [file (dirlist-newest-first dirname)]
            #:when (<= max-to-keep i))
        (delete-file file)))

    (call-with-output-file
        (build-path
         dirname
         (number->string (sighting-when s)))
      (lambda (op)
        (fprintf op "~s~%" s))
      #:exists 'truncate)))

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
