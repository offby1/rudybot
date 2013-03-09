#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket

;; if this directory contains no subdirectories
;;   read the contents of all the files, stick them in a list
;;   delete the directory
;;   create a file with the same name as the directory we just deleted
;;   write the list into that file

(require "userinfo.rkt")

(define old-sightings-root "/home/erich/live-bot/sightings.db")
(define new-sightings-root "/home/erich/live-bot/userinfo.db")

(define (nick-dirs)
  (reverse
   (fold-files
    (lambda (path flavor accum)
      (let* ([rel (find-relative-path old-sightings-root path)]
             [depth (length (explode-path rel))])
        (if (and (= 2 depth)
                 (directory-exists? path))
            (cons path accum)
            accum)))
    '()
    old-sightings-root)))

(define (upgrade! nick-dir)
  (fprintf (current-error-port) "~a ... ~%" nick-dir)
  (let* ([files
          (map (lambda (rel)
                 (build-path nick-dir rel))
               (directory-list nick-dir))]
         [structs (for/list ([f files])
                    (call-with-input-file f read))])
    (delete-directory/files nick-dir)
    (let ([nick-file (regexp-replace #rx"/$"
                                     (path->string (simplify-path nick-dir))
                                     "")])
    (call-with-output-file nick-file
      (lambda (op)
        (pretty-print (list (cons 'sightings structs)) op)
        (newline op))))))

(module+ main
  (for ([nick (nick-dirs)])
    (upgrade! nick))
  (rename-file-or-directory old-sightings-root new-sightings-root))
