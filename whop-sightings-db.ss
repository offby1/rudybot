#lang scheme

(define *sightings-database-directory-name* "sightings.db")

(define (main . args)
  (let ((tempdir (make-temporary-file "mztmp~a"  'directory)))
    (for ([orig (in-list
                 (filter
                  (lambda (fn)
                    (not (equal? #\. (string-ref (path->string fn) 0))))
                  (directory-list *sightings-database-directory-name*)))])
      (let* ((orig (path->string orig))
             (old (build-path *sightings-database-directory-name*
                              orig))
             (tmp (build-path tempdir orig))
             (dir (build-path *sightings-database-directory-name*
                              (substring orig 0 1)))
             (new (build-path dir orig)))
        (rename-file-or-directory old tmp)
        (with-handlers
            ([exn:fail:filesystem?
              (lambda (e)
                'oh-well)])
          (make-directory dir))
        (rename-file-or-directory tmp new)

        (fprintf
         (current-error-port)
         "~s done~%" orig)))

    (delete-directory tempdir)
    ))