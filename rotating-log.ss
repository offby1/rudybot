#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         mzlib/trace)

(define (create-logging-op dirname max-bytes)
  (let-values (((pipe-ip pipe-op) (make-pipe)))
    (thread
     (lambda ()

       ;; TODO -- perhaps examine the directory to see what files
       ;; already exist, and start with the first available name.
       (define generate-file-name
         (let ((counter 0))
           (lambda ()
             (begin0
                 (build-path dirname (format "log-~a" counter))
               (set! counter (add1 counter))))))

       (fprintf
        (current-error-port)
        "Thread ~s running~%" (current-thread))

       (let loop ()
         (let ((ready (sync (peek-bytes-evt 1 0 #f pipe-ip))))
           (when (not (eof-object? ready))

             ;; TODO -- keep generating names until we find one that
             ;; isn't already used.
             (call-with-output-file (generate-file-name)
               (lambda (file-op)
                 (fprintf
                  (current-error-port)
                  "Writing to ~s~%" file-op)

                 ;; To prevent accidental creation of a zillion empty
                 ;; files while I'm debugging this code
                 (sleep 1/10)

                 (file-stream-buffer-mode file-op 'line)
                 (call/ec
                  (lambda (done)
                    (for ([line (in-lines pipe-ip)])
                      (display line file-op)
                      (newline file-op)
                      (when (<= max-bytes (file-position file-op))
                        (done)))))))

             (loop))))

       (fprintf
        (current-error-port)
        "Thread ~s exiting~%" (current-thread))))

    pipe-op))



(define (sorted-pathlist dir)
  (sort
   (map (lambda (rfn)
          (build-path dir rfn))
        (directory-list dir))
   string<? #:key path->string))

(define (fold-sorted-files proc init dir)
  (for/fold ([return-value init])
      ([fn (in-list (sorted-pathlist dir))])
      (if (file-exists? fn)
          (proc return-value fn)
          return-value)))

(define (all-file-content dir)
  (fold-sorted-files (lambda (accum fn)
                       (string-append accum (call-with-input-file fn port->string)))
                     ""
                     dir))

(define (file-sizes dir)
  (reverse
   (fold-sorted-files  (lambda (accum fn)
                         (cons (file-size fn) accum))
                       '()
                       dir)))

(define rotating-log-tests

  (let ((dir (make-temporary-file "rotating-log-tests~a" 'directory))
        (max-bytes 10))
    (test-suite
     "loop"
     #:after
     (lambda ()
       (if #f
           (delete-directory/files dir)
           (fprintf (current-error-port)
                    "Not deleting directory ~a~%" dir))
       )
     (test-begin
      (let* (
             (logger-op (create-logging-op dir max-bytes))
             (data "Hey doodz!\nLookit me getting all logged and shit!!"))
        (display data logger-op)
        (close-output-port logger-op)
        (sleep 1)

        (check-equal? (all-file-content dir)
                      (string-append data "\n")
                      "concatenation of all files yields our input data")

        (check-true (<= (length (filter (lambda (x) (< x max-bytes)) (file-sizes dir)))
                        1)
                    "at most one file is < max-bytes bytes"))))))

(define (main . args)
  (exit (run-tests rotating-log-tests 'verbose)))
(provide main)
