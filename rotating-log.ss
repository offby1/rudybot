#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         mzlib/trace)

(define (create-logging-op dirname max-bytes)
  (let-values (((pipe-ip pipe-op) (make-pipe)))
    (values
     pipe-op
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

        (let loop ()
          (let ((ready (sync (peek-bytes-evt 1 0 #f pipe-ip))))
            (when (not (eof-object? ready))

              ;; TODO -- keep generating names until we find one that
              ;; isn't already used.
              (call-with-output-file (generate-file-name)
                (lambda (file-op)

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

              (loop)))))))))



;; Like directory-list, but returns _useful_ information -- namely,
;; paths with "dir" prepended.  I wish there were a built-in function
;; that did this.
(define (dirlist dir)
  (map (lambda (rfn)
         (build-path dir rfn))
       (directory-list dir)))

(define (sorted-pathlist dir)
  (sort
   (dirlist dir)
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

(define (too-big fn max-bytes)
  (call-with-input-file
      fn
    (lambda (ip)
      (let ((sizes
             (for/fold ([sizes-by-line '()])
                 ([line (in-lines ip)])
                 (cons (string-length line) sizes-by-line))))
        (cond
         ;; If the file has no lines at all, it's not too big.
         ((null? sizes)
          #f)

         ;; If it has only one line, it's not too big, no matter how
         ;; big that line is.  (Since we don't want to break big log
         ;; records apart.)
         ((= 1 (length sizes))
          #f)

         ;; If it has a bunch of lines, then all but the last must
         ;; clock in at under max-bytes.  We can ignore all but the
         ;; _second_-to-last, since those are all smaller; if the
         ;; second-to-last is small enough, then we're not too big.
         (else
          (< max-bytes (last (drop-right sizes 1)))))))))

(define rotating-log-tests
  (let ((dir (make-temporary-file "rotating-log-tests~a" 'directory))
        (max-bytes 10) )
    (let-values (((logger-op logging-thread)
                  (create-logging-op dir max-bytes))
                 ((data) "Hey doodz!\nLookit me getting all logged and shit!!"))

      (test-suite
       "I hate that I'm forced to give it a name"
       #:before
       (lambda ()
         (display data logger-op)
         (close-output-port logger-op)
         (sync logging-thread))
       #:after
       (lambda ()
         (if #t
             (delete-directory/files dir)
             (fprintf (current-error-port)
                      "Not deleting directory ~a~%" dir)))

       (test-case "concatenation of all files yields our input data"
                  (check-equal? (all-file-content dir)
                                (string-append data "\n")))

       (test-case "_if_ any file is bigger than max-bytes, _then_
it'd have been smaller if you ignored the last record."
                  ;; In other words: each file allows -at most one- record
                  ;; (namely, the last record) to cause it to exceed the maximum
                  ;; size.
                  (check-true (andmap (compose not (lambda (fn)
                                                     (too-big fn max-bytes)))
                                      (dirlist dir))))

       (test-case "at most one file is < max-bytes bytes"
                  (check-true (<= (length (filter (lambda (x) (< x max-bytes)) (file-sizes dir)))
                                  1)))))))

(define (main . args)
  (exit (run-tests rotating-log-tests 'verbose)))
(provide main)
