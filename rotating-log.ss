#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         mzlib/trace
         srfi/26)

(define (create-logging-op dirname max-bytes [format-template "log-~a"])
  (let-values (((pipe-ip pipe-op) (make-pipe)))
    (values
     pipe-op
     (thread
      (lambda ()

        (define generate-file-name
          (let ((counter 0))
            (lambda ()
              (let ((candidate (build-path dirname (format format-template counter))))
                (set! counter (add1 counter))
                (if (or (file-exists? candidate)
                        (directory-exists? candidate)
                        (link-exists? candidate))
                    (generate-file-name)
                    candidate)))))

        (let loop ()
          ;; I don't want to call call-with-output-file unless there's
          ;; actually some data to read -- otherwise I'll create an
          ;; empty file.  But I don't want to consume any data from
          ;; ip, either, since that would require somewhat complex
          ;; code ... so here I wait until there is some data, but
          ;; don't actually consume it.
          (let ((ready (sync (peek-bytes-evt 1 0 #f pipe-ip))))
            (when (not (eof-object? ready))
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
  (map (cut build-path dir <>)
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

(define (with-logging-thingy
         proc
         #:name [name "unknown"]
         #:max-bytes [max-bytes 10]
         #:template  [template "log.~a"])
  (define dir (make-parameter #f))
  (let-values (((logger-op logging-thread)
                (create-logging-op dir max-bytes template)))

    (dynamic-wind
        (lambda ()
          (dir (make-temporary-file "rotating-log-tests~a" 'directory))
          (fprintf
           (current-error-port)
           "~a: dynamic-wind: entering~%" name))
        (lambda ()
          (fprintf
           (current-error-port)
           "~a: dynamic-wind: before calling proc~%" name)
          (begin0
              (proc logger-op dir)
            (fprintf
             (current-error-port)
             "~a: dynamic-wind: after calling proc~%" name)))
        (lambda ()
          (close-output-port logger-op)
          (sync logging-thread)
          (delete-directory/files (dir))
          (fprintf
           (current-error-port)
           "~a: dynamic-wind: cleaned up~%" name)))))

(define rotating-log-tests
  (let ((data "Hey doodz!\nLookit me getting all logged and shit!!")
        (max-bytes 10))
    (test-suite
     "I hate that I'm forced to give it a name"
     (with-logging-thingy
      (lambda (logger-op dir)
        (test-suite
         "oh well"
         (test-case "concatenation of all files yields our input data"
                    (check-equal? (all-file-content (dir))
                                  (string-append data "\n")))

         (test-case "_if_ any file is bigger than max-bytes, _then_
it'd have been smaller if you ignored the last record."
                    ;; In other words: each file allows -at most one- record
                    ;; (namely, the last record) to cause it to exceed the maximum
                    ;; size.
                    (check-true (andmap (compose not (lambda (fn)
                                                       (too-big fn max-bytes)))
                                        (dirlist (dir)))))

         (test-case "at most one file is < max-bytes bytes"
                    (check-true (<= (length
                                     (filter (lambda (x) (< x max-bytes))
                                             (file-sizes (dir))))
                                    1)))))
      #:max-bytes max-bytes
      #:name "rotating-log-tests"))))

(define some-more-tests
  (let ((template "snorkly-~a-yodelay-ee-hoo"))
    (test-suite
     "Kevin"
     (with-logging-thingy
      (lambda (logger-op dir)
        (test-case
         "Log files are named as we specify"
         (display "yoo hoo" logger-op)
         (sleep 1/10)                   ;TODO -- have some way to
                                        ;avoid sleeping here
         (check-equal? (list (format template 0))
                       (map path->string (directory-list (dir)))))
        (test-case
         "Skips over existing log files"
         (let ((another-dir (make-temporary-file "rotating-log-tests~a" 'directory)))
           ;; somehow create a file that has the same name that the
           ;; first log file will have.

           ;; now log something.

           ;; Now check that our first file is unmolested, and the log
           ;; stuff is in the second file.
           (check-false "Maybe I should actually write this test"))))
      #:max-bytes (expt 10 6)
      #:template template
      #:name "some-more-tests"))))

(define (main . args)
  (exit (run-tests (test-suite "El Grande" rotating-log-tests some-more-tests) 'verbose)))
(provide main)
