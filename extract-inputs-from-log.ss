#lang scheme

(require  mzlib/etc)

(define (pump ip op)
  (for ([line (in-lines ip)])
    (display line op)
    (newline op))
  (close-output-port op))

(call-with-input-file
      (build-path (this-expression-source-directory) "big-log")
  (lambda (ip)
    (pump ip (current-output-port))))

