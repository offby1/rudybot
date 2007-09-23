(module test-utils mzscheme
(require          (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
                  (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
                  "vprintf.ss")
;; returns #f if we didn't find what we're looking for.

(define (expect/timeout ip regex seconds)
  (let* ((ch (make-channel))
         (reader
          (thread
           (lambda ()
             (let loop ()
               (vtprintf "expect/timeout about to spend ~a seconds looking for ~s from ~s ...~%"
                         seconds
                         regex
                         (object-name ip))
               (let ((line (read-line ip)))
                 (vtprintf "expect/timeout got ~s~%" line)
                 (cond
                  ((eof-object? line)
                   (channel-put ch #f))
                  ((regexp-match regex line)
                   (vtprintf "expect/timeout: That's a match!~%")
                   (channel-put ch #t))
                  (else
                   (vtprintf "expect/timeout: nope: it doesn't match ~s; retrying~%"
                             regex)
                   (loop)))

                 ))))))
    (begin0
      (and (sync/timeout seconds ch)
           ch)

      (kill-thread reader))))

(provide (all-defined))
)