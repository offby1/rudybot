#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui new-parse-tests 'verbose))"
|#
(module new-parse mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define-struct parse-result (sexprs trailing-garbage) #f)

(define (parse str)
  (let ((ip (open-input-string str)))
    (let loop ((exprs '()))
      (with-handlers
          ([exn:fail:read?
            (lambda (e)
              (make-parse-result
               (reverse exprs)
               (let* ((srcloc  (car (exn:fail:read-srclocs e)))
                      (pos     (srcloc-position srcloc))
                      (span    (srcloc-span     srcloc)))
                 (substring str (sub1 pos) (string-length str)))
               ))])
        (let ((one-expr (read ip)))
          (if (eof-object? one-expr)
              (make-parse-result (reverse exprs) #f)
              (loop (cons one-expr exprs))))))))

(define new-parse-tests

  (test-suite
   "new-parse"
   (test-case
    "trailing junk"
    (let ((r (parse "a b c 123 ) junk")))
      (check-equal? (parse-result-sexprs r)
                    '(a b c 123))
      (check-equal? (parse-result-trailing-garbage r)
                    ") junk")))

   (test-equal?
    "clean"
    (parse-result-sexprs (parse "a b c 123 ( not-junk-at-all )"))
    '(a b c 123 (not-junk-at-all)))))

(provide (all-defined))
)
