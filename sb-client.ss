#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(require
 (planet schematics/schemeunit:3)
 (planet schematics/schemeunit:3/text-ui)
 mzlib/trace)

(define *server* #f)
(define *out* #f)
(define *in* #f)
(define *err* #f)

(define (drain-port ip)
  (let loop ((so-far '()))
    (if (sync/timeout 1/10 ip)
        (loop (cons (read-char ip) so-far))
        (apply string (reverse so-far)))))

(trace drain-port)
(define (sandbox-eval string)
  (when (not *server*)
    (set!-values (*server* *out* *in* *err*)
                 (subprocess *out* *in* *err* "/bin/cat"
                             ;; "--number"
                             ))
    (fprintf
     (current-error-port)
     "Fired up sandbox server.  PID is ~a; status is ~a~%"
     (subprocess-pid *server*)
     (subprocess-status *server*)))
  (display string *in*)
  (newline *in*)
  (flush-output *in*)
  (drain-port *out*))
(trace sandbox-eval)
(define (process ip op)
  (for ((line (in-lines ip)))
    (let ((ip (open-input-string line)))
      (with-handlers
          ([exn:fail:read?
            (lambda (e)
              (fprintf op
                       "Can't parse ~s: ~s~%" line
                       (exn-message e)))])

        (fprintf op "~a" (sandbox-eval (read ip))))
      (flush-output op)
      (let ((leftovers (read-line ip)))
        (when (string? leftovers)
          (fprintf
           (current-error-port)
           "Leftover crud in ~a: ~s~%" ip leftovers))))))

(define (one-test inp expected)
  (let ((results (open-output-string)))
    (process (open-input-string inp) results)
    (let ((results (get-output-string results)))
      (cond
       ((string? expected)
        (check-equal? results expected))
       ((regexp? expected)
        (check-regexp-match expected results ))))))

(define (main . args)
  (exit
   (run-tests
    (test-suite
     "The Big Suite"
     (one-test "yo ho ho" "yo\n")
     (one-test " (+ 2 3) " "(+ 2 3)\n")
     (one-test "\"ya ha ha" #rx"^Can't parse")))))

(provide main)
