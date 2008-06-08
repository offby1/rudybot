#!/usr/bin/env mzscheme
#lang scheme
(define *command* (list "git-describe" "--tag"))

;; (define-values (child stdout-ip stdin-op stderr-ip)
;;   (apply subprocess #f #f (current-error-port) *command*))

;; (close-output-port stdin-op)
;; (subprocess-wait child)
;; (unless (zero? (subprocess-status child))
;;   (error 'make-git-description
;;          "Bummer: ~s returned exit code ~a" *command*  (subprocess-status child)))

(require scheme/system)

(match-define
 (list stdout-ip stdin-op pid stderr-ip controller)
 (process "git-describe --tag"))

(controller 'wait)

(unless (eq? 'done-ok (controller 'status))
  (for ((line (in-lines stderr-ip)))
    (printf "~a~%" line))
  (error 'make-git-description
         "Bummer: ~s returned exit code ~a" *command*  (controller 'status)))

(define *description-string* (read-line stdout-ip))
(define *file-name* "version.ss")
(when (or (not (file-exists? *file-name*))
          (not (equal? *description-string* (call-with-input-file *file-name* read))))
  (call-with-output-file *file-name* (lambda (op)
                                       (write *description-string* op)
                                       (newline op))
                         #:exists 'truncate/replace)
  (fprintf (current-error-port) "Created or updated ~a with ~s~%" *file-name* *description-string*))
