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

(for-each (lambda (ip)
            (for ((line (in-lines ip)))
              (printf "~a: One line: ~s~%" ip line)))
          (list stderr-ip
                stdout-ip))

(unless (eq? 'done-ok (controller 'status))
  (error 'make-git-description
         "Bummer: ~s returned exit code ~a" *command*  (controller 'status)))
