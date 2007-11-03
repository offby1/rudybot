#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module megahal-client mzscheme
(require (lib "trace.ss")
         "vprintf.ss")

;; I don't really know what to do with this.

(define *program-full-name* "/usr/bin/megahal")

(define (get-megahal-response text)
  (let-values (((spobj stdout-ip input-op stderr-ip)
                (subprocess #f #f #f
                            *program-full-name*
                            "--no-prompt"
                            "--no-wrap"
                            "--no-banner")))

    (let ((status (subprocess-status spobj)))
      (when (not (eq? 'running status))
        (error 'get-megahal-response "Couldn't start program: status is ~s" status)))

    (display (regexp-replace* #rx"[\n\r]" text " ") input-op)
    (newline input-op)
    (newline input-op)
    (flush-output input-op)
    (begin0
        (read-line stdout-ip)
      (close-output-port input-op)
      (subprocess-kill spobj #t))))


(for-each (lambda (s)
            (printf "~a => ~a~%"
                    s
                    (get-megahal-response s)))
          (list
            "hey, megahal, how are things?"
            "oh really?  you don't say."
            "what time is it?")
)

(provide (all-defined))
)
