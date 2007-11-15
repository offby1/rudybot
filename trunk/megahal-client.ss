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
      (if (eq? 'running status)
          (begin
            (display (regexp-replace* #rx"[\n\r]" text " ") input-op)
            (newline input-op)
            (newline input-op)
            (flush-output input-op)
            (begin0
                (read-line stdout-ip)
              (close-output-port input-op)
              (subprocess-kill spobj #t)))

          (begin
            (vtprintf
             "Couldn't start program: status is ~s (~s) ~%"
             status
             (let loop ((chars '()))
               (let ((ready (char-ready? stderr-ip)))
                 (if ready
                     (let ((ch (read-char stderr-ip)))
                       (if (eof-object? ch)
                           (list->string (reverse chars))
                           (loop (cons ch chars))))
                     (list->string (reverse chars)))
                 )))

            #f)))))

(provide get-megahal-response)
)
