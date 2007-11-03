#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0"
|#
(module megahal-client mzscheme
(require (lib "trace.ss")
         "vprintf.ss")

;; I don't really know what to do with this.

(define *the-subprocess* #f)

(define *program-full-name* "/usr/bin/megahal!!")

(define get-megahal-response
  (with-handlers
      ([void
        (lambda (e)
          (vtprintf "Couldn't start megahal: ~a~%"
                    (exn-message e))
          (lambda (text)
            (format "Uh oh, can't talk to megahal: ~a"
                    (exn-message e))))])

    (let-values (((spobj stdout-ip input-op stderr-ip)
                  (subprocess #f #f #f
                              *program-full-name*
                              "--no-prompt"
                              "--no-wrap"
                              "--no-banner")))
      (when (not (eq? 'running (subprocess-status spobj)))
        (error "Couldn't start program"))

      (set! *the-subprocess* spobj)
      (lambda (text)
        (display (regexp-replace* #rx"[\n\r]" text " ") input-op)
        (newline input-op)
        (newline input-op)
        (flush-output input-op)
        (read-line stdout-ip)))))


(for-each (lambda (s)
            (printf "~a => ~a~%"
                    s
                    (get-megahal-response s)))
          (list
            "hey, megahal, how are things?"
            "oh really?  you don't say."
            "what time is it?")
)
(when (subprocess? *the-subprocess*)
  (subprocess-kill *the-subprocess* #t))
(provide (all-defined))
)
