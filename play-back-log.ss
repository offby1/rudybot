#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(require (only-in srfi/13 string-reverse)
         (only-in "irc-process-line.ss" irc-process-line)
         (only-in "quotes.ss" start-dealer!)
         "vars.ss")

(define (consume-lines sequence line-filter line-consumer)
  (for ([line sequence]
        #:when (line-filter line))
    (line-consumer line)))

(define *ch* (make-channel))

(define *leading-crap* #px"^........................ <= ")
(define *length-of-leading-crap* 28)

(define putter
  (thread
   (lambda ()
     (let ([ip (open-input-file "big-log")])
       (define (read-from-string s)
         (read (open-input-string s)))
       (consume-lines
        (in-lines ip)
        ((curry regexp-match) *leading-crap*)
        (lambda (line)
          (channel-put *ch* (read-from-string (substring line *length-of-leading-crap*)))))
       (channel-put *ch* eof)
       ))))

(provide main)
(define (main)

  ;; Gotta set some parameters etc before invoking irc-process-line
  (start-dealer!)

  (parameterize ([*logger* (lambda  args (printf "I'm logging ~s~%" args))]
                 [*irc-output* (current-output-port)]
                 [*connection-start-time* (current-seconds)])
    (let loop ([lines-processed 0])
      (let ([datum (channel-get *ch*)])
        (when (and
               (not (eof-object? datum))
               (or #f (< lines-processed 10)))

          (irc-process-line datum)
          (loop (add1 lines-processed))))))

  (kill-thread putter))
