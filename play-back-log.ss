#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(define-values (*pipe-ip* *pipe-op*) (make-pipe))

(define *leading-crap* #px"^........................ <= ")
(define *length-of-leading-crap* 28)

;; Read lines from the log, discard some, massage the rest, and put
;; each of those into the pipe.
(define putter
  (thread
   (lambda ()
     (let ([ip (open-input-file "big-log")])
       (for ([line (in-lines ip)]
             #:when (((curry regexp-match) *leading-crap*) line))
         (display (read (open-input-string (substring line *length-of-leading-crap*))) *pipe-op*)
         (newline *pipe-op*))
       (close-output-port *pipe-op*)))))

(provide main)
(define (main)

  (let/ec break
    (for ([line (in-lines *pipe-ip*)]
          [lines-processed (in-naturals)])

      (when (> lines-processed 10)
        (break))

      (printf "Pretend I'm processing ~s~%" line)))

  (kill-thread putter))
