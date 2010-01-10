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

  (let ([servers (make-hash)]
        [numeric-verbs (make-hash)]
        [verbs (make-hash)]
        [speakers (make-hash)])
    (define (inc! dict key) (dict-update! dict key add1 1))
    (for ([line (in-lines *pipe-ip*)]
          [lines-processed (in-naturals)])

      (cond
       ((and (positive? (string-length line))
             (equal? #\: (string-ref line 0)))
        (let ([line (substring line 1)])
          (match line
            [(pregexp #px"^([^ ]+) ([0-9]{3})" (list _ servername number-string))
             (inc! servers servername)
             (inc! numeric-verbs number-string)
             ]
            [(pregexp #px"^(.+?) " (list _ speaker))
             (inc! speakers speaker)
             ])))
       (else
        ;; non-colon lines -- pretty much just NOTICE and PING
        (match line
          [(pregexp #px"^(.+?) " (list _ verb))
           (inc! verbs verb)
           ])
        )))

    (printf "Servers seen: " )
    (pretty-print servers)
    (printf "Verbs seen: " )
    (pretty-print verbs)
    (printf "Numeric verbs seen: ")
    (pretty-print numeric-verbs)
    (printf "Speakers seen: ")
    (pretty-print
     (sort #:key cdr
           (hash-map speakers cons)
           <)))

  (kill-thread putter))
