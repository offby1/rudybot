#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(require
 (except-in "dispatchees.ss" main)
 "side-effects.ss")

(define-values (*pipe-ip* *pipe-op*) (make-pipe))

(require file/gunzip)

;; Return a new input port that will yield the same data as zipped-ip,
;; except ... unzipped.
(define (make-unzipper zipped-ip)
  (let-values ([(pipe-ip pipe-op) (make-pipe)])
    (thread
     (lambda ()
       (define (cleanup) (close-output-port pipe-op))
       (with-handlers
           ([values (lambda (e) (cleanup))])
         (gunzip-through-ports zipped-ip pipe-op))
       (cleanup)))
    pipe-ip))

;; Read lines from the log, discard some, massage the rest, and put
;; each of those into the pipe.
(define putter
  (thread
   (lambda ()
     (define *leading-crap* #px"^........................ <= ")
     (define *length-of-leading-crap* 28)

     (call-with-input-file "big-log.gz"
       (lambda (ip)
         (for ([line (in-lines (make-unzipper ip))]
               [lines-read (in-range 10000)]
               #:when (((curry regexp-match) *leading-crap*) line))
           (display (read (open-input-string (substring line *length-of-leading-crap*))) *pipe-op*)
           (newline *pipe-op*))
         (close-output-port *pipe-op*))))))

(define (do-one-line line)
  (match line
    ;; ":lindbohm.freenode.net 002 rudybot :Your host is lindbohm.freenode.net ..."
    [(pregexp #px"^:(\\S+) ([0-9]{3}) (\\S+) (.*)$" (list _ servername number-string target random-crap))
     (inc! 'servers servername)
     (inc! 'numeric-verbs number-string)

     (when (equal? #\: (string-ref random-crap 0))
       (set! random-crap (substring random-crap 1)))

     (case (string->symbol number-string)
       ;; Semantics of the various messages gleaned from (e.g.) http://www.mirc.net/raws/?view=328
       ((|328|)
        (match random-crap
          [(pregexp #px"(\\S+) :(.*)$" (list _ channel URL))
           (do-328 channel URL)]))

       ((|332|)
        (match random-crap
          [(pregexp #px"(\\S+) :(.*)$" (list _ channel topic))
           (do-332 channel topic)]))
       ((|333|)
        (match random-crap
          [(pregexp #px"(\\S+) (\\S+) ([0-9]+)$" (list _ channel creator topic-set-time))
           (do-333 channel creator topic-set-time)]))

       ((|353|)
        (match random-crap
          [(pregexp #px"= (\\S+) :(.*)$" (list _ channel users))
           (do-353 channel  (regexp-split " " users))]
          [(pregexp #px"@ (.*)$" (list _ stuff))
           ;; dunno what this is, but it doesn't come up often
           #f
           ]))

       ((|372|)
        ;; message of the day blather
        #t
        )
       (else
        (inc! 'randomness random-crap)))
     ]

    ;; "alephnull!n=alok@122.172.25.154 PRIVMSG #emacs :subhadeep: ,,doctor"
    [(pregexp #px"^:(\\S+) (\\S+) (\\S+){0,1} :(.*)$" (list _ speaker verb target text))
     (inc! 'verbs 'PRIVMSG)
     (do-PRIVMSG speaker target text)]

    ;; "ChanServ!ChanServ@services. MODE #scheme +o arcfide "
    [
     (pregexp #px"^:(\\S+) ((\\S+) )+" (list _ speaker words ...))
     (note-speaker! speaker)
     ]
    ;; non-colon lines -- pretty much just NOTICE and PING
    [(pregexp #px"^(\\S+) (.*)?" (list _ verb random-crap))
     (do-notice verb random-crap)
     ]
    ))

(provide main)
(define (main)
  (for ([line (in-lines *pipe-ip*)])
    (with-handlers
        ([values
          (lambda (e)
            (fprintf
             (current-error-port)
             "~s~%" line)
            (raise e))])

    (do-one-line line)))

  (pretty-print-tables)

  (kill-thread putter))
