#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

(require (except-in (planet offby1/offby1/zdate) main))

(define-values (*pipe-ip* *pipe-op*) (make-pipe))

;; Read lines from the log, discard some, massage the rest, and put
;; each of those into the pipe.
(define putter
  (thread
   (lambda ()
     (define *leading-crap* #px"^........................ <= ")
     (define *length-of-leading-crap* 28)

     (let ([ip (open-input-file "big-log")])
       (for ([line (in-lines ip)]
             #:when (((curry regexp-match) *leading-crap*) line))
         (display (read (open-input-string (substring line *length-of-leading-crap*))) *pipe-op*)
         (newline *pipe-op*))
       (close-output-port *pipe-op*)))))

(provide main)
(define (main)

  ;; a bunch of hash tables in which we'll keep track of interesting
  ;; stuff we've parsed
  (define *tables*
    (make-immutable-hash
     (map
      (lambda (name) (cons name (make-hash)))
      '(
        lone-verbs
        notices
        numeric-verbs
        oddball-speakers
        randomness
        servers
        speaker-nicks
        speaker-hosts
        targets
        texts
        verbs
        ))))

  (define (inc! dict-name key) (dict-update! (hash-ref *tables* dict-name) key add1 0))

  (define (note-speaker! s)
    (match s
      [(pregexp #px"^(.*)!(.*)@(.*)" (list _ nick attrs host))
       (inc! 'speaker-nicks nick)
       (inc! 'speaker-hosts host)]
      [_ (inc! 'oddball-speakers s)]))

  (for ([line (in-lines *pipe-ip*)])
    (cond
     ((and (positive? (string-length line))
           (equal? #\: (string-ref line 0)))
      (let ([line (substring line 1)])
        (match line
          ;; ":lindbohm.freenode.net 002 rudybot :Your host is lindbohm.freenode.net ..."
          [(pregexp #px"^(\\S+) ([0-9]{3}) (\\S+) (.*)$" (list _ servername number-string target random-crap))
           (inc! 'servers servername)
           (inc! 'numeric-verbs number-string)

           (when (equal? #\: (string-ref random-crap 0))
             (set! random-crap (substring random-crap 1)))

           (case (string->symbol number-string)
             ;; Semantics of the various messages gleaned from (e.g.) http://www.mirc.net/raws/?view=328
             ((|328|)
              (match random-crap
                [(pregexp #px"(\\S+) :(.*)$" (list _ channel URL))
                 (printf "Channel ~a; URL ~a~%"
                         channel URL)]))

             ((|332|)
              (match random-crap
                [(pregexp #px"(\\S+) :(.*)$" (list _ channel topic))
                 (printf "Channel ~a; topic ~a~%"
                         channel topic)]))
             ((|333|)
              (match random-crap
                [(pregexp #px"(\\S+) (\\S+) ([0-9]+)$" (list _ channel creator topic-set-time))
                 (printf "Channel ~a; topic set by ~a at ~a~%"
                         channel creator (zdate (string->number topic-set-time)))]))
             ((|353|)
              (match random-crap
                [(pregexp #px"= (\\S+) :(.*)$" (list _ channel users))
                 (printf "Channel ~a; users ~a~%" channel (regexp-split " " users))]))
             ((|372|)
              ;; message of the day blather
              #t
              )
             (else
              (inc! 'randomness random-crap)))
           ]

          ;; "alephnull!n=alok@122.172.25.154 PRIVMSG #emacs :subhadeep: ,,doctor"
          [(pregexp #px"^(\\S+) (\\S+) (\\S+){0,1} :(.*)$" (list _ speaker verb target text))
           (note-speaker! speaker)
           (inc! 'verbs verb)
           (inc! 'targets target)
           (inc! 'texts text)]

          ;; "ChanServ!ChanServ@services. MODE #scheme +o arcfide "
          [
           (pregexp #px"^(\\S+) ((\\S+) )+" (list _ speaker words ...))
           (note-speaker! speaker)
           ]
          )))
     (else
      ;; non-colon lines -- pretty much just NOTICE and PING
      (match line
        [(pregexp #px"^(\\S+) (.*)?" (list _ verb random-crap))
         (inc! 'lone-verbs verb)
         (when (equal? "NOTICE" verb)
           (inc! 'notices random-crap))
         ]))))

  (let ()
    (define (keys dict)
      (sort (dict-map dict (lambda (k v) k))
            string<? #:key symbol->string))

    (for ([k (in-list (keys *tables*))])
      (printf "~a: " k)
      (pretty-print
       (sort #:key cdr
             (hash-map (hash-ref *tables* k) cons)
             <))))

  (kill-thread putter))
