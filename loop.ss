#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#
#lang scheme

(require scheme/date
         scheme/port
         (lib "trace.ss")
         (lib "13.ss" "srfi")
         (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" )))

;; This value depends on the server; this seems to work for freenode
(define *bot-gives-up-after-this-many-silent-seconds* (make-parameter 250))
(define *my-nick* "rudybot")

(define *log-ports* (make-parameter (list (current-error-port)
                                          (open-output-file
                                           "big-log"
                                           #:mode 'text
                                           #:exists 'append))))
(define (log . args)
  (define (fresh-line op)
    (let-values ([(line column pos)
                  (port-next-location op)])
      (unless (zero? column)
        (newline op))))

  (for ((op (in-list (*log-ports*))))
    (fresh-line op)
    (apply fprintf op args)
    (newline op)))

(for ((op (in-list (*log-ports*))))
  (fprintf (current-error-port)
           "Whopping port ~a~%" op)
  (port-count-lines! op)
  (file-stream-buffer-mode op 'line))

(define *authenticated?* #f)

(define (slightly-more-sophisticated-line-proc line op)
  (define (out format-string . args)
    (let ((str (apply format format-string args)))
      (log "=> ~s" str)
      (fprintf op "~a~%" str)))

  (log "<= ~s" line)
  (let ((toks (string-tokenize line)))
    (match (car toks)

      ["ERROR"
       (log "Uh oh!")]

      ["NOTICE"
       (unless *authenticated?*
         (out "NICK ~a" *my-nick*)
         (out "USER luser unknown-host localhost :duh, version 0")
         (set! *authenticated?* #t))]

      ["PING"
       (out "PONG ~a" (cadr toks))]

      [(regexp #rx"^:(.*)!(.*)@(.*)$" (list _ nick id host))
       (define (leading-alnum str)
         (match str
           [(regexp #px"^([[:alnum:]]+)" (list _ alnum))
            alnum]
           [_ #f]))
       (if (equal? nick *my-nick*)
           (log "I seem to have said ~s" (cdr toks))
           (match (cdr toks)
             [(list "JOIN" target)
              (log "~a joined ~a" nick target)]
             [(list "PART" target (regexp #px"^:(.*)" (list _ first-word )) rest ...)
              (log "~a left ~a, saying ~a" nick target (string-join (cons first-word rest)))]

             [(list "PRIVMSG"
                    target
                    (regexp #px"^:(.*)" (list _ first-word ))
                    rest ...)
              (log "Message is ~s" (cons first-word rest))
              (if (equal? target *my-nick*)
                  (begin
                    (log "~a privately said ~a to me"
                         nick
                         (string-join (cons first-word rest)))

                    (out "PRIVMSG ~a :Well, ~a to you too"
                         nick
                         (string-join (cons first-word rest))))
                  (match first-word
                    [(regexp #px"^([[:alnum:]]+)[,:]" (list _ addressee))
                     (log "~a spake unto ~a in ~a, saying ~a"
                          nick
                          addressee
                          target
                          (string-join rest))
                     (when (equal? addressee *my-nick*)
                       (out "PRIVMSG ~a :~a: Well, ~a to you too"
                            target
                            nick
                            (string-join rest)))]
                    [_
                     (log "~a mumbled something uninteresting in ~a"
                          nick
                          target)]))]

             [_
              (log "~a said ~s, which I don't understand" nick (cdr toks))]))]

      [(regexp #rx"^:(.*)" (list _ host))
       (match (cdr toks)
         [(list digits mynick blather ...)
          (case (string->symbol digits)
            ((|001|)
             (log "Yay, we're in")
             (set! *authenticated?* #t)
             (out "JOIN #scheme!"))
            ((|366|)
             (log "I, ~a, seem to have joined channel ~a."
                  mynick
                  (car blather)))
            ((|433|)
             (log "Nuts, gotta try a different nick")
             (set! *my-nick* (string-append *my-nick* "_"))
             (out "NICK ~a" *my-nick*)))])]
      [_ (log "Duh?")])))

(define (connect-and-run server-maker (consecutive-failed-connections 0))
  (when (positive? consecutive-failed-connections)
    (fprintf (current-error-port)
             "~a consecutive-failed-connections~%"
             consecutive-failed-connections)
    (sleep (expt 2 consecutive-failed-connections)))

  (with-handlers ([exn:fail:network?
                   (lambda (exn)
                     (printf "Oh noes! ~a!~%" (exn-message exn))
                     (connect-and-run server-maker (add1 consecutive-failed-connections)))])
    (let-values (((ip op)
                  (server-maker)))
      (let ((ch (make-channel)))
        (let do-one-line ((cfc consecutive-failed-connections))
          (let ((reader (thread (lambda ()
                                  (let ((line (read-line ip)))
                                    (channel-put ch line)))))
                (line (sync/timeout (*bot-gives-up-after-this-many-silent-seconds*) ch))
                (retry (lambda ()
                         (close-input-port ip)
                         (close-output-port op)
                         (connect-and-run server-maker (add1 cfc)))))

            (kill-thread reader)

            (cond
             ((not line)
              (fprintf (current-error-port)
                       "Bummer: ~a seconds passed with no news from the server~%"
                       (*bot-gives-up-after-this-many-silent-seconds*))
                                        ;(retry)
              )
             ((eof-object? line)
              (fprintf (current-error-port)
                       "Uh oh, server hung up on us~%")
              ;; (retry)
              )
             ((string? line)
              (slightly-more-sophisticated-line-proc line op)
              (do-one-line 0))
             (else
              (error 'do-the-bot-thing "I don't know what to do with ~s" line)))))))))


(define (make-flaky-server)
  (random-seed 0)
  (when (zero? (random 10))
    (raise (make-exn:fail:network
            "de network, she be broke"
            (current-continuation-marks))))

  (let-values (((ip op)
                (make-pipe)))
    (thread
     (lambda ()
       (when (not (port-closed? op))
         (call-with-input-file "../irc/example input"
           (lambda (ip)
             (let loop ()
               (let ((datum (read ip)))
                 (when (not (eof-object? datum))
                   (display datum op)
                   (newline op)
                   (loop))))))
         )))
    (values ip
            (open-output-nowhere)
            )))

(define (real-server)
  (let-values (((ip op)
                (tcp-connect
                 "localhost"
                 ;; "irc.freenode.org"
                 6667)))
    (file-stream-buffer-mode op 'line)
    (values ip op)))

(define (make-preloaded-server op)
  (lambda ()
    (values (let-values (((ip op)
                          (make-pipe)))
              (thread
               (lambda ()
                 (display "foO!\r\n" op)
                 (display "PING :localhost.\r\n" op)
                 (display ":niven.freenode.net 001 rudybot :Welcome to the freenode IRC Network rudybot\r\n" op)))
              ip)
            op)))

(define (make-log-replaying-server log-file-name)
  (lambda ()
    (let-values (((ip op)
                  (make-pipe)))
      (thread
       (lambda ()
         (call-with-input-file log-file-name
           (lambda (ip)
             (for ((line (in-lines ip)))
               (match line
                 [(regexp #px"^<= (\".*\")" (list _ datum))
                  (display (read (open-input-string datum)) op)
                  (newline op)]
                 [_ #f]))
             (close-output-port op)))))

      (values ip
              (relocate-output-port
               (current-output-port)
               #f #f 1 #f)))))

;; (define (main . args)
;;   (parameterize ((*bot-gives-up-after-this-many-silent-seconds* 1/4)
;;                  (*log-ports* (list (current-error-port))))
;;     (connect-and-run
;;      (make-log-replaying-server "big-log"))))

(define (main . args)
  (connect-and-run real-server))

(provide (all-defined-out))
