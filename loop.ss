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
(define *desired-nick* "rudybot")

(define log #f)
(let ((log-ports (list (current-error-port)
                       (open-output-file
                        "big-log"
                        #:mode 'text
                        #:exists 'append))))

  (set! log
        (lambda args

          (define (fresh-line op)
            (let-values ([(line column pos)
                          (port-next-location op)])
              (unless (zero? column)
                (newline op))))

          (for ((op (in-list log-ports)))
            (fresh-line op)
            (apply fprintf op args)
            (newline op))))

  (for ((op (in-list log-ports)))
    (port-count-lines! op)
    (file-stream-buffer-mode op 'line)))

(define *state* 'start)

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
       (case *state*
         ((start)
          (out "NICK ~a" *desired-nick*)
          (out "USER luser unknown-host localhost :duh, version 0")
          (set! *state* 'attempted-auth)))]

      ["PING"
       (out "PONG ~a" (cadr toks))]

      [(regexp #rx"^:(.*)!(.*)@(.*)$" (list _ nick id host))
       (if (equal? nick *desired-nick*)
           (log "I seem to have said ~s" toks)
           (match (cdr toks)
             [(list "JOIN" target)
              (log "~a joined ~a" nick target)]
             [(list "PART" target sayonara ...)
              (log "~a left ~a, saying ~a" nick target sayonara)]
             [(list "PRIVMSG" target blather ...)

              (define (leading-alnum str)
                (match str
                  [(regexp #px"^([[:alnum:]]+)" (list _ alnum))
                   alnum]
                  [_ #f]))

              (let ((blather (if (equal? #\: (string-ref (car blather) 0))
                                 (cons (substring (car blather) 1)
                                       (cdr blather))
                                 blather)))

                (log "~a said ~a to ~a" nick blather target)

                (cond

                 ;; They spoke directly to us -- i.e., with /query
                 ((equal? (leading-alnum target)
                          (leading-alnum *desired-nick*))
                  (out "PRIVMSG ~a :Well, ~a to you too"
                       nick
                       (string-join blather)))

                 ;; More common: they technically spoke to the
                 ;; channel, but prefixed their message with our nick
                 ((equal? (leading-alnum (car blather))
                          (leading-alnum *desired-nick*))
                  (out "PRIVMSG ~a :~a: Well, ~a to you too"
                       target
                       nick
                       (string-join (cdr blather))))))]

             [_
              (log "~a said ~s, which I don't understand" nick (cdr toks))]))]

      [(regexp #rx"^:(.*)" (list _ host))
       (match (cdr toks)
         [(list digits mynick blather ...)
          (case (string->symbol digits)
            ((|001|)
             (log "Yay, we're in")
             (set! *state* 'authenticated)
             (out "JOIN #scheme"))
            ((|366|)
             (log "I, ~a, seem to have joined channel ~a."
                  mynick
                  (car blather)))
            ((|433|)
             (log "Nuts, gotta try a different nick")
             (set! *desired-nick* (string-append *desired-nick* "_"))
             (out "NICK ~a" *desired-nick*)))])]
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
              (retry))
             ((string? line)
              (slightly-more-sophisticated-line-proc line op)
              (do-one-line 0))
             (else
              (error 'do-the-bot-thing "I don't know what to do with ~s" line)))))))))


(define (make-flaky-server)
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
            (relocate-output-port
             (current-output-port)
             #f #f 1 #f)
            )))

(define real-server
  (lambda ()
    (let-values (((ip op)
                  (tcp-connect
                   "localhost"
                   ;; "irc.freenode.org"
                   6667)))
      (file-stream-buffer-mode op 'line)
      (values ip op))))

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

;; (define (main . args)
;;   (random-seed 0)
;;   (let ((op (open-output-string)))
;;     (parameterize ((*bot-gives-up-after-this-many-silent-seconds* 1/4))
;;     (connect-and-run
;;      (make-preloaded-server op)))
;;     (printf "We emitted ~s~%" (get-output-string op))))

(define (main . args)
  (random-seed 0)
  (connect-and-run real-server))

(provide (all-defined-out))
