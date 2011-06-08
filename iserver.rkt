#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require
 (except-in "incubot.rkt" main)
 (only-in "vars.rkt" *incubot-logger*)
 (only-in "log-parser.rkt" utterance-text))

(define (log fmt . args)
  (when (*incubot-logger*)
    (apply (*incubot-logger*) (string-append "incubot-server:" fmt) args)))

(provide make-incubot-server)
(define make-incubot-server
  (match-lambda
   [(? string? ifn)
    (with-handlers ([exn:fail:filesystem?
                     (lambda (e)
                       (log "Uh oh: ~a; using empty corpus" (exn-message e))
                       (make-incubot-server (make-corpus)))])

      ;; Load the server up asynchronously, so we don't have to wait
      ;; for it.
      (let ([the-server (make-incubot-server (make-corpus))])
        (begin0
            the-server
          (thread
           (lambda ()
             (log "Reading log from ~a..." ifn)
             (time
              (with-handlers ([exn? (lambda (e)
                                      (log "Ooops: ~a~%" (exn-message e))
                                      (lambda ignored #f))])

                (call-with-input-file ifn
                  (lambda (inp)
                    (let/ec return
                      (for ([(utterance i) (in-indexed (in-port read inp))])
                        (the-server 'put (utterance-text utterance))
                        (when (= i 100000)
                          (return))))
                    (log "Reading log from ~a...done~%" inp))))))))))]

   [(? corpus? c)
    ;; TODO, low priority: Racket threads have a built-in "mailbox",
    ;; which is essentially an async channel; we could replace one of
    ;; these channels with it.
    (let ([*to-server*   (make-channel)]
          [*from-server* (make-channel)])
      (define funcs-by-symbol
        (make-immutable-hash
         `((get .
                ,(lambda (inp c)
                   (channel-put *from-server* (incubot-sentence inp c))
                   c))
           (put .
                ,(lambda (sentence c)
                   (channel-put *from-server* #t)
                   (add-to-corpus sentence c))))))
      (thread
       (lambda ()
         (let loop ([c c])
           (match (channel-get *to-server*)
             [(cons symbol inp)
              (loop ((hash-ref funcs-by-symbol symbol) inp c))]))))

      (lambda (command-sym inp)
        (channel-put *to-server* (cons command-sym inp))
        (channel-get *from-server*)))]))

(provide main)
(define (main . args)
  (parameterize
      ([*incubot-logger* (curry fprintf (current-error-port))])
    (let ([s (make-incubot-server
              (open-input-string
               (string-append
                "#s(utterance \"2010-01-19T03:01:31Z\" \"offby1\" \"##cinema\" \"Let's make hamsters race\")"
                "\n"
                "#s(utterance \"2010-01-19T03:01:31Z\" \"offby1\" \"##cinema\" \"Gimme some dough\")")
               ))])
      (define (get input) (s 'get input))
      (define (put sentence) (s 'put sentence))

      (define (try input) (printf "~a => ~s~%" input (time (get input))))

      (try "Oh shit")
      (try "Oops, ate too much cookie dough")
      (try "OOPS, ATE TOO MUCH COOKIE DOUGH")
      (put "What is all this shit?")
      (try "hamsters")
      (try "Oh shit"))))
