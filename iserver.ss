#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require
 (except-in "incubot.ss" main)
 (only-in "vars.ss" *incubot-logger*)
 (only-in "log-parser.ss" utterance-text))

(define (log fmt . args)
  (when (*incubot-logger*)
    (apply (*incubot-logger*) fmt args)))

(provide make-incubot-server)
(define make-incubot-server
  (match-lambda
   [(? string? ifn)
    (call-with-input-file ifn make-incubot-server)]
   [(? input-port? inp)
    (log "Reading log from ~a..." inp)
    (make-incubot-server
     (time
      (with-handlers ([exn? (lambda (e)
                              (log "Ooops: ~a~%" (exn-message e))
                              (lambda ignored #f))])

        (begin0
            (make-corpus-from-sexps inp)
          (log "Reading log from ~a...done~%" inp)))))]
   [(? corpus? c)
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
        (log "incubot ~a ~s" command-sym inp)
        (channel-put *to-server* (cons command-sym inp))
        (channel-get *from-server*)))]))

(provide main)
(define (main . args)
  (parameterize
      ([*incubot-logger* (lambda (fmt . args) (apply fprintf (current-error-port) fmt args))])
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
