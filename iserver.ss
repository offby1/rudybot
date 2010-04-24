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

(define (pf fmt . args)
  (apply fprintf (current-error-port) fmt args))

(define (log fmt . args)
  (when (*incubot-logger*)
    (apply (*incubot-logger*) fmt args)))

(provide make-incubot-server)
(define (make-incubot-server ifn)
  (let ([*to-server*   (make-channel)]
        [*from-server* (make-channel)])
    (define funcs-by-symbol
      (make-immutable-hash
       (list
        (cons 'get
              (lambda (inp c)
                (channel-put *from-server* (incubot-sentence inp c))
                c))
        (cons 'put
              (lambda (sentence c)
                (channel-put *from-server* #t)
                (add-to-corpus sentence c))))))
    (thread
     (lambda ()
       (let ([c (time
                 (with-handlers ([exn? (lambda (e)
                                         (fprintf (current-error-port)
                                                  "Ooops: ~a~%" (exn-message e))
                                         (exit 1))])
         
                   (fprintf (current-error-port) "Reading log from ~a..." ifn)
                   (begin0
                       (call-with-input-file ifn 
                         (lambda (ip)
                           (make-corpus-from-sexps 
                            ip
                            utterance-text)))
                     (fprintf (current-error-port) "Reading log from ~a...done" ifn))))])
         
         (let loop ([c c])
           (match (channel-get *to-server*)
             [(cons symbol inp)
              (loop ((hash-ref funcs-by-symbol symbol) inp c))])))))
    
    (lambda (command-sym inp)
      (log "incubot ~a ~s" command-sym inp)
      (channel-put *to-server* (cons command-sym inp))
      (channel-get *from-server*))))

(provide main)
(define (main . args)
  (let ([s (make-incubot-server "parsed-log")])
    (define (get input) (s 'get input))
    (define (put sentence) (s 'put sentence))

    (define (try input) (printf "~a => ~s~%" input (time (get input))))

    (try "Oh shit")
    (try "Oops, ate too much cookie dough")
    (put "What is all this shit?")
    (try "hamsters")
    (try "Oh shit")))

