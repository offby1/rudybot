#lang racket

(require
 (except-in "incubot.rkt" main)
 (only-in "corpus.rkt"
   add-string-to-corpus
   corpus?
   make-corpus
   )
 (only-in "vars.rkt" *incubot-logger*)
 (only-in "utterance.rkt" utterance-text))

(provide make-incubot-server)
(define (make-incubot-server)
  (define server-thread
    ;; Reads 'get' or 'put-string' commands from a client, and does the
    ;; corresponding work.
    (thread
     (thunk
      (let loop ([c (make-corpus '())])
        (let ([message (thread-receive)])
          (match message
            [(list client-thread verb string)
             (loop
              (with-handlers ([exn? (lambda (e)
                                      (log "Drat: ~a" e)
                                      (thread-send client-thread e)
                                      (loop c))])
                (case verb
                  ((get)
                   (thread-send client-thread (incubot-sentence string c))
                   c)
                  ((put-string)
                   (thread-send client-thread #t)
                   ;; TODO -- perhaps ignore exceptions while adding
                   ;; to corpus
                   (add-string-to-corpus string c))
                  (else
                   (error "Unknown verb ~s" verb)))))]
            [(list client-thread ...)
             (log  "Got unexpected message ~s" message)
             (thread-send client-thread #f)
             (loop c)]
            [_ (fprintf (current-error-port) "Whiskey Tango Foxtrot: ~s~%" message)
               (loop c)]))))))

  (lambda (command-sym inp)
    (thread-send server-thread (list (current-thread) command-sym inp))
    (thread-receive)))
