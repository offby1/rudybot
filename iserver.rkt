#lang racket

(require
 "incubot.rkt"
 (only-in "corpus.rkt"
   add-string-to-corpus
   corpus?
   make-corpus
   )
 (only-in "vars.rkt" *incubot-logger*)
 (only-in "utterance.rkt" utterance-text)
 racket/async-channel)

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
            [(list client-channel verb string)
             (call-with-values
                 (thunk
                  (with-handlers ([exn? (lambda (e)
                                          (log "Drat: ~a" e)
                                          (values #f c))])
                    (case verb
                      ((get)
                       (values
                        (incubot-sentence string c)
                        c))
                      ((put-string)
                       (values #t
                               (add-string-to-corpus string c)))
                      (else
                       (log "Unknown verb ~s" verb)
                       (values #f c)))))
               (lambda (response corpus)
                 (async-channel-put client-channel response)
                 (loop corpus)))]
            [(list client-channel ...)
             (log  "Got unexpected message ~s" message)
             (async-channel-put client-channel #f)
             (loop c)]
            [_ (fprintf (current-error-port) "Whiskey Tango Foxtrot: ~s~%" message)
               (loop c)]))))))

  (lambda (command-sym inp)
    (define client-channel (make-async-channel 1))
    (thread-send server-thread (list client-channel command-sym inp))
    (async-channel-get client-channel)))
