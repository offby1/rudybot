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
                 (thread-send client-thread response)
                 (loop corpus)))]
            [(list client-thread ...)
             (log  "Got unexpected message ~s" message)
             (thread-send client-thread #f)
             (loop c)]
            [_ (fprintf (current-error-port) "Whiskey Tango Foxtrot: ~s~%" message)
               (loop c)]))))))

  (lambda (command-sym inp)
    (thread-send server-thread (list (current-thread) command-sym inp))
    (thread-receive)))
