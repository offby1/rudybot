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
                                      (thread-send client-thread #f)
                                      (loop c))])
                (case verb
                  ((get)
                   (thread-send client-thread (incubot-sentence string c))
                   c)
                  ((put-string)
                   ;; BUGBUG -- OK, this is INSANELY fragile.  Turns
                   ;; out that we must ensure that the call to thread-send
                   ;; comes after the call to add-string-to-corpus,
                   ;; even though we want to return the value of the
                   ;; latter, since add-string-to-corpus might fail,
                   ;; and if it does, the exception handler above will
                   ;; push out a #f _in addition to_ the #t that we
                   ;; (mistakenly) already pushed out.  And when that
                   ;; happens, the client gets out of sync with us.

                   ;; So this whole idea of using mailboxes to
                   ;; communicate betwixt client and server needs to
                   ;; be rethought.
                   (begin0
                       (add-string-to-corpus string c)
                     (thread-send client-thread #t)))
                  (else
                   (log "Unknown verb ~s" verb)
                   (thread-send client-thread #f)
                   c))))]
            [(list client-thread ...)
             (log  "Got unexpected message ~s" message)
             (thread-send client-thread #f)
             (loop c)]
            [_ (fprintf (current-error-port) "Whiskey Tango Foxtrot: ~s~%" message)
               (loop c)]))))))

  (lambda (command-sym inp)
    (thread-send server-thread (list (current-thread) command-sym inp))
    (thread-receive)))
