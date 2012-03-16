#lang racket

(require
 (except-in "incubot.rkt" main)
 (only-in "corpus.rkt"
   add-to-corpus
   corpus?
   make-corpus
   )
 (only-in "vars.rkt" *incubot-logger*)
 (only-in "utterance.rkt" utterance-text))

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
           (thunk
            (log "Reading log from ~a..." ifn)
            (with-handlers ([exn? (lambda (e)
                                    (log "Ooops: ~a~%" (exn-message e))
                                    (lambda ignored #f))])

              ;; ifn needs to point to a file of serialzed
              ;; "utterances", as produced by log-parser.rkt.

              ;; Yeah, I know; that's stupid.  I should peek the port,
              ;; and if the first character is a #, then assume it's
              ;; indeed the output of log-parser.rkt; but otherwise
              ;; assume it's an actual IRC log, and then should more
              ;; or less invoke log-parser's guts automatically.
              (call-with-input-file ifn
                (lambda (inp)
                  (let/ec return
                    (for ([(utterance i) (in-indexed (in-port read inp))])
                      (the-server 'put (utterance-text utterance))
                      ;; We bail out early because a) reading
                      ;; everything takes forever; b) we'd run out of
                      ;; memory (on the puny EC2 instance on which
                      ;; this runs "inp production").  This totally
                      ;; sucks; it means most of the log isn't
                      ;; available, and therefore the bot's answers
                      ;; are less amusing than they otherwise would
                      ;; be.
                      (when (= i 100000)
                        (return))

                      ;; Oh, and we sleep in the hope that this will
                      ;; let the main thread run a bit faster.
                      (sleep)
                      ))
                  (log "Reading log from ~a...done~%" inp)))))))))]

   [(? corpus? c)
    (define server-thread
      ;; Reads 'get' or 'put' commands from a client, and does the
      ;; corresponding work.
      (thread
       (thunk
        (let loop ([c c])
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
                    ((put)
                     (thread-send client-thread #t)
                     (add-to-corpus string c))
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
      (thread-receive))]))
