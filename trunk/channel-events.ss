#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui channel-events-tests 'verbose))"
|#
(module channel-events mzscheme
(require (lib "kw.ss")
         (lib "async-channel.ss")
         (lib "trace.ss")
         (only (lib "1.ss" "srfi") any)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "assert.ss" ("offby1" "offby1.plt"))
         "resettable-alarm.ss"
         (only "globals.ss" register-version-string)
         "parse.ss"
         "thread.ss"
         "vprintf.ss")
(register-version-string "$Id$")

(define-values (struct:channel-idle-event make-channel-idle-event channel-idle-event? channel-idle-event-ref channel-idle-event-set!)
    (make-struct-type 'channel-idle-event #f 2 0
                      #f (list (cons prop:evt 0))
                      (make-inspector) #f '(0 1)))

;(trace make-channel-idle-event)
(define (channel-idle-event-input-examiner r)
  (channel-idle-event-ref r 1))

;(trace channel-idle-event-input-examiner)

(define/kw (internal-make-channel-idle-event
            criterion
            interval
            periodic?)
  (let ((alarm (make-resettable-alarm
                interval
                #:periodic? periodic?)))
    (make-channel-idle-event
     alarm
     (lambda (irc-message)
       (when (criterion irc-message)
         ((alarm-reset-button alarm)))
       #f                               ;so that the main loop doesn't
                                        ;think we've handled the
                                        ;current message
       ))))

(define-values (struct:channel-request-event make-channel-request-event channel-request-event? channel-request-event-ref channel-request-event-set!)
    (make-struct-type 'channel-request-event #f 2 0
                      #f (list (cons prop:evt 0))
                      (make-inspector) #f '(0 1)))

(define (channel-request-event-input-examiner c)
  (channel-request-event-ref c 1))

(define (internal-make-channel-request-event criterion)
  (let ((c (make-async-channel)))
    (make-channel-request-event
     c
     (lambda (message)
       (and (criterion message)
            (async-channel-put c message))))))

(define/kw (make-channel-message-event
            criterion
            #:key
            [timeout #f]
            [periodic? #f]
            )
  (check-type 'make-channel-message-event procedure? criterion)
  (when timeout
    (check-type 'make-channel-message-event real? timeout)
    (check-type 'make-channel-message-event positive? timeout))
  (when periodic?
    (assert timeout))
  (if timeout
      (internal-make-channel-idle-event criterion timeout periodic?)
    (internal-make-channel-request-event criterion )))
;(trace make-channel-message-event)

(define (channel-message-event? thing)
  (or (channel-idle-event? thing)
      (channel-request-event? thing)))

(define (channel-message-event-input-examiner e)
  (check-type 'channel-message-event-input-examiner channel-message-event? e)
  (if (channel-idle-event? e)
      (channel-idle-event-input-examiner e)
    (channel-request-event-input-examiner e)))

;; there's no way to kill this thread ...

;; the returned procedure returns #t if and only if the action has
;; handled the message -- meaning the bot shouldn't say "golly, I have
;; no idea what that message means".
(define/kw (make-channel-action
            criterion action
            #:key
            [timeout #f]
            [periodic? #f]
            [responds? #f]
            descr
            )
  (when (and timeout responds?)
    (raise (make-exn:fail:contract
            "make-channel-action: neither timeout nor responds? are #f"
            (current-continuation-marks))))
  (let ((cme (make-channel-message-event
              criterion
              #:periodic? periodic?
              #:timeout timeout )))

    (thread-with-id
     (lambda ()
       (let loop ()
         (let ((why (sync cme)))
           (when why
             (action why))
           (loop))))
     #:descr descr)

    (lambda args
      (let ((interested? (apply (channel-message-event-input-examiner cme) args)))
        (and responds?
             interested?)))))




(define (on-snooze? m)
  (and (PRIVMSG? m)
       (member "#snooze" (PRIVMSG-receivers m))))

(define *timeout* 2/5)

(define (make-yammerer e string)
  (thread
   (lambda ()
     (let loop ()
       ((channel-idle-event-input-examiner e)
        (parse-irc-message string))
       (sleep (/ *timeout* 2))
       (loop))
     )))

(define channel-events-tests

  (test-suite
   "channel-events"

   (test-suite
    "actions"
   (test-case
    "channel goes idle when we're not yammering at it"
    (let* ((evidence (make-channel))
           (a
            (make-channel-action
             on-snooze?
             (lambda (thing)
               (channel-put evidence #t))
             #:timeout *timeout*
             #:periodic? #t)))

      (check-not-false (sync/timeout (* 2 *timeout*) evidence) "uh oh, the action didn't trigger.")
      (check-not-false (sync/timeout (* 2 *timeout*) evidence) "uh oh, the action didn't trigger the second time.")
      (let ((y (thread
                (lambda ()
                  (let loop ()
                    (a (parse-irc-message ":x!x@z PRIVMSG #snooze :wakey wakey"))
                    (loop))))))
        (check-false (sync/timeout (* 2 *timeout*) evidence) "idle action triggered when channel wasn't idle!")
        (kill-thread y))))

    (test-case
     "action triggers when we do yammer"
     (let* ((evidence (make-channel))
            (e (make-channel-action
                on-snooze?
                (lambda (thing)
                  (channel-put evidence #t))
                #:responds? #t)))
       (let ((handled? (e (parse-irc-message ":x!x@z PRIVMSG #snooze :wakey wakey"))))
         (check-not-false handled? "uh oh, our message didn't get handled")
         (check-true (sync/timeout (* 2 *timeout*)  evidence) "uh oh, our action didn't trigger"))))

   (test-case
    "channel doesn't go idle when we are yammering at it"
    (let ((e (make-channel-message-event on-snooze?
                                         #:periodic? #f
                                         #:timeout *timeout*)))
      (let ((relevant (make-yammerer e ":x!x@z PRIVMSG #snooze :wakey wakey")))
        (check-false (sync/timeout (* 2 *timeout*) e) "channel unexpectedly went idle")
        (kill-thread relevant)
        (let ((irrelevant (make-yammerer e ":x!x@z PRIVMSG #other-channel :wakey wakey")))
          (check-not-false (sync/timeout (* 2 *timeout*) e) "channel unexpectedly failed to go idle")
           (kill-thread irrelevant))))))))


(provide
 channel-events-tests
 make-channel-action
 channel-idle-event-input-examiner
 make-channel-message-event
))
