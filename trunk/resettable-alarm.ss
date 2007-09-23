#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui resettable-alarm-tests 'verbose))"
|#
(module resettable-alarm mzscheme
(require (lib "kw.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only "globals.ss" register-version-string)
         "vprintf.ss")
(register-version-string "$Id$")

;; Like an alarm event, but you can hit the "reset" button _before_ it
;; goes off (unlike a real alarm clock, whose snooze button is for
;; _after_it buzzes), which causes the alarm to reset.  I.e., if you
;; set the alarm to go off five seconds from now, wait four seconds,
;; then hit the snooze button, the alarm will eventually go off nine
;; seconds after you set it.

;; You can control whether it's periodic or not, which simply means:
;; will it go off just once, N seconds from now; or will it instead go
;; off -every- N seconds (subject to snoozage delays, of course).

(define-values (struct:resettable-alarm
                make-resettable-alarm
                resettable-alarm?
                resettable-alarm-ref
                resettable-alarm-set!)
    (make-struct-type 'resettable-alarm #f 3 0
                      #f (list (cons prop:evt 0))
                      (make-inspector) #f '(0 1 2)))

(define (alarm-reset-button r)
  (resettable-alarm-ref r 1))

(define (resettable-alarm-id r)
  (resettable-alarm-ref r 2))

(define/kw (public-make-resettable-alarm
            interval
            #:key
            [id 'unknown]
            [periodic? #f])
  (let* ((s (make-semaphore))
         ;; This seems like an expensive way to go about this -- every
         ;; time they hit the snooze button, we kill one thread and
         ;; create another.  It works, though :-)
         (sleeper (lambda ()
                    (let loop ()
;;                       (vtprintf "snooze sleeper ~a sleeping for ~a seconds~%"
;;                                 id interval)
                      (sleep interval)
                      (semaphore-post s)
;;                       (vtprintf "snooze sleeper ~a posted~%"
;;                                 id)
                      (when periodic? (loop))))))
    (let ((t (thread sleeper)))
      (make-resettable-alarm
       s
       (lambda/kw (#:key [fatal? #f])
         (kill-thread t)
;;          (vtprintf "reset button for ~a killed thread ~a~%"
;;                    id (eq-hash-code t))
         (when (not fatal?)
           (set! t (thread sleeper))
;;            (vtprintf "reset button for ~a made fresh sleeper ~a~%"
;;                    id (eq-hash-code t))
           ))
       id))))



(define resettable-alarm-tests

  (test-suite
   "resettable-alarm"
   (test-case
    "triggers like any alarm"
    (check-not-false
     (let ((ra (public-make-resettable-alarm 1/10 #:id 'triggers-like-any)))
       (sync/timeout 2/10 ra))
     "damn, it didn't get triggered."))

   (test-case
    "triggers repeatedly when asked"
    (let ((ra (public-make-resettable-alarm
               1/10
               #:periodic? #t
               #:id 'triggers-repeatedly)))
      (check-not-false (sync/timeout 2/10 ra))
      (check-not-false (sync/timeout 2/10 ra))
      (check-not-false (sync/timeout 2/10 ra))
      (check-not-false (sync/timeout 2/10 ra))
      ))

   (test-case
    "dies when asked"
    (let ((ra (public-make-resettable-alarm
               1/100
               #:periodic? #t
               #:id 'killable)))
      (check-not-false (sync/timeout 2/100 ra))
      (check-not-false (sync/timeout 2/100 ra))
      ((alarm-reset-button ra) #:fatal? #t)
      (check-false (sync/timeout 2 ra))
      ))

   (test-case
    "doesn't trigger if we tickle it"
    (let ((ra (public-make-resettable-alarm
               1/10
               #:id 'tickle-me-Elmo)))
      (check-false (sync/timeout 9/100 ra))
      ((alarm-reset-button ra))
      (check-false (sync/timeout 9/100 ra))
      ((alarm-reset-button ra))
      (check-false (sync/timeout 9/100 ra))
      ((alarm-reset-button ra))
      (check-false (sync/timeout 9/100 ra))
      (check-not-false
       (sync/timeout 2/100 ra)
       "it didn't")))
   ))

(provide (all-defined-except
          make-resettable-alarm
          resettable-alarm-ref
          resettable-alarm-set!)
         (rename public-make-resettable-alarm make-resettable-alarm ))
)
