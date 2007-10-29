#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --load "$0"
|#

;; An idea for a server.  "Cin" on #scheme asked how to write a server
;; that can broadcast a message to every client; this is my response.

;; You might find some earlier versions of this at
;; http://paste.lisp.org/display/49964, if it hasn't been
;; garbage-collected.

;; this module manages a collection of channels.  The problem that it
;; solves: our server wants to send a message to each of its clients.
;; The thread that sends the message needs to have some sort of sink
;; for each client.  The client's output port is an obvious candidate,
;; but I have this vague feeling that it's a bad idea to expose those
;; ... so instead we hide the port behind an async channel.  This way,
;; our server thread calls "put-on-all-channels", and each client's
;; thread will copy the stuff it receives from the other end of the
;; channel, to its output port.

;; Now, the collection of channels is effectively global, and we don't
;; want threads modifying it willy nilly; so this module encapsulates
;; the modifications, ensuring that only one thread modifies it.

(module ocl mzscheme
(require (only (lib "1.ss" "srfi") remove!)
         (lib "async-channel.ss")
         (lib "thread.ss")
         (lib "trace.ss"))
(define *the-channels* '())
(define *the-semaphore* (make-semaphore 1))

;; I don't think there's any need to block on the semaphore here -- I
;; don't at all care if a channel pops into existence while the
;; for-each is running (if it does, and we put the message on it,
;; fine; if we fail to put the message on it, also fine, since we'll
;; do so the next time we're called); and I don't _think_ I care if
;; the channel has gotten unregistered when we put the message onto it
;; ... it'll get gc'd eventually.  I hope.
(define (put-on-all-channels message)
  (for-each
   (lambda (ch) (async-channel-put ch message))
   *the-channels*))

(define (new-channel)
  (call-with-semaphore
   *the-semaphore*
   (lambda ()
     (set! *the-channels* (cons (make-async-channel) *the-channels*))
     (car *the-channels*))))

(define (unregister-channel victim)
  (call-with-semaphore
   *the-semaphore*
   (lambda ()
     (unless (async-channel? victim)
       (error 'make-or-unregister-channel "Wanted an async-channel; instead got ~s" victim))
     (let ((original-length (length *the-channels*)))
       (set! *the-channels* (remove! (lambda (ch)
                                       (eq? ch victim))
                                     *the-channels*))
       (unless (= (add1 (length *the-channels*))
                  original-length)
         (error 'make-or-unregister-channel "didn't find exactly one copy of our channel") )))))

(provide
 new-channel
 unregister-channel
 put-on-all-channels
 ))

(module bleep mzscheme
(require (lib "async-channel.ss")
         (lib "trace.ss")
         (lib "thread.ss")
         (lib "date.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         ocl)

;; create a server that listens on port 1234 ... accepts any number of
;; clients ... and, every few seconds, sends each client the same
;; "broadcast message".

(thread
 (lambda ()
   (let loop ((times-run 0))
     (put-on-all-channels
      (format
       "Broadcast message ~a~%"
       times-run))
     (sleep 1)
     (loop (add1 times-run)))))

(fprintf (current-error-port) "Server starting!~%")

(run-server
 1234
 (lambda (ip op)
   (file-stream-buffer-mode op 'line)
   (thread
    (lambda ()
      (let loop ()
        (let ((line (read-line ip)))
          (when (not (eof-object? line))
            (put-on-all-channels line)
            (loop))))))
   (let ((ch (new-channel)))
     (let loop ()
       (with-handlers
           ([exn:fail:network?
             (lambda (e)
               ;; assume that the client has died, and that the
               ;; exception we got was, more or less, "Broken pipe".
               (unregister-channel ch))])
         (display (async-channel-get ch) op)
         (loop)))))
 #f)

)

(require bleep)
