#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
|#
(module sighting mzscheme
(require (lib "serialize.ss")
         (lib "async-channel.ss")
         "globals.ss")
(register-version-string "$Id$")
(define *sightings-database-file-name* "sightings.db")

(define *the-channel* (make-async-channel))

;; Rather than let our clients (who may be in lots of different
;; threads) call us willy-nilly, we force them all to call this
;; function, which is atomic.  That way there's no risk that multiple
;; concurrent writers will interfere with each other.
(define (enqueue-sightings-update value)
  (async-channel-put *the-channel* value))

;; Since our clients are stuffing values onto *the-channel*, we have
;; to retrieve them and save them.
(define update-server
  (thread
   (lambda ()
     (let loop ()
       (let ((write-me (async-channel-get *the-channel*)))

         ;; TODO: I'm pretty sure I've seen the output file get
         ;; corrupted, which suggests that this "write" was
         ;; interrupted partway through.  I suppose the fix is to
         ;; write to a temporary file instead, and then atomically
         ;; rename the temporary file to
         ;; *sightings-database-file-name*.

         ;; I wonder if that sort of atomicity should be built into
         ;; call-with-output-file; that might be a nice feature for
         ;; PLT to add.
         (call-with-output-file *sightings-database-file-name*
           (lambda (op)
             (write write-me op))
           'replace))
       (loop)))))

;; This function basically prevents our callers from knowing the name
;; of the sightings file ...
(define (maybe-call-with-sighting-data proc)
  (when (file-exists? *sightings-database-file-name*)
    (call-with-input-file *sightings-database-file-name*
      (lambda (ip)
        (let ((datum (read ip)))
          (when (not (eof-object? datum))
            (proc datum)))))))

(define-serializable-struct sighting (who where when was-action? words) #f)

;; For reasons I don't understand, if we say (provide (all-defined))
;; here, we get an error.  See
;; http://groups.google.com/group/plt-scheme/browse_thread/thread/2feae94cca6a8bd8
;; for a discussion.

;; (Actually I don't want to provide *sightings-database-file-name* or
;; *the-channel* anyway.)
(provide enqueue-sightings-update
         maybe-call-with-sighting-data
         (struct sighting (who where when was-action? words))
         )
)
