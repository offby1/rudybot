(module sighting mzscheme
(require (lib "serialize.ss")
         (lib "async-channel.ss"))

(define *sightings-database-file-name* "sightings.db")

(define *the-channel* (make-async-channel))

(define update-server
  (thread
   (lambda ()
     (let loop ()
       (let ((write-me (async-channel-get *the-channel*)))
         (call-with-output-file *sightings-database-file-name*
           (lambda (op)
             (write write-me op))
           'truncate/replace))
       (loop)))))

(define (enqueue-sightings-update value)
  (async-channel-put *the-channel* value))

(define (maybe-call-with-sighting-data proc)
  (when (file-exists? *sightings-database-file-name*)
    (call-with-input-file *sightings-database-file-name*
      (lambda (ip)
        (proc (read ip))))))

(define-serializable-struct sighting (who where when was-action? words) #f)
(provide enqueue-sightings-update
         maybe-call-with-sighting-data
         (struct sighting (who where when was-action? words)))
)
