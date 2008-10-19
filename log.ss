#lang scheme

(define *receiver* (make-log-receiver (current-logger)
                                      'debug))

(define *log-file-name* "sandbox-log")

(define logging-thread
  (thread
   (lambda ()
     (let loop ()
       (match (sync *receiver*)
         [(vector level string marks)
          (call-with-output-file
              *log-file-name*
              (lambda (op)
                (fprintf
                 op
                 "~s: ~a: ~s~%"
                 level
                 string
                 (continuation-mark-set->context marks)))
              #:exists 'append)])
       (loop)))))

(log-fatal "Oh noooo!!")
(printf "Waiting for logging thread ...~%")
(sleep 1)
