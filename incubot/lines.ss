#lang scheme

(define (file->filtered-port ifn)
  (let-values (((pipe-ip pipe-op) (make-pipe 500)))
    (thread
     (lambda ()
       (call-with-input-file
           ifn
         (lambda (ip)
           (let loop ()
             (regexp-match #px"^.*? <= " ip)
             (let ((datum (read ip)))
               (if (eof-object? datum)
                 (close-output-port pipe-op)
                 (begin
                   (display datum pipe-op)
                   (newline pipe-op)
                   (loop)))))))))
    pipe-ip)
  )
