#lang scheme

(require  mzlib/etc)

(define (make-parsed-input-port ifn)
  (let-values ([(ip op) (make-pipe)])
    (let ([ch (make-channel)])

      ;; Reads from input file, puts to channel
      (thread
       (lambda ()
         (call-with-input-file
             ifn
           (lambda (unparsed-ip)
             (for ([line (in-lines unparsed-ip)])

               ;; TODO -- rather than putting "line" here, we put some
               ;; modification of line.  Hence "parsing" :-)
               (channel-put ch line)

               )
             (channel-put ch eof)))))

      ;; gets from channel; writes to pipe
      (thread
       (lambda ()
         (let loop ()
           (let ([datum (channel-get ch)])
             (if (eof-object? datum)
                 (close-output-port op)
                 (begin
                   (display datum op)
                   (newline op)
                   (loop)))))))

      ip)))

(let ([ip (make-parsed-input-port
           (build-path (this-expression-source-directory)
                       "quotes.ss"
                       ;; "big-log"
                       ))])
  (for ([line (in-lines ip)])
    (display line)
    (newline))
  (close-input-port ip))


