#lang scheme

(require mzlib/etc
         (only-in srfi/13 string-reverse))

(define (snorgulate-input-port unparsed-ip line-mangler)
  (let-values ([(ip op) (make-pipe)])
    (let ([ch (make-channel)])

      ;; Reads from unparsed-ip, puts to channel
      (thread
       (lambda ()
         (for ([line (in-lines unparsed-ip)])
           (channel-put ch (line-mangler line)))

         (channel-put ch eof)))

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

;; Roughly like open-input-file: takes a file name, and returns an
;; input port.  But the input port doesn't directly yield the bytes
;; from the file; rather, they're massaged by line-mangler.
(define (make-parsed-input-port ifn [line-mangler values])
  (snorgulate-input-port
   (open-input-file ifn)
   line-mangler))

(let ([ip (make-parsed-input-port
           (build-path (this-expression-source-directory)
                       "quotes.ss"
                       ;; "big-log"
                       )
           string-reverse)])
  (for ([line (in-lines ip)])
    (display line)
    (newline))
  (close-input-port ip))
