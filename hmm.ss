#lang scheme

(require (only-in srfi/13 string-reverse))

(define (consume-from-port ip port->bytes line-filter line-consumer)
  (let loop ()
    (let ([datum (port->bytes ip)])
      (when (not (eof-object? datum))
        (when (line-filter datum)
          (line-consumer datum))
        (loop)))))

(define *ch* (make-channel))

(define putter
  (thread
   (lambda ()
     (let ([ip (open-input-string "foo\nbar\nbaz")])

       (consume-from-port
        ip
        read-line
        ((curry regexp-match) "^b")
        (compose ((curry channel-put) *ch*) string-reverse))

       (channel-put *ch* eof)))))

(let loop ()
  (let ([datum (channel-get *ch*)])
    (when (not (eof-object? datum))
      (display datum)
      (newline)
      (loop))))
