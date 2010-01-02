#lang scheme

(require (only-in srfi/13 string-reverse))

(define (consume-from-port ip port->string line-filter line-consumer)
  (let loop ()
    (let ([datum (port->string ip)])
      (when (not (eof-object? datum))
        (when (line-filter datum)
          (line-consumer datum))
        (loop)))))

(define *ch* (make-channel))

(define *leading-crap* #px"^........................ <= ")
(define *length-of-leading-crap* 28)

(define putter
  (thread
   (lambda ()
     (let ([ip (open-input-file "big-log")])
       (define (read-from-string s)
         (read (open-input-string s)))
       (consume-from-port
        ip
        read-line
        ((curry regexp-match) *leading-crap*)
        (lambda (line)
          (channel-put *ch* (read-from-string (substring line *length-of-leading-crap*)))))
       (channel-put *ch* eof)
       ))))

(let loop ([lines-processed 0])
  (let ([datum (channel-get *ch*)])
    (when (and
           (not (eof-object? datum))
           (< lines-processed 10))

      (printf "~a\t~a~%" lines-processed datum)
      (loop (add1 lines-processed)))))

(kill-thread putter)
