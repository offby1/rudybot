#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (lib "trace.ss"))

(define (data->input-port d)
  (let ((ch (make-channel)))
    (thread
     (lambda ()
       (for ((datum (in-list d)))
         (channel-put ch datum))
       (channel-put ch eof)))

    (let ((buffer #f))
      (define (maybe-refill-buffer!)
        (when (not buffer)
          (let ((datum (channel-get ch)))
            (when (procedure? datum)
              (raise (datum
                      "de network, she be broke"
                      (current-continuation-marks))))
          (set! buffer datum))))
      (define (shrink-buffer-by! nbytes)
        (when (not (bytes? buffer))
          (error 'shrink-buffer-by! "Buffer is ~s, which isn't a byte string" buffer))
        (when (< (bytes-length buffer) nbytes)
          (error 'shrink-buffer-by! "Buffer ~s is only ~a bytes long, but I was asked to shrink it by ~a bytes"
                 buffer
                 (bytes-length buffer)
                 nbytes))
        (set! buffer (subbytes buffer nbytes))
        (when (zero? (bytes-length buffer))
          (set! buffer #f)))
      (make-input-port
       "Exceptional"

       ;; read
       (lambda (bytes)
         (maybe-refill-buffer!)
         (if (eof-object? buffer)
             buffer
             (let ((nbytes (min (bytes-length bytes)
                                (bytes-length buffer))))
               (for ((i (in-range nbytes)))
                 (bytes-set! bytes i (bytes-ref buffer i)))
               (shrink-buffer-by! nbytes)
               nbytes)))

       ;; peek
       (lambda (bytes how-many control)
         (error 'peek-is-unimplemented))

       ;; close
       (lambda ()
         (printf "Yeah, we're closed.  Right.~%"))))))

(define (main . args)

  (let ((ip (data->input-port (list #"hey you\n"))))
    (for ((line (in-lines ip)))
      (printf "Line: ~s~%" line))
    (close-input-port ip))

  (let ((ip (data->input-port (list #"John "
                                    #"Paul\n"
                                    #"George\n"
                                    #"Ringo"
                                    #"\n\n")

                              )))
    (for ((line (in-lines ip)))
      (printf "Line: ~s~%" line))))

(provide (all-defined-out))
