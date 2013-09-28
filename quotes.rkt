#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  racket -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang racket
(require racket/runtime-path)

(define *the-channel* (make-channel))

(define-runtime-path quotes-file "quotes")

(define *dealer*
  (thread
   (lambda ()
     (let re-read ()
       ;; Instead of, or in addition to, reading the quotes file
       ;; .. how about

       ;; SELECT * FROM log
       ;; JOIN          log_word_map ON log_word_map.log_id = log.rowid
       ;; WHERE         log_word_map.word = 'lets'
       ;; AND           (log.text GLOB '[lL]et?s*' OR log.text GLOB '[lL]ets*')

       (fprintf (current-error-port)
                "Reading quotes file~%")
       (let push-one ([all (shuffle (call-with-input-file quotes-file read))])
         (if (null? all)
             (re-read)
             (begin
               (channel-put *the-channel* (car all))
               (push-one (cdr all)))))))))

(provide one-quote)
(define (one-quote)
  (channel-get *the-channel*))

(provide main)
(define (main . args)
  (display (one-quote))
  (newline))
