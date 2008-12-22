#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require srfi/13
         (lib "thread.ss")
         (except-in "progress.ss" main))

(define-struct db (stuff) #:prefab)

(define (irc-lines->db filename)
  (cwif
   filename
   (lambda (ip)
     (port->db
      (strip-irc-protocol-chatter
       ip)))
   (lambda (fn) (format "Reading ~s" fn))))

(provide/contract [port->db [input-port? . -> . db?]])
(define (port->db ip)
  (let ((note!
         (make-notifier
          (lambda (times-called)
            (fprintf (current-error-port)
                     "Read ~a lines from ~s~%" times-called ip)))))
    (make-db
     (for/fold ([db (make-immutable-hash '())])
         ([string (in-lines ip)])
         (note!)
         (for/fold ([db db])
             ([word (in-list (string-tokenize string))])
             (hash-update db word (lambda (existing)
                                    ;; Only save this string if it's
                                    ;; longer than any other we've seen.
                                    (if (< (string-length existing)
                                           (string-length string))
                                        string
                                        existing))
                          ""))))))

(provide/contract [lookup [string? db? . -> . (or/c string? false/c)]])
(define (lookup word db)
   (hash-ref (db-stuff db) word #f))

;; ip -> ip
;; input lines look like this:
":|tommie|!n=~@93.190.182.214 PRIVMSG #scheme :hello."
;; i.e., they're quoted Scheme strings.
(define (strip-irc-protocol-chatter ip)
  (define (transform line)
    (regexp-replace
     #px"PRIVMSG #[^[:blank:]]+ *:"
     (regexp-replace
      #px"^:[^[:blank:]]* *"
      line
      "")
     ""))
  (let-values (((pipe-ip pipe-op)
                (make-pipe 500)))
    (thread (lambda ()
              (let loop ()
                (let ((line (read ip)))
                  (if (eof-object? line)
                      (close-output-port pipe-op)
                      (begin
                        (display (transform line) pipe-op)
                        (newline pipe-op)
                        (loop)))))))
    pipe-ip))

(define (cwif fn proc message-fn)
  (call-with-input-file fn
    (lambda (ip)
      (fprintf (current-error-port) "~a ... " (message-fn fn))
      (begin0
          (proc ip)
        (fprintf (current-error-port) "done~%")))))

;; echo system | nc -q 1 localhost 2222
(provide main)
(define (main . args)
  (let ((db (irc-lines->db
             "irc-lines")))
    (run-server
     2222
     (lambda (ip op)
       (for ([word (in-lines ip)])
         (display (lookup word db) op)
         (newline op)
         (flush-output op)))
     #f
     raise)))
