#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require srfi/13
         (lib "thread.ss")
         (lib "etc.ss")
         (except-in "progress.ss" main))

(define-struct db (stuff) #:prefab)

;; ip -> ip
(define (make-filter writer)
  (let-values (((ip op)
                (make-pipe 500)))
    (thread
     (lambda ()
       (writer op)
       (close-output-port op)))
    ip))

(define (strip-logging-artifacts ip)
  (make-filter
   (lambda (op)
     (let loop ()
       (regexp-match #px"^.*? <= " ip)
       (let ((datum (read ip)))
         (when (not (eof-object? datum))
           (display datum op)
           (newline op)
           (loop)))))))

(define (irc-lines->db filename)
  (call-with-input-file
   filename
   (lambda (ip)
     (port->db
      (strip-irc-protocol-chatter
       (strip-logging-artifacts ip))))))

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

;; input lines look like this:
;; :|tommie|!n=~@93.190.182.214 PRIVMSG #scheme :hello.
(define (strip-irc-protocol-chatter ip)
  (define (transform line)
    (regexp-replace
     #px"PRIVMSG #[^[:blank:]]+ *:"
     (regexp-replace
      #px"^:[^[:blank:]]* *"
      line
      "")
     ""))
  (make-filter
   (lambda (op)
     (let loop ()
       (let ((line (read-line ip)))
         (when (not (eof-object? line))
           (display (transform line) op)
           (newline op)
           (loop)))))))

(provide main)
(define (main . args)
  (let ((db (irc-lines->db
             (build-path
              (this-expression-source-directory)
              'up "big-log"))))
    (fprintf
     (current-error-port)
     "Server starting!~%")
    (for ([word (in-lines (current-input-port))])
      (display (lookup word db))
      (newline))))
