#lang scheme
(require srfi/13
         (except-in "progress.ss" main)
         "db.ss")

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
       ;; This is a bit weird.  First we consume the timestamp and a
       ;; bit more from the port by calling regexp-match ... then we
       ;; "read" the remainder, which we're assuming is a quoted
       ;; Scheme string.
       (regexp-match #px"^.*? <= " ip)
       (let ((datum (read ip)))
         (when (not (eof-object? datum))
           (display datum op)
           (newline op)
           (loop)))))))

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
     (for ([line (in-lines ip)])
       (fprintf op "~a~%" (transform line))))))

(provide/contract [prefiltered-port->db [input-port? . -> . db?]])
(define (prefiltered-port->db ip)
  (let ((note!
         (make-notifier
          (lambda (times-called)
            (fprintf (current-error-port)
                     "Read ~a lines~%" times-called)))))
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

(provide/contract [irc-lines->db [(or/c string? path?) . -> . db?]])
(define (irc-lines->db filename)
  (call-with-input-file
   filename
   (lambda (ip)
     (port-count-lines! ip)
     (begin0
         (prefiltered-port->db
          (strip-irc-protocol-chatter
           (strip-logging-artifacts ip)))
       (fprintf
        (current-error-port)
        "Read ~a lines from ~a~%"
        (call-with-values (lambda ()
                            (port-next-location ip))
          (lambda args (car args)))
        ip)))))

