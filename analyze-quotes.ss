#lang scheme

;; jordanb says the quotes aren't coming out randomly.  I don't
;; particularly believe him, but let's see.  I'll find (what look
;; like) quotes in the log, and then measure how often each appears,
;; and then ... somehow ... do some sorta statistical analysis (the
;; details of which are unclear at the moment).

(define *ifn*  "big-log")
(call-with-input-file *ifn*
  (lambda (ip)
    (define (hash-table-increment! table key)
      (hash-update! table key add1 0))
    (let ((counts-by-quote (make-hash) )
          (histogram (make-hash)))
      (printf "Reading from ~a ...~%" *ifn*)
      (printf "Read ~a lines.~%"
              (for/and ((line (in-lines ip))
                        (count (in-naturals)))
                       (match line
                         [(regexp #px"=> \"PRIVMSG #emacs :(.*)\"$" (list _ stuff))
                          (when (and (not (regexp-match #px"^\\w+:" stuff))
                                     (not (regexp-match #px"Arooooooooooo" stuff)))
                            (hash-table-increment! counts-by-quote stuff))]
                         [_ #f])
                       count)
              )
      (printf "Snarfed ~a distinct quotes.~%" (hash-count counts-by-quote))
      (for (((k v) (in-hash counts-by-quote)) )
        (hash-table-increment! histogram v))
      (printf "Histogram:~%")
      (pretty-display (sort (hash-map histogram cons)
                            <
                            #:key car)))))

