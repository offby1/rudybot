#lang scheme

;; jordanb says the quotes aren't coming out randomly.  I don't
;; particularly believe him, but let's see.  I'll find (what look
;; like) quotes in the log, and then measure how often each appears,
;; and then ... somehow ... do some sorta statistical analysis (the
;; details of which are unclear at the moment).

(call-with-input-file "big-log"
  (lambda (ip)
    (call/ec
     (lambda (return)
       (define (hash-table-increment! table key)
         (hash-update! table key add1 0))
       (let ((counts-by-quote (make-hash) )
             (histogram (make-hash)))
         (for ((line (in-lines ip)))
           (match line
             [(regexp #px"=> \"PRIVMSG #emacs :(.*)\"$" (list _ stuff))
              (when (and (not (regexp-match #px"^\\w+:" stuff))
                         (not (regexp-match #px"Arooooooooooo" stuff)))
                (hash-table-increment! counts-by-quote stuff))]
             [_ #f]))
         (printf "Snarfed ~a quotes.~%" (hash-count counts-by-quote))
         (for (((k v) (in-hash counts-by-quote)) )
           (hash-table-increment! histogram v))
         (printf "Histogram:~%")
         (pretty-display (sort (hash-map histogram cons)
                               <
                               #:key car)))))))

