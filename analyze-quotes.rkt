;; Run me in "drRacket"
#lang racket
(require plot)

(module+ test (require rackunit))

;; jordanb says the quotes aren't coming out randomly.  I don't
;; particularly believe him, but let's see.  I'll find (what look
;; like) quotes in the log, and then measure how often each appears,
;; and then ... somehow ... do some sorta statistical analysis (the
;; details of which are unclear at the moment).

(define (bounding-box vecs)
  (for/fold ([xmin (vector-ref (car vecs) 0)]
             [xmax (vector-ref (car vecs) 0)]
             [ymin (vector-ref (car vecs) 1)]
             [ymax (vector-ref (car vecs) 1)])
      ([p (in-list vecs)])
      (let ([x (vector-ref p 0)]
            [y (vector-ref p 1)])
        (values (min x xmin)
                (max x xmax)
                (min y ymin)
                (max y ymax)))))

(module+ test
  (define-simple-check (check-bb vectors xmin xmax ymin ymax)
    (equal? (call-with-values (lambda () (bounding-box vectors))
              list)
            (list xmin xmax ymin ymax)))

  (check-bb '(#(0 0)) 0 0 0 0)
  (check-bb '(#(0 1)) 0 0 1 1)
  (check-bb '(#(0 0) #(0 1)) 0 0 0 1)
  (check-bb '(#(0 0) #(0 1) #(1 0)) 0 1 0 1))

(module+ main
  (define *ifn*  "big-log")
  (call-with-input-file *ifn*
    (lambda (ip)
      (define (hash-table-increment! table key)
        (hash-update! table key add1 0))
      (let ([counts-by-quote (make-hash) ]
            [histogram (make-hash)])
        (printf "Reading from ~a ...~%" *ifn*)
        (printf "Read ~a lines.~%"
                (for/and ([line (in-lines ip)]
                          [count (in-naturals)])
                  (match line
                    [(regexp #px"=> \"PRIVMSG #emacs :(.*)\"$" (list _ stuff))
                     (when (and (not (regexp-match #px"^\\w+:" stuff))
                                (not (regexp-match #px"Arooooooooooo" stuff)))
                       (hash-table-increment! counts-by-quote stuff))]
                    [_ #f])
                  count)
                )
        (printf "Snarfed ~a distinct quotes.~%" (hash-count counts-by-quote))
        (for ([(k v) (in-hash counts-by-quote)] )
          (hash-table-increment! histogram v))
        (printf "Histogram: ~a~%" histogram)
        (let ([vecs (hash-map histogram vector)])
          (let-values ([(xmin xmax ymin ymax) (bounding-box vecs)])
            (plot (points vecs)
                  #:x-label "Number of Occurrences"
                  #:y-label "Quotes"
                  #:x-min xmin
                  #:x-max xmax
                  #:y-min ymin
                  #:y-max ymax)))))))

