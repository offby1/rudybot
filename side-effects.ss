#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -l errortrace --require $0 --main -- ${1+"$@"}
|#

#lang scheme

;; a bunch of hash tables in which we'll keep track of interesting
;; stuff we've parsed
(define *tables* (make-hash))

(provide inc!)
(define (inc! dict-name key)
  (let ([table (dict-ref *tables* dict-name (make-hash))])
    (dict-set! *tables* dict-name table)
    (dict-update! table key add1 0)))

(provide note-speaker!)
(define (note-speaker! s)
  (match s
    [(pregexp #px"^(.*)!(.*)@(.*)" (list _ nick attrs host))
     (inc! 'speaker-nicks nick)
     (inc! 'speaker-hosts host)]
    [_ (inc! 'oddball-speakers s)]))

(provide pretty-print-tables)
(define (pretty-print-tables)
  (define (keys dict)
    (sort (dict-map dict (lambda (k v) k))
          string<? #:key symbol->string))

  (for ([k (in-list (keys *tables*))])
    (when (not (eq? 'texts k))
      (printf "~a: " k)
      (pretty-print
       (sort #:key cdr
             (hash-map (hash-ref *tables* k) cons)
             <)))))
