#lang racket
(require
 db
 "utterance.rkt")

(provide (except-out (struct-out corpus) corpus))
(struct corpus (db) #:transparent)

(define (random-favoring-smaller-numbers k)
  (let (
        ;; 0 <= r < 1, but smaller numbers are more likely
        [r (/(sub1 (exp (random))) (sub1 (exp 1)))])

    (inexact->exact
     (truncate
      (* r k) ;; 0 <= this < k
      ))))

(provide in-corpus?)
(define/contract (in-corpus? w c)
  (string? corpus? . -> . boolean?)
  (query-rows "SELECT word FROM log_word_map WHERE word = ? LIMIT 1" w))

(provide corpus-size)
(define/contract (corpus-size c)
  (corpus? . -> . natural-number/c)
  (query-value (corpus-db c) "SELECT COUNT(DISTINCT word) FROM log_word_map" ))

;; favor longer utterances over shorter ones.
(provide random-choose)
(define/contract (random-choose seq)
  (-> list? any/c)
  (let ([sorted (sort seq > #:key string-length)])
    (list-ref
     sorted
     (random-favoring-smaller-numbers (length seq)))))

(provide random-choose-string-containing-word)
(define/contract (random-choose-string-containing-word rare c)
  (string? corpus? . -> . string?)
  "frotz")


(provide (rename-out [public-make-corpus make-corpus]))
(define/contract (public-make-corpus . sentences)
  (->* () () #:rest (listof string?) corpus?)
  (make-corpus-from-sequence  (in-list sentences)))

(define (make-corpus-from-sequence seq [limit #f])
  (corpus
   (sqlite3-connect
    #:database "/tmp/parsed-log.db"
    #:mode 'create)))

(provide make-corpus-from-sexps)
;; TODO -- somehow arrange that, if we get a fatal signal, we finish
;; writing out the current sexp, so that the output file remains
;; well-formed.
(define (make-corpus-from-sexps inp [limit #f])
  (make-corpus-from-sequence
   (in-port
    (lambda (ip)
      (let ([datum (read ip)])
        ;; this sure seems kludgy.  I wonder if there's a better way
        (if (eof-object? datum)
            datum
            (utterance-text datum))))
    inp)
   limit))

(provide make-corpus-from-file)
(define (make-corpus-from-file ifn)
  (call-with-input-file ifn
    (lambda (ip)
      (make-corpus-from-sequence (in-lines ip)))))

(provide add-to-corpus)
(define/contract (add-to-corpus s c)
  (string? corpus? . -> . corpus?)

  (define (offensive? s)
    (regexp-match #px"\\bnigger\\b" s))

  c)

(provide word-popularity)
(define/contract (word-popularity w c)
  (string? corpus? . -> . natural-number/c)
  0)
