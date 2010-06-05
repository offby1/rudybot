#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require
 scheme/set
 scheme/include
 (only-in "log-parser.ss" utterance-text ))

(include "incubot-tests.ss")

(provide (except-out (struct-out corpus) make-corpus))
(define-struct corpus (strings strings-by-word) #:transparent)

(provide (rename-out [public-make-corpus make-corpus]))
(define/contract (public-make-corpus . sentences)
  (->* () () #:rest (listof string?) corpus?)
  (make-corpus-from-sequence  (in-list sentences)))

(define (random-favoring-smaller-numbers k)
  (let (
        ;; 0 <= r < 1, but smaller numbers are more likely
        [r (/(sub1 (exp (random))) (sub1 (exp 1)))])

    (inexact->exact
     (truncate
      (* r k) ;; 0 <= this < k
      ))))

;; favor longer utterances over shorter ones.
(define/contract (random-choose seq)
  (-> list? any/c)
  (let ([sorted (sort seq > #:key string-length)])
    (list-ref
     sorted
     (random-favoring-smaller-numbers (length seq)))))

(define/contract (strings-containing-word w c)
  (-> string? corpus? (listof string?))
  (hash-ref (corpus-strings-by-word c) w))

(provide  incubot-sentence)
(define incubot-sentence
  (match-lambda*
   [(list (? list? s) (? corpus? c))
    (incubot-sentence (wordlist->wordset s) c)]
   [(list (? string? s) (? corpus? c))
    (incubot-sentence (string->words s) c)]
   [(list (? set? ws) (? corpus? c))
    (let ([rare (rarest ws c)])
      (and rare
           (random-choose (strings-containing-word rare c))))]))

(define/contract (in-corpus? s c)
  (string? corpus? . -> . boolean?)
  (set-member? (corpus-strings c) s))

(define (make-corpus-from-sequence seq [limit #f])
  (let/ec return
    (for/fold ([c (make-corpus
                   (set)
                   (make-immutable-hash '()))])
        ([sentence seq]
         [forms-read (in-naturals)])
        (when (equal? limit forms-read)
          (return c))

      (add-to-corpus sentence c))))

(provide make-corpus-from-sexps)
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
  (-> string? corpus? corpus?)
  (make-corpus
   (set-add (corpus-strings c) s)
   (for/fold ([h (corpus-strings-by-word c)])
       ([w (in-set (string->words s))])
       (hash-update h w (curry cons s) '()))))

(define/contract (wordlist->wordset ws)
  ((listof string?) . -> . set?) ;; it'd be nice if I could say "a set whose
  ;; elements are all strings"
  (define (strip rx) (curryr (curry regexp-replace* rx) ""))
  (apply
   set
   (filter (compose positive? string-length)
           (map (compose
                 (strip #px"^'+")
                 (strip #px"'+$")
                 (strip #px"[^'[:alpha:]]+"))
                ws))))

(define/contract (string->words s)
  (string? . -> . set?)
  (wordlist->wordset (regexp-split #rx" " (string-downcase s))))

(define/contract (word-popularity w c)
  (string? corpus? . -> . natural-number/c)
  (length (hash-ref (corpus-strings-by-word c) w '())))

(define/contract (rarest ws c)
  (-> set? corpus? (or/c string? #f))
  (let ([result (foldl (lambda (w accum)
                         (let ([p (word-popularity w c)])
                           (cond
                            ((positive? p)
                             (cond
                              ((not accum)
                               (cons w p))
                              ((< p (cdr accum))
                               (cons w p))
                              (else
                               accum)))
                            (else
                             accum))))
                       #f
                       (set-map ws values))])
    (and result
         (car result))))

