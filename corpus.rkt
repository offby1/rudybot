#lang racket
(require
 "utterance.rkt"
 (prefix-in db: db)
 racket/trace
 )

(define verbose? (make-parameter #f))
(define (pe fmt . args)
  (when (verbose?)
    (apply fprintf (current-error-port) fmt args)))

(define (query-rows connection stmt . args)
  (let ([result (apply db:query-rows connection stmt args)])
    (pe "~a ~a => ~a~%" stmt args result)
    result))

(define (query-value connection stmt . args)
  (let ([result (apply db:query-value connection stmt args)])
    (pe "~a ~a => ~a~%" stmt args result)
    result))

(define (query-exec connection stmt . args)
  (let ([result (apply db:query-exec connection stmt args)])
    (pe "~a ~a => ~a~%" stmt args result)
    result))

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
  (not (null? (query-rows (corpus-db c) "SELECT word FROM log_word_map WHERE word = ? LIMIT 1" w))))

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
  (string? corpus? . -> . (or/c string? #f))
  (let
      ([candidates
        (query-rows
         (corpus-db c)
         #<<Q
SELECT text
FROM log
JOIN log_word_map
ON log.rowid = log_word_map.log_id
WHERE log_word_map.word = ?
Q
         rare)])
    (and (not (null? candidates))
         (vector-ref (car candidates) 0))))

(provide (rename-out [public-make-corpus make-corpus]))
(define/contract (public-make-corpus . sentences)
  (->* () () #:rest (listof string?) corpus?)
  (make-corpus-from-sequence sentences))

(define (id-of-newest-log db)
  (pe "log: ~a~% "(query-rows db "SELECT rowid, log.* from log"))
  (query-value db "SELECT MAX(rowid) FROM log"))

(define (log-utterance! db u)
  (query-exec
   db
   "insert into log values (?, ?, ?, ?)"
   (utterance-timestamp u)
   (utterance-speaker   u)
   (utterance-target    u)
   (utterance-text      u)))

(define/contract (log-word! db w log-id)
  (db:connection? string? integer? . -> . any)
  (query-exec
   db
   "insert into log_word_map values (?, ?)"
   w log-id))

(provide add-utterance-to-corpus)
(define (add-utterance-to-corpus u c)
  (log-utterance! (corpus-db c) u)
  (let ([log-id (id-of-newest-log (corpus-db c))])
    (for ([w (string->words (utterance-text u))])
      (log-word! (corpus-db c) w log-id))))

(provide add-sentence-to-corpus)
(define (add-sentence-to-corpus s c)
  (add-utterance-to-corpus
   (utterance
    0
    "bogus speaker"
    "bogus target"
    s)
   c))

(provide make-corpus-from-sequence)
(define (make-corpus-from-sequence sentences [limit #f])
  (let ([conn (db:sqlite3-connect
               #:database "/tmp/corpus.db"
               #:mode 'create)])
    (define c (corpus conn))

    (query-exec
     (corpus-db c)
     "CREATE TABLE IF NOT EXISTS
        log(timestamp TEXT, speaker TEXT, target TEXT, text TEXT)")
    (query-exec
     (corpus-db c)
     "CREATE TABLE IF NOT EXISTS
        log_word_map(word TEXT, log_id INTEGER)")

    (db:start-transaction (corpus-db c))

    (for ([s sentences])
      (cond
       ((string? s)
        (add-sentence-to-corpus s c))
       ((utterance? s)
        (add-sentence-to-corpus (corpus-db c) (utterance-text s)))))
    (db:commit-transaction (corpus-db c))

    (pe "log: ~a~% "(query-rows (corpus-db c) "SELECT * from log"))
    (pe "log_word_map: ~a~% "(query-rows (corpus-db c) "SELECT * from log_word_map"))
    c))

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

  (add-sentence-to-corpus s c)
  c)

(provide word-popularity)
(define/contract (word-popularity w c)
  (string? corpus? . -> . natural-number/c)
  (query-value (corpus-db c) "SELECT COUNT(log_id) FROM log_word_map WHERE word = ?" w))

(provide string->words)
(define/contract (string->words s)
  (string? . -> . set?)
  (wordlist->wordset (regexp-split #rx" " (string-downcase s))))

(define (setof pred)
  (lambda (thing)
    (and (set? thing)
         (for/and ([item (in-set thing)])
                  (pred item)))))

(provide wordlist->wordset)
(define/contract (wordlist->wordset ws)
  ((listof string?) . -> . (setof string?))
  (define (strip rx) (curryr (curry regexp-replace* rx) ""))
  (apply
   set
   (filter (compose positive? string-length)
           (map (compose
                 (strip #px"^'+")
                 (strip #px"'+$")
                 (strip #px"[^'[:alpha:]]+"))
                ws))))
