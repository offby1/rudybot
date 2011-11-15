#lang racket
(require
 "utterance.rkt"
 (prefix-in db: db)
 racket/trace
 )

(define (pe fmt . args)
  (apply fprintf (current-error-port) fmt args))

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
  (string? corpus? . -> . string?)
  "frotz")


(provide (rename-out [public-make-corpus make-corpus]))
(define/contract (public-make-corpus . sentences)
  (->* () () #:rest (listof string?) corpus?)
  (make-corpus-from-sequence sentences))

(define (get-last-row-id db)
  (query-value db "SELECT last_insert_rowid()"))


(define (log-utterance! db u)
  (query-exec
   db
   "insert into log values (?, ?, ?, ?)"
   (utterance-timestamp u)
   (utterance-speaker   u)
   (utterance-target    u)
   (utterance-text      u)))
(trace log-utterance!)

(define (log-sentence! db s)
  (query-exec
   db
   "insert into log values (?, ?, ?, ?)"
   0
   "bogus speaker"
   "bogus target"
   s))
(trace log-sentence!)

(define/contract (log-word! db w log-id)
  (db:connection? string? integer? . -> . any)
  (query-exec
   db
   "insert into log_word_map values (?, ?)"
   w log-id))
(trace log-word!)

(provide add-utterance-to-corpus)
(define (add-utterance-to-corpus ut c)
  (log-utterance! (corpus-db c) ut)
  (let ([log-id (get-last-row-id (corpus-db c))])
    (for ([w (string->words (utterance-text ut))])
      (log-word! (corpus-db c) w log-id))))

(define (make-corpus-from-sequence sentences [limit #f])
  (let ([conn (db:sqlite3-connect
               #:database 'memory
               #:mode 'create)])
    (query-exec
     conn
     "CREATE TABLE IF NOT EXISTS
        log(timestamp TEXT, speaker TEXT, target TEXT, text TEXT,
            PRIMARY KEY (timestamp, speaker, target)
            ON CONFLICT IGNORE)")
    (query-exec
     conn
     "CREATE TABLE IF NOT EXISTS
        log_word_map(word TEXT, log_id INTEGER,
            PRIMARY KEY (word, log_id)
            ON CONFLICT IGNORE)")

    (for ([s sentences])
      (cond
       ((string? s)
        (log-sentence! conn s))
       ((utterance? s)
        (log-utterance! conn s)))
      (for ([w (string->words s)])
        (log-word! conn w (get-last-row-id conn))))

    (corpus conn)))

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
  (query-value (corpus-db c) "SELECT COUNT(log_id) FROM log_word_map WHERE word = ?" w))

(provide string->words)
(define/contract (string->words s)
  (string? . -> . set?)
  (wordlist->wordset (regexp-split #rx" " (string-downcase s))))
(trace string->words)

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
