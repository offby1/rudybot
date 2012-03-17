#lang racket
(require
 (prefix-in db: db)
 racket/trace
 unstable/debug
 )

(define (query-rows connection stmt . args)
  (let ([result (apply db:query-rows connection stmt args)])
    result))

(define (query-value connection stmt . args)
  (let ([result (apply db:query-value connection stmt args)])
    result))

(define (query-exec connection stmt . args)
  (let ([result (apply db:query-exec connection stmt args)])
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

(provide corpus-word-count)
(define/contract (corpus-word-count c)
  (corpus? . -> . natural-number/c)
  (query-value (corpus-db c) "SELECT COUNT(DISTINCT word) FROM log_word_map" ))

;; favor longer sentences over shorter ones.
(provide random-choose)
(define/contract (random-choose seq)
  (-> (listof string?) any/c)
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
          (random-choose (map (curryr vector-ref 0) candidates)))))

(define (id-of-newest-log db)
  (query-value db "SELECT MAX(rowid) FROM log"))

(define (log-sentence! db s)
  (query-exec
   db
   "insert into log values (?)"
   s))

(define/contract (log-word! db w log-id)
  (db:connection? string? integer? . -> . any)
  (query-exec
   db
   "insert into log_word_map values (?, ?)"
   w log-id))

(provide add-sentence-to-corpus)
(define (add-sentence-to-corpus s c)
  (log-sentence! (corpus-db c) s)
  (let ([log-id (id-of-newest-log (corpus-db c))])
    (for ([w (string->words s)])
      (log-word! (corpus-db c) w log-id))))

(define *db-file-name* (make-parameter "/tmp/corpus.db"))

(provide make-test-corpus-from-sentences)
(define (make-test-corpus-from-sentences [sentences '("waka ja waka"
                                                      "Some thing"
                                                      "Some thing else")])
  (parameterize ([*db-file-name* "/tmp/test-corpus.db"])
    (make-corpus-from-sentences
     sentences
     #:nuke-existing? #t)))

(provide (rename-out [make-corpus-from-sentences make-corpus]))
(define/contract (make-corpus-from-sentences sentences
                                              #:limit [limit #f]
                                              #:nuke-existing? [nuke-existing? #f])
  ( ->* ((listof string?)) (#:limit boolean? #:nuke-existing? boolean?) corpus?)

  (when nuke-existing?
    (with-handlers ([exn:fail:filesystem? (lambda (e) void)])
      (delete-file (*db-file-name*))
      (fprintf (current-error-port) "Nuked ~s~%" (*db-file-name*))))
  (let ([conn (db:sqlite3-connect
               #:database (*db-file-name*)
               #:mode 'create)])
    (define c (corpus conn))

    (dprintf "Connected to database ~a; will create tables if necessary~%" (*db-file-name*))
    (query-exec
     (corpus-db c)
     "CREATE TABLE IF NOT EXISTS
        log(text TEXT)")
    (query-exec
     (corpus-db c)
     "CREATE TABLE IF NOT EXISTS
        log_word_map(word TEXT, log_id INTEGER)")

    (db:start-transaction (corpus-db c))

    (for ([s sentences])
      (add-sentence-to-corpus s c))
    (db:commit-transaction (corpus-db c))

    c))

(provide make-corpus-from-file)
(define (make-corpus-from-file ifn)
  (call-with-input-file ifn
    (lambda (ip)
      (make-corpus-from-sentences (in-lines ip)))))

(provide add-string-to-corpus)
(define/contract (add-string-to-corpus s c)
  (string? corpus? . -> . corpus?)

  (define (offensive? s)
    (regexp-match #px"\\bnigger\\b" s))

  (if (offensive? s)
      (log "Not adding offensive string to corpus")
      (add-sentence-to-corpus s c))
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
