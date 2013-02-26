#lang at-exp racket
(require
 (prefix-in db: db)
 unstable/debug
 (only-in mzlib/etc this-expression-source-directory)
 (only-in "utils.rkt" safely)
 )

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
  (not (null? (db:query-rows (corpus-db c) "SELECT word FROM log_word_map WHERE word = ? LIMIT 1" w))))

(provide corpus-sentence-count)
(define (corpus-sentence-count c)
  (let ([v (db:query-value
            (corpus-db c)
            "SELECT MAX(rowid) FROM log")])
    (if (db:sql-null? v)
        0
        v)))

(provide corpus-rank-by-popularity)
(define/contract (corpus-rank-by-popularity c wordset)
  (corpus? (set/c string?) . -> . (listof (vector/c string? natural-number/c)))
  (apply db:query-rows
         (corpus-db c)
         (format
          "SELECT word, occurrences FROM word_popularity WHERE WORD IN (~a) ORDER BY occurrences ASC;"
          (string-join (build-list (set-count wordset) (const "?")) ","))
         (set-map wordset values)))

;; favor longer sentences over shorter ones.
(provide random-choose)
(define/contract (random-choose seq)
  (-> (listof string?) any/c)
  (let ([sorted (sort seq > #:key string-length)])
    (list-ref
     sorted
     (random-favoring-smaller-numbers (length seq)))))

;; TODO -- this feels inefficient, since we are sucking 100 rows from
;; the db, and then discarding all but one.  See if it's worth doing
;; this differently.

;; At the least, we might split this into two pieces: one piece
;; retrieves a row from log_word_map at random, then the second piece
;; retrieves the corresponding row from log.  At least that way we're
;; not pulling out a pile of long sentences, but rather a pile of
;; word-ID pairs, which are presumably smaller.  Of course that way we
;; wouldn't be able to make our selection based on the sentence's
;; length.
(provide random-choose-string-containing-word)
(define/contract (random-choose-string-containing-word rare c)
  (string? corpus? . -> . (or/c string? #f))
  (let
      ([candidates
        (db:query-rows
         (corpus-db c)
         ;; The AND part of the WHERE clause excludes utterances where
         ;; the word in question is at the start, and is followed by a
         ;; colon.  That's because our caller will throw such a
         ;; leading thing away, believing it to be someone's nick, and
         ;; hence not suitable for display as a witticism.

         ;; For example, if 'rare' is 'offby1', and there is an entry
         ;; in the log like ``offby1: your bot rocks my world'', this
         ;; query will skip that entry.

         ;; Note that this is a bit too strict -- it will also
         ;; exclude, e.g., ``offby1 has two hands: a left and a
         ;; right'' because the LIKE clause will "see" ``offby1''
         ;; followed by a colon.  Without proper regexp support built
         ;; into the database, I don't see an easy way around this.
         @string-append{
                        SELECT text
                        FROM   log
                        JOIN   log_word_map
                        ON     log.rowid = log_word_map.log_id
                        WHERE  log_word_map.word = ?
                        AND    text NOT LIKE ?
                        LIMIT  100
                        }
         rare
         (string-append rare "%:%"))])
  (and (not (null? candidates))
       (random-choose (map (curryr vector-ref 0) candidates)))))

(define (id-of-newest-log db)
  (db:query-value db "SELECT MAX(rowid) FROM log"))

(define (log-sentence! db s)
  (safely
   (db:query-exec
    db
    "insert into log values (?)"
    s)))

(define/contract (log-word! db w log-id)
  (db:connection? string? integer? . -> . any)
  (safely
   (db:call-with-transaction
    db
    (thunk
     (db:query-exec
      db
      "INSERT INTO log_word_map VALUES (?, ?)"
      w log-id)
     (db:query-exec
      db
      "insert or ignore into word_popularity  values (?, 0) "
      w)
     (db:query-exec
      db
      "UPDATE word_popularity SET occurrences = occurrences + 1 WHERE word = ?"
      w)))))

(provide add-sentence-to-corpus)
(define (add-sentence-to-corpus s c)
  (log-sentence! (corpus-db c) s)
  (let ([log-id (id-of-newest-log (corpus-db c))])
    (for ([w (string->lowercased-words s)])
      (log-word! (corpus-db c) w log-id))))

(define *db-file-name*
  (make-parameter
   (build-path (this-expression-source-directory)
               "corpus.db")))

(provide make-test-corpus-from-sentences)
(define (make-test-corpus-from-sentences [sentences '("waka ja waka"
                                                      "Some thing"
                                                      "Some thing else")])
  (parameterize ([*db-file-name* "/tmp/test-corpus.db"])
    (make-corpus-from-sentences
     sentences
     #:nuke-existing? #t
     #:create-tables? #t)))

(provide (rename-out [make-corpus-from-sentences make-corpus]))
(define/contract (make-corpus-from-sentences sentences
                                              #:limit [limit #f]
                                              #:create-tables? [create-tables? #f]
                                              #:nuke-existing? [nuke-existing? #f])
  ( ->* ((listof string?))
        (#:limit boolean? #:nuke-existing? boolean? #:create-tables? boolean?)
        corpus?)

  (when nuke-existing?
    (with-handlers ([exn:fail:filesystem? (lambda (e) void)])
      (delete-file (*db-file-name*))
      (fprintf (current-error-port) "Nuked ~s~%" (*db-file-name*))))
  (let ([conn (db:sqlite3-connect
               #:database (*db-file-name*)
               #:mode 'create
               #:busy-retry-limit 20)])
    (define c (corpus conn))

    (dprintf "Connected to database ~a; ~a create tables~%"
             (*db-file-name*)
             (if create-tables? "will" "will not"))

    (when create-tables?
      (for ([command
             '(
               "CREATE TABLE IF NOT EXISTS log(text TEXT)"
               "CREATE TABLE IF NOT EXISTS log_word_map(word TEXT, log_id INTEGER)"
               "CREATE INDEX IF NOT EXISTS idx1 ON log_word_map(word)"

               "CREATE TABLE IF NOT EXISTS word_popularity (word TEXT PRIMARY KEY, occurrences INTEGER)"

               ;; to backfill word_popularity:

               ;; INSERT INTO word_popularity(word, occurrences)
               ;;      SELECT word, count(word)
               ;;        FROM log_word_map
               ;;    GROUP BY word ;
               )])
        (db:query-exec (corpus-db c) command)))

    (db:start-transaction (corpus-db c))

    (for ([s sentences])
      (add-sentence-to-corpus s c))
    (safely
     (db:commit-transaction (corpus-db c)))

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

  (when (not (offensive? s))
    (add-sentence-to-corpus s c))
  c)

(provide string->lowercased-words)
(define/contract (string->lowercased-words s)
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

                 ;; Nix leading single-quotes.
                 (strip #px"^'+")

                 ;; Nix trailing single-quotes.
                 (strip #px"'+$")

                 ;; keep only single-quotes and letters.
                 (strip #px"[^'[:alpha:]]+")
                 )
                ws))))
