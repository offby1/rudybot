#lang racket
(require
 (prefix-in db: db)
 (only-in mzlib/etc this-expression-source-directory)
 )

(provide (except-out (struct-out corpus) corpus))
(struct corpus (db) #:transparent)

(define *leak?* #t)

(define (random-favoring-smaller-numbers k)
  (let (
        ;; 0 <= r < 1, but smaller numbers are more likely
        [r (/(sub1 (exp (random))) (sub1 (exp 1)))])

    (inexact->exact
     (truncate
      (* r k) ;; 0 <= this < k
      ))))

(define (log-sentence! db s)

  (db:query-exec
   db
   "insert into log values (?)"
   s))

(define (log-word! db w log-id)
  (void))

(define (id-of-newest-log db)
  (db:query-value db "SELECT MAX(rowid) FROM log"))

(define/contract (wordlist->wordset ws)
  ((listof string?) . -> . (set/c string?))
  (define (strip rx) (curryr (curry regexp-replace* rx) ""))
  (apply
   set
   (filter (compose positive? string-length)
           (map (compose
                 (strip #px"^'+")
                 (strip #px"'+$")
                 (strip #px"[^'[:alpha:]]+"))
                ws))))

(provide string->words)
(define/contract (string->words s)
  (string? . -> . set?)
  (wordlist->wordset (regexp-split #rx" " (string-downcase s))))

(provide add-sentence-to-corpus)
(define (add-sentence-to-corpus s c)
  (log-sentence! (corpus-db c) s)
  (let ([log-id (if *leak?* (id-of-newest-log (corpus-db c)) 99)])
    (for ([w (string->words s)])
      (log-word! (corpus-db c) w log-id))))

(define *db-file-name*
  (make-parameter
   (build-path (this-expression-source-directory)
               "corpus.db")))

(provide  make-corpus)
(define (make-corpus)

  (with-handlers ([exn:fail:filesystem? (lambda (e) void)])
    (delete-file (*db-file-name*))
    (fprintf (current-error-port) "Nuked ~s~%" (*db-file-name*)))

  (let ([conn (db:sqlite3-connect
               #:database (*db-file-name*)
               #:mode 'create
               #:busy-retry-limit 20)])
    (define c (corpus conn))

    (for ([command
           '(
             "CREATE TABLE IF NOT EXISTS log(text TEXT)"
             )])
      (db:query-exec (corpus-db c) command))

    c))
