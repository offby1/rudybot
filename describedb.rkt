#lang at-exp racket
(require db
         (only-in racket/async-channel make-async-channel async-channel-get))

(define (connect-to-db [name "definitions.db"])
  (let ([conn (sqlite3-connect #:database name #:mode 'create)])
    (query-exec conn "CREATE TABLE IF NOT EXISTS dtable (term VARCHAR(128) NOT NULL, descr TEXT)")
    conn))

(define (add-definition conn term descr)
  (query-exec conn "INSERT INTO dtable VALUES (?, ?);" term descr))

(define (del-defintion conn term [entry-id 0])
  (call-with-transaction
   conn
   (thunk
    (let* ([all-definitions
            (query-rows
             conn
             @string-append{
                            SELECT   rowid, descr
                            FROM     dtable
                            WHERE    term=?
                            ORDER BY rowid ASC
                            LIMIT    ?
                            }
             term (add1 entry-id))]
           [target (last all-definitions)])
      (query-exec conn  "DELETE FROM dtable WHERE rowid=?" (vector-ref target 0))))))

(define (get-defs conn term)
  (query-rows conn "SELECT descr FROM dtable WHERE term=? ORDER BY rowid ASC" term))

(define (make-definitions-server)
  (define server-thread
    (thread
     (thunk
      (define conn (connect-to-db))
      (let loop ()
        (match-let ([(list channel args) (thread-receive)])
          (match args
            [(list 'add term descr)    (add-definition conn term descr)]
            [(list 'del term)          (del-defintion conn term)]
            [(list 'del term entry-id) (del-defintion conn term entry-id)]
            [(list 'get term)          (get-defs conn term)]))

        (loop)))))

  (lambda args
    (define client-channel (make-async-channel 1))
    (thread-send server-thread (list client-channel args))
    (async-channel-get client-channel)))

(module+ test
  (let ([conn (connect-to-db "describe-test.db")])
    (call-with-transaction
     conn
     (thunk
      (define original-count (length (get-defs conn "cat")))
      (add-definition conn "cat" "kitty")
      (printf "This had better be 1: ~s~%" (- (length (get-defs conn "cat")) original-count))
      (del-defintion conn "cat")
      (printf "This had better be 0: ~s~%" (- (length (get-defs conn "cat")) original-count))))
    ))
