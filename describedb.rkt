#lang racket
(require db
         (only-in racket/async-channel make-async-channel async-channel-get))

(define (connect-to-db)
  (let ([conn (sqlite3-connect #:database "local.db" #:mode 'create)])
    (query-exec conn "CREATE TABLE IF NOT EXISTS dtable (id INTEGER PRIMARY KEY AUTOINCREMENT, term VARCHAR(128) NOT NULL, descr TEXT);")
    conn))

(define (add-definition conn term descr)
  (query-exec conn "INSERT INTO dtable VALUES (NULL, ?, ?);" term descr))

(define (del-defintion conn term [entry-id 0])
  (query-exec conn  "DELETE FROM dtable WHERE id=?" entry-id))

(define (get-def conn term)
  (query-rows conn "SELECT id, descr FROM dtable WHERE term=?" term))

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
            [(list 'get term)          (get-def conn term)]))

        (loop)))))

  (lambda args
    (define client-channel (make-async-channel 1))
    (thread-send server-thread (list client-channel args))
    (async-channel-get client-channel)))

(module+ test
  (let ([conn (connect-to-db)])
    (add-definition conn "cat" "kitty")
    (printf "This had better be 'kitty':        ~s~%" (get-def conn "cat"))
    (del-defintion conn "cat")
    (printf "This had better be fewer 'kitty's: ~s~%" (get-def conn "cat"))
    ))
