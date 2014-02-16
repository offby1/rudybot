#lang racket
(require db)

(define (connect-to-db)
  (let ([conn (sqlite3-connect #:database "local.db" #:mode 'create)])
    (create-db conn)
    (if conn conn #f)))

(define (create-db conn)
  (query-exec conn "create table if not exists dtable (id integer PRIMARY KEY AUTOINCREMENT, did varchar(128) not null, descr text);")  #t)

(define (purge-db conn)
  (query-exec conn "drop table dtable"))

(define (add-definition conn did descr)
  (let ([q (format "insert into dtable values (NULL, '~a', '~a');" did descr)])
    (query-exec conn q)))

(define (do-create)
  (let ([c (connect-to-db)])
    (create-db c)))

(define (do-query text)
  (let ([c (connect-to-db)])
    (query-rows c text)))

(define (get-def conn did)
  (query-rows conn (format "select id, descr from dtable where did='~a'" did)))

(define (get-def-count conn did)
  (car (query-list conn (format "select count(did) from dtable where did='~a'" did))))

(define (do-del-defintion conn id)
  (let ([q (format "delete from dtable where id=~a;" id)])
    (query-exec conn q)))

(define (del-defintion conn did [entry-id 0])
  (let ([defs (get-def conn did)])
    (when (>= entry-id (length defs))
      (error (format "`~a'[~a] is not found" did entry-id)))

    (let ([def-id (vector-ref (list-ref defs entry-id) 0)])
      (do-del-defintion conn def-id))))

(define defc (connect-to-db))

(define (defc-add-def did descr) 
  (add-definition defc did descr))

(define (defc-get-def did) 
  (get-def defc did))

(define (defc-kill-db)
  (purge-db defc))

(define (defc-get-def-count did)
  (get-def-count defc did))

(define (defc-del-definition did [entry-id 0])
  (del-defintion defc did entry-id))

(provide defc-add-def defc-get-def defc-kill-db defc-get-def-count defc-del-definition)

