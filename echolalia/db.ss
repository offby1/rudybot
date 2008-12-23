#lang scheme

(define-struct db (stuff) #:prefab)
(provide (struct-out db))

(provide/contract [lookup [string? db? . -> . (or/c string? false/c)]])
(define (lookup word db)
   (hash-ref (db-stuff db) word #f))
