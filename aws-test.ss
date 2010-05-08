#lang scheme

;; http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/index.html?SDB_API_CreateDomain.html

;; create a simple DB "domain" to hold log records.

;; send a few records to that domain.

;; retrieve last few records from that domain.

(require "ssl-ports.ss"
         (only-in "aws-common.ss" rfc-2822-date make-GET-POST-PUT))
(define-values (GET POST PUT) (make-GET-POST-PUT "sdb.amazonaws.com" #t))

(printf "Thar she blows: ~s~%" (GET ""))
