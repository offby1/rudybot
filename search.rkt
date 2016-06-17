#lang racket

(require (only-in net/url
                  call/input-url
                  make-url
                  make-path/param
                  get-pure-port)
         (only-in json read-json))
(provide search)
(module+ test (require rackunit rackunit/text-ui))

;; Do a simple Google search.
(define (search text)

  (call/input-url
   (make-url
    "https"               ;; scheme
    #f                    ;; user
    "www.googleapis.com"  ;; host
    #f                    ;; port
    #t                    ;; path-absolute?

    ;; path
    (map ((curryr make-path/param) '()) (list "customsearch" "v1"))

    ;; query
    `(
      [key . ,(bytes->string/utf-8 (get-preference 'google-API-key))]
      [cx . "012774403818167417067:yduodfpvsvw"]
      [q . ,text]
      [num . "1"]
      )
    #f ;; fragment
    )
   get-pure-port
   read-json))

(module+ test
  (define-test-suite search-tests
    (let ((result (list-ref (hash-ref (search "cat pictures") 'items) 0)))
      ;; Obviously these tests aren't reliable, as search results
      ;; change over time.
      (check-equal? "Silly Cat Pictures -Adora Cats - Android Apps on Google Play" (hash-ref result 'title))
      (check-equal? "https://play.google.com/store/apps/details?id=adora.cats" (hash-ref result 'link))))
  (run-tests search-tests 'verbose))
