#lang racket

(require (only-in net/url
                  call/input-url
                  make-url
                  make-path/param
                  get-pure-port)
         (only-in json read-json))
(provide search)
(module+ test (require rackunit rackunit/text-ui))

;; Control the search API at https://cse.google.com/manage/all

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
      [cx . "012774403818167417067:n89ppe2mrss"]
      [q . ,text]
      [num . "1"]
      )
    #f ;; fragment
    )
   get-pure-port
   read-json))

(module+ test
  (define-test-suite search-tests
    (let ((result (list-ref (hash-ref (search "\"with-temp-buffer\"") 'items) 0)))
      ;; Obviously these tests aren't reliable, as search results
      ;; change over time.
      (check-equal? (hash-ref result 'title) "GNU Emacs Lisp Reference Manual: Current Buffer")
      (check-equal? (hash-ref result 'link) "https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html")))
  (run-tests search-tests 'verbose))
