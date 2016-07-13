#lang racket

(require (only-in net/url
                  call/input-url
                  make-url
                  make-path/param
                  get-pure-port)
         (only-in json read-json))
(provide google-search)
(module+ test (require rackunit rackunit/text-ui))

;; Control the search API at https://cse.google.com/manage/all

;; Do a simple Google search.  Returns a nested hash table full of
;; symbols and strings and whatnot.
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

      ;; You can try out the search engine by hand with your browser
      ;; at
      ;; https://cse.google.com/cse/publicurl?cx=012774403818167417067:n89ppe2mrss
      [cx . "012774403818167417067:n89ppe2mrss"]

      [q . ,text]
      [num . "1"]
      )
    #f ;; fragment
    )
   get-pure-port
   read-json))

;; Given a string, return either a cons of "title" and "link", or else
;; return #f.  Handles the case where Google assumes our search query
;; contains misspellings, and hence offers a spelling suggestion.
(define/contract  (google-search search-string [search search])
  (->* (string?) (procedure?) (or/c false/c cons?))
  (let ((blob (search search-string)))
    (let loop ((items (hash-ref blob 'items #f))
               (corrected #f))
      (if items
          (let ((ht (list-ref items 0)))
            (cons (hash-ref ht 'title)
                  (hash-ref ht 'link)))
          (and (not corrected)
               (let ((spelling  (hash-ref blob 'spelling #f)))
                 (and spelling
                      (let ((correction (hash-ref spelling 'correctedQuery)))
                        (loop (hash-ref (search correction) 'items #f)
                              #t)))))
          ))))

(module+ test

  (define/contract (simulated-search text)
    (string? . -> . hash?)
    (if (string=? text "corrected")
        '#hasheq((items . (#hasheq( (link . "https://correctamundo") (title . "Correctamundo!")))))
        '#hasheq((spelling . #hasheq((correctedQuery . "corrected"))))))

  (define (hopeless-search text)
    '#hasheq((spelling . #hasheq((correctedQuery . "you cannot spell")))))

  (define-test-suite search-tests
    (let ((result (list-ref (hash-ref (search "\"with-temp-buffer\"") 'items) 0)))
      ;; Obviously these tests aren't reliable, as search results
      ;; change over time.
      (check-equal? (hash-ref result 'title) "GNU Emacs Lisp Reference Manual: Current Buffer")
      (check-equal? (hash-ref result 'link) "https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html"))

    (check-equal?
     (google-search "uups I done spelt it rong" simulated-search)
     (cons "Correctamundo!" "https://correctamundo"))

    (check-false (google-search "doesn't matter" hopeless-search)))

  (run-tests search-tests 'verbose))
