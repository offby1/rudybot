;; -*-scheme-*-

;; Not a complete file, as you can see; meant to be included into
;; incubot.rkt.
(module+ test
  (require
   (prefix-in db: db)
   rackunit
   rackunit/text-ui)

  (define-binary-check (check-sets-equal? actual expected)
    (and (set-empty? (set-subtract actual expected))
         (set-empty? (set-subtract expected actual))))

  (define-test-suite string->lowercased-words-tests
    (check-sets-equal? (string->lowercased-words "...") (set))
    (check-sets-equal? (string->lowercased-words "Hey you!!") (set "hey" "you"))
    (check-sets-equal? (string->lowercased-words "HEY YOU!!") (set "hey" "you"))
    (check-sets-equal? (string->lowercased-words "YO MOMMA") (set "yo" "momma"))
    (check-sets-equal? (string->lowercased-words "Don't get tripped up by 'apostrophes'")
                       (set "don't" "get" "tripped" "up" "by" "apostrophes"))
    ;; apostrophes can really trip you up the most
    )

  (define-test-suite rarest-tests
    (let ([c (make-test-corpus-from-sentences)])
      (check-equal? (rarest (set "some" "else") c) "else")
      (check-equal? (rarest (set "some") c) "some")
      (check-false (rarest (set "ummagumma") c))))

  (define (word-popularity w c)
    (db:query-value (corpus-db c) "SELECT COUNT(log_id) FROM log_word_map WHERE word = ?" w))

  (define (corpus-word-count c)
    (db:query-value (corpus-db c) "SELECT COUNT(DISTINCT word) FROM log_word_map" ))

  (define-test-suite popularity-tests
    (check-equal? (word-popularity "frotz" (make-test-corpus-from-sentences)) 0)
    (check-equal? (word-popularity "else"  (make-test-corpus-from-sentences)) 1)

    ;; Note that if a word appears twice or more in a given sentence, we
    ;; only count it once.  No particular reason, except that this seems
    ;; like it will be easy.
    (check-equal? (word-popularity "waka"  (make-test-corpus-from-sentences)) 1)

    (check-equal? (word-popularity "some"  (make-test-corpus-from-sentences)) 2)
    (check-equal? (word-popularity "thing" (make-test-corpus-from-sentences)) 2)

    (let ([bigger (add-string-to-corpus "Pound cake" (make-test-corpus-from-sentences))])
      (check-equal? (word-popularity "frotz" bigger) 0)
      (check-equal? (word-popularity "else"  bigger) 1)
      (check-equal? (word-popularity "some"  bigger) 2)
      (check-equal? (word-popularity "thing" bigger) 2)
      (check-equal? (word-popularity "pound" bigger) 1)
      (check-equal? (word-popularity "cake"  bigger) 1)))

  (define-test-suite incubot-sentence-tests
    (let ([corpus (make-test-corpus-from-sentences)])
      (define (legitimate-response? thing)
        (or (not thing)
            (in-corpus? thing corpus)))
      (let* ([input-1 "For Phillip Morris ... from Western Union"]
             [output-1 (incubot-sentence input-1 corpus)]
             [input-2 "I have no words in common with input-1"]
             [output-2 (incubot-sentence input-2 corpus)])
        (check-not-false (legitimate-response? output-1) )
        (check-not-false (legitimate-response? output-2))

        ;; Since the two input sentences have nothing in common, we should
        ;; have come up with different outputs for each ... unless we
        ;; failed to come up with anything for either.
        (check-not-false (or (and (not output-1)
                                  (not output-2))
                             (not (equal? output-1 output-2))))

        (check-equal?
         (incubot-sentence "What else do you want?" (make-test-corpus-from-sentences))
         "Some thing else"))))

  (define-test-suite lets-tests
    (let ([corpus (make-test-corpus-from-sentences
                   '("Let's start with a capital letter"
                     "let's ignore case"
                     "LET'S SHOUT"))])
      (define (try input)
        (check-not-false (incubot-sentence input corpus) input))
      (try "let's")
      (try "Let's")
      (try "LET'S")
      (check-false (incubot-sentence "Snorgulation" corpus))))

  (define-test-suite censorship-tests
    (let* ([c (make-test-corpus-from-sentences)]
           [original-size (corpus-word-count c)])
      (set! c (add-string-to-corpus "This is an inoffensive sentence." c))
      (set! c (add-string-to-corpus "By dint of containing the nasty word 'nigger', this is an offensive sentence." c))
      (check-equal? (- (corpus-word-count c) original-size)
                    1)))

  (define-test-suite all-tests
    string->lowercased-words-tests
    rarest-tests
    incubot-sentence-tests
    lets-tests
    popularity-tests)

  (*incubot-logger* (lambda args (apply fprintf (current-error-port) args) (newline (current-error-port))))
  (run-tests all-tests 'verbose))
