;; Not a complete file, as you can see; meant to be included into
;; incubot.rkt.
(require (planet schematics/schemeunit:3:4)
         (planet schematics/schemeunit:3/text-ui))

(define-binary-check (check-sets-equal? actual expected)
  (and (set-empty? (set-subtract actual expected))
       (set-empty? (set-subtract expected actual))))

(define-test-suite string->words-tests
  (check-sets-equal? (string->words "...") (set))
  (check-sets-equal? (string->words "Hey you!!") (set "hey" "you"))
  (check-sets-equal? (string->words "HEY YOU!!") (set "hey" "you"))
  (check-sets-equal? (string->words "YO MOMMA") (set "yo" "momma"))
  (check-sets-equal? (string->words "Don't get tripped up by 'apostrophes'")
                     (set "don't" "get" "tripped" "up" "by" "apostrophes"))
  ;; apostrophes can really trip you up the most
  )

(provide make-test-corpus)
(define (make-test-corpus)
  (public-make-corpus
   "waka ja waka"
   "Some thing"
   "Some thing else"))


(define-test-suite rarest-tests
  (let ([c (make-test-corpus)])
    (check-equal? (rarest (set "some" "else") c) "else")
    (check-equal? (rarest (set "some") c) "some")
    (check-false (rarest (set "ummagumma") c))))



(define-test-suite popularity-tests
  (check-equal? (word-popularity "frotz" (make-test-corpus)) 0)
  (check-equal? (word-popularity "else"  (make-test-corpus)) 1)

  ;; Note that if a word appears twice or more in a given sentence, we
  ;; only count it once.  No particular reason, except that this seems
  ;; like it will be easy.
  (check-equal? (word-popularity "waka"  (make-test-corpus)) 1)

  (check-equal? (word-popularity "some"  (make-test-corpus)) 2)
  (check-equal? (word-popularity "thing" (make-test-corpus)) 2)

  (let ([bigger (add-to-corpus "Pound cake" (make-test-corpus))])
    (check-equal? (word-popularity "frotz" bigger) 0)
    (check-equal? (word-popularity "else"  bigger) 1)
    (check-equal? (word-popularity "some"  bigger) 2)
    (check-equal? (word-popularity "thing" bigger) 2)
    (check-equal? (word-popularity "pound" bigger) 1)
    (check-equal? (word-popularity "cake"  bigger) 1)))

(define-test-suite incubot-sentence-tests
  (let ([corpus (make-test-corpus)])
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
       (incubot-sentence
        "What else do you want?"
        (make-test-corpus))
       "Some thing else"))))

(define-test-suite lets-tests
  (let ([corpus (public-make-corpus
                 "Let's start with a capital letter"
                 "let's ignore case"
                 "LET'S SHOUT")])
    (define (try input)
      (check-not-false (incubot-sentence input corpus)))
    (try "let's")
    (try "Let's")
    (try "LET'S")
    (check-false (incubot-sentence "Snorgulation" corpus))))

(define-test-suite censorship-tests
  (let* ([c (make-test-corpus)]
         [original-size (set-count (corpus-strings c))])
    (set! c (add-to-corpus "This is an inoffensive sentence." c))
    (set! c (add-to-corpus "By dint of containing the nasty word 'nigger', this is an offensive sentence." c))
    (check-equal? (- (set-count (corpus-strings c)) original-size)
                  1)))

(define-test-suite all-tests
  string->words-tests
  rarest-tests
  incubot-sentence-tests
  lets-tests
  popularity-tests
  )


(define (main . args)
  (*incubot-logger* (lambda args (apply fprintf (current-error-port) args) (newline (current-error-port))))
  (let ([status (run-tests
                 all-tests
                 'verbose)])
    (when (positive? status)
      (exit 1))

    (with-handlers ([exn:fail:filesystem?
                     (lambda (e)
                       (printf "Skipping some tests because ~s~%" (exn-message e)))])
      (let ([c (time
                (call-with-input-file
                    ;; biggest .txt file I could find already on my box
                    "parsed-log"
                  (lambda (inp)
                    (make-corpus-from-sexps inp 1000))))])
        (for ([inp (in-list (list
                             "Oh shit"
                             "Oops, ate too much cookie dough"
                             "It's almost inconceivable that none of these words appears in that manual"
                             "I'm impressed that I can find stuff already."))])
          (printf "~a => ~a~%" inp (incubot-sentence inp  c)))
        (for ([inp (in-list (list
                             (list "whOa" "nellie")
                             (list "Oops" "ate" "too" "much" "cookie" "dough")))])
          (printf "~a => ~a~%" inp (incubot-sentence inp  c)))))))

(provide main)
