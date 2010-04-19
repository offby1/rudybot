#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require schemeunit schemeunit/text-ui
         scheme/set
         mzlib/trace
         (only-in "vars.ss" *incubot-logger*))

(define-struct corpus (strings strings-by-word) #:transparent)

(define/contract (random-choose seq)
  (-> list? any/c)
  (list-ref seq (random (length seq))))

(define/contract (strings-containing-word w c)
  (-> string? corpus? (listof string?))
  (hash-ref (corpus-strings-by-word c) w))

(define (log fmt . args)
  (when (*incubot-logger*)
    (apply fprintf (current-output-port) fmt args)
    (apply (*incubot-logger*) fmt args)))

(provide  incubot-sentence)
(define incubot-sentence
  (match-lambda*
   [(list (? string? s) (? corpus? c))
    (incubot-sentence (string->words s) c)]
   [(list (? set? ws) (? corpus? c))
    (let ([rare (rarest ws c)])
      (log "Looking for witticism for word ~s" rare)
      (and rare
           (random-choose (strings-containing-word rare c))))]))

(define/contract (in-corpus? s c)
  (string? corpus? . -> . boolean?)
  (set-member? (corpus-strings c) s))

(define (hash-increment h key)
  (hash-set h key (add1 (hash-ref h key 0))))
(define (hash-append h key value)
  (hash-set h key (cons value (hash-ref h key '()))))

(provide public-make-corpus)
(define/contract (public-make-corpus . sentences)
  (->* () () #:rest (listof string?) corpus?)
  (for/fold ([c (make-corpus
                 (set)
                 (make-immutable-hash '()))])
      ([s (in-list sentences)])
      (add-to-corpus s c)))

(provide add-to-corpus)
(define/contract (add-to-corpus s c)
  (-> string? corpus? corpus?)
  (log "Adding ~s to incubot corpus" s)
  (make-corpus
   (set-add (corpus-strings c) s)
   (for/fold ([h (corpus-strings-by-word c)])
       ([w (in-set (string->words s))])
       (hash-append h w s))))

(define (legitimate-response? thing corpus)
  (or (not thing)
      (in-corpus? thing corpus)))

(define/contract (string->words s)
  (string? . -> . set?) ;; it'd be nice if I could say "a set whose
  ;; elements are all strings"
  (define (strip rx) (curryr (curry regexp-replace* rx) ""))
  (apply set
         (filter (compose positive? string-length)
                 (map (compose
                       (strip #px"^'+")
                       (strip #px"'+$")
                       (strip #px"[^'[:alpha:]]+"))
                      (regexp-split #rx" " (string-downcase s))))))

(define-binary-check (check-sets-equal? actual expected)
  (and (set-empty? (set-subtract actual expected))
       (set-empty? (set-subtract expected actual))))

(define-test-suite string->words-tests
  (check-sets-equal? (string->words "...") (set))
  (check-sets-equal? (string->words "Hey you!!") (set "hey" "you"))
  (check-sets-equal? (string->words "YO MOMMA") (set "yo" "momma"))
  (check-sets-equal? (string->words "Don't get tripped up by 'apostrophes'")
                     (set "don't" "get" "tripped" "up" "by" "apostrophes")))

(define/contract (word-popularity w c)
  (string? corpus? . -> . natural-number/c)
  (length (hash-ref (corpus-strings-by-word c) w '())))

(define (make-test-corpus)
  (public-make-corpus
   "waka ja waka"
   "Some thing"
   "Some thing else"))

(define/contract (rarest ws c)
  (-> set? corpus? (or/c string? #f))
  (let ([result (foldl (lambda (w accum)
                         (let ([p (word-popularity w c)])
                           (cond
                            ((positive? p)
                             (cond
                              ((not accum)
                               (cons w p))
                              ((< p (cdr accum))
                               (cons w p))
                              (else
                               accum)))
                            (else
                             accum))))
                       #f
                       (set-map ws values))])
    (and result
         (car result))))

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
    (let* ([input-1 "For Phillip Morris ... from Western Union"]
           [output-1 (incubot-sentence input-1 corpus)]
           [input-2 "I have no words in common with input-1"]
           [output-2 (incubot-sentence input-2 corpus)])
    (check-not-false (legitimate-response? output-1 corpus) )
    (check-not-false (legitimate-response? output-2 corpus))

    ;; Since the two input sentences have nothing in common, we should
    ;; have come up with different outputs for each ... unless we
    ;; failed to come up with anything for either.
    (check-not-false (or (and (not output-1)
                              (not output-2))
                         (not (equal? output-1 output-2))))

    (check-equal? (incubot-sentence "What else do you want?" (make-test-corpus))
                  "Some thing else")
                                    )))

(define-test-suite all-tests
  string->words-tests
  rarest-tests
  incubot-sentence-tests
  popularity-tests
  )

(define (main . args)
  (let ([status (run-tests all-tests 'verbose)])
    (when (positive? status)
      (exit 1))
    (let ([c (time
              (call-with-input-file
                  ;; biggest .txt file I could find already on my box
                  ;; from the "ipython" package
                  "/tmp/davinci.txt"
                (lambda (inp)
                  (for/fold ([c (public-make-corpus)])
                      ([line (in-lines inp)])
                      (add-to-corpus line c)))))])
      (for ([inp (in-list (list
                           "Oh shit"
                           "Oops, ate too much cookie dough"
                           "It's almost inconceivable that none of these words appears in that manual"
                           "I'm impressed that I can find stuff already."))])
        (printf "~a => ~a~%" inp (incubot-sentence inp  c))))))

(provide main)
