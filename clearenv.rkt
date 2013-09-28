#lang racket

(require ffi/unsafe)
(provide clearenv)

(module+ test (require rackunit rackunit/text-ui))

;; The 'clearenv' function doesn't exist on some systems (notably Mac
;; OS X), so we use 'unsetenv' in a loop instead.
(define (clearenv)
  (let ([unsetenv (get-ffi-obj 'unsetenv #f (_fun _bytes -> _int))])
    (let loop ()
      (match
          ;; I've seen this ptr-ref fail, too (on Jon Schuster's
          ;; Macbook); no idea why.
          (ptr-ref (get-ffi-obj 'environ #f _pointer) _bytes)
        [(regexp #rx"^(.*?)=(.*)$" (list _ k v))
         (unsetenv k)
         (loop)]
        [#f (void)]))))

(module+ test
  (define hmm-tests
   (test-suite
    "loop"
    (test-case
     "dunno"
     (clearenv)
     (for ([v '("FOO" "HOME" "PATH" "EDITOR" "SNICKERDOODLE")])
       (check-false (getenv v) v)))))
  (run-tests hmm-tests))
