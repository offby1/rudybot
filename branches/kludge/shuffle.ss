;; Hey Emacs, this is -*-scheme-*- code!
;; $Id$

(module shuffle mzscheme
(require (planet "foof-loop.ss" ("offby1" "foof-loop.plt")))

(define (shuffle-vector! vector)
    (loop ((for element i (in-vector vector)))
      (let ((j (random (+ i 1))))
        (vector-set! vector i (vector-ref vector j))
        (vector-set! vector j element))))

(define (shuffle-list seq)
  (let ((v (list->vector seq)))
    (shuffle-vector! v)
    (vector->list v)))

(provide shuffle-list)
)
