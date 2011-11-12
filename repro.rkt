#lang racket

(require racket/sandbox)

(define evaluator (make-evaluator '(begin (require racket))))

(with-handlers ([exn:fail?
                 (lambda (e)
                   (displayln (exn-message e)))])
  (evaluator "(require ffi/unsafe) (require ffi/cvector) (make-cvector _int 1000000000)"))
(displayln "Keepin' on keepin' on")
