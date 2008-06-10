#lang scheme

(let loop ((units 12345000001)
           (divisors '((seconds . 60)
                       (minutes . 60)
                       (hours   . 24)
                       (days    .  7)
                       (weeks   . 52)
                       (years   . 100))))

  (cond
   ((zero? units)
    (printf "That's all, folks~%"))
   ((null? divisors)
    (printf "~a centuries~%" units))
   (else
    (let ((d (car divisors)))
      (let-values (((q r) (quotient/remainder units (cdr d))))
        (printf "~a ~a~%" r (car d))
        (loop q (cdr divisors)))))))

