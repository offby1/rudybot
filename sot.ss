#lang scheme

(require (planet "numspell.ss" ("neil" "numspell.plt")))

(define (seconds->english secs)
  (let loop ((units secs)
             (divisors '((second . 60)
                         (minute . 60)
                         (hour   . 24)
                         (day    .  7)
                         (week   . 52)
                         (year   . 100)))
             (accum '()))

    (cond
     ((zero? units)
      accum)
     ((null? divisors)
      (cons `(century . ,units) accum))
     (else
      (let ((d (car divisors)))
        (let-values (((q r) (quotient/remainder units (cdr d))))
          (loop
           q
           (cdr divisors)
           (cons (cons (car d) r) accum))))))))

(define (number->english/plural n unit-name)

  (define (y->ie n unit-name)
    (cond
     ((equal? 1 n)
      unit-name)
     ((equal? #\y (string-ref unit-name (sub1 (string-length unit-name))))
      (string-append (substring unit-name 0 (- (string-length unit-name) 1))
                     "ie"))
     (else unit-name)))

  (string-append (number->english n)
                 " "
                 (y->ie n unit-name)
                 (if (equal? 1 n)
                     ""
                     "s")))

(define (seconds->approx secs)
  (let* ((result (seconds->english secs))
         (i (min 2 (length result))))
    (string-join
     (map (lambda (p)
            (number->english/plural
              (cdr p)
              (symbol->string (car p))))
          (take result i))
     ", ")))

(provide/contract
 [seconds->english (-> natural-number/c (listof (cons/c symbol? natural-number/c)))]
 [seconds->approx  (-> natural-number/c string?)])