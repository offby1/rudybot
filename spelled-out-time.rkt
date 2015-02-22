#lang racket

(require (planet neil/numspell/numspell))

(module+ test (require rackunit rackunit/text-ui))

(define (seconds->english secs)
  (let loop ([units secs]
             [divisors '((second . 60)
                         (minute . 60)
                         (hour   . 24)
                         (day    .  7)
                         (week   . 52)
                         (year   . 100))]
             [accum '()])

    (cond
     [(zero? units)
      (if (null? accum)
          '((second . 0))
          accum)]
     [(null? divisors)
      (cons `(century . ,units) accum)]
     [else
      (let ([d (car divisors)])
        (let-values ([(q r) (quotient/remainder units (cdr d))])
          (loop
           q
           (cdr divisors)
           (cons (cons (car d) r) accum))))])))

(define (number->english/plural n unit-name)

  (define (y->ie n unit-name)
    (cond
     [(equal? 1 n)
      unit-name]
     [(equal? unit-name "century")
      "centurie"]
     [else unit-name]))

  (string-append (number->english n)
                 " "
                 (y->ie n unit-name)
                 (if (equal? 1 n)
                     ""
                     "s")))

(define (safe-take lst pos)
  (let ([pos (min pos (length lst))])
    (take lst pos)))

(define (spelled-out-time secs)
  (let* ([result (safe-take (seconds->english secs) 1)]
         [final (list (car result))]
         [final (if (and (< 1 (length result))
                         (zero? (cdr (second result))))
                     final
                     (append final (cdr result)))])
    (string-join
     (map (lambda (p)
            (number->english/plural
              (cdr p)
              (symbol->string (car p))))
          final)
     ", ")))

(module+ test
 (define-binary-check (check-spelled-out-time input-seconds expected-string)
   (equal? (spelled-out-time input-seconds)
           expected-string))

 (define spelled-out-time-tests

  (test-suite
   "spelled-out-time"
   (check-spelled-out-time 0 "zero seconds")
   (check-spelled-out-time 1 "one second")
   (check-spelled-out-time 2 "two seconds")
   (check-spelled-out-time 25 "twenty-five seconds")
   (check-spelled-out-time 123 "two minutes")
   (check-spelled-out-time 3611 "one hour")
   (check-spelled-out-time 75532 "twenty hours")
   (check-spelled-out-time 7229 "two hours")
   (check-spelled-out-time (+ 17 (* 24 3600)) "one day")
   (check-spelled-out-time (* 2 24 3600) "two days")
   (check-spelled-out-time (* 1 60 60 24 7 52 100)   "one century")
   (check-spelled-out-time (* 1 60 60 24 7 52 100 10)"ten centuries")))

 (run-tests spelled-out-time-tests 'verbose))

(provide/contract
 [seconds->english (-> natural-number/c (listof (cons/c symbol? natural-number/c)))]
 [spelled-out-time  (-> natural-number/c string?)])
