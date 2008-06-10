#lang scheme

(require (planet "numspell.ss" ("neil" "numspell.plt"))
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

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

(define (spelled-out-time secs)
  (let* ((result (seconds->english secs))
         (final (list (car result)))
         (final (if (and (< 1 (length result))
                         (zero? (cdr (second result))))
                     final
                     (append final (cdr result)))))
    (string-join
     (map (lambda (p)
            (number->english/plural
              (cdr p)
              (symbol->string (car p))))
          final)
     ", ")))


(define spelled-out-time-tests

  (test-suite
   "spelled-out-time"
   (test-equal? "one second"          (spelled-out-time 1) "one second")
   (test-equal? "two seconds"         (spelled-out-time 2) "two seconds")
   (test-equal? "twenty-five seconds" (spelled-out-time 25) "twenty-five seconds")
   (test-equal? "two minutes, three seconds" (spelled-out-time 123) "two minutes, three seconds")
   (test-equal? "one hour"            (spelled-out-time 3611) "one hour")
   (test-equal? "two hours"           (spelled-out-time 7229) "two hours")
   (test-equal? "one day"             (spelled-out-time (+ 17 (* 24 3600))) "one day")))

(define (main . args)
  (exit (test/text-ui spelled-out-time-tests 'verbose)))

(provide/contract
 [seconds->english (-> natural-number/c (listof (cons/c symbol? natural-number/c)))]
 [spelled-out-time  (-> natural-number/c string?)])
(provide main)
