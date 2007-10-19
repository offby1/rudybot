#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module quotes mzscheme
(require (only (planet "memoize.ss" ("dherman" "memoize.plt" )) define/memo*)
         "globals.ss"
         "vprintf.ss"
         "quote-of-the-day.ss")
(register-version-string "$Id$")

(provide one-quote
         random-choice)

;; no need to memoize this, but what the hell.
(define/memo* (all-quotes)
  (call-with-input-file (*quotes-file-name*) read))

;; during normal operation we want our bot to act randomly.  But when
;; we're testing it, we want it to act predictably.  Thus we have a
;; parameter called *random?* which determines which way it acts.

;; I know that I could start it with a known seed when I test, but for
;; reasons I can't articulate, that seems less pleasant than simply
;; having "rnd" always return 0.
(define (rnd . args)
  (if (not (*random?*))
      0
    (apply random args)))

(define (random-choice seq)
  (list-ref seq (rnd (length seq))))

(define (one-quote)
  (let try-again ()
    (let ((r  (rnd 100)))
      ;; we special-case zero for ease of testing.
      (cond ((zero? r)
             "I've laid out in my will that my heirs should continue working on my .emacs -- johnw")
            ((< r 91)
             ;; TODO: here's a cute idea -- if
             ;; requestor appears to be jordanb
             ;; himself, return something utterly
             ;; unlike the usual jordanb quote --
             ;; something saccharine and Hallmark-y
             (list-ref
              (all-quotes)
              (rnd (length (all-quotes)))))
            (else
             (with-handlers
                 ((exn:fail:network?
                   (lambda (e)
                     (vtprintf "Warning: quote-of-the-day yielded an exception: ~a~%"
                               (exn-message e))
                     (try-again))))
               (let ((p (random-choice (quotes-of-the-day))))
                 (string-append (car p)
                                "  --"
                                (cdr p)))))))))

)
