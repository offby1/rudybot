#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module quotes mzscheme
(require (only (planet "memoize.ss" ("dherman" "memoize.plt" )) define/memo*)
         "globals.ss"
         "quote-of-the-day.ss"
         "shuffle.ss"
         "vprintf.ss")

(register-version-string "$Id$")

(provide one-quote
         random-choice)

;; Strictly speaking, there's no need to memoize this, but it's
;; painful to imagine us hitting the disk every time someone asks for
;; a quote.  (Never mind that a decent file system will probably cache
;; the file anyway ...)
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

;; rather than just choosing an element of a list at random, we'll
;; shuffle the list the first time we're called, and then we'll just
;; return subsequent elements, reshuffling when needed.  This makes it
;; less likely that we'll return the same thing twice in a row.
(define random-choice
  (let ((the-lists (make-hash-table 'equal)))
    (lambda (seq)
      (let ((this-list (hash-table-get the-lists seq '())))

        (when (null? this-list)
          (vtprintf "Shuffling a list whose first element is ~s~%"
                    (car seq))
          (set! this-list (shuffle-list seq))
          (hash-table-put! the-lists seq this-list))

        (begin0
            (car this-list)
          (hash-table-put! the-lists seq (cdr this-list)))))))

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
