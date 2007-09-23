#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui headline-tests 'verbose))"
|#
(module headline mzscheme
(require (only (lib "file.ss")
               get-preference
               put-preferences)
         (lib "trace.ss")
         (only (lib "19.ss" "srfi")
               current-time
               make-time
               subtract-duration
               time-duration
               time-second
               time-utc
               time>=?
               time=?
               time?)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "assert.ss"  ("offby1"     "offby1.plt"))
         (only (planet "memoize.ss" ("dherman" "memoize.plt" )) define/memo*)
         "globals.ss"
         "tinyurl.ss"
         "vprintf.ss")

(register-version-string "$Id$")

;; this is what our queue returns.
(define-struct entry (timestamp title link) (make-inspector))
(define (public-make-entry time title link)
  (check-type 'make-entry time? time)
  (check-type 'make-entry string? title)
  (check-type 'make-entry string? link)
  (make-entry time title link))

;; This is memoized so that we don't hit tinyurl.com more than we have to.
(define/memo* (maybe-make-URL-tiny e)
  (let ((new-entry (apply make-entry (cdr (vector->list (struct->vector e))))))
    (let ((original-url (entry-link new-entry)))
      (when (< (*tinyurl-url-length-threshold*) (string-length original-url))
          (set-entry-link! new-entry (make-tiny-url original-url)))
      new-entry)))

(define *prefs-file-semaphore* (make-semaphore 1))

;; strictly for testing
(define (reset-prefs-file-semaphore!)
   (set! *prefs-file-semaphore* (make-semaphore 1)))

(define (reliably-put-pref value)
  (let retry ()
    (put-preferences
     (list (*atom-timestamp-preference-name*))
     (list value)
     (lambda (lockpath)
       (vtprintf "preference file is locked (~s); retrying~%" lockpath)
       (sleep (/ (add1 (random 10)) 10))
       (retry)))))

;; TODO -- note-spewed! and already-spewed? should take the name of an
;; IRC server, so that we can keep track of what we've spewed
;; per-server, as opposed to globally.
(define (note-spewed! e)
  (assert (not (already-spewed? e)))

  (let ((last-spewed (get-preference (*atom-timestamp-preference-name*)))
        (title-link (list (entry-title e)
                          (entry-link e))))

    ;; we've never noted anything before, so save this entry.
    (if (not last-spewed)
        (reliably-put-pref (list (time-second (entry-timestamp e)) title-link))

      (begin

        ;; what we're noting has the same timestamp as what we'd
        ;; previously noted, so append the title-link info to what's
        ;; already there.
        (if (equal? (time-second (entry-timestamp e))
                    (car last-spewed))
            (begin
              (assert (not (member title-link last-spewed)))
              (reliably-put-pref (append last-spewed (list title-link))))
          ;; what we're noting is newer than what we'd previously noted,
          ;; so just clobber what's there.
          (reliably-put-pref (list (time-second (entry-timestamp e)) title-link))))
      )
    ))
;(trace note-spewed!)

(define (already-spewed? e)
  (let ((e-secs (time-second (entry-timestamp e)))
        (last-spewed (get-preference (*atom-timestamp-preference-name*)))
        (title-link (list (entry-title e)
                          (entry-link e))))

    (if (not last-spewed)
        #f
      (if (equal? e-secs (car last-spewed))
           (member title-link last-spewed)
        (< e-secs (car last-spewed))))))
;(trace already-spewed?)

(define headline-tests

  (let ((short (make-entry 'irrelevant "irrelevant" "http://short.com"))
        (long  (make-entry 'irrelevant "irrelevant" long-url)))
    (test-suite
     "headline"
     (test-case
      "maybe-make-URL-tiny"
      (with-handlers
          ([exn:fail:network?
            (lambda (e)
              (fprintf (current-error-port)
                       "Can't contact tinyurl; skipping the test~%"))])
        (check-regexp-match  #rx"http://tinyurl.com/....."
                             (entry-link (maybe-make-URL-tiny long)))))
     (test-case
      "leaves short ones alone"
      (check-equal? (entry-link (maybe-make-URL-tiny short))
                    "http://short.com"))

     (test-case
      "notes spewage"
      (around
       (begin
         (*use-real-atom-feed?* #f)
         (reliably-put-pref #f))

       ;; note  an entry is already spewed.
       ;; check that it is marked as such.
       ;; check that some other entry isn't marked as such.
       (let* ((now  (current-time time-utc))
              (a-while-ago (subtract-duration now (make-time time-duration 0 (* 24 3600))))
              (e1 (make-entry now
                              "What time is it?"
                              "http://its.howdy.doody/time"))
              (e2 (make-entry now
                              "Your mother wears .."
                              "http://army.mil/boots")))

         (note-spewed! e1)
         (check-not-false (already-spewed? e1))
         (check-false (already-spewed? e2))

         ;; now try an earlier time -- even though we haven't
         ;; explicitly noted it, it should still count as having been
         ;; spewed, since we've spewed something more recent than it.
         (let ((e3 (apply make-entry (cdr (vector->list (struct->vector e2))))))
           (set-entry-timestamp! e3 a-while-ago)
           (check-not-false (already-spewed? e3)))

         (note-spewed! e2)
         (check-not-false (already-spewed? e1))
         (check-not-false (already-spewed? e2))
         )
       (reliably-put-pref #f))
      )
     )))

(provide (all-defined-except make-entry reset-prefs-file-semaphore!)
         (rename public-make-entry make-entry))
)
