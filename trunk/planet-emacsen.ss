#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui planet-tests 'verbose))"
|#

(module planet-emacsen mzscheme
(require (only (lib "etc.ss") this-expression-source-directory)
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))

         (lib "kw.ss")
         (only (lib "list.ss")
               last-pair
               sort)
         (lib "trace.ss")
         (only (lib "1.ss" "srfi")
               first second third fourth
               filter)
         (only (lib "19.ss" "srfi" )
               date->time-utc
               make-time
               time-nanosecond
               time-second
               time-type
               time-utc->date
               time=?
               time<?
               time>?)
         (rename (lib "19.ss" "srfi" )
                 19:make-date make-date)
         (only (lib "uri-codec.ss" "net")
               current-alist-separator-mode)

         (only (planet "rfc3339.ss" ("neil" "rfc3339.plt"))
               rfc3339-string->srfi19-date/constructor)
         (only (planet "htmlprag.ss" ("neil" "htmlprag.plt" ))
               html->shtml)
         (only "port.ss" port->string/close)

         (only (planet "zdate.ss" ("offby1" "offby1.plt"))
               date->string
               zdate)
         (only (planet "assert.ss" ("offby1" "offby1.plt"))
               check-type)
         (planet "sxml.ss" ("lizorkin" "sxml.plt"))
         "cached-channel.ss"
         "headline.ss"
         "globals.ss"
         "thread.ss"
         "vprintf.ss")
(register-version-string "$Id$")


;; how often (in seconds) do we re-create and read the input port
(define *planet-poll-interval* (make-parameter 3600))

;; returned entries are sorted oldest first.

;; void -> (listof entry?)
(define (snarf-em-all ip)

  (sort
   (map
    (lambda (entry)
      (let* ((updated
              (date->time-utc
               (rfc3339-string->srfi19-date/constructor
                (car
                 ((sxpath '(updated *text*))
                  entry))
                19:make-date)))
             (title
              (car
               ((sxpath '(title *text*))
                entry)))
             (link
              (car
               ((sxpath '(link @ href *text*))
                entry))))
        (make-entry updated title link)))

    ((sxpath '(feed entry))

     ;; Hitmill to Shitmill / Port to String / I can debug / Anything
     ;; / ... Burma Shave
     (html->shtml
      (begin0
        (port->string/close ip)
        (close-input-port ip)))))

   (lambda (e1 e2)
     (time<?
      (entry-timestamp e1)
      (entry-timestamp e2)))))
;;(trace snarf-em-all)
;; this is for display purposes, not for serializing.

(define (entry->string entry)
  (define (de-html str)
    (apply string-append ((sxpath '(// *text*)) (html->shtml str))))
  (check-type 'entry->string entry? entry)
  (format "(~a) ~a: ~a"
          (date->string
           (time-utc->date (entry-timestamp entry) 0)
           "~A, ~B ~d ~Y ~k:~M ~z")
          (de-html (entry-title entry))
          (entry-link entry)))

;; for testing
(define (fake-atom-feed)
  (let ((fn (build-path
               (this-expression-source-directory)
               "example-planet-emacsen.xml")))
      (vtprintf "snarfing test data from ~s~%"
                fn)
      (open-input-file fn)))

(define/kw (queue-of-entries
            #:key
            [whence fake-atom-feed]
            [filter (lambda (e) #t)])

  (check-type 'queue-of-entries procedure? whence)

  (let ((the-channel (make-cached-channel #f)))

    (thread-with-id
     (lambda ()
       (let loop ()

         (with-handlers
             ([exn:fail:network?
               (lambda (e)
                 (fprintf
                  (current-error-port)
                  (format "can't snarf from procedure ~s~%"
                          (object-name whence)))
                 #t)])
           (for-each
            (lambda (e)
              (when (filter e)
                (cached-channel-put the-channel e)))

            (snarf-em-all (whence))))

         (sleep (*planet-poll-interval*))
         (loop)))
     #:descr "planet emacs producer")

    the-channel)  )

;;(trace queue-of-entries)



(define planet-tests

  (test-suite
   "planet"

   (test-case
    "delivers an entry raht quick-like"
    (before
     (begin
       (*use-real-atom-feed?* #f)
       (reliably-put-pref #f))
     (check-pred entry? (sync (queue-of-entries)) "It's not an entry!!")))))


(provide
 make-cached-channel
 entry->string
 entry-timestamp
 fake-atom-feed
 planet-tests
 queue-of-entries
 *planet-poll-interval*
 )
)

