#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui cached-channel-tests 'verbose))"
|#
(module cached-channel mzscheme
(require (lib "trace.ss")
         (lib "async-channel.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "assert.ss" ("offby1" "offby1.plt")) check-type)
         "globals.ss"
         "vprintf.ss")
(register-version-string "$Id$")

;; This is roughly like an async-channel, except:

;; * you must call cached-channel-put to store stuff, instead of
;;   async-channel-put;

;; * you can see the last thing gotten from it by calling
;;   cached-channel-cache.

(define-values (struct:cached-channel
                make-cached-channel
                cached-channel?
                cached-channel-ref
                cached-channel-set!)
    (make-struct-type 'cached-channel #f 3 0
                      #f (list (cons prop:evt 0))
                      #f #f '(0 1)))

(define (cached-channel-cache cc)
  (cached-channel-ref cc 2))

(define (public-make-cached-channel . args)
  (letrec ((async (apply make-async-channel args))
           (rv (make-cached-channel
                (wrap-evt
                 async
                 (lambda (result)
                   (vtprintf "cached-channel yielded ~s~%" result)
                   (cached-channel-set! rv 2 result)
                   result))
                async
                #f)))
    rv))

(define (cached-channel-put cc v)
  (async-channel-put (cached-channel-ref cc 1)
                     v))


(provide
 cached-channel?
 cached-channel-cache
 cached-channel-put
 (rename public-make-cached-channel make-cached-channel)
 )
)
