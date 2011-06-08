#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require racket/trace
         (only-in "lexer.rkt" prefix->canonical-nick)
         (only-in racket/date find-seconds)
         (only-in (planet offby1/offby1:2:2/zdate) zdate)
         rackunit
         rackunit/text-ui

         ;; This is a symlink, created thus:

         #|
         ln -s /home/erich/doodles/plt-scheme/web/amazon/ ~/.racket/5.1.1/collects/
         |#
         (only-in amazon/upload-queue
                  close-upload-queue
                  make-simple-db-upload-queue
                  simpledb-enqueue
                  )
         (only-in amazon/simpledb
                  simpledb-post)
         )

(define pe (curry fprintf (current-error-port)))

;; Eventually I should make my amazon package presentable,
;; and upload it to PLaneT.

(struct sighting (who when description) #:prefab)

;; grovel log, pulling out new-style sexps (and associated timestamps)
;;   actually, only get those whose command is JOIN, PART, QUIT, or
;;   PRIVMSG
;; come up with a struct sighting

(define (trim-action utterance)
  (match utterance
    [(regexp #px"^\1ACTION (.*)\1$" (list _ action))
     ;; actions are generally in the present tense, e.g. "backs away
     ;; slowly".  I wish I could convert to past tense here, but alas.
     (format ": ~a" (bytes->string/utf-8 action))]
    [_ (format "saying ~s" (bytes->string/utf-8 utterance))]))

(define (timestamp+message->description m)
  (match m
    [(cons timestamp (? dict? parsed))

     (let* ([command (car (dict-ref parsed 'command))]
            [params  (dict-ref parsed 'params)]
            [command-str (bytes->string/utf-8 command)])
       (case (string->symbol command-str)
         ((JOIN)
          (format "~aing ~a"      (string-downcase command-str) (second (first params))))
         ((PART)
          (format "~aing from ~a" (string-downcase command-str) (second (first params))))
         ((QUIT)
          (let ([parting-words (second (first params))])
            (if (bytes=? #"" parting-words)
                (format "quitting")
                (format "quitting, saying ~s" (bytes->string/utf-8 parting-words)))))
         ((PRIVMSG)
          (format "~a ~a" (trim-action
                           (second (second params)))
                  (second (first params)) ))))]
    [_ 'wtf]))

(define (make-numbered-dict prefix seq)
  (for/list ([(elt i) (in-indexed seq)])
    (cons (format "~a~a" prefix i) elt)))

;; Massage our rfc1459 structure into a flat list suitable for shoving
;; into simpledb
(define (message->flat-alist m)
  (match m
    [(? dict? parsed)
     (let ([prefix  (dict-ref parsed 'prefix)]
           [nick    (dict-ref parsed 'nick '())]
           [command (dict-ref parsed 'command)]
           [params  (dict-ref parsed 'params)])

       (for/fold ([alist
                   ;; work around some bugaceous data in the logs
                   (if (equal? params '((param . #f)))
                       '()
                       (make-numbered-dict "param-" (map second params)))])
           ([k '(command nick prefix)]) ;order matters because I'm too
                                        ;lazy to make the tests ignore order
           (let ([v (dict-ref parsed k '())])
             (if (null? v)
                 alist
                 `((,(symbol->string k) . ,(car v))
                   ,@alist)))))]))

(define (ensure-nick parsed)
  (cond
   ((dict-ref parsed 'nick #f) parsed)
   ((dict-ref parsed 'prefix #f)
    => (lambda (prefix)
         (if (pair? prefix)
             (dict-set parsed 'nick (list (prefix->canonical-nick (first prefix))))
             parsed)))
   (else
    parsed)))

(define (log-line->alist l)
  (match l
    [(regexp #px"^([[:graph:]]+) <= (\\(.*\\))$" (list _ timestamp stuff))
     (cons (format "~a" (zdate timestamp #:format "~s"))
           (message->flat-alist (ensure-nick (read (open-input-string stuff)))))]
    [_ #f])  )


(define-test-suite tests
  (check-equal? (trim-action #"frotz")
                "saying \"frotz\"")
  (check-equal? (trim-action #"\1ACTION you said it\1")
                ": you said it")
  (check-equal? (timestamp+message->description
                 '("123.456"
                   (prefix #"colonel Mustard")
                   (command #"PRIVMSG")
                   (params (param #"in the study")
                           (param #"\1ACTION whuppin' hisself on de haid wit de candlestick\1"))))
                ": whuppin' hisself on de haid wit de candlestick in the study")
  (check-equal? (timestamp+message->description '("123.456"
                                                  (prefix #"colonel Mustard")
                                                  (command #"PRIVMSG")
                                                  (params (param #"in the study")
                                                          (param #"gimme seltzer"))))
                "saying \"gimme seltzer\" in the study")

  (check-equal? (timestamp+message->description '("123.456"
                                                  (prefix #"rudybot!~luser@ec2-204-236-167-175.us-west-1.compute.amazonaws.com")
                                                  (command #"PART")
                                                  (params (param #"#scheme"))))
                "parting from #scheme")

  (check-equal? (timestamp+message->description '("123.456"
                                                  (prefix #"monqy")
                                                  (command #"QUIT")
                                                  (params (param #"Quit: can't see a thing"))))
                "quitting, saying \"Quit: can't see a thing\"")

  (check-equal? (timestamp+message->description
                 '("123.456"
                   (prefix #"monqy")
                   (command #"QUIT")
                   (params (param #""))))
                "quitting")
  (check-equal?
   (message->flat-alist
    '((prefix #"monqy!~chap@pool-71-102-217-117.snloca.dsl-w.verizon.net")
      (command #"QUIT")
      (params
       (param #"Quit: cant see a thing")
       (param #"Oh, by the way"))))
   '(("prefix"  . #"monqy!~chap@pool-71-102-217-117.snloca.dsl-w.verizon.net")
     ("command" . #"QUIT")
     ("param-0" . #"Quit: cant see a thing")
     ("param-1" . #"Oh, by the way")))
  (check-equal?
   (message->flat-alist
    '((prefix) (command #"PING") (params (param #"niven.freenode.net"))))
   '(("command" . #"PING")
     ("param-0" . #"niven.freenode.net")))
  (check-equal?
   (message->flat-alist
    '((prefix  #"ade!~ade@72.1.197.9")
      (nick    #"ade")
      (command #"QUIT")
      (params  (param . #f))))
   '(("prefix"  . #"ade!~ade@72.1.197.9")
     ("nick"    . #"ade")
     ("command" . #"QUIT")))
  (check-equal?
   (make-numbered-dict "frotz" '(0 1 2))
   '(("frotz0" . 0)
     ("frotz1" . 1)
     ("frotz2" . 2)))

  (check-equal?
   (log-line->alist "2000-01-01T00:00:00Z <= ((prefix #\"nick!nick@nite\") (command #\"begone\") (params))")
   '("946684800" ("prefix" . #"nick!nick@nite") ("nick" . #"nick") ("command" . #"begone"))))

(define-values [enqueue-log-message-for-simpledb-batch flush-simpledb-queue]
  (let ([upload-queue #f])
    (values
     (lambda (m)
       (when (not upload-queue)
         (set! upload-queue  (make-simple-db-upload-queue #:domainname "freenode")))

       (displayln m)
       ;; (simpledb-enqueue upload-queue m)
       )

     (lambda ()
       (close-upload-queue upload-queue)
       ))))

(provide main)
(define (main . args)
  (define input-file-names
    (command-line
     #:program "upload-yadda-yadda"
     #:args input-file-names
     input-file-names))

  (cond
   ((null? input-file-names)
    (displayln "You didn't specify any input files; running unit tests instead of parsing" (current-error-port))
    (exit (if (positive?  (run-tests tests)) 1 0)))
   ((< 1 (length input-file-names))
    (error 'log-parser "I want at most one input file name; instead you gave me ~s" input-file-names))
   (else

    (define (my-call-with-input-file name proc)
      (if (string=? name "-")
          (proc (current-input-port))
          (call-with-input-file name proc)))

    ;; I should really fetch the high-water mark from simpledb, and
    ;; then only upload stuff that's newer.  That way, running this
    ;; twice in a row, the second time will be quicker.
    (let ([input-file-name (car input-file-names)])
      (my-call-with-input-file
       input-file-name
       (lambda (ip)
         (pe "Reading from ~a..." ip)
         (for ([line (in-lines ip)])
           (cond

            ;; A query like this will find the two most recent log
            ;; entries from this nick:

            ;; select *
            ;;    from freenode
            ;;    where prefix like 'pjb%'
            ;;      and ItemName() > '0'
            ;;    order by ItemName()
            ;; limit 2
            ((log-line->alist line) => enqueue-log-message-for-simpledb-batch))

           )))
      (flush-simpledb-queue)
      (pe "done~%")))))
