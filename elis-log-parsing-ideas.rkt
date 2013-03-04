#lang racket/base

(require racket/match racket/pretty (for-syntax racket/base))

(struct utterance (timestamp speaker target text) #:prefab)

;; All timings are done by running 5 times, dropping highest and lowest,
;; averaging the rest, and rounding to nearest ms.

;; Original version
;; cpu time: 12622 real time: 12618 gc time: 134
(define (string->utterance0 s)
  (match s
    [(regexp #px"^ *([[:print:]]*?) <= +(\".*\")" (list _ timestamp raw-string))
     (let ([parsed-string (read (open-input-string raw-string))])
       (match parsed-string
         [(regexp #px"^:(.*?)!(.*?)@(.*?) PRIVMSG ([[:print:]]+?) :(.*)"
                  (list _ nick id host target text))
          (utterance timestamp nick target text)]
         [_ #f]))]
    [_ #f]))
(define (parse-file0 input-file output-file)
  (call-with-input-file input-file
    (lambda (inp)
      (call-with-output-file output-file #:exists 'truncate
        (lambda (outp)
          (for ([line (in-lines inp)])
            (let ([utz (string->utterance line)])
              (when utz (pretty-print utz outp)))))))))

;; Simple printout
;; cpu time: 4295 real time: 4295 gc time: 34
(define (parse-file1 input-file output-file)
  (call-with-input-file input-file
    (lambda (inp)
      (call-with-output-file output-file #:exists 'truncate
        (lambda (outp)
          (for ([line (in-lines inp)])
            (let ([utz (string->utterance line)])
              (when utz (fprintf outp "~s\n" utz)))))))))

;; Avoid non-greedy regexps
;; cpu time: 3052 real time: 3051 gc time: 36
(define (string->utterance1 s)
  (match s
    [(regexp #px"^ *([^ ]*) <= +(\".*\")" (list _ timestamp raw-string))
     (let ([parsed-string (read (open-input-string raw-string))])
       (match parsed-string
         [(regexp #px"^:([^!]*)!([^@]*)@([^ ]*) PRIVMSG ([^:]+) :(.*)"
                  (list _ nick id host target text))
          (utterance timestamp nick target text)]
         [_ #f]))]
    [_ #f]))

;; Use this to convert the log file
(define (convert-log input-log output-log)
  (call-with-input-file input-log
    (lambda (inp)
      (call-with-output-file output-log #:exists 'truncate
        (lambda (outp)
          (for ([line (in-lines inp)])
            (define (assert c)
              (unless c (error 'convert-log "bad log line: ~a" line)))
            (assert (not (regexp-match? #rx"^ " line)))
            (define m (regexp-match #rx"^([^ ]*) (<=|=>) (.*)$" line))
            (if (not m)
              (displayln line outp)
              (let ([s (read (open-input-string (cadddr m)))])
                (assert (string? s))
                (fprintf outp "~a ~a ~a\n" (cadr m) (caddr m) s)))))))))
;; (convert-log "big-log" "new-big-log")
;; (exit)

;; Using new format, no need for reading from the string
;; cpu time: 2383 real time: 2382 gc time: 29
(define (string->utterance2 s)
  (match s
    [(regexp #px"^([^ ]*) <= (.*)$" (list _ timestamp string))
     (match string
       [(regexp #px"^:([^!]*)!([^@]*)@([^ ]*) PRIVMSG ([^:]+) :(.*)"
                (list _ nick id host target text))
        (utterance timestamp nick target text)]
       [_ #f])]
    [_ #f]))

;; Combine the two regexps
;; cpu time: 1937 real time: 1936 gc time: 25
(define (string->utterance3 s)
  (match s
    [(regexp #px"^([^ ]*) <= :([^!]*)!([^@]*)@([^ ]*) PRIVMSG ([^:]+) :(.*)$"
          (list _ timestamp   nick    id      host            target   text))
     (utterance timestamp nick target text)]
    [_ #f]))

;; selectors for the version to use
(define-syntax string->utterance (make-rename-transformer #'string->utterance3))
(define-syntax parse-file        (make-rename-transformer #'parse-file1))

(module+ main
  (time (parse-file "new-big-log" "parsed")))
