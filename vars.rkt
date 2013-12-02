#lang racket/base

(require racket/file
         "utils.rkt")
(provide (all-defined-out))

;; This value depends on the server; this seems to work for freenode
(define *my-nick*
  (box (from-env "BOTNICK" "rudybot")))
(define *my-master*
  ;; use authentication by default, but allow to set a regexp for convenience
  (box (let ([master (from-env "BOTMASTER" "")])
         (and (not (equal? master ""))
              (regexp (string-append "^" master "$"))))))

;; This should probably be a hash table, keyed by network name.
(define *initial-channels* ; env var can be "#foo,#bar"
  (make-parameter (from-env "BOTCHANNELS" '(
                                            "##SICP"
                                            "##cinema"
                                            "#emacs"
                                            "#racket"
                                            "#scheme"
                                            ) #rx",")))
(define *nickserv-password*
  (make-parameter (from-env "BOTPASSWD" (get-preference '|rudybot-freenode-nickserv-password|))))
(define *bot-gives-up-after-this-many-silent-seconds* (make-parameter 250))
(define *irc-server-hostname* (make-parameter "localhost"))
(define *irc-server-port* (make-parameter "6667"))

(define *sandboxes* (make-hash))
(define *max-values-to-display* 5)

(define *start-time*            (current-seconds))
(define *connection-start-time* (make-parameter #f))

;; some state is put globally, to be able to separate functions conveniently
(define *irc-output*      (make-parameter (current-output-port)))
(define *current-words*   (make-parameter #f))
(define *response-target* (make-parameter #f))
(define *for-whom*        (make-parameter #f))
(define *full-id*         (make-parameter #f))

;; Not sure it makes sense for these two to be separate; but adding
;; *incubot-logger* seemed the quickest way to get logging in incubot
(define *logger*          (make-parameter void))
(define *incubot-logger*  (make-parameter #f))

(provide log)
(define (log fmt . args)
  (apply (*logger*) fmt args))


;; Maybe I should use rnrs/enums-6 to guard against typos
(define *authentication-state* (box 'havent-even-tried))

;; Lines much longer than this will cause the server to kick us for
;; flooding.
(define *max-output-line* 500)

;; This retrieves a sentence from the "incubot" server.
(define *incubot-server* (make-parameter (lambda ignored #f)))
