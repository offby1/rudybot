#lang racket

(module+ test (require rackunit rackunit/text-ui))

;; TODO -- consider keeping this data someplace other than (or in
;; addition to) a file on disk -- like Amazon's S3.  That way if I
;; want to move the bot to a new hosting site, I don't have to bring
;; the directory along with me.
(define *userinfo-database-directory-name* (make-parameter "test-userinfo.db"))
(define *sightings-to-keep* 2)
(define *messages-to-keep* 20)

;; each user has an info record that is an alist mapping a type symbol (either
;; 'sightings or 'messages) to a value.  The value will be a list of sighting
;; records and a list of message values in these cases respectively, and a
;; bunch of additional information bits can hang there.

(define (canonicalize-nick n)
  ;; TODO -- consider nixing _leading_ underscores as well; I've seen
  ;; those in the wild.

  ;; I've forgotten why I put (?<=.) in the regexp :-(
  (string-downcase (regexp-replace #rx"(?<=.)([`_]*)$" n "")))

(module+ test
  (define-test-suite canonicalize-nick-tests
    (check-equal? "plunderblunder" (canonicalize-nick "plunderblunder"))
    (check-equal? "sam" (canonicalize-nick "sam`"))
    (check-equal? "sam" (canonicalize-nick "sam_`_`"))
    ))

(define (canonical-nick->infopath n)
  (let ([base (build-path (*userinfo-database-directory-name*)
                          (substring n 0 1))])
    (unless (directory-exists? base) (make-directory* base))
    (build-path base n)))

(define (info-ref nick [default #f])
  (let* ([nick (canonicalize-nick nick)]
         [cell (cache-lookup nick)])
    (or (mcdr cell)
        (let* ([infofile (canonical-nick->infopath nick)]
               [info (if (file-exists? infofile)
                       (let ([info (with-handlers
                                       ([exn:fail:read? (lambda (e) default)])
                                     (call-with-input-file infofile read))])
                         (if (list? info)
                           info
                           default))
                       default)])
          (set-mcdr! cell info)
          info))))

(define (info-set! nick val)
  (let* ([nick (canonicalize-nick nick)]
         [infofile (canonical-nick->infopath nick)]
         [cell (cache-lookup nick)])
    (set-mcdr! cell val)
    (call-with-output-file infofile #:exists 'truncate
                           (lambda (o) (write val o)))))

(define info-cache '()) ; maps nick to its info
(define info-cache-max 20)
(define (cache-lookup key)
  (let loop ([n (sub1 info-cache-max)] [cache info-cache] [prev #f])
    (cond [(or (null? cache) (zero? n))
           ;; not found, or too many: trim, and create a new entry
           (when prev (set-mcdr! prev '()))
           (let ([new (mcons key #f)])
             (set! info-cache (mcons new info-cache))
             new)]
          [(equal? key (mcar (mcar cache)))
           ;; found: move to front if needed, and return
           (when prev
             (set-mcdr! prev (mcdr cache))
             (set-mcdr! cache info-cache)
             (set! info-cache cache))
           (mcar cache)]
          ;; else: march on
          [else (loop (sub1 n) (mcdr cache) cache)])))

;; generic setter for some property
(define (userinfo-ref nick key [default #f])
  (dict-ref (info-ref nick '()) key default))
(define (userinfo-set! nick key val)
  (info-set! nick (dict-set (info-ref nick '()) key val)))

;; generic list-of-values constructor for a lookup-* and note-* functions
(define (make-limited-list-info key get-nick get-time limit)
  (define (trim vals) (drop vals (max 0 (- (length vals) limit))))
  (define (lookup nick) (trim (userinfo-ref nick key '())))
  (define (add! val)
    (let* ([nick (get-nick val)]
           ;; lookup in db, sort, trim
           [vals (userinfo-ref nick key '())]
           [vals (sort (cons val vals) < #:key get-time)]
           [vals (trim vals)])
      (userinfo-set! nick key vals)))
  (values lookup add!))

;; Some limited lists infos

(define-struct sighting (who where when action? words) #:prefab)
(define-values (lookup-sightings note-sighting)
  (make-limited-list-info
   'sightings sighting-who sighting-when *sightings-to-keep*))

(define-struct message (who from where when words) #:prefab)
(define-values (lookup-messages note-message)
  (make-limited-list-info
   'messages message-who message-when *messages-to-keep*))

(provide *userinfo-database-directory-name*)
(provide/contract
 [struct sighting ([who string?]
                   [where string?]
                   [when natural-number/c]
                   [action? (or/c string? not)]
                   [words (listof string?)])]
 [lookup-sightings (-> string? (listof sighting?))]
 [note-sighting (-> sighting? void?)]
 [canonicalize-nick (-> string? string?)]
 [userinfo-ref (->* (string? any/c) (any/c) any)]
 [userinfo-set! (-> string? any/c any/c any)])

(module+ test
  (define-test-suite all-tests
    canonicalize-nick-tests)
  (run-tests all-tests))

