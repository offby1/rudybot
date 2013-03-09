#lang racket

(require rackunit
         rackunit/text-ui
         "userinfo.rkt")

(define sighting-tests

  (test-suite
   "sighting-test"
   (test-case
    "yow"
    (let ([s (make-sighting "1" "2" 3 #f (list "hey" "you"))])
      (note-sighting s)
      (check-not-false (member s (lookup-sightings "1")))
      (check-equal? (lookup-sightings "snorkuplexity") '())))
   (test-case
    "persistent"
    (parameterize ([*userinfo-database-directory-name* "persistent-test.db"])
      (let ([stuff (map make-sighting
                        (list "fred" "paul" "mary" "fred_")
                        (list "2" "3" "4" "5")
                        (list 9 8 7 6)
                        (list "QUIT" #f #f "SNORK")
                        (list (list "znork?")
                              (list "I" "am" "NOT" "dead")
                              (list "I" "am" "Jesus'" "mom")
                              (list "I'm" "fred" "with" "a" "trailing" "underscore")))])
        (let ([writing? (not (directory-exists? (*userinfo-database-directory-name*)))])
          ;; if the db doesn't exist, note some stuff.
          ;; if the db does exist, check for what we noted.
          (if writing?
            (printf "Putting test data into ~a; run me again and I'll check the contents.~%"
                    (*userinfo-database-directory-name*))
            (printf "Reading test data from ~a~%"
                    (*userinfo-database-directory-name*)))

          (for ([s (in-list stuff)])

            (if writing?
                (note-sighting s)
                (check-not-false (member s (lookup-sightings (canonicalize-nick (sighting-who s)))))))))))
   (test-case
    "case-insensitive"
    (let ([s (make-sighting "BOB" "2" 3 #f (list "hey" "you"))])
      (note-sighting s)
      (let ([looked-up-uc (lookup-sightings "BOB")]
            [looked-up-lc (lookup-sightings "bob")])
        (check-equal? looked-up-lc looked-up-uc)))
    (let ([s (make-sighting "bob" "2" 3 #f (list "hey" "you"))])
      (note-sighting s)
      (let ([looked-up-lc  (lookup-sightings "bob")]
            [looked-up-uc  (lookup-sightings "BOB")])
        (check-equal? looked-up-uc looked-up-lc)))

    (delete-directory/files (build-path (*userinfo-database-directory-name*) "b" "bob"))

    (let ([s (make-sighting "BOB" "2" 3 #f (list "hey" "you"))])
      (note-sighting s)
      (let ([looked-up-lc  (lookup-sightings "bob")]
            [looked-up-uc  (lookup-sightings "BOB")])
        (check-equal? looked-up-uc looked-up-lc))))))

(run-tests sighting-tests 'verbose)

