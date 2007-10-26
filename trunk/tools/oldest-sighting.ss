(require (lib "serialize.ss")
         (lib "date.ss")
         (lib "etc.ss")
         (only (lib "1.ss" "srfi") fold)
         "../sighting.ss"
         "../spelled-out-time.ss")

(let* ((all (map
             deserialize
             (map
              cdr
              (with-input-from-file
                  (build-path
                   (this-expression-source-directory)
                   ".."
                   "sightings.db")
                read))))
       (oldest
        (car
         (fold
          (lambda (s seq)
            (if (< (sighting-when s)
                   (sighting-when (car seq)))
                (list s)
                seq))
          (list (car all))
          all))))
  (printf "~a, ~a ago: ~s~%"
          (sighting-who oldest)
          (spelled-out-time (- (current-seconds )
                               (sighting-when oldest)))
          (sighting-words oldest)))
