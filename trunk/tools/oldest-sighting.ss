(require (lib "serialize.ss")
         (lib "date.ss")
         (only (lib "1.ss" "srfi") fold)
         "sighting.ss")
(let* ((all (map deserialize (map cdr (with-input-from-file "sightings.db" read))))
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
  (format "~a at ~a: ~a"
          (sighting-who oldest)
          (date->string (seconds->date (sighting-when oldest)) #t)
          (sighting-words oldest)))
