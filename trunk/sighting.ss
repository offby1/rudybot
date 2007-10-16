(module sighting mzscheme
(require (lib "serialize.ss"))
(define-serializable-struct sighting (who where when was-action? words) #f)
(provide (struct sighting (who where when was-action? words)))
)
