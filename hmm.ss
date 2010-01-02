#lang scheme

(require (only-in srfi/13 string-reverse))

(define (do-stuff ip port->bytes line-filter line-mangler line-consumer)
  (let loop ()
    (let ([datum (port->bytes ip)])
      (when (not (eof-object? datum))
        (when (line-filter datum)
          (line-consumer (line-mangler datum)))
        (loop)))))

(let ([ip (open-input-string "foo\nbar\nbaz")]
      [op (open-output-string)])

  (do-stuff ip read-line ((curry regexp-match) "^b") string-reverse
            (lambda (line) (display line op) (newline op)))

  (close-output-port op)
  (get-output-string op))