#lang scheme

(require (only-in srfi/13 string-reverse))

(define (do-stuff ip op port->bytes line-filter line-mangler)
  (let loop ()
    (let ([datum (port->bytes ip)])
      (if (eof-object? datum)
          (close-output-port op)
          (begin
            (when (line-filter datum)
              (display (line-mangler datum) op)
              (newline op))
            (loop))))))

(let ([ip (open-input-string "foo\nbar\nbaz")]
      [op (open-output-string)])

  (do-stuff ip op read-line ((curry regexp-match) "^b") string-reverse)
  (get-output-string op))