(require (lib "etc.ss"))

;; convert the quotes file into a format that Emacs' "yow" command
;; understands.
(call-with-output-file
    (build-path
     (this-expression-source-directory)
     'up
     "quotes.cookies")
  (lambda (op)
    (for-each
     (lambda (str)
       (fprintf op "~a~a~%" str #\000))
     (call-with-input-file
         (build-path
          (this-expression-source-directory)
          'up
          "quotes")
       read))))
