
(define-language
  'uncover-free-language

  (make-begin
    `(,(lift-letrec-language 'return)

      ;; Makes letrec LAZY
      (define-syntax free
	(syntax-rules ()
	  [(_ ls expr ...)
	   (begin expr ...)]))

      )))



