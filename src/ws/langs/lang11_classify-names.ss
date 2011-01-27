



(define-language
  'classify-names-language

  (make-begin
    `(,(lift-letrec-language 'return)

      (define-syntax props
	(syntax-rules ()
	  [(_ . stuff) (void)]))
      )))

