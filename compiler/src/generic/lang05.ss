


(define-language
  'lift-letrec-language

  (make-begin
    `(,(base-language 'return)

      ;; Makes letrec LAZY
      (define-syntax lazy-letrec
	(syntax-rules ()
	  [(_ ([lhs rhs] ...) body)        
	   (letrec ([lhs 
		     (lambda () 
		       (let-syntax ([lhs (identifier-syntax (lhs))] ...)
			 rhs))]
		    ...)
	     (let-syntax ([lhs (identifier-syntax (lhs))] ...)
	       body))]))
      )))



