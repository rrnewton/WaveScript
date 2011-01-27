








(define-language
  'add-places-language

  (make-begin
    `(,(subtract-bindings '(lazy-letrec)
			  (add-control-flow-language 'return))

      (define-syntax lazy-letrec
	(syntax-rules ()
	  [(_ ([lhs heartbeat formplace membplace rhs] ...) body)
	   (letrec ([lhs 
		     (lambda () 
		       (let-syntax ([lhs (identifier-syntax (lhs))] ...)
			 rhs))]
		    ...)
	     (let-syntax ([lhs (identifier-syntax (lhs))] ...)
	       body))]))



      )))

