




(define-language
  'annotate-heartbeats-language

  (make-begin
    `(,
      (subtract-bindings '(lazy-letrec)
			 (classify-names-language 'return))

      (define-syntax lazy-letrec
	(syntax-rules ()
	  [(_ ([lhs heartbeat rhs] ...) body)        
	   (letrec ([lhs 
		     (lambda () 
		       (let-syntax ([lhs (identifier-syntax (lhs))] ...)
			 rhs))]
		    ...)
	     (let-syntax ([lhs (identifier-syntax (lhs))] ...)
	       body))]))

      )))

