

  (define-syntax define-values 
   (lambda (x)
    (define iota 
      (case-lambda [(n) (iota 0 n)]
		   [(i n) (if (= n 0) '() (cons i (iota (+ i 1) (- n 1))))]))
    (syntax-case x ()  
     [(define-values (vars ...) exp)
      (with-syntax ([(nums ...) (datum->syntax  
				 #'define-values 
				 (iota (length (syntax->datum #'(vars ...)))))])
	#'(begin  
	    (define newtempvar (call-with-values (lambda () exp) vector))
	    (define vars (vector-ref newtempvar nums))
	    ...))])))


;; This is included in R6RS:
#;    
(define-syntax let-values
      (lambda (x)
	(define domvlet
	  (lambda (bindings ids tmps body)
	    (if (null? bindings)
		`((,#'lambda ,ids ,@body) ,@tmps)
		(syntax-case (car bindings) ()
			     [(*ids expr)
			      (with-syntax ([*tmps (generate-temporaries #'*ids)])
				(with-syntax ([body (domvlet (cdr bindings)
							     (append #'*ids ids)
							     (append #'*tmps tmps)
							     body)])
				  #'(call-with-values
					(lambda () expr)
				      (lambda *tmps body))))]))))
	(syntax-case x ()
		     [(_ (((id ...) expr) ...) form ...)
		      (for-all (lambda (ls) (for-all identifier? ls))
			      #'((id ...) ...))
		      (domvlet #'(((id ...) expr) ...) '() '() #'(form ...))])))

