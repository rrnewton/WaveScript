
(define-pass flatten-iterate-spine
  [Expr/Types
     (lambda (x tenv fallthru)
       ;; Coerces an expression to be simple, producing new bindings.
       (define (make-simple-shallow x)
	 (if (simple-expr? x)
	     (values x '())	  
	     (mvlet ([(type) (recover-type x tenv)]
		     [(name) (unique-name 'tmp)])	    
	       (values name
		       `((,name ,type ,x))))))       
       (match x
	 [(iterate ,fun ,[make-simple-shallow -> src binds])
	  (if (null? binds)
	      `(iterate ,fun ,src)
	      `(lazy-letrec ,binds (iterate ,fun ,src)))]
	 
	 [,other (fallthru other tenv)]
	 ))]
  )
