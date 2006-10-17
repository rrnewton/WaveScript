;;;; .title Pass: Generate Token Machine 


    ;; This processes a signal expression and generates TM bindings for it.
    ;; An expression of this kind generates a single token-handler.
    ;; Note: We lose type information here.
    (define (Expr expr)
      
      (match expr 

	;; Just keep walking down the structure:
	[(let* () ,[e]) e]
	[(let* ([,lhs ,ty ,[rhs]] ,rest ...) ,e)
	 `(let ([,lhs ,rhs])
	    ,(process-expr `(let* ,rest ,e)))]

	[(smap ,f ,s) `(subcall ,f ,s)]

	;; This operator only sometimes returns.
	;; It thus captures its continuation...
	[(sfilter ,f ,s)
	 'NOTFINISHED
	 ;(if (f ,s) ,s)
	 ]
	

	)      

      )

    ;; This translates a Regiment procedure into a TM handler.
    ;; Currently, this is a trivial transformation.
    (define (Lambda bind)
      (match bind
	[[,lhs ,ty ,annots (lambda ,v* ,ty* ,e)]	
	 ;; This tokhandler does not use subtokids and has no stored.
	 `[,lhs ,v* (stored)
		,(Expr e)]
	 ]))

    (define (process-decl decl)
      (match decl
	[(AGGR (OUTPUT ,v) (VIA ,t) 
	       (SEED ,[Expr -> e1])
	       (FUN [,fn ,ty ,[Expr -> e2]]) 
	       (INPUT ,[Expr -> e3]))

	 ;; We need to subscribe to the input signal.
	 
	 [

	  `(greturn )]
	 

	 

 	 0]

	[(EMIT ) 0]

	[(ELECT ) 0]

	)
      )




(define gentm
  (let ()


    (lambda (prog)
      (match prog
        [(,lang ;add-places-language 
	  (quote (program (props ,table ...)
		   (commdecls ,binds ,fin)
		   ,type)))
	 
	 0
	 
	 
	 ]))))

