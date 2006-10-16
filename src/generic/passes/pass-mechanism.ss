

;;(datum->syntax-object  #'_  (list ingram outgram bnds expr prog))
;;(list ingram outgram bnds expr prog)


(define-syntax define-pass
  (lambda (x)
    (syntax-case x ()
      [(_ name clauses ...)
       (let ([gen-code 
	      (lambda (ingram outgram bnds expr prog fuser)
		(define output-language 
		  (datum->syntax-object #'name
		   (string->symbol 
		    (string-append (symbol->string (datum name)) "-language"))))
		(define inspec (if ingram 
				   (with-syntax ([ig ingram]) #'`(input (grammar ,ig PassInput)))
				   #''(input)))
		(define outspec (if outgram 
				    (with-syntax ([og outgram]) #'`(output (grammar ,og PassInput)))
				    #''(output)))
		;; Fill in some defaults:
		(unless prog 
		  (set! prog (with-syntax ([ol output-language])
			       #'(lambda (pr Expr) 
				   (match pr
				     [(,input-language (quote (program ,body ,type)))
				      `(ol '(program ,(Expr body) ,type))])
				   ))))
		(unless fuser
		  (set! fuser (lambda (ls k) (apply k ls))))

		(unless expr 
		  (set! expr (lambda (x fallthrough) (fallthrough x))))

		(if bnds 
		  (set! expr
		    (with-syntax ([expr expr])
		      #`(lambda (x fallthrough)
			  (expr x 
				;; We stick the varbinds handling between the user code and the automatic looping.
				(lambda (x)
				  ;; Now we set up the varbinds and give them to the userfun.
				  (let ([fun #,bnds])
				    (match x
				      [(lambda ,vars ,types ,bod)
				       (fun vars types (list bod) 
					    ;; Reconstruct function:
					    (lambda (vars types exprs)
					      `(lambda ,vars ,types ,(car exprs)))
					    process-expr
					    )]
#;
				      [(letrec ([,vars ,types ,rhss] ...) ,body)
				       (fun vars types (cons body rhss)
					    (lambda (vars types exprs)
					      `(letrec ([,vars ,types ,rhss] ...) ,body))
					    process-expr)]

				      ;; TODO: for, ... anything else?
				      
				      ;; If it's not a variable-binding construct, we just let it 
				      ;; go through to the final, auto-looping fallthrough:
				      [,other (fallthrough other)]
				      ))))))))

		(with-syntax ([(i o b e p f inspec outspec) 
			       (list ingram outgram bnds expr prog fuser
				     inspec outspec)])
		  ;; Now we plug in defaults:
		  #;
		  (if (and prog (or bnds expr))
		      (error 'define-pass "cannot define both Program and Bindings/Expr: ~s" 
			     #'_))


		  #`(define name 
		      ;; Jeez, this is lame but just so the compiler associates the name with the closure:
		      (letrec* ([name (lambda (prog) (tmp prog))]
				[process-expr (core-generic-traverse e f)]
				[tmp (build-compiler-pass 
				      'name 
				      inspec 
				      outspec 
				      (lambda (x) (p x process-expr)))])
			name
			))

		))])
	 ;; Here are all the default values for these components:
	 (let loop ([cl #'(clauses ...)]
		    [ingram #f]
		    [outgram #f]
		    [bnds #f]
		    [expr #f]
		    [prog #f]
		    [fuser #f])
	   (if (null? cl)
	       (gen-code ingram outgram bnds expr prog fuser)
	       (syntax-case (car cl) (InputGrammar OutputGrammar Bindings Expr Program)
			    [(InputGrammar g)  (loop (cdr cl) #'g    outgram bnds expr prog fuser)]
			    [(OutputGrammar g) (loop (cdr cl) ingram #'g     bnds expr prog fuser)]
			    [(Bindings f)      (loop (cdr cl) ingram outgram #'f  expr prog fuser)]
			    [(Expr f)          (loop (cdr cl) ingram outgram bnds #'f  prog fuser)]
			    [(Program f)       (loop (cdr cl) ingram outgram bnds expr #'f  fuser)]
			    [(Fuser f)         (loop (cdr cl) ingram outgram bnds expr prog #'f )]
			    ))))
       ])))



#;

(let () 
  (define-pass foo 
    [Bindings (lambda (vars types exprs reconst loop)
		(inspect vars))]
    )
  
  )