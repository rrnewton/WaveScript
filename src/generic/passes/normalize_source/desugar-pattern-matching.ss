
;;;; .title       Pass: Desugar Pattern Matching
;;;; .author      Ryan Newton

;;;; This desugars pattern matching in binding forms.
;;;; Once that's done, it can run the type inferencer for the first time.

(module desugar-pattern-matching mzscheme
  (require "../../../plt/common.ss")
  (provide pass_desugar-pattern-matching test_desugar-patterns)
  (chezimports)

;; Desugar pattern matching within lambda's, letrecs, and "match" statements. <br>
;; TODO: This should really not go in the source_loader.
(define-pass pass_desugar-pattern-matching     
    (define (break-pattern pat)
      (match pat
	[,s (guard (symbol? s)) 
	    (values s '())]
	[#(,[pv* binds*] ...)
	 (let ([v (unique-name 'pattmp)]
	       [len (length pv*)])
	   (let ([newbinds 
		  `([,pv* 'type (tupref ,(iota len) ,(make-list len len) ,(make-list len v))] ...)])
	     (values v
		     `( ,@(filter (lambda (b) (not (eq? (car b) '_))) newbinds)
			,@(apply append binds*)
			))))]))

  (define (make-let* binds bod)
    (match  binds
      [() bod]
      [(,bind . ,[rest]) `(letrec (,bind) ,rest)]))

  (define process-expr
    (lambda (expr fallthrough)
      (match expr 
	[(lambda (,[break-pattern -> formal* binds* ] ...) ,types ,[bod])
	 (if (null? binds*)
	     `(lambda (,formal* ...) ,types ,bod)
	     `(lambda (,formal* ...) ,types 
		      ,(make-let* (apply append binds*) bod)))]
	[(,let ((,[break-pattern -> lhs* binds*] ,type* ,[rhs*]) ...) ,[bod])
	 (guard (memq let '(let letrec)))
	 `(,let ([,lhs* ,type* ,rhs*] ...  )
	    ,(make-let* (apply append binds*) bod))]

	;; This isn't "pattern-matching" but we desugar it here so
	;; that the type checker doesn't need to deal with it.	 
	[(let* ,binds ,bod) (process-expr (make-let* binds bod) fallthrough)]

	;; Only handles one-armed matches right now:
	;;    [(match ,[x] [ ,[break-pattern -> var* binds*]  ,[rhs*] ] ...)
	[(match ,[x] (,[break-pattern -> var binds] ,[rhs]))
	 `(letrec ([,var 'notypeyet ,x] ,binds ...)
	    ,rhs)]
	
	[,other (fallthrough other)])))

;; TODO: When it works, could redo this with Expr/ExtraArg
  [Expr process-expr]
  
  ;; After desugaring pattern matching, then we can typecheck the prog for the first time:
  [Program (lambda (prog Expr)
	  (match prog
	    [(,inputlang '(program ,bod ,type))
;	     (inspect (Expr bod))
	     ;; NOTE: Doesn't use top level type:
	     (mvlet ([(p t) (annotate-program (Expr bod))])
	       `(desugar-pattern-matching-language 
		 '(program ,p ,t)))]))]
)

; ================================================================================

(define these-tests
  `(["Run a basic test of the pattern match expander."
     (cadr (cadadr
	    (reunique-names 
	     (strip-types 
	      (,pass_desugar-pattern-matching 
	       '(foo '(program (lambda (#(foo #(bar baz)) x) ('t1 't2) foo) UncheckedType)))))))
     (lambda (pattmp x)
       (letrec ([foo (tupref 0 2 pattmp)])
	 (letrec ([pattmp_1 (tupref 1 2 pattmp)])
	   (letrec ([bar (tupref 0 2 pattmp_1)])
	     (letrec ([baz (tupref 1 2 pattmp_1)]) 
	       foo)))))]

    [(,pass_desugar-pattern-matching '(foo '(program (match 3 [x x]) Int)))
     (desugar-pattern-matching-language
      '(program (letrec ([x Int 3]) x) Int))]

    ))

(define test-this (default-unit-tester "desugar-pattern-matching.ss: For reading regiment source files." these-tests))
(define test_desugar-patterns test-this)

)




#;

  (define process-expr
    (lambda (expr fallthrough)
     (let loop ([expr expr] [env '()] [subst '()])
       (match expr 
	 [,c (guard (constant? c)) c]
	 [(quote ,d) `(quote ,d)]
	 [,var (guard (symbol? var))
	       (let ((entry (assq var subst)))
		 (if entry (cadr entry) var))]          
	 [(if ,[test] ,[conseq] ,[altern])
	  (guard (not (memq 'if env)))
	  `(if ,test ,conseq ,altern)]
	 ;; Pre type checking!!
	 [(lambda (,[break-pattern -> formal* binds* ] ...) ,optionaltypes ... ,expr)
	  (guard (not (memq 'lambda env)))     
	  (let ([bod (loop expr
			   (append formal* env)
					;(append (apply append subst*) subst)
			   subst
			   )]
		[binds (apply append binds*)])
	    (if (null? binds)
		`(lambda (,formal* ...) ,optionaltypes ... ,bod)
		`(lambda (,formal* ...) ,optionaltypes ...  (letrec (,binds ...) ,bod))))]

	 [(letrec ((,[break-pattern -> lhs* binds*] ,type* ,[rhs*]) ...) ,[bod])
	  (guard (not (memq 'letrec env)))
	  `(letrec ( [,lhs* ,rhs*] ...
		     ,@(apply append binds*)
		     )
	     ,bod)]

	 ;; This isn't "pattern-matching" but we desugar it here so
	 ;; that the type checker doesn't need to deal with it.	 
	 [(let* ,binds ,expr)
	  (process-expr (match  binds
			  [() expr]
			  [(,bind . ,[rest])
			   `(letrec (,bind) ,rest)])
			fallthrough)]

	 ;; Only handles one-armed matches right now:
	 ;;    [(match ,[x] [ ,[break-pattern -> var* binds*]  ,[rhs*] ] ...)
	 [(match ,[x] (,[break-pattern -> var binds] ,[rhs]))
	  `(letrec ([,var ,x]
		    ,binds ...)
	     ,rhs)]    

	 ;; This is extremely dangerous... just looping down all constructs...
	 ;; This means for-loops, etc, are just treated as applications.
	 [(app ,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
	 ))))