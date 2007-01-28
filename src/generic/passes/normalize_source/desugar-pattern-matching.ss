
;;;; .title       Pass: Desugar Pattern Matching
;;;; .author      Ryan Newton

;;;; This desugars pattern matching in binding forms.
;;;; Once that's done, it can run the type inferencer for the first time.

;;;; NOTE: this pass also desugars type assertions within the formals
;;;; list.  It pulls these outside the lambda.

(module desugar-pattern-matching mzscheme
  (require "../../../plt/common.ss")
  (provide pass_desugar-pattern-matching test_desugar-patterns)
  (chezimports)

;; Desugar pattern matching within lambda's, letrecs, and "match" statements. <br>
;; TODO: This should really not go in the source_loader.
(define-pass pass_desugar-pattern-matching     
    (define (break-pattern pat)
      (match pat
	[(assert-type ,typ ,[form binds _]) (values form binds typ)]
	[,s (guard (symbol? s)) 
	    (values s '() #f)]
	[#(,[pv* binds*] ...)
	 (let ([v (unique-name 'pattmp)]
	       [len (length pv*)])
	   (let ([newbinds 
		  `([,pv* 'type (tupref ,(iota len) ,(make-list len len) ,(make-list len v))] ...)])
	     (values v
		     `( ,@(filter (lambda (b) (not (eq? (car b) '_))) newbinds)
			,@(apply append binds*)
			)
		     #f)))]))

  (define (make-let* binds bod)
    (match  binds
      [() bod]
      [(,bind . ,[rest]) `(letrec (,bind) ,rest)]))

  (define (build-assert t*)
    `(,@(map (lambda (t) (or t `(quote ,(unique-name 'alpha)))) t*)
      -> ',(unique-name 'beta)))

  (define process-expr
    (lambda (expr fallthrough)
      (match expr 
	[(lambda (,[break-pattern -> formal* binds* type-assertion*] ...) ,types ,[bod])
	 (let ([lam (if (null? binds*)
			`(lambda (,formal* ...) ,types ,bod)
			`(lambda (,formal* ...) ,types 
				 ,(make-let* (apply append binds*) bod)))])
	   (if (ormap id type-assertion*)
	       `(assert-type ,(build-assert type-assertion*) ,lam)
	       lam))]
	[(,let ((,[break-pattern -> lhs* binds* type-assertion*] ,type* ,[rhs*]) ...) ,[bod])
	 (guard (memq let '(let letrec)))
	 ;; Shouldn't have assertions on the variable names here:
	 (ASSERT (not (ormap id type-assertion*)))
	 `(,let ([,lhs* ,type* ,rhs*] ...  )
	    ,(make-let* (apply append binds*) bod))]

	;; This isn't "pattern-matching" but we desugar it here so
	;; that the type checker doesn't need to deal with it.	 
	[(let* ,binds ,bod) (process-expr (make-let* binds bod) fallthrough)]

	;; Only handles one-armed matches right now:
	;;    [(match ,[x] [ ,[break-pattern -> var* binds*]  ,[rhs*] ] ...)
	[(match ,[x] (,[break-pattern -> var binds type-assertion*] ,[rhs]))
	 ;; Shouldn't have assertions on the variable names here for now:
	 (ASSERT (not (ormap id type-assertion*)))
	 `(letrec ([,var 'notypeyet ,x] ,binds ...)
	    ,rhs)]

	;; [2006.11.15] Going to add special stream-of-tuples field-naming syntax.
	[(let-as ,v (,fldname* ...) ,[rhs] ,[body])
	 (guard (symbol? v) (andmap symbol? fldname*))
	 (let ([len (length fldname*)])
	 ;; Each field-name gets bound to a projection function.
	 `(let ([,v ,rhs])
	    (let ([,fldname* 
                    ,(map (lambda (i) `(lambda (x) (tupref ,i ,len ,v)))
                          (iota len))])
	      ,body))
	    )]
	;; This is let-as's counterpart for projecting out stream values.
	[(dot-project ,[v] (,[flds] ...))
	 ;(guard (symbol? v) (andmap symbol? flds))
	 (let ([tmp (unique-name 'tmp)])
           `(smap (lambda (,tmp) 
		  (tuple ,@(map (lambda (fld) `(app ,fld ,tmp)) flds)))
		,v))]
	
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
