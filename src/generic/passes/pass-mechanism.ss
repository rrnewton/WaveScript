;;;; .title Pass Mechanism
;;;; .author Ryan Newton
;;;; 
;;;; This file declares "define-pass" which is a super-sweet syntactic
;;;; sugar for concisely defining pass-transformations.  It lets you
;;;; optionally provide only the cases you need.

(module pass-mechanism mzscheme 
  (require "../../plt/iu-match.ss"
           "../../plt/chez_compat.ss"
           "../util/helpers.ss"
           (only "../constants.ss" chezimports ASSERT DEBUGASSERT)
           "../compiler_components/regiment_helpers.ss"
           "../compiler_components/reg_core_generic_traverse.ss"
           )

  (provide define-pass)
  (chezimports)
  (require-for-syntax "../../plt/chez_compat.ss"
                      ;(only "../util/helpers.ss" compose)
                      "../compiler_components/reg_core_generic_traverse.ss")

;; Usage:
;;   (define-pass <pass-name> <clauses and defs> ...)
;; Clauses can be:
;;   [InputGrammar <g>]
;;   [OutputGrammar <g>]
;;      These define input/output grammars for the pass in the style of grammar_checker.ss.
;;      These grammars are checked only in debugmode. See the obsoleted build-pass function.
;;
;;   [Program <fun: prog, ExprFun -> pass-result>]
;;      This form takes a user procedure to handle the top-level input to the pass, the "program".
;;
;;   [Expr <fun: expr, FallthroughFun -> intermediate>]
;;      This form takes an expression-handling procedure in the style of core-generic-traverse.
;;   [Expr/Types <fun: expr, type-env, FallthroughFun -> intermediate>]
;;      This form takes an expression-handling procedure in the style of core-generic-traverse/types.
;;      Cannot be used with Expr, or Bindings.
;;
;;   [Fuser <fun: [intermediate ...], <fun: [intermediate ...] -> expr> -> intermediate>]
;;      This form takes a result-fusing procedure in the style of core-generic-traverse.
;;
;;   [Bindings <fun: vars types exprs reconstr exprfun -> intermediate>]
;;      UNFINISHED:
;;
;;      This has a complex interface, but allows abstraction over
;;      *all* variable binding forms in the Regiment/Wavescript
;;      language.  The user function is stuck *between* the "Expr"
;;      function and the auto-traversal function provided by
;;      core-generic-traverse.  That is, for a given expression, the
;;      "Expr" function gets the first shot at it.  If it passes the
;;      expression on to its "continuation", then the "Bindings"
;;      function gets a shot.  (The bindings function will catch any
;;      binding form.)  And finally, if the expression is not a
;;      binding form it falls through to the auto-traversal function.
;;
;;      The user function provided takes six arguments:
;;         vars:  The variables bound within this scope.
;;         types: The types of those variables.
;;         exprs: The expressions within the scope.
;;         reconstr:       Function to reconstruct a binding expression from vars, types, exprs.
;;         exprfun:        Entrypoint to the complete expression-handling procedure.  (For use on subexpressions.)
;;
(define-syntax define-pass
  (lambda (x)
    (syntax-case x ()
      [(_ name clauses ...)
       (let ([gen-code 
	      (lambda (ingram outgram bnds expr/types expr prog fuser extra-defs)

		(define output-language 
		  (datum->syntax-object #'name
		   (string->symbol 
		    (string-append (symbol->string (syntax-object->datum #'name)) "-language"))))
		(define inspec (if ingram 
				   (with-syntax ([ig ingram]) #'`(input (grammar ,ig PassInput)))
				   #''(input)))
		(define outspec (if outgram 
				    (with-syntax ([og outgram]) #'`(output (grammar ,og PassInput)))
				    #''(output)))
		(define gentrav
		  (begin 
		    (if (and expr expr/types)
			(syntax-error (format "Cannot have both Expr and Expr/Types clause in define-pass:\n ~s" #'_)))
		    (if expr/types
			#'core-generic-traverse/types
			#'core-generic-traverse)))

		(define dso datum->syntax-object)

		;; Fill in some defaults:
		(set! expr (or expr expr/types #'(lambda (x fallthrough) (fallthrough x))))
		(set! fuser (or fuser #'(lambda (ls k) (apply k ls))))
		(unless prog 
		  (set! prog (with-syntax ([ol output-language])
			       #'(lambda (pr Expr) 
				   (match pr
				     [(,input-language (quote (program ,body ,type)))
				      `(ol '(program ,(Expr body) ,type))]
				     [,other (error 'name "\nBad pass input:\n   ~s\n" other)])
				   ))))

		;; If the user provides a function for variable bindings, we
		;; paste that together with the "Expr" function.
		;; [2006.11.01] Changed this to not use quasisyntax
		(if bnds 
		    (set! expr
		    (with-syntax ([expr expr] 
				  [fuser fuser]
				  [funbody bnds]
				  [ellipses (datum->syntax-object #'_ '...)]
				  )
		      #'(lambda (x fallthrough)
			  (define fuse fuser)
			  (expr x 
				;; We stick the varbinds handling between the user code and the automatic looping.
				(lambda (x)
				  ;; Now we set up the varbinds and give them to the userfun.
				  (let ([fun funbody])
				    (match x

				      [(lambda ,vars ,types ,bod)
				       (fun vars types (list bod) 
					    ;; Reconstruct function:
					    (lambda (vars types results)
					      (fuser results
						     (lambda (bod)			
						       `(lambda ,vars ,types ,bod)
						       )))
					    ;; Expression function:
					    process-expr)]

				      [(letrec ,binds ,body)
				       (guard (andmap (lambda (ls) (= (length ls) 3)) binds))
				       (let ([vars (map car binds)]
					     [types (map cadr binds)]
					     [rhs*  (map caddr binds)])
					 (fun vars types (cons body rhs*)
					      (lambda (vars types exprs)
						(fuser exprs
						       (lambda (bod . rhs*)
							 `(letrec ,(map list vars types rhs*) ,bod)
							 )))
					      process-expr))]

				      [(let ,binds ,body)
				       (guard (andmap (lambda (ls) (= (length ls) 3)) binds))
				       (let ([vars (map car binds)]
					     [types (map cadr binds)]
					     ;; We do the RHSs because the user doesn't get them:
					     [rhs*  (map process-expr (map caddr binds))])
					 (fun vars types (list body)
					      (lambda (vars types results)
						(DEBUGASSERT (= 1 (length results)))
						(fuser (append results rhs*)
						       (lambda (bod . newrhs*)
							 (DEBUGASSERT (= (length newrhs*) (length rhs*)))
							 `(let ,(map list vars types newrhs*)
							    ,bod))))
					      process-expr))]

				      [(for (,i ,[process-expr -> st] ,[process-expr -> en]) ,bod)
				       (fun (list i) '(Int) (list bod)
					    (lambda (vars types results)
					      (ASSERT (equal? types '(Int)))
					      (fuser (list st en (car results))
						     (lambda (st en bod) 
						       `(for (,(car vars) ,st ,en) ,bod))))
					    process-expr)]

				      ;; To catch errors:
				      [(,head ,other ellipses)
				       (guard (memq head '(for letrec let let* lazy-letrec lambda)))
				       (error 'pass-mechanism:Bindings 
					      "unhandled or badly formed binding syntax: ~s" 
					      (cons head other))]

				      ;; TODO: for, ... anything else?
				      
				      ;; If it's not a variable-binding construct, we just let it 
				      ;; go through to the final, auto-looping fallthrough:
				      [,other 
				       (ASSERT (compose not binding-form?) other)
				       (fallthrough other)]
				      ))))))))






		(with-syntax ([(i o b e p f inspec outspec (extra-defs ...) gentrav)
			       (list ingram outgram bnds expr prog fuser
				     inspec outspec extra-defs gentrav)])
		  ;; Now we plug in defaults:
		  #;
		  (if (and prog (or bnds expr))
		      (error 'define-pass "cannot define both Program and Bindings/Expr: ~s" 
			     #'_))

		  #'(define name 
		      (let () 
			extra-defs ...
			;; Jeez, this is lame but just so the compiler associates the name with the closure:
			(letrec ([name (lambda (prog) (tmp prog))]
				  [process-expr (gentrav e f)]
				  [tmp (build-compiler-pass 
					'name 
					inspec 

					outspec 
					(lambda (x) (p x process-expr)))])
				 name
				 )))

		))])
	 ;; Here are all the default values for these components:
	 (let loop ([cl (syntax->list #'(clauses ...))]
		    [ingram #f]
		    [outgram #f]
		    [bnds #f]
		    [expr/types #f]
		    [expr #f]
		    [prog #f]
		    [fuser #f]
		    [extras '()])
	   #|
	   (let ([tf (lambda (x) (if x #t #f))])
	     (printf "loop ~s ~s ~s ~s e/t:~s e:~s ~s ~s ~s\n"
		     (length cl)
		     (tf ingram) (tf outgram) (tf  bnds) (tf expr/types) (tf expr) (tf prog) (tf fuser) (tf  extras)))
	   |#

	   (if (null? cl)
	       (gen-code ingram outgram bnds expr/types expr prog fuser (reverse extras))
	       (syntax-case (car cl) (InputGrammar OutputGrammar Bindings Expr/Types Expr Program Fuser)
			    [(InputGrammar g)  (loop (cdr cl) #'g    outgram bnds expr/types expr prog fuser extras)]
			    [(OutputGrammar g) (loop (cdr cl) ingram #'g     bnds expr/types expr prog fuser extras)]
			    [(Bindings f)      (loop (cdr cl) ingram outgram #'f  expr/types expr prog fuser extras)]
			    [(Expr/Types f)    (loop (cdr cl) ingram outgram bnds #'f        expr prog fuser extras)]
			    [(Expr f)          (loop (cdr cl) ingram outgram bnds expr/types #'f  prog fuser extras)]
			    [(Program f)       (loop (cdr cl) ingram outgram bnds expr/types expr #'f  fuser extras)]
			    [(Fuser f)         (loop (cdr cl) ingram outgram bnds expr/types expr prog #'f   extras)]
			    [else (loop (cdr cl) ingram outgram bnds expr/types expr prog fuser 
					(cons (car cl) extras))]
			    ))))
       ])))


) ;; End module.

;(require pass-mechanism)
