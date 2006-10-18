;;;; .title Pass Mechanism
;;;; .author Ryan Newton
;;;; 
;;;; This file declares "define-pass" which is a super-sweet syntactic
;;;; sugar for concisely defining pass-transformations.  It lets you
;;;; optionally provide only the cases you need.

(module pass-mechanism mzscheme 
  (require "../../plt/iu-match.ss"
           "../../plt/chez_compat.ss"
           (only "../constants.ss" chezimports ASSERT))
  (provide define-pass)
  (chezimports)

;; Usage:
;;   (define-syntax <pass-name> <clauses and defs> ...)
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
;;
;;   [Fuser <fun: [intermediate ...], <fun: [intermediate ...] -> expr> -> intermediate>]
;;      This form takes a result-fusing procedure in the style of core-generic-traverse.
;;
;;   [Bindings <fun: vars types exprs reconstr exprfun fallthroughfun -> intermediate>]
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
;;         fallthroughfun: Function to continue processing the expression in question with the auto-traverser.
;;
(define-syntax define-pass
  (lambda (x)
    (syntax-case x ()
      [(_ name clauses ...)
       (let ([gen-code 
	      (lambda (ingram outgram bnds expr prog fuser extra-defs)
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

		(with-syntax ([(i o b e p f inspec outspec (extra-defs ...))
			       (list ingram outgram bnds expr prog fuser
				     inspec outspec extra-defs)])
		  ;; Now we plug in defaults:
		  #;
		  (if (and prog (or bnds expr))
		      (error 'define-pass "cannot define both Program and Bindings/Expr: ~s" 
			     #'_))

		  #`(define name 
		      (let () 
			extra-defs ...
			;; Jeez, this is lame but just so the compiler associates the name with the closure:
			(letrec* ([name (lambda (prog) (tmp prog))]
				  [process-expr (core-generic-traverse e f)]
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
		    [expr #f]
		    [prog #f]
		    [fuser #f]
		    [extras '()])
	   (if (null? cl)
	       (gen-code ingram outgram bnds expr prog fuser (reverse extras))
	       (syntax-case (car cl) (InputGrammar OutputGrammar Bindings Expr Program)
			    [(InputGrammar g)  (loop (cdr cl) #'g    outgram bnds expr prog fuser extras)]
			    [(OutputGrammar g) (loop (cdr cl) ingram #'g     bnds expr prog fuser extras)]
			    [(Bindings f)      (loop (cdr cl) ingram outgram #'f  expr prog fuser extras)]
			    [(Expr f)          (loop (cdr cl) ingram outgram bnds #'f  prog fuser extras)]
			    [(Program f)       (loop (cdr cl) ingram outgram bnds expr #'f  fuser extras)]
			    [(Fuser f)         (loop (cdr cl) ingram outgram bnds expr prog #'f   extras)]
			    [else (loop (cdr cl) ingram outgram bnds expr prog fuser 
					(cons (car cl) extras))]
			    ))))
       ])))


) ;; End module.
