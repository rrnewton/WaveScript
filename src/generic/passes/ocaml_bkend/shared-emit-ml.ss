

(module shared-emit-ml mzscheme 
  (require  "../../../plt/common.ss"
	    "../../compiler_components/c_generator.ss" )
  (provide sharedEmitCases
	   make-dispatcher
	   make-seq make-tuple)
  (chezprovide )  
  (chezimports (except helpers test-this these-tests))

;; This is used to bundle up the methods we want the parent to use.
(define-syntax make-dispatcher
  (syntax-rules (else)
    [(_ exp syms ...) 
     (let ([x exp])
       (case x [(syms) syms] ... 
	   [else (error 'make-dispatcher "unmatched: ~s" x)]))]))

;;; Manufacturing syntax.
;;;
;;; (These constructs are the same between Caml & SML.)

(define (make-seq . exprs) (list "(" (insert-between ";\n" exprs) ")"))
(define (make-tuple . args)  (list "(" (insert-between ", " args) ")"))

#;
(define (with-fun-binding name formals funbody body)
  (make-letrec `([,name ,(make-fun formals funbody)]) body))

; ======================================================================
;; Expressions.

(define sharedEmitCases
  (lambda (obj)
    
    (define Var (curry obj 'Var))
    
    ;; .param exp      The expression to process.	
    ;; .param emitter  Function that generates the emit code, given an argument.
    (define Expr ;(Expr tenv)
      (lambda (exp emitter)
	(match exp
	  [,v (guard (symbol? v) (regiment-constant? v)) (obj 'Const v)]
	  [,v (guard (symbol? v)) (obj 'Var v)]
	  [',c (obj 'Const c)]

	  [(assert-type (Sigseg ,elt) nullseg)    (obj 'DispatchOnArrayType 'nullseg elt)]
	  [(assert-type (Array ,elt) Array:null)  (obj 'DispatchOnArrayType 'Array:null elt)]

	  [(tuple ,[rands] ...)   (apply make-tuple rands)]
	  [(tupref ,i ,len ,[v])
	   (let ([pat 
		  (apply make-tuple
			 (append (make-list i "_") '("x")			
				 (make-list (- len i 1) "_")))])
	     (obj 'make-let `((,pat ,v)) "x"))]
	  
	  [(let ([,[Var -> v] ,ty ,[rhs]]) ,[bod])
	   (obj 'make-let `((,v ,rhs)) bod)]
	  [(begin ,[e*] ...)  (indent (apply make-seq e*) "  ")]
	  [(emit ,vq ,[x]) (emitter x)]
	  [(set! ,[Var -> v] ,[e])  `("(",v " := " ,e")")]
	  [(if ,[t] ,[c] ,[a])   `("(if ",t"\nthen ",c"\nelse ",a")\n")]

	  ;; This differs in ocaml/sml.
	  [(for (,i ,[st] ,[en]) ,[bod])
	   (obj 'make-for (Var i) st en bod)]
	  [(while ,[tst] ,[bod]) `("(while ",tst" do\n ",bod"\ndone)")]
	  [(,prim ,rand* ...) (guard (regiment-primitive? prim))
	   (obj 'Prim (cons prim rand*) emitter)]
	  [(assert-type ,t (,prim ,rand* ...)) (guard (regiment-primitive? prim))
	   (obj 'Prim `(assert-type ,t (,prim . ,rand*)) emitter)]

	  [(assert-type ,t ,[x]) 
					;(printf "MISC ASCRIPTION: ~s on ~s\n" t x)
	   x]
	  [,unmatched (error 'emit-mlton:Expr "unhandled form ~s" unmatched)]

	  )))


    (define Iterate
      (lambda (iter)
	(match iter
	  [((name ,name) (output-type ,ty) 
	    (code (let ([,lhs* ,ty* ,rhs*] ...)
		    (lambda (,x ,vq) (,ty1 ,ty2) ,bod)))
	    (incoming ,up)
	    (outgoing ,down* ...))

	   ;;(if (null? down*) (inspect (vector "huh? why null?" name up down* bod)))
	   (let* ([emitter (Emit down*)])
	     `(" (* WS type: input:",(format "~a" ty1)" vq:",(format "~a" ty2)" -> ",(format "~a" ty)" *)\n"
	       " ",(Var name)" = \n"
	       ;; First we bind the iterator state.
	       ,(obj 'make-let `([,(Var vq) #() "()"]
			    ,@(map (lambda (lhs ty rhs) 
				     (list (Var lhs) ty (Expr rhs emitter)))
				lhs* ty* rhs*))
			  ;; Then we bind the actual function:

			  ;; TODO: We should really just pull the iterator state to the top of the program.
			  ;; Then we don't need to have this internal letrec here:


		     (list
		      (obj 'make-let `([,(Var name) 
				     ,(obj 'make-fun (list (list "("(Var x)" : "(obj 'Type ty1)")"))
					   (indent (Expr bod emitter) "    "))])
			   (Var name))
		      "\n")

#;			  
			  (list (with-fun-binding 
				 
				 
				 (Var name))
				"\n"
				))))])))


    ;; Generates code for an emit.  (curried)
    ;; .param down*   A list of sinks (names of functions) to emit to.
    ;;                These can also be (NUM NAME) pairs for indexed ports.
    (define (Emit down*)
      ;;(ASSERT (not (null? down*)))
      (lambda (expr)
	;; Just call each of the sites with the argument.
	(obj 'make-let `([emitted ,expr])
		  (apply make-seq
			 (append 
			  (map (lambda (down) 
				 (cond 
				  [(eq? down 'BASE) `("baseSink emitted")]
				  ;; If we're emitting *to* a unionN, we include the index tag.
				  [(pair? down)
				   (ASSERT fixnum? (car  down))
				   (ASSERT symbol? (cadr down))
				   
				   (ASSERT (= (length down) 2))
				   (list (Var (cadr down))
					 "("(obj 'Const (car down))", emitted )")]
				  [else 
				   (ASSERT symbol? down)
				   `(,(Var down)" emitted")]))
			    down*)
			  '("()")))
		  )))
        
    ;; Return a bundle of methods:
    (values Expr Iterate Emit)
    ))



)

