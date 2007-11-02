

(module shared-emit-ml mzscheme 
  (require  "../../../plt/common.ss"
	    "../../compiler_components/c_generator.ss" )
  (provide sharedEmitCases
	   make-dispatcher
	   make-seq make-tuple-code
	   real-primitive?)
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

;; This is a "begin" statement.
(define (make-seq . exprs) (list "(" (insert-between ";\n" exprs) ")"))

;; A tuple expression.
(define (make-tuple-code . args)  (list "(" (insert-between ", " args) ")"))

#;
(define (with-fun-binding name formals funbody body)
  (make-letrec `([,name ,(make-fun formals funbody)]) body))

;; Should be used everywhere below rather than "regiment-primitive?"
(define (real-primitive? x) (and (regiment-primitive? x) (not (eq? x 'tuple))))

; ======================================================================
;;; Handling different AST variants:

;; This is the "parent class" that contains methods shared between the
;; Caml and MLton backends.
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

	  [(tuple ,[rands] ...)   (apply make-tuple-code rands)]
	  [(tupref ,i ,len ,[v])
	   (let ([pat 
		  (apply make-tuple-code
			 (append (make-list i "_") '("x")			
				 (make-list (- len i 1) "_")))])
	     (obj 'make-let `((,pat ,v)) "x"))]
	  
	  [(let ([,[Var -> v] ,ty ,[rhs]]) ,[bod])
	   (obj 'make-let `((,v ,ty ,rhs)) bod)]
	  [(begin ,[e*] ...)  (indent (apply make-seq e*) "  ")]
	  [(emit ,vq ,[x]) (emitter x)]
	  [(set! ,[Var -> v] ,[e])  `("(",v " := " ,e")")]
	  [(if ,[t] ,[c] ,[a])   `("(if ",t"\nthen ",c"\nelse ",a")\n")]

	  ;; This differs in ocaml/sml.
	  [(for (,i ,[st] ,[en]) ,[bod])
	   (obj 'make-for (Var i) st en bod)]
	  [(while ,[tst] ,[bod]) (obj 'make-while tst bod)]


;	  [(foreign-app . ,x) (obj 'ForeignApp x)]
	  [(foreign-app ',realname (assert-type ,type ,rator) ,[rand*] ...) (obj 'ForeignApp realname type rator rand*)]
;	  [(foreign-app ',realname (assert-type ,type ,rator) ,[rand*] ...) 
;	   (make-app (symbol->string rator) rand*)]

	  [(__foreign ',cname ',files ',ty) 
	   (obj 'ForeignEntry cname files ty)]
	  [(__foreign . ,_) (error 'emit-mlton "missed __foreign entry: ~s" (cons '__foreign _))]

	  ;; TODO: FOREIGN_SOURCE

     ;; strip out annotations
     [(,prim ,annot ,rand* ...)
      (guard (and (real-primitive? prim) (pair? annot) (eq? 'annotations (car annot))))
      (obj 'Prim (cons prim rand*) emitter)]

	  [(,prim ,rand* ...) (guard (real-primitive? prim))
	   (obj 'Prim (cons prim rand*) emitter)]
	  ;; Here we leave type assertions on all primitives.
	  [(assert-type ,ty (,prim ,rand* ...)) (guard (real-primitive? prim))
	   (obj 'Prim `(assert-type ,ty (,prim . ,rand*)) emitter)]

	  [(wscase ,x [,name* ,fun*] ...)
	   (obj 'WScase x name* fun* emitter)]
	  [(construct-data ,name ,[arg]) 
	   (obj 'make-app (obj 'VariantName name) (list arg))]
	  
	  ;; All non-primitives lose their assertions:
	  [(assert-type ,t ,[x])
           ;;(printf "MISC ASCRIPTION: ~s on ~s\n" t x)
	   x]
	  [,unmatched (error 'emit-mlton:Expr "unhandled form ~s" unmatched)]

	  )))



    ;; Updated this to return all iterator state so it can be aggregated at the top level.
    ;; Returns two values: code and bindings.
    (define Operator
      (lambda (entry)
        (match entry

          [(iterate (name ,name) 
                    (output-type ,ty)
                    (code (iterate ,annot ,itercode ,_))
                    (incoming ,up)
                    (outgoing ,down* ...))
	   
	   (define (loop itercode)
	     (match itercode 
	       [(let ([,lhs* ,ty* ,rhs*] ...) (lambda (,x ,vq) (,ty1 ,ty2) ,bod))
		(let* ([emitter (Emit down*)])
		  (values
		   ;; The first return value is binding-text for the function:
		   `(" (* WS type: input:",(format "~s" ty1)" vq:",(format "~a" ty2)" -> ",(format "~a" ty)" *)\n"
		     ,(obj 'make-fun-binding name 
			   (list (list "("(Var x)
				       (maybe-ascribe ty1)
				       ;;(list " : "(obj 'Type ty1))
				       ")"))
			   (indent (Expr bod emitter) "    ")))

		   ;; The second return value is a list of bindings for iterator state:
		   (map make-bind 
		     `([,(Var vq) #() "()"]
		       ,@(map (lambda (lhs ty rhs) 
				(list (Var lhs) ty (Expr rhs emitter)))
			   lhs* ty* rhs*)))
		   ))]
	       [(lambda (,x ,vq) (,ty1 ,ty2) ,bod)
		(warning 'emit-ml "got iterate without state-let, standardize-letrec failed")
		(loop `(let () (lambda (,x ,vq) (,ty1 ,ty2) ,bod)))
		]))
	   (loop itercode)]

	  [(iterate . ,_) (inspect (cons "EMIT ML UNMATCHED ITERATE" (cons 'iterate _)))]
	  
	  [(__readFile (name ,name) (output-type ,ty) (code ,c)
                  (incoming ,up* ...) (outgoing ,down* ...))
	   (values (obj 'ReadFile name c up* down*) ())]

	  [(unionN (name ,name) (output-type ,ty) (code ,_) (incoming ,up* ...) (outgoing ,down* ...))
	   (let ([emitter (Emit down*)])
	     (values (obj 'make-fun-binding name '("x")((Emit down*) "x")) ()))]
    
	  ;; For MLTON, this could probably be implemented on top of unionList/unionN with no loss of efficiency.
	  [(_merge (name ,name) (output-type ,ty) (code ,_) (incoming ,astrm ,bstrm) (outgoing ,down* ...))
	   (values 
	    (list "\n  (* Merge operator: *)\n"
		  (obj 'make-fun-binding name '("x") ((Emit down*) "x") ))
	    ())]

	  ;; These just fizzle for mlton... passes data through.

	  [(,gnuplot (name ,name) (output-type ,ty)
		     (code ,_) (incoming ,up)  (outgoing ,down* ...))
	   (guard (memq gnuplot '(gnuplot_array_stream gnuplot_sigseg_stream
 			          gnuplot_array_stream2d gnuplot_sigseg_stream2d)))
	   (values 
	    (list "\n  (* Gnuplot operator (does nothing currently): *)\n"
		  (obj 'make-fun-binding name '("x") ((Emit down*) "x") ))
	    ())]


	  
	  [(spawnprocess (name ,name) (output-type ,ty)
			 (code (spawnprocess ,cmd ,__))
			 (incoming ,up)  (outgoing ,down* ...))
	   (let ([proc (Var (unique-name "process"))]
		 [emitter (Emit down*)])
	     (values
	      (list "\n  (* Spawnprocess operator: *)\n"
		    (obj 'make-fun-binding name '("x") (obj 'make-app proc (list "x")) ))
	      (list 
	       (make-bind `[,proc (String -> #())
				  ,(obj 'make-app "spawnprocess" 
					(list (Expr cmd emitter)))])))
	     )]


	  )))

    ;; Generates code for an emit.  (curried)
    ;; .param down*   A list of sinks (names of functions) to emit to.
    ;;                These can also be (NUM NAME) pairs for indexed ports.
    (define (Emit down*)
      ;;(ASSERT (not (null? down*)))
      (lambda (expr)
	;; Just call each of the sites with the argument.
	(obj 'make-let `(["emitted" ,expr])
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
        

    (define (coerce-id x) (if (symbol? x) (Var x) x))

    ;; Simpler than make-let... just produces the "v = rhs" text.
    ;; This can then be packaged differently to make lets, letrecs, etc.
    (define (make-bind bind)
      (match bind
	[[,lhs ,rhs]     (list (coerce-id lhs) " = " rhs)]
	;; Type is a sexp or a string:
	[[,lhs ,ty ,rhs] (list (coerce-id lhs)
			       (maybe-ascribe ty)
			       " = " rhs)]
			       ))

    ;; Helper for ascription:
    (define (maybe-ascribe ty)
      (if (and (mlton-ascribe-types)
	       (when (polymorphic-type? ty)
		 (warning 'mlton-ascribe-types "Got polymorphic type! ~s" ty)
		 #f))
	  (list " : " (if (string? ty) ty (obj 'Type ty)))
	  ""))       

    ;; Return a bundle of methods:
    (values Expr Operator Emit make-bind)
    ))

)

