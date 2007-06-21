

(module shared-emit-ml mzscheme 
  (require  "../../../plt/common.ss"
	    "../../compiler_components/c_generator.ss" )
  (provide protoExpr
	   make-seq make-app make-tuple)
  (chezprovide )  
  (chezimports (except helpers test-this these-tests))

;;; Manufacturing syntax.
;;;
;;; (These constructs are the same between Caml & SML.)

(define (make-seq . exprs) (list "(" (insert-between ";\n" exprs) ")"))
(define (make-app rator rands) (list "(" rator " "(insert-between " " rands) ")"))
(define (make-tuple . args)  (list "(" (insert-between ", " args) ")"))

; ======================================================================
;; Expressions.

(define protoExpr
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

      [(for (,i ,[st] ,[en]) ,[bod])
       `("(for ",(Var i)" = ",st" to ",en" do\n ",bod"\n done)")]
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
    
    Expr))

#;
(define Expr ;(Expr tenv)
  (lambda (exp emitter)
    (match exp
      [,v (guard (symbol? v) (regiment-constant? v)) (Const v)]
      [,v (guard (symbol? v)) (Var v)]
      [',c (Const c)]

      [(assert-type (Sigseg ,elt) nullseg)    (DispatchOnArrayType 'nullseg elt)]
      [(assert-type (Array ,elt) Array:null)  (DispatchOnArrayType 'Array:null elt)]

      [(tuple ,[rands] ...)   (apply make-tuple rands)]
      [(tupref ,i ,len ,[v])
       (let ([pat 
	      (apply make-tuple
		     (append (make-list i "_") '("x")			
			     (make-list (- len i 1) "_")))])
	 `("(let ",pat" = ",v" in x)\n"))]
      
      [(let ([,[Var -> v] ,ty ,[rhs]]) ,[bod])
       `("(let ",v" = ",rhs" in\n ",bod")")]
      [(begin ,[e*] ...)  `("begin\n" ,@(indent (insert-between ";\n" e*) "  ") "\nend")]
      [(emit ,vq ,[x]) (emitter x)]
      [(set! ,[Var -> v] ,[e])  `("(",v " := " ,e")")]
      [(if ,[t] ,[c] ,[a])   `("(if ",t"\nthen ",c"\nelse ",a")\n")]

      ;; This is a really lame hack for now... emulating "break":
      [(for (,i ,[st] ,[en]) ,[bod])
       `("(for ",(Var i)" = ",st" to ",en" do\n ",bod"\n done)")]
      ;[(break) "(broke := true)"]
      [(while ,[tst] ,[bod]) `("(while ",tst" do\n ",bod"\ndone)")]


      [(,prim ,rand* ...) (guard (regiment-primitive? prim))
       (Prim (cons prim rand*) emitter)]
      [(assert-type ,t (,prim ,rand* ...)) (guard (regiment-primitive? prim))
       (Prim `(assert-type ,t (,prim . ,rand*)) emitter)]

      [(assert-type ,t ,[x]) 
       ;(printf "MISC ASCRIPTION: ~s on ~s\n" t x)
       x]
      [,unmatched (error 'emit-caml:Expr "unhandled form ~s" unmatched)]

)))


)

