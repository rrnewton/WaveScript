
;===============================================================================
;;;;      ---- Pass Lift Let  ---- 
;;;;
;;;; .author Ryan Newton

;===============================================================================

(module ws-lift-let mzscheme
  (require "../../../plt/common.ss"
	   "ws-remove-complex-opera.ss")
  (provide ws-lift-let
	   ws-lift-let-grammar
           )
  (chezimports)

;; This is a bit complex because it programmatically splits out the stream-primitives.
(define ws-lift-let-grammar
  (let ([streamprims (map car wavescript-stream-primitives)])
    
    (append `(;; [2007.04.01] Adding union-types to this:
	      ;; The LHS should only have typevars... but we don't have a special production for that:
	      (Program ((quote program) Query ('union-types ((Var Type ...) [Var Type] ...) ...) Type))

	      (Query Var)
	      (Query ('let ((LHS Type Query) ...) Query))
	      (Query ('iterate ('let ((LHS Type Block) ...)
				 ('lambda (Var Var) (Type Type) Block)) Simple))
	      (Query ('unionN Simple ...))
	      (Query (StreamOp Simple ...))

	      (Simple Var)
	      (Simple Const)
	      (Simple ('tuple))

	      (Const ('quote Datum))
	      (Datum ,atom?)
	      
	      (ComplexConst ('quote ComplexDatum))
	      ;; This is kind of lame:
	      (ComplexConst ('cons ComplexConst ComplexConst))
	      (ComplexConst ('foreign Const ComplexConst ComplexConst))
	      ;; This is constant for the duration of the stream query
	      (ComplexConst AppConstructs)
	      (ComplexConst Var)

	      (ComplexDatum ,atom?)
	      (ComplexDatum (ComplexDatum ...))

	      (Query ComplexConst)

	      (Value Var) 
	      (Value Const)
	      (Value ('tuple Simple ...))
	      (Value ('tupref Int Int Simple))
	      (Value ('if Simple Block Block))
	      (Value ('wscase Simple (Var ('lambda (Var ...) (Type ...) Block)) ...))
	      (Value (Prim Simple ...))
	      (Value AppConstructs)

	      (AppConstructs ('foreign-app Const Var Simple ...))
	      (AppConstructs ('construct-data Var Simple ...))

	      (Value 'BOTTOM)
	      (Value ComplexConst)

	      ;; Some frustrating duplication here:
	      (Block Simple)
	      (Block ('let ((LHS Type Value) ...) Block))
	      (Block ('begin Block ...))
	      (Block ('for (Var Simple Simple) Block))
	      (Block ('while Block Block))
	      (Block ('set! Var Simple))
	      (Block ('if Simple Block Block))
	      (Block ('wscase Simple (Var ('lambda (Var ...) (Type ...) Block)) ...))
	      (Block ('break))
	      (Block (EffectPrim Simple ...))
	      (Block AppConstructs)


	      (Simple ('assert-type Type Simple))
	      (Value ('assert-type Type Value))
	      (Query ('assert-type Type Query))
	      (Block ('assert-type Type Block))
	       
	       )
	    (map (lambda (x)
		   (match x 
		     [(Prim ',name) (guard (memq name streamprims)) `(StreamOp ',name)]
		     [(Prim ',name) (guard (assq name wavescript-effectful-primitives)) `(EffectPrim ',name)]
		     [,oth oth]))
	      (filter (lambda (x) (not (memq (car x) '(Expr Program LetOrSimple Datum Const))))
		ws-remove-complex-opera*-grammar))
	     )))

;; This doesn't allow let's in RHS position.  It lifts them out.  This
;; expands the scope of the lifted bindings, and thus depends on
;; unique variable names. 
(define-pass ws-lift-let
    (define (make-begin . expr*)
      (match (match `(begin ,@expr*)
	       [(begin ,[expr*] ...) (apply append expr*)]
	       [,expr (list expr)])
	[(,x) x]
	[(,x ,x* ...) `(begin ,x ,x* ...)]))

    (define process-expr
      (lambda (x fallthru)
	(match x


	  ;; Ascriptions are redundant right within the RHS:
	  ;; But we leave them for primitives:
	  [(let ([,v ,ty (assert-type ,ty3 (,prim ,[rand*] ...))]) ,[bod])
	   (guard (regiment-primitive? prim))
	   `(let ([,v ,ty (assert-type ,ty3 (,prim . ,rand*))]) ,bod)]
	  ;; A 'constant' primitive:
	  [(let ([,v ,ty (assert-type ,ty3 ,prim)]) ,[bod])
	   (guard (regiment-primitive? prim))
	   `(let ([,v ,ty (assert-type ,ty3 ,prim)]) ,bod)]
	  ;; And for quoted constants:
	  [(let ([,v ,ty (assert-type ,ty3 (quote ,c))]) ,[bod])
	   `(let ([,v ,ty (assert-type ,ty3 (quote ,c))]) ,bod)]

	  ;; For any other RHS we remove because the ascription might obscure let-lifting:
	  [(let ([,v ,ty (assert-type ,ty3 ,rhs)]) ,bod)
	   (process-expr `(let ([,v ,ty ,rhs]) ,bod)  fallthru)]

	  ;; Do lifts:
	  [(let ([,v ,ty (let ([,v2 ,ty2 ,e2]) ,rhs)]) ,bod)
	   (process-expr 
	    `(let ([,v2 ,ty2 ,e2]) (let ([,v ,ty ,rhs]) ,bod))
	    fallthru)]

#;
	  ;; This is annoying, but it's the same thing:
	  ;; (By the way, this is one reason it would be nice to have
	  ;; the ascription as an implicit expression *TAG* rather
	  ;; than as its on expression variant.)
	  [(let ([,v ,ty (assert-type ,ty3 (let ([,v2 ,ty2 ,e2]) ,rhs))]) ,bod)
	   (process-expr 
	    `(let ([,v2 ,ty2 ,e2]) (let ([,v ,ty (assert-type ,ty3 ,rhs)]) ,bod))
	    fallthru)]
	  ;; Lifting out begins too.
	  [(let ([,v ,ty (begin ,e1 ,e2 ,rest ...)]) ,bod)
	   (process-expr 
	    (make-begin e1 `(let ([,v ,ty ,(apply make-begin (cons e2 rest))]) ,bod))
	    fallthru)]
	  ;; Otherwise can't lift here:
	  [(let ([,v ,ty ,[rhs]]) ,[bod]) `(let ([,v ,ty ,rhs]) ,bod)]
	  [(let . ,_) (error 'ws-lift-let "unhandled let: ~s" `(let . ,_))]


	  [(begin ,[e])  e]
	  [(iterate (let ([,v* ,ty* ,[rhs*]] ...) (lambda (,x ,y) (,tyx ,tyy) ,[bod])) ,[strm])
	   `(iterate (let ([,v* ,ty* ,rhs*] ...) (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,strm)]
	  [,oth (fallthru oth)])))
    ;; Assumes lets only bind one variable (except for iterates)
    [Expr process-expr]
    [Program (lambda (p E)
	       (match p 
		 [(,lang '(program ,[E -> bod] ,meta* ... ,ty))
		  ;; Ensure that there's a 'union-types' entry in the output:
		  (let ([uniondefs (or (assq 'union-types meta*) '(union-types))])
		    ;`(,lang '(program ,bod ,uniondefs ,(remq uniondefs meta*) ... ,ty))
		    `(,lang '(program ,bod ,uniondefs ,ty))
		    )
		  ]))]

    [OutputGrammar ws-lift-let-grammar])

) ;; End module
