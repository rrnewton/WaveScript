;;;; .title Pass: Deglobalize
;;;; April 2006

;;;; I am making a new, much simplified deglobalize.
;===============================================================================


;;;; Input grammar:
;;;; NOTE: This grammar is too loose.

;;;; <Pgm>  ::= <Let>
;;;; <Let>  ::= (lazy-letrec ([<Var> <Type> <Exp>]*) <Var>)
;;;; <Exp>  ::= <Simple>
;;;;          | (if <Simple> <Simple> <Simple>)
;;;;          | (lambda (<Var>*) (<Type>*) <Let>)
;;;;          | (<primitive> <Simple>*)
;;;; <Simple> ::= (quote <Lit>) | <Var>


;  ======================================================================

#|
(module pass20_deglobalize2 mzscheme

  (require (lib "include.ss")
	   (lib "trace.ss")
	   "../generic/constants.ss"
           "../plt/iu-match.ss"
           "../plt/hashtab.ss"
	   "../plt/prim_defs.ss"
           (all-except "../plt/hm_type_inference.ss" test-this these-tests)
           (all-except "../plt/tsort.ss" test-this these-tests)
           (all-except "../plt/tml_generic_traverse.ss" test-this these-tests)
           (all-except "../plt/helpers.ss" test-this these-tests)
           (all-except "../plt/regiment_helpers.ss" test-this these-tests))
  
  (provide deglobalize test20 tests20 test-deglobalize tests-deglobalize
           delazy-bindings ;; [2006.04.02] Exposing this for use in pass17, should make its own pass if its going to stick around.        

	   lambda->heads
           )

  (chezimports )
|#

(define process-letrec
  (lambda (expr tenv dfg)
    
    ;; The primitives that *necessarily* incur network communication.
    (define comm-prims '(rfold 
			 khood 
			 anchor-at 
			 anchor-maximizing))
    
    (match expr
      [(lazy-letrec ([,lhs* ,type* ,annots* ,rhs*] ...) ,body)

       (let ([tenv (tenv-extend tenv lhs* type* #t)]
	     ;; Put it back together:
	     [binds (map list lhs* type* annots* rhs*)])
	 

	 ;; This is a table of all signals/regions that are the result of comm-prims. 
	 ;; Type: [[var type] ...]
	 (define comm-outputs
	   (foldl (match-lambda ([,lhs ,ty ,annots ,rhs] ,acc)
		    (match rhs 
		      [(,cp ,args ...) (guard (regiment-primitive? cp)
					      (memq cp comm-prims))
		       ;`([,lhs ,(recover-type lhs tenv)] . ,acc)
		       `([,lhs ,ty] . ,acc)
		       ]
		      [,other acc]))
	     '()  binds))

	 (define (process-varref x)
	   (let ([entry (assq x comm-outputs)]) 
	     (cond 
	      [(not entry) x]
	      [(types-compat? (cadr entry) '(Area 'a)) `(REGEVT ,x)]
	      [(types-compat? (cadr entry) '(Signal 'a)) `(SIGEVT ,x)]
	      [else (error 'get-segment "bad entry in comm-outputs table: ~s" entry)]
	      )))

	 ;; This gets the chain of decls that contribute to a given
	 ;; stream value, up *until* that chain is broken by a
	 ;; communication operation.
	 ;; 
	 ;; The invariant to check later is that the top level
	 ;; declarations split neatly into disjoint chunks of code
	 ;; that plug into each top-level communication construct.
	 (trace-define (get-segment var)
	     (define  (loop var)
	       (define bnd (assq var binds))
	       (unless bnd (error 'get-segment "var '~a' not in binds: ~s" var binds))
	       (match bnd
		 [(,lhs ,ty ,annots ,rhs)
		  (match rhs ;; No match-recursion here.
		    ;;		[,v (guard (regiment-constant? v)) ]

		    ;; Variable references are either "local" or references to 
		    ;; the output of other communication expressions.		    
		    [,x (guard (symbol? x))   
			(list `[,lhs ,ty ,(process-varref x)])]

		    ;; This gets imported whole-hog into the segment
		    [(lambda ,v* ,ty* ,body)  (list `[,lhs ,ty ,rhs])]

		    [(rmap ,fun ,reg)
		     (ASSERT (symbol? fun))(ASSERT (symbol? reg))
		     (let ([newty (match ty
				    [Region (Signal #())] ; Sig Unit
				    [(Area ,alpha) `(Signal ,alpha)] ; Areas become local signals.
				    [,other (error 'get-segment "bad type for rmap output: ~s" other)])])
		       (append  `([,lhs ,ty (smap ,fun ,reg)]) 
				;; Gather all the code that goes into fun and the Area.
				(loop fun) (loop reg)))]

		    [(smap ,fun ,reg)      
		     (ASSERT (symbol? fun))(ASSERT (symbol? reg))
		     (append `([,lhs ,ty (smap ,fun ,reg)]) 
			     (loop fun) (loop reg))]

		    ;; This breaks the segment.
		    [(,commprim ,args ...) (guard (regiment-primitive? commprim)
						  (memq commprim comm-prims))
		     ()]
		    
		    [,other (error 'get-segment "bad rhs: ~s" other)]
		    )]
		 [#f '????]))
	   (let ([binds (reverse (loop var))])
	     (if (null? binds) 
		 (process-varref var)		 
		 `(let* ,binds ,(process-varref var)))))


	 (DEBUGASSERT (andmap type? type*))
	 (ASSERT (symbol? body))

	 ;; Generate communication constructs for each of the communicating operations.
	 (foldl 
	     (match-lambda ([,lhs ,ty ,annots ,rhs] ,acc)
	       ;; This is structured so as to insure we handle all comm-prims:
	       (match rhs 
		 [(,commprim ,args ...) (guard (regiment-primitive? commprim)
					       (memq commprim comm-prims))
		  (trace-match COMM? rhs
		     [(rfold ,fun ,seed ,reg)
		      (ASSERT (symbol? fun)) (ASSERT (symbol? reg))
		      (cons 
		       ;; Construct a top-level communication construct.
		       `(AGGR (OUTPUT ,lhs)
			      (VIA 'TREE?)
			      (SEED 'SEED?) ; recur on seed?
			      (FUN ,(match (assq fun binds)
				      [[,l ,t ,a ,r] `[,l ,t ,r]]))
			      (INPUT ,(get-segment reg)))
		       ;; Continue the rest of the computation with this signal name.
		       acc)]

		     ;; For the time being this only works for constant radii:
		     [(khood ,a (quote ,n))
		      (ASSERT (symbol? a)) (ASSERT (number? n))
		      (cons 
		       `(EMIT (OUTPUT ,lhs)
			      (HOPS ,(ASSERT integer? n))
			      (INPUT ,(get-segment a)))
		       acc)]
		     
		     ;; These probably should have been desugared into an anchor-maximizing...
		     ;[(anchor-at (quote ))		      ]
		     
		     ;; This should take a Region to restrict to....
		     ;; Otherwise, what's it's cadence?
		     [(anchor-maximizing ,fun)
		      (ASSERT (symbol? fun))
		      (cons 
		       `(ELECT (OUTPUT ,lhs)
			       (SCOREFUN ,(assq fun binds))  ;,(get-segment fun))
			       (INPUT (Timer ,(cadr (ASSERT (assq 'heartbeat annots))))))
		       acc)]
		     
		     [,other (error 'deglobalize "unmatched application of communicating primitive: ~s" other)]
		     )]
		 ;; Non comm-prim expressions do nothing:
		 [,else acc]))
	   () binds)

	 )])))

'
(process-letrec 
 `(lazy-letrec ([tmp Region () world]
		[read (Node -> Integer) () 
		      (lambda (n) Node (sense 'temp n))]
		[f (Integer Integer -> Integer) ()
		   (lambda (a b) (+ a b))]
		[v1 (Area Integer) () (rmap read tmp)]
		[v2 (Signal Integer) () (rfold f u v1)]
		)
	       v2)
 (empty-tenv)
 ()
)

'
(process-letrec 
 `(lazy-letrec (;[a Anchor () (anchor-at '30 '40)]
		[id (Node -> Anchor) () (lambda (n) (Node) (nodeid n))]
		[a Anchor ([heartbeat 10000]) (anchor-maximizing id)]
		[tmp Region () (khood a '2)]
		[read (Node -> Integer) ()
		      (lambda (n) Node (sense 'temp n))]
		[f (Integer Integer -> Integer) ()
		   (lambda (a b) (+ a b))]
		[v1 (Area Integer) () (rmap read tmp)]
		[v2 (Signal Integer) () (rfold f u v1)]
		)
	       v2)
 (empty-tenv)
 ()
)



'
(rc '(rfold + 0 (rmap (lambda (n) (sense 'temp n)) world)) 'verbose)

'
(lift-letrec-language
  '(program
     (lazy-letrec
       ((resultoftoplevel_10      (Signal Integer)
          (rfold tmpnonprim_14 '0 tmprmap_13))
         (tmpnonprim_14
           (Integer Integer -> Integer)
           (lambda (a_7 b_6)
             (Integer Integer)
             (lazy-letrec
               ((resultofanonlambda_9 Integer (+ a_7 b_6)))
               resultofanonlambda_9)))
         (tmprmap_13 (Area Integer) (rmap tmpnonprim_12 tmpworld_11))
         (tmpnonprim_12
           (Node -> Integer)
           (lambda (n_5)
             (Node)
             (lazy-letrec
               ((resultofanonlambda_8 Integer (sense 'temp n_5)))
               resultofanonlambda_8)))
         (tmpworld_11 Region world))
       resultoftoplevel_10)
     (Signal Integer)))


; ======================================================================

'
(process-expr
 '(smap add1 (rfold + 0 (rmap sense world)))
 (empty-tenv)
 (lambda (x) x))


(define process-expr
  (lambda (expr tenv k)
      (trace-match PE expr

	[world  'ERRRRRRRR]

	;; The possibility that the final value is local is
	;; handled in 'deglobalize' so we don't worry about it here:
	[,x (guard (symbol? x)) (k x)]

	[(smap ,fun ,reg)
	 (ASSERT (symbol? fun))
;	 (ASSERT (symbol? reg))
	 ;; Add this onto the continuation
	 (process-expr reg tenv (lambda (x) `(smap ,fun ,x)))
	 ]

	[(rmap ,fun ,reg)
	 (ASSERT (symbol? fun))
;	 (ASSERT (symbol? reg))
	 ;; Add this onto the continuation
	 (process-expr reg tenv (lambda (x) `(smap ,fun ,x)))
	 ]

	[(rfold ,fun ,seed ,[reg])
	 (ASSERT (symbol? fun))
	 (ASSERT (symbol? reg))
	 (cons 
	  ;; Construct a top-level communication construct.
	  `(AGGR (TO ,name)
		 (VIA 'TREE?)
		 (SEED 'SEED?) ; recur on seed?
		 (FUN ,fun)
		 (STREAM ,reg))
	   ;; Continue the rest of the computation with this signal name.
	  (k name))]

	;; These bindings must be non-recursive and topologically sorted.
	;; (That is, lazy-letrec has been converted to let*.)
	[(let* () ,body)
	 (process-expr #f () body tenv dfg k)]
	[(let* ([,lhs ,ty ,annots ,rhs] ,rest ...) ,body)
	 (process-expr rhs (tenv-extend tenv (list lhs) (list rhs)) dfg
		       (lambda (x)
			 (process-expr `(let* ,rest ,body) 
				       (tenv-extend tenv (list lhs) (list ty))
				       (lambda (y) 
					 `(let* ([,lhs ,ty ,x]) ,y)))))]
	
#|
	;; Don't need to make a new token name, the name of this
	;; function is already unique:
	[(lambda ,formals ,types ,body)
	 (let ([newenv (tenv-extend tenv formals types #f)])
	   (mvlet ([(entry primbinds tokenbinds) (process-letrec body newenv dfg)])
	     (define returntype (recover-type body newenv))
					;	       (fprintf (current-error-port) "RETTY: ~s\n" returntype)
					;	   (if (not (null? tokenbinds))
					;	       (error 'deglobalize 
					;		      "Should not get any tokens from internal letrec right now!: ~s"
					;		      tokenbinds))	
	     ;; WARNING: This can't generate meaningful code for a distributed lambda.
	     (values '() 
		     (cons (if (distributed-type? returntype)
			       ;; Distributed lambda's aren't *called* dynamically.  They must be *wired*.
			       `[,name ,formals (error 'process-expr "cannot generate good code for this lambda.")]
			       `[,name ,formals (let* ,primbinds ,entry)];(call ,entry))]
			       )
			   tokenbinds))))]


	;; FIXME FIXME... this is lame.
	[(sense ,node) ;; Transforms into a local sense.
	 (values `([,name (sync-sense)]) '())]

	;; [2006.02.15] For now we have a cheater clock provided for us by the simulator:
	[(sense 'clock ,node) (values `([,name (my-clock)]) '())]

	[(sense (quote ,type) ,node) ;; Transforms into a local sense.  Doesn't use node...
	 (guard (symbol? type))
	 (values `([,name (sync-sense (quote ,type))]) '())]	  

	[(sense . ,other)
	 (error 'deglobalize "invalid sense syntax: ~a" `(sense . ,other))]

	;; These are primapps that depend on distributed components.  They're tricky.
	;; What does it mean ultimately to return a signal within a cons-cell for example?
	;; I'm trying to decide whether to do a bit of automatic
	;; lifting, or force the user to use smap2.
	[(,prim ,rand* ...)
	 (guard (basic-primitive? prim)
		(ormap (lambda (x) (if (symbol? x) (check-prop 'distributed x) #f)) rand*))
	 (error 'deglobalize:process-expr
		"primapp depends on distributed value: ~s" `(,prim ,rand* ...))
	 ]

	;; This is a local primitive that depends on only local values.  That means its constant.
	;; All args are simple:
	[(,prim ,rand* ...) (guard (basic-primitive? prim))
	 (values `([,name ,(match `(,prim . ,rand*)
			     [(nodeid ,_) `(my-id)]
			     [(tuple ,x* ...) `(vector ,x* ...)]
			     [(tupref ,a ,b ,x) `(vector-ref ,x ,a)]
			     [,else expr])])
		 '())]

	[(,prim ,rand* ...) (guard (distributed-primitive? prim))
	 (mvlet ([(form memb) (token-names name)])
	   (values '() 
		   (append 
		    (explode-primitive form memb prim rand* annots tenv dfg)
		    (if finalname
			(primitive-return prim memb)
			'()))
		   ))]
	
|#

	[,unmatched
	 (error 'deglobalize2 "invalid expression: ~s"
		unmatched)])
      ))

; ==============================================================================

;; This is the main procedure for the pass itself.
(define deglobalize2
  (let ()

    (lambda (prog)
;      (pretty-print prog) (newline)
      (match prog
        [(,lang ;add-places-language 
	  (quote (program (props ,table ...)
		   (control-flow ,cfg ...)
		   (data-flow ,dfg ...)
		   lazlet ;(lazy-letrec ,binds ,fin)
		   ,type)))


		`(deglobalize2-lang 
		  '(program 
;		       (bindings ,@constbinds)
		     (commdecls
		      ,@(process-letrec lazlet (empty-tenv) dfg)
		     )))
		]))))


