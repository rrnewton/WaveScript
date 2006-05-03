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
  
  (provide deglobalize2
	   test-this
	   test-deglobalize2
;	   process-expr
	   process-letrec

	   ;; TEMP:
	   transform-type
           )

  (chezimports (except hm_type_inference test-this these-tests))

  ;; This transforms Regions and removes Nodes.
  (define (transform-type ty)
      (let tt ((ty ty))
      (cond
       [(eq? 'Node ty) #()] ;; This becomes unit.
       [(types-compat? ty '(Area 'a)) => (match-lambda ((Area ,a))
					   `(Signal ,(transform-type a)))]
       [(types-compat? ty '(Signal 'a)) => (match-lambda ((Signal ,a))
					   `(Signal ,(transform-type a)))]
       [else (match ty
	       [(,[tt -> in*] ... -> ,[tt -> out]) `(,in* ... -> ,out)]
	       [#(,[tt -> t*] ...)   (apply vector t*)]
	       [(,Tc ,[tt -> args] ...) (ASSERT (symbol? Tc)) 
		`(,Tc ,args ...)]
	       [,v (ASSERT (symbol? v)) v])])))

  (define process-letrec
    (lambda (expr tenv dfg)

      ;; Recursively pull out all the bindings (including inside lambdas)
      (define (get-all-binds exp)
	(match exp
	  [(lazy-letrec ([,lhs* ,type* ,annots* ,rhs*] ...) ,body)
	   (map get-all-binds rhs*)

	   ]
	  )
	
	)
      
    ;; The primitives that *necessarily* incur network communication.
    (define comm-prims '(rfold rdump
			 khood 
			 anchor-at 
			 anchor-maximizing
			 anchor-maximizing-within))
    
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

	 ;; This gets the chain of decls that contribute to a given
	 ;; stream value, up *until* that chain is broken by a
	 ;; communication operation.
	 ;;
	 ;; The invariant to check later is that the top level
	 ;; declarations split neatly into disjoint chunks of code
	 ;; that plug into each top-level communication construct.
	 (define (get-segment var)
	   (define (loop var)
	     (define bnd (assq var binds))
;	     (unless bnd (inspect (assq var dfg)))
	     (unless bnd (error 'get-segment "var '~a' not in binds: ~s" var binds))
	     (match bnd 
	       [(,lhs ,ty ,annots ,rhs)
		(match rhs 
		  ;; This breaks the segment.
		  [(,commprim ,args ...) (guard (regiment-primitive? commprim)
						(memq commprim comm-prims))
		   ()]
		  ;; Otherwise trace the dependencies, and include those decls.
		  [,other (cons `[,lhs ,ty ,annots ,rhs]
				(apply append (map loop (regiment-free-vars other))))]
		  )]))
	   (let ([x (loop var)])
	     (if (null? x) var
		 `(lazy-letrec ,(tsort-bindings x) ,var))))
	 
	 ;; This simple rearranges bindings so that a binding precedes its uses.
	 ;; Only works if there are no cycles.
	 (define (tsort-bindings bnds)
	   (define edges (map (lambda (bnd)
				[list (car bnd) (regiment-free-vars (last bnd))])
			   bnds))
	   (if (cyclic? edges)
	       (error 'deglobalize2:tsort-bindings
		      "Not allowed to have cycles in node-code let bindings: \n~s"
		      edges))
	   (let ([result (reverse!
			  (filter id 
			    (map (lambda (v) (assq v bnds))
			      (tsort edges))))])
	     result))

	 (define (transform-varref x)
	   (ASSERT (symbol? x))
	   (let ([entry (assq x comm-outputs)]) 
	     (cond 
	      [(not entry) x]
	      [(types-compat? (cadr entry) '(Area 'a)) =>
	       (match-lambda ((Area ,a))
		 (if (distributed-type? a)
		     `(REGEVT ,x RID)
		     `(REGEVT ,x)))]
	      [(types-compat? (cadr entry) '(Signal 'a)) `(SIGEVT ,x)]
	      [else (error 'get-segment "bad entry in comm-outputs table: ~s" entry)]
	      )))

	 ;; This "deglobalizes" a signal-generating expression.  That
	 ;; is, it reduces (non-communicating) region-operations to
	 ;; the equivalent signal operations, and it removes
	 ;; references to Node objects.  The language becomes impure
	 ;; because sensor readings are now non-deterministic
	 ;; procedures rather than projections from Nodes in a "world"
	 ;; region.
	 (define (transform-expr expr)
	   (define (Bind bind)
	     (match bind
	       [[,lhs ,ty ,annots ,rhs]
		`[,lhs ,(transform-type ty)
		       ,(match rhs
			  [world `(Timer ,(cadr (ASSERT (assq 'heartbeat annots))))]
			  [,[Expr -> other] other])]]
	       [,other (error 'transform-expr "bad binding: ~s" other)]))
	   (define (Expr exp)
	     (match exp
	       [,x (guard (symbol? x) (not (regiment-constant? x)))
		   (transform-varref x)]
	       [(quote ,v) `(quote ,v)]
	       [(lambda ,v* (,[transform-type -> ty*] ...) ,[body])
		`(lambda ,v* ,ty* ,body)]
	       [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
	       [(lazy-letrec (,(Bind -> bnds) ...) ,[bod])
	       ;; If there's just one binding we get rid of the letrec.	
;		(if (and (= 1 (length bnds)) (eq? bod (caar bnds)))
	;	    (last (apply Bind (car bnds)))
		`(lazy-letrec ,bnds ,bod)]

	       [(rmap ,[fun] ,[reg]) `(smap ,fun ,reg)]
	       [(nodeid ,_) `(my-id)]

	       [(,p ,[x*] ...) (guard (regiment-primitive? p))
		`(,p ,x* ...)]
	       [,other (error 'transform-expr "unmatched: ~s" other)]
	       ))
	   (Expr expr))
	 
	 (define (transform-bind bnd)
	   (match bnd
	     [[,lhs ,ty ,annots ,rhs]
	      `[,lhs ,(transform-type ty) ,(transform-expr rhs)]]))


	 ;(break get-segment)
	 ;(inspect (get-segment 'v1))

	 (printf "COMMBINDS: ~s\n" comm-outputs)
       
	 
	 (DEBUGASSERT (andmap type? type*))
	 (ASSERT (symbol? body))

	 ;; Generate communication constructs for each of the communicating operations.
	 (let scanbinds ([binds binds])
	   (foldl
	     (match-lambda ([,lhs ,ty ,annots ,rhs] ,acc)
	       ;; This is structured so as to insure we handle all comm-prims:
	       (trace-match COMM? rhs 
		 [(,commprim ,args ...) (guard (regiment-primitive? commprim)
					       (memq commprim comm-prims))
		  (match rhs

		    ;; A fold becomes an aggregation along a tree.
		    [(rfold ,fun ,seed ,reg)
		      (ASSERT (symbol? fun)) (ASSERT (symbol? reg))
		      (cons 
		       ;; Construct a top-level communication construct.
		       `(AGGR (OUTPUT ,lhs)
			      (VIA ,(cadr (ASSERT (assq 'tree annots))))
			      (SEED ,(transform-expr seed))
			      (FUN ,(transform-bind (assq fun binds)))
			      (INPUT ,(transform-expr (get-segment reg))))
		       ;; Continue the rest of the computation with this signal name.
		       acc)]

		    [(rdump ,reg)
		     (ASSERT (symbol? reg))
		     (cons
		      `(DUMP (OUTPUT ,lhs)
			     (VIA ,(cadr (ASSERT (assq 'tree annots))))
			     (INPUT ,(transform-expr (get-segment reg))))
		      acc)]

		    ;; A khood becomes an "EMIT" -- tree formation.
		    ;; For the time being this only works for constant radii:
		    [(khood ,a (quote ,n))
		     (ASSERT (symbol? a))
		     (cons 
		      `(EMIT (OUTPUT (REGEVT ,lhs (my-id)))
			     (HOPS ,(ASSERT integer? n))
			     (INPUT ,(transform-expr (get-segment a))))
		      acc)]

		     ;; These probably should have been desugared into an anchor-maximizing...
		     ;[(anchor-at (quote ))		      ]

		     ;; This should take a Region to restrict to....
		     ;; Otherwise, what's it's cadence?
		     [(anchor-maximizing ,fun ,reg)
		      (ASSERT (symbol? fun))
		      (cons 
		       `(ELECT (OUTPUT ,lhs)
			       (SCOREFUN ,(transform-bind (assq fun binds)))
			       (INPUT ,(transform-expr (get-segment reg)))
			       )
		       ;; Continue the rest of the computation with this signal name.
		       acc)]

		     [,other (error 'deglobalize "unmatched application of communicating primitive: ~s" other)])]

		 ;; Keep looking for communicating primitives inside lambdas: 
		 [(lambda ,v* ,ty* (lazy-letrec ,binds ,bod))
		  (ASSERT (symbol? bod))
		  (append (scanbinds binds)
			  acc)]
		 [(lambda ,other ...)
		  (error 'scanbinds "bad lambda form: ~s" `(lambda ,other ...))]

		 ;; Other non-comm-prim expressions do nothing:
		 [,else acc]))
	   () 
	   binds
	   )))])))

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
		   ,lazlet ;(lazy-letrec ,binds ,fin)
		   ,type)))


		`(deglobalize2-lang 
		  '(program 
;		       (bindings ,@constbinds)
		     (commdecls
		      ,@(process-letrec lazlet (empty-tenv) dfg)
		     )))
		]))))

  ;;======================================================================
  
  (define these-tests 
    `(
      [(,transform-type 'Anchor) (Signal #())]

      ["A rfold a single anchor."
       (process-letrec 
	`(lazy-letrec (;[a Anchor () (anchor-at '30 '40)]
		       [theworld Region ([heartbeat 300000]) world]
		       [nid (Node -> Integer) () (lambda (n) (Node) (lazy-letrec ([res0 Integer () (nodeid n)]) res0))]
		       [ank Anchor ([heartbeat 10000]) (anchor-maximizing nid theworld)]
		       [tmp Region () (khood ank '2)]
		       [read (Node -> Integer) ()
			     (lambda (n) (Node) 
				    (lazy-letrec ([res1 Integer () (sense 'temp n)]) res1))]
		       [f (Integer Integer -> Integer) ()
			  (lambda (a b) (Integer Integer) 
				  (lazy-letrec ([res2 Integer () (+ a b)]) res2))]
		       [v1 (Area Integer) () (rmap read tmp)]
		       [v2 (Signal Integer) ([tree tmp]) (rfold f '39 v1)]
		       )
		      v2)
	(empty-tenv) ())
       unspecified
       ]

      ;; This doesn't work yet, need to manually write in the DFG.
      #;
      [" Rfold a region of regions."
       (process-letrec 
	`(lazy-letrec (
		       [nid (Node -> Integer) () (lambda (nod) (Node) (lazy-letrec ([x0 Integer () (nodeid nod)]) x0))]
		       [promote (Node -> Anchor) () (lambda (nod2) (Node) (lazy-letrec ([x1 Anchor () (node->anchor nod2)]) x1))]
		       [nbrhood (Anchor -> Region) ()
				(lambda (anc) (Anchor) (lazy-letrec ([thehood Region () (khood anc '2)]) thehood))]
		       [plus (Integer Integer -> Integer) () 
			     (lambda (a b) (Integer Integer) (lazy-letrec ([x2 Integer () (+ a b)]) x2))]

		       [mapnid (Region -> (Area Integer)) () 
			       (lambda (r) (Region) (lazy-letrec ([x3 (Area Integer) () (rmap nid r)]) x3))]
		       [foldplus ((Area Integer) -> (Signal Integer)) () 
				 (lambda (r) (Area Integer) (lazy-letrec ([thefold (Signal Integer) () (rfold plus '0 r)]) 
									 thefold))]

		       [theworld Region ([heartbeat 300000]) world]
		       [clusters (Area Region) () (rmap nbrhood theworld)]
		       [ids (Area (Area Integer)) () (rmap mapnid clusters)]
		       [sums (Area Integer) () (rmap foldplus ids)]
		       [result (Signal Integer) ([tree thehood]) (rdump sums)]
		       )
		      result)
	(empty-tenv) ())
       unspecified]
      
      ))

  (define test-this (default-unit-tester 
		      "Deglobalize2: transform Regiment into node-level stream-processing and communication constructs."
		      these-tests
		      ))
  (define test-deglobalize2 test-this)


  ;;======================================================================


#;
(process-letrec 
 `(lazy-letrec ([tmp Region () world]
		[read (Node -> Integer) () 
		      (lambda (n) (Node) (sense 'temp n))]
		[f (Integer Integer -> Integer) ()
		   (lambda (a b) (Integer Integer) (+ a b))]
		[v1 (Area Integer) () (rmap read tmp)]
		[v2 (Signal Integer) () (rfold f u v1)]
		)
	       v2)
 (empty-tenv)
 ()
)



#;
(rdump (rmap (rfold + 0)
	     (rmap (rmap nodeid)
		   (rmap (lambda (n) (khood (node->anchor n) 2))
			 world))))



#;
(rc '(rfold + 0 (rmap (lambda (n) (sense 'temp n)) world)) 'verbose)

#;
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

; ==============================================================================


) ;; End module

















#|	     
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
		     (ASSERT (symbol? fun))
		     (let ([newty (match ty
				    [Region (Signal #())] ; Sig Unit
				    [(Area ,alpha) `(Signal ,alpha)] ; Areas become local signals.
				    [,other (error 'get-segment "bad type for rmap output: ~s" other)])])
		       (append  `([,lhs ,ty (smap ,fun ,(process-varref reg))]) 
				;; Gather all the code that goes into fun and the Area.
				(loop fun) (loop reg)))]

		    [(smap ,fun ,sig)      
		     (ASSERT (symbol? fun))
		     (append `([,lhs ,ty (smap ,fun ,(process-varref sig))]) 
			     (loop fun) (loop sig))]

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

|#






#;
(process-expr
 '(smap add1 (rfold + 0 (rmap sense world)))
 (empty-tenv)
 (lambda (x) x))

#;
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
	  `(AGGR (TO name??????)
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
