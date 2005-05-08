
;; INCOMPLETE: this doesn't actually use the CFG right now.

;;; Pass 16: deglobalize
;;; April 2004
;===============================================================================

;;; This pass represents the biggest jump in the compiler.  It
;;; transforms my simplified global language into a local program to
;;; be run in each node in the sensor network.

;;; We introduce a simple imperative language for the token-handlers
;;; within each node programs.  In this case, it's naturally a subset
;;; of scheme, but it gets reduced further in the next pass 
;;; (cleanup-token-machine).



;;; Input grammar:

;;; <Pgm>  ::= <Let>
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)
;;; <Formalexp> ::= (<var>*)
;;; <Simple> ::= (quote <Lit>) | <Var>


;;; Output grammar:

;;;  <Pgm> ::= (program (bindings <Decl>*) <SOCPgm> <NodePgm>)
;;;  <SOCPgm> ::= <Statement*>
;;;  <NodePgm> ::= (nodepgm <Entry> (bindings <Decl>*) (tokens <TokBinding>*))
;;;;;;;  <Entry>  ::= <Token>
;;;  <Decl> ::= (<var> <Exp>)
;;;  <TokBinding> ::= (<Token>  <Code>*)
;;; <TODO> DECIDE ON LOCAL BINDINGS:
;;;  <TokBinding> ::= (<Token> (bindings <Decl>*) <Code>*)

;;;  <Code> ::= <Statement>*
;;;  <Statement>  ::= <BasicStuff?>
;;;                | (emit <Token> <Simple>*)
;;;                | (relay <Token>)
;;;                | (dist <Token>)
;;;                | (return <Token> <Simple>)
;;;                | <Macro> 
;;;  <Macro> ::= (flood <Token>)
;;;            | (elect-leader <Token> [<Token>])  ;; <TODO> optional second argument.. decider
;;;  <Simple> ::= (quote <Lit>) | <Var>

;;;  <Token> ::= <Symbol> | ...???
;;;  <Exp>  ::= ???


;;========================================
;; EXAMPLE:

;; This program just returns 3.  It has a generic binding defining a
;; single constant, it has no startup tokens (startups must be
;; *tokens* not other bindings.)  It has no socpgm-exclusive bindings,
;; and the socprogram merely returns the single value, then finishes.
#;(program
  (bindings (result '3))
  (socpgm (bindings ) (soc-return result) (soc-finished))
  (nodepgm (tokens ) (startup )))


;; THESE ARE TEMPORARY... using numbers to signify return values that are regions..
(define RMAP-NUM 39)
(define ANCH-NUM 49)
(define CIRC-NUM 59)


;===============================================================================
;; Some CHANGES (not keeping a complete log):

;;[2004.06.09] RRN: Adding a 'soc-return' form.  Anything

;;[2004.06.09] RRN: Adding implicit 'I-am-SOC' boolean variable for
;; use by node programs.. This is only for generated code, or my
;; handwritten test cases.

;; [2004.06.13] RRN: Moved functions for dealing with token names into helpers.ss

;===============================================================================

(define proptable 'not-defined-yet)

(define (check-prop p s)
  (let ((entry (assq s proptable)))
    (if entry (memq p (cdr entry))
	(error 'deglobalize:check-prop
	       "This should not happen!  ~nName ~s has no entry in ~s."
	       s proptable))))

    (define (simple? x) 
      (match x
          [(quote ,imm) #t]
          [,var (guard (symbol? var)) #t]
	  [,otherwise #f]))

;    (define symbol-append
;      (lambda args
;	(string->symbol (apply string-append (map symbol->string args)))))

(define activate-prim-code
  (lambda (prim memb args parent)
    (case prim
      [(rfold)
       (match args
	      [(,rator_tok ,seed_val)	       
	       `((return this              ;; Value
			 (to ,memb) ;; To
			 (via ,parent)           ;; Via
			 (seed ,seed_val)       ;; With seed
			 (aggr ,rator_tok))     ;; and aggregator
		 )])])))

(define push-prim-code
  (lambda (prim formal memb args parent)
    (case prim
      [(rfold)
       (match args
	      [(,rator_tok ,seed_val)
	       `( (return ,formal              ;; Value
			  (to ,memb) ;; To
			  (via ,parent)           ;; Via
			  (seed ,seed_val)       ;; With seed
			  (aggr ,rator_tok))     ;; and aggregator
		  )])])))


;; [2004.07.28] This is a new version I'm making, which takes pre-classified
#;(define emit-primitive-handlers
  (lambda (classified-primapp form memb heartbeat)
    (match classified-primapp
	   [(push-comp ,prim ,args ...) 
	    (let ([parent (get-membership-name region_tok)])
	      `([,parent (v) (call ,form v)]
		[,form (v) ,@(push-prim-code prim 'v memb args parent)]))]
	   [(activate-comp ,prim ,region_tok ,args ...)
	    (let ([parent (get-membership-name region_tok)])
	      `([,parent (v) (activate ,form v)]
		[,form () 
		       ,@(activate-prim-code prim memb args parent)  
		       (timed-call ,(/ 1000 heartbeat) ,form)]))]
	   [,other (error 'emit-primitive-handlers
			 "unknown primitive classification: ~s" other)])))

;  (push-comp prim args)
;  (activate-comp prim args)

;; (Name, DistributedPrim, Args) -> TokenBinds
;; This produces a list of token bindings.
;; It generates code for one node in the dataflow graph.  It uses
;; get_membership_name to figure out from where control flow will come
;; on incoming edges.  
;; Below, "parent" won't be available if we don't have the full stream-graph...
(define explode-primitive
  (lambda (form memb prim args heartbeat)
;	(disp "Explode primitive" name prim args)
	  (case prim
	    [(sparsify) (void)]
	    
	    ;; -=<TODO>=- UNSURE OF THIS
	    ;; SEEMS PRETTY USELESS!
	    ;[(sense)
	    ;`([,form () (local-sense)]
					;;[,memb () ]
	    ;)]

	    [(rmap) ;; CHECK
	     (let* ([rator_tok (car args)]
		    [region_tok (cadr args)]
		    [parent (get-membership-name region_tok)]
		    [push? (not (check-prop 'region region_tok))])
	       (if push?
		   `([,parent (v) (call ,form v)]
		     [,form (v)
			    (call ,memb
				  (call ,rator_tok v))])
		   `([,parent () (activate ,form)]
		     [,form () 
			    (call ,memb (call ,rator_tok this))
			    (timed-call ,heartbeat ,form)])
		   ))]

	    ;; [2004.06.28] This may or may not take an argument, and
	    ;; that depends on the *type* that it's specialized too,
	    ;; doesn't it?  My idea being that if it's operating over
	    ;; a *Region*, the argument to the form token is an
	    ;; implicit "this".  But, hmm.  We don't *have* a type
	    ;; checker right now...

	    ;; For the moment I will try to always pass the argument,
	    ;; or maybe we'll go for polymorphism on the part of this
	    ;; formation token....
	    [(rfold) ;; CHECK
	     (let  ([rator_tok (car args)]
		    [seed_val (cadr args)]
		    [region_tok (caddr args)]
;		    [return_handler (new-token-name 'rethand-tok)]
		    )
	       (let ([parent (get-membership-name region_tok)]     
		     [push? (not (check-prop 'region region_tok))])
		 `([,parent ,(if push? '(v) '()) 
			    ,(if push? 
				 `(call ,form v)
				 `(activate ,form v))]
		   [,form ,(if push? '(v) '())
			  (return
			     ,(if push? 'v 'this)                     ;; Value
			     (to ,memb)             ;; To
;			     (via ,parent)          ;; Via
			     (via global-tree)          ;; Via
			     (seed ,seed_val)       ;; With seed
			     (aggr ,rator_tok))     ;; and aggregator
			  ,@(if push? '()
				`((timed-call ,(/ 1000 heartbeat) ,form)))]
;		 [,memb (v) ] ;; This occurs at the fold-point
		 )))]

	    ;; <TODO>: FIXME
	    [(cluster)
	     (let  ([region_tok (caddr args)])
	       (let ([parent (get-membership-name region_tok)]
		     [spread-tok (new-token-name 'cluster-spread-tok)]
		     [leader-tok (new-token-name 'cluster-leader-tok)]
		     ;[push? (not (check-prop 'region region_tok))]
		     )
		 
		 ;; <TODO> <FIXME> ALLOW VARIABLE ARGUMENTS FOR TOKENS!!!
		 `(
		   ;[,parent (v) (emit ,form (my-id))] ;,spread-cluster)]
		   ;[,parent (v) (elect-leader ,memb (my-id))]
		   ;[elect-leader
		   [,form (id)
			  (if (check-tok ,parent)
			      (begin (relay) (call ,leader-tok))
			      ;; Here we remove ourselves if we've overflowed?
					;(remove-tok ,spread-cluster)
			      )]
;		   [(call 2
;		 [,memb (v) ] ;; This occurs at the fold-point
		   )))]

	    
	    [(filter)
	     (match args
	       [(,pred_tok ,region_tok)
		(let ([parent (get-membership-name region_tok)])
		  `( [,parent (v) (call ,form v)] ;; member of that area
		     [,form (v) (if (call ,pred_tok v)
				    (call ,memb v))] ))])]
			 
	    ;; This is not a region; it carries no value on its membership token!
	    [(anchor-at) ;; 
	     (let ([consider (new-token-name 'cons-tok)]
		   [leader (new-token-name 'leader-tok)]
		   [target (car args)]
		   )
	       `([,form () (flood ,consider)]
		 [,consider () 
;			    (if (< (locdiff (loc) ,target) 10.0)
			    (if (< (locdiff (loc) ,target) 10)
				(elect-leader ,memb)
				'#f)]
		 [,form () (draw-mark ,target (rgb 0 100 100))]
		 ;; DEBUGGING
		 ;; This just lights up the node when it becomes anchor, for visualization:
		 [,memb () 
			;; Note that we are an anchor.
;			(set-simobject-homepage! 
;			 this (cons 'anchor (simobject-homepage this)))
			(light-up 0 255 255)]
		 ))]

	    [(circle)
	     (let ([anch (car args)]
		   [rad (cadr args)])
;		   (arg (unique-name 'arg)))
	       `(;; Anchor membership carries no arguments:
		 [,(get-membership-name anch) () (call ,form)]
;		 [,form () (emit ,memb this)]
		 [,form () (emit ,memb)]
		 ;; Display stuff:
;		 [,form () (draw-circle (loc) 20)]
		 [,memb ()
			;; Note that we are inside a "hood".
;			(set-simobject-homepage! 
;			 this (cons 'circle (simobject-homepage this)))
			(light-up 0 100 100)]
		 [,memb () (if (< (dist) ,rad) (relay))]
		 )
	       )]

	    [(khood)
	     (let ([anch (car args)]
		   [rad (cadr args)])
;		   (arg (unique-name 'arg)))
	       `(;; Anchor membership carries no arguments:
		 [,(get-membership-name anch) () (call ,form)]
;		 [,form () (emit ,memb this)]
		 [,form () (emit ,memb)]
		 ;; Display stuff:
;		 [,form () (draw-circle (loc) 20)]
		 [,memb ()
			;; Note that we are inside a "hood".
;			(set-simobject-homepage! 
;			 this (cons 'circle (simobject-homepage this)))
			(light-up 0 100 100)]
		 [,memb () (if (< (dist) ,rad) (relay))]
		 )
	       )]


#;	    [(circle-at)
	     (let ([rad (cadr args)]
		   [loc (car args)])
	       `(
		 [,form () (emit ,memb)]
		 [,memb () (if (< (dist ,form) ,rad) (relay))]
		 ))]	   
		 
            ;; Does this work with flickering?
	    [(union)
	     (let ([mem_a (get-membership-name (car args))]
		   [mem_b (get-membership-name (cadr args))])	       
	       `([,form () (iftok (and ,mem_a ,mem_b)
				  (call ,memb this)
				  (remtok ,memb))]
		 ;; These may be duplicate token entries:
		 [,mem_a (v) (call ,form)]
		 [,mem_b (v) (call ,form)]
    ;	       [,memb ... Don't know what yet... that depends on varrefs ]
		 ))]

	    [(when-any)
	     (let* ([rator_tok (car args)]
		    [region_tok (cadr args)]
		    [mem_reg (get-membership-name region_tok)])
	       ;; If we pass, fire that event!
	       `([,mem_reg (v) (if (call ,rator_tok v)
				   (call ,memb v))]))]

	    [else `([UNHANDLED-EXPLODE-PRIM (,prim) (void)])])))


    ;; LetrecExpr -> (Entry, Cbinds, TokenBinds)
    ;; This produces a list of constant bindings, token bindings, and
    ;; a token entry point of zero arguments.  The entrypoint is the
    ;; root, or finally returned edge of the data flow graph...
    (define process-letrec
      (lambda (expr)
        (match expr
	       [(lazy-letrec ([,lhs* ,heartbeat* ,formplace ,membplace ,rhs*] ...) ,body)
		(if (symbol? body)
		    (let loop ((lhs* lhs*) (heartbeat* heartbeat*) (rhs* rhs*)
			       (cacc '()) (tacc '()))
		      (if (null? lhs*)
			  (values (if (check-prop 'distributed body)
				      (mvlet (((form memb) (token-names body)))
					     form)
				      body)
				  cacc tacc)
			  ;; UHH TODO: membership or formation?
					;(map get-formation-name lhs*) 
			  (mvlet ([(cbinds tbinds) (process-expr (car lhs*) (car heartbeat*) (car rhs*))])
;				 (disp "GOT CBINDS TBINDS:" cbinds tbinds)
				 (loop (cdr lhs*) (cdr heartbeat*) (cdr rhs*)
				       (append cbinds cacc) 
				       (append tbinds tacc)))))
		    (error 'deglobalize "Body of letrec should be just a symbol at this point."))]
	  )))


 ;; This produces code for returning a value to the SOC 
 ;; from a particular primitive application.
(define primitive-return
  (lambda (prim tokname) ;; The tokname is a membership-name
    (case prim
      [(anchor-at)
       `([,tokname ()
	  ;; At each formation click, we output this node.
	  ;(soc-return (list 'ANCH this))
	  (soc-return ,ANCH-NUM)
		   ])]

      [(circle circle-at)     
       `([,tokname 
	  ; FIXME FIXME FIXME: This matches by coincidence only with the other names..
	  ()
	  ;; At each formation click, we output this circle: 
	  ;;   For now this just lists the tokname, this should be the
	  ;; membership tokname for the circle.  Later we'll put some
	  ;; other hack in.
	  ;(soc-return '(CIRC ,tokname this))
	  (soc-return CIRC-NUM)
	  ])]

      [(filter)     
       `([,tokname (v)
	  (soc-return (list 'FILTRATION ',tokname (my-id) this))])]

      ;; The membership for a fold means we're at the single point
      ;; that aggregates data.
      [(rfold)
       `([,tokname (v) ;; This value is a sample in the stream
	  (soc-return v)])]
      
      [(khood khood-at)
       `([,tokname 
	  () (soc-return '(KHOOD ,tokname))])]

      [(smap)
       `([,tokname (v) (soc-return v)])]

      [(rmap)
       `([,tokname (v)
		   (soc-return ,RMAP-NUM)])] ;(list 'RMAP (my-id)))])]

      ;; When the membership token has fired, the event has fired!
      [(when-any)
       `([,tokname (v) ;; Event value
	    (soc-return v)])]
      
      [else (error 'primitive-return 
		   "This function incomplete; doesn't cover: ~s. Ryan, finish it! "
		   prim)]
      )))

   ;; (Name, Expr) -> (Cbinds, TokenBinds)
   ;; This processes an expression and returns both its constant
   ;; bindings, and it's token bindings.
    (define process-expr
      (lambda (name heartbeat expr)	
	(let ((finalname (check-prop 'final name)))
        (match expr

	  ;; The result of the 'world' primitive just wires the world
          ;; spark to whatever value-name this is, and wires the
          ;; formation token straight to the membership.
          [world (values '() 
			 `([,(get-formation-name name) () (call ,(get-membership-name name))]
			   [spark-world () (call ,(get-membership-name name))]))]

          ;; The possibility that the final value is local is
	  ;; handled in 'deglobalize' so we don't worry about it here:
	  [,x (guard (simple? x))      
	      (values `([,name ,expr]) ;`([,name (begin (return ,x))])
		      '())  ]
	  
          ;; All args are simple:
          [(if ,test ,conseq  ,altern)
	   (values `([,name ,expr]) '())]
;               ,[test-binds   test-toks] 
;	       ,[conseq-binds conseq-toks] 
;	       ,[altern-binds altern-toks])
;	   (values (append test-binds conseq-binds altern-binds)
;		   (append test-toks conseq-toks altern-toks))]


  	 ;; Don't need to make a new token name, the name of this
  	 ;; function is already unique:
	  [(lambda ,formalexp ,[process-letrec -> entry constbinds tokenbinds])
;	   (if (not (null? tokenbinds))
;	       (error 'deglobalize 
;		      "Should not get any tokens from internal letrec right now!: ~s"
;		      tokenbinds))	   
	   (values '() 
		   (cons `[,name ,formalexp (let* ,constbinds ,entry)];(call ,entry))]
			 tokenbinds))]


	  ;; FIXME FIXME... this is lame.
	  [(sense ,_ ...)
	   (values `([,name (local-sense)]) '())]

	  ;; TODO:	   
          [(,prim ,rand* ...) (guard (basic-primitive? prim))
	   (values `([,name ,(case prim
			       [(nodeid) `(my-id)]
			       [else expr])])
		   '())]

          [(,prim ,rand* ...) (guard (distributed-primitive? prim))
	   (mvlet ([(form memb) (token-names name)])
		  (values '() 
			  (append 
			   (explode-primitive form memb prim rand* heartbeat)
			   (if finalname
			       (primitive-return prim memb)
			       '()))
			  ))]
	  
          [,unmatched
            (error 'deglobalize "invalid expression: ~s"
                   unmatched)]))))

(define deglobalize
  (let ()

    (lambda (prog)
;      (pretty-print prog) (newline)
      (match prog
        [(,lang ;add-places-language 
	  (quote (program (props ,table ...) (control-flow ,cfg ...)
			  (lazy-letrec ,binds ,fin))))
	 ;; This is essentially a global constant for the duration of compilation:
	 (set! proptable table)
	 	 
	 ;; Make sure the final value is tagged simple:
;	 (if (memq 'local (assq fin proctable))
;	 (let ((temp (filter (lambda (ls) (memq 'final ls)
	 (let* ([leaves (map car (filter (lambda (ls) (memq 'leaf (cdr ls))) table))]
		[leaftoks (map (lambda (name) (symbol-append 'leaf-pulsar_ name)) leaves)])

	   (mvlet ([(entry constbinds tokenbinds) (process-letrec `(lazy-letrec ,binds ,fin))])
		
   ;	       (disp "Got the stuff " entry constbinds tokenbinds (assq entry constbinds))
		;; This pass uses the same language as the prior pass, lift-letrec
					;`(,input-language '(program ,body))
		`(deglobalize-lang '(program 
				     (bindings ,@constbinds)
				     ,(if (assq entry constbinds)
					  ;; Socpgm bindings are null for now:
					  `(socpgm (bindings ) 
;[2005.04.22] Getting rid of this temporarily
;	;					   (soc-return-finished ,entry)
						   (soc-return ,entry)
						   )
					  `(socpgm (bindings) (call spread-global))
					  )
			      (nodepgm (tokens ,@tokenbinds
					       ;; Make a pulsator for each leaf:
					       ,@(map (lambda (leaf leaftokname) 
							`[,leaftokname () 
							    (call ,(get-formation-name leaf))
							    (timed-call ,slow-pulse ,leaftokname)])
						      leaves leaftoks)
					       ;; Pulse the global gradient at one hertz:
					       [spread-global ()
							      (emit global-tree)
							      (timed-call 1000 spread-global)]
					       [global-tree () (relay)]

					       ;; THIS IS A YUCKY WAY TO DO IT:
;					       [spark-world () (call ,(get-membership-name 'world) this)]
					       )
				       				       
				       ;; <TODO> It's the LEAVES that need priming:
				       ,(if (assq entry constbinds)
					   `(startup )
					   ;; Here I should really only prime the world_tok if the
					   ;; world prim is used in the program:
					   `(startup ,@leaftoks ,@(if (assq 'spark-world tokenbinds)
								      (list 'spark-world)
								      '()))))
				       ))))
	 ]))))

;========================================

(define these-tests 
  `(
;    [(lazy-letrec () '3) unspecified]

    [(mvlet ([(a b) (get-names 'x)]) (list a b))
     (f_token_x m_token_x)]

    [(deglobalize '(lang '(program 
			   (props [result_1 final local])
			   (control-flow )
			   (lazy-letrec ((result_1 #f _ _ '3)) result_1))))
     unspecified]

    [(deglobalize '(lang '(program 
			   (props [b local]
				  [a local]
				  [anch distributed anchor]
				  [circ distributed final region]
				  )
			   (control-flow soc anch circ)
			   (lazy-letrec
			    ((b #f _ _ (cons '2 '()))
			     (a #f _ _ (cons '1 b))
			     (anch 0.5 _ _ (anchor-at a))
			     (circ 1.0 _ _ (circle anch '50)))
			    circ))))
     unspecified]


;[2004.08.03] Eliminating for now... might bring it back later...

    ;; Rfold should generate two handlers.  One of one arg and one of two.
;    ["test emit-primitive-handlers on rfold"
;     (emit-primitive-handlers '(activate-comp rfold myreg fun_f seed_s) 'form_tok 'memb_tok 2.0)
;     ,(lambda (ls)
;	(and (= 2 (length ls))
;	     (= 1 (+ (length (cadar ls)) (length (cadadr ls))))))]

  ))


(define test-this (default-unit-tester
		    "20: Deglobalize: to convert global to local program."
		    these-tests))

(define test20 test-this)
(define tests20 these-tests)
(define test-deglobalize test-this)
(define tests-deglobalize these-tests)

;==============================================================================




'(t '(letrec ((a (anchor-at '(30 40)))
		(r (circle-at 50 a))
		(f (lambda (tot next)
		     (cons (+ (car tot) (sense next))
			   (+ (cdr tot) 1))))
		(g (lambda (tot) (/ (car tot) (cdr tot))))
		(avg (smap g (rfold f (cons 0 0) r)))
		)
	 3))

'(deglobalize-lang
  '(program
     (socpgm (call result_12))
     (nodepgm
       result_12
       (bindings
         ((tmp_13 (cons '40 '()))
          (tmp_14 (cons '0 '0))
          (result_12 '3)
          (tmp_9 (cons '30 tmp_13))))
       (tokens
         ((g_2 (lazy-letrec
                 ((tmp_16 (cdr tot_6))
                  (tmp_17 (car tot_6))
                  (result_10 (/ tmp_17 tmp_16)))
                 (call result_10)))
          (f_4 (lazy-letrec
                 ((tmp_18 (cdr tot_8))
                  (tmp_19 (+ tmp_18 '1))
                  (tmp_21 (car tot_8))
                  (tmp_22 (+ tmp_21 tmp_20))
                  (result_11 (cons tmp_22 tmp_19)))
                 (call result_11)))
          (f_token_a_5 () (flood token_24))
          (token_24
            ()
            (if (< (locdiff (loc) tmp_9) 10.0)
                (elect-leader m_token_a_5))))))))






'(program ;;result???
	 (binds [target '(30 40)])
	 (tokens [form_a () (flood consider)] 
		 [consider () (if (< (locdiff (loc) (target)) 10.0)
			       (elect-leader memb_a)
			       '#f)]
		 [memb_a () (call form_r)]
		 [form_r () (emit memb_r)]
		 [memb_r () (begin (if (< (dist) 50) 
				       (relay))
				   (call fold_it))]
		 [memb_r:ret (v) (call map_it v)]
		 [fold_it () (return memb_r (aggregator f) (sense))]
		 [f (x y) (+ x y)]
		 
		 [map_it (v) (call g v)]
		 [g (v) (begin (...) (output __))]
		 )
	 )

'(f_token_result_2
  ((m_token_tmp_3 () (call f_token_result_2))
   (f_token_result_2 () (emit m_token_result_2))
   (m_token_result_2
    ()
    (if (< (dist f_token_result_2) '50) (relay)))
   (f_token_tmp_3 () (flood token_6))
   (token_6
    ()
    (if (< (locdiff (loc) tmp_1) 10.0)
	(elect-leader m_token_tmp_3)))))
