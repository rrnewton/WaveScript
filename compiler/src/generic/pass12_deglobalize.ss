;(require (lib "trace.ss") (lib "iu-match.ss") "../plt/helpers.ss")

;;; Pass 10: deglobalize
;;; April 2004
;===============================================================================

;;; This pass represents the biggest jump in the compiler.  It
;;; transforms my simplified global language into a local program to
;;; be run in each node in the sensor network.


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

;;; RRN: Should we introduce a simple imperative language here???

;;;  <Pgm> ::= (program (bindings <Decl>*) <SOCPgm> <NodePgm>)
;;;  <SOCPgm> ::= <Statement*>
;;;  <NodePgm> ::= (nodepgm <Entry> (bindings <Decl>*) (tokens <TokBinding>*))
;;;  <Entry>  ::= <Token>
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
'(program
  (bindings (result '3))
  (socpgm (bindings ) (soc-return result) (soc-finished))
  (nodepgm (tokens ) (startup )))

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
	(error 'pass10_deglobalize:check-prop
	       "This should not happen!  ~nName ~s has no entry in ~s."
	       s proptable))))

    (define (simple? x) 
      (match x
          [(quote ,imm) #t]
          [,var (guard (symbol? var)) #t]
	  [,otherwise #f]))

    (define symbol-append
      (lambda args
	(string->symbol (apply string-append (map symbol->string args)))))


;; (Name, DistributedPrim, Args) -> TokenBinds
;; This produces a list of token bindings.
(define explode-primitive
  (lambda (form memb prim args)
;	(disp "Explode primitive" name prim args)
	  (case prim
	    [(sparsify) (void)]
	    
	    ;; -=<TODO>=- UNSURE OF THIS
	    ;; SEEMS PRETTY USELESS!
	    [(sense)
	     `([,form () (local-sense)]
	       ;[,memb () ]
	       )]

	    [(smap)
	     (let* ([rator_tok (car args)]
		   [region_tok (cadr args)]
		   [parent (get-membership-name region_tok)])
	       `([,parent (v) (call ,form v)]
		 [,form (v) 
			(call ,memb
			      (call ,rator_tok v))]
		 ;[,memb (v) ...]  ;; This carries the value actively...
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
	    [(rfold)
	     (let  ([rator_tok (car args)]
		    [seed_val (cadr args)]
		    [region_tok (caddr args)]
;		    [return_handler (new-token-name 'rethand-tok)]
		    )
	       (let ([parent (get-membership-name region_tok)])
	       `([,parent (v) (call ,form v)]
		 [,form (v) (return
			     v              ;; Value
;			     ,return_handler ;; To
			     (to ,memb) ;; To
			     (via ,parent)           ;; Via
			     (seed ,seed_val)       ;; With seed
			     (aggr ,rator_tok))]    ;; and aggregator
;		 [,memb (v) ] ;; This occurs at the fold-point
		 )))]

	    ;; This is not a region; it carries no value on its membership token!
	    [(anchor-at)
	     (let ([consider (new-token-name 'cons-tok)]
		   [leader (new-token-name 'leader-tok)]
		   [target (car args)])
	       `([,form () (flood ,consider)]
		 [,consider () (if (< (locdiff (loc) ,target) 10.0)
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
	     (let ([rad (cadr args)]
		   [anch (car args)])
;		   (arg (unique-name 'arg)))
	       `(;; Anchor membership carries no arguments:
		 [,(get-membership-name anch) () (call ,form)]
		 [,form () (emit ,memb this)]
		 ;; Display stuff:
		 [,form () (draw-circle (loc) 20)]
		 [,memb (v)
			;; Note that we are inside a "hood".
;			(set-simobject-homepage! 
;			 this (cons 'circle (simobject-homepage this)))
			(light-up 0 100 100)]
;		 [,memb () (if (< (dist ,form) ,rad) (relay))]
;		 [,memb () (if (< (dist ,memb) ,rad) (relay))]
		 [,memb (v) (if (< (dist) ,rad) (relay))]
		 )
	       )]


	    [(khood)
	     (let ([rad (cadr args)]
		   [anch (car args)])
;		   (arg (unique-name 'arg)))
	       `(;; Anchors' membership tokens have no arguments:
		 [,(get-membership-name anch) () (call ,form)]
		 [,form () (emit ,memb this)]
		 ;; Display stuff:
		 [,form () (draw-circle (loc) 20)]
		 [,memb (v) 
			;; Note that we are inside a "hood".
			(set-simobject-homepage! 
			 this (cons 'khood (simobject-homepage this)))
			(light-up 0 100 100)]
;		 [,memb () (if (< (dist ,form) ,rad) (relay))]
;		 [,memb () (if (< (dist ,memb) ,rad) (relay))]
		 [,memb (v) (if (< (dist) ,rad) (relay))]
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

	    [else `([UNHANDLED-EXPLODE-PRIM (,prim) (void)])])))


    ;; LetrecExpr -> (Entry, Cbinds, TokenBinds)
    ;; This produces a list of constant bindings, token bindings, and
    ;; a token entry point of zero arguments.  The entrypoint is the
    ;; root, or finally returned edge of the data flow graph...
    (define process-letrec
      (lambda (expr)
        (match expr
	       [(lazy-letrec ([,lhs* ,rhs*] ...) ,body)
		(if (symbol? body)
		    (let loop ((lhs* lhs*) 
			       (rhs* rhs*)
			       (cacc '())
			       (tacc '()))
		      (if (null? lhs*)
			  (values (if (check-prop 'distributed body)
				      (mvlet (((form memb) (token-names body)))
					     form)
				      body)
				  cacc tacc)
			  ;; UHH TODO: membership or formation?
					;(map get-formation-name lhs*) 
			  (mvlet ([(cbinds tbinds) (process-expr (car lhs*) (car rhs*) )])
;				 (disp "GOT CBINDS TBINDS:" cbinds tbinds)
				 (loop (cdr lhs*) (cdr rhs*)
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
	  (soc-return (list 'ANCH this))])]

      [(circle circle-at)     
       `([,tokname 
	  ()
	  ;; At each formation click, we output this circle: 
	  ;;   For now this just lists the tokname, this should be the
	  ;; membership tokname for the circle.  Later we'll put some
	  ;; other hack in.
	  (soc-return '(CIRC ,tokname))])]

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
      
      [else (error 'primitive-return 
		   "This function incomplete; doesn't cover: ~s. Ryan, finish it! "
		   prim)]
      )))

   ;; (Name, Expr) -> (Cbinds, TokenBinds)
   ;; This processes an expression and returns both its constant
   ;; bindings, and it's token bindings.
    (define process-expr
      (lambda (name expr)	
	(let ((finalname (check-prop 'final name)))
        (match expr
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
		   (cons `[,name ,formalexp (lazy-letrec ,constbinds (call ,entry))]
			 tokenbinds))]

	  ;; TODO:
          [(,prim ,rand* ...) (guard (basic-primitive? prim))
	   (values `([,name ,expr]) '())]

          [(,prim ,rand* ...) (guard (distributed-primitive? prim))   
	   (mvlet ([(form memb) (token-names name)])
		  (values '() 
			  (append 
			   (explode-primitive form memb prim rand*)
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
        [(,input-language (quote (program (props ,table ...) (lazy-letrec ,binds ,fin))))
	 ;; This is essentially a global constant for the duration of compilation:
	 (set! proptable table)
	 	 
	 ;; Make sure the final value is tagged simple:
;	 (if (memq 'local (assq fin proctable))
;	 (let ((temp (filter (lambda (ls) (memq 'final ls)

	 (mvlet ([(entry constbinds tokenbinds) (process-letrec `(lazy-letrec ,binds ,fin))])
		
   ;	       (disp "Got the stuff " entry constbinds tokenbinds (assq entry constbinds))
		;; This pass uses the same language as the prior pass, lift-letrec
					;`(,input-language '(program ,body))
		`(deglobalize-lang '(program 
				     (bindings ,@constbinds)
				     ,(if (assq entry constbinds)
					  ;; Socpgm bindings are null for now:
					  `(socpgm (bindings ) (soc-return ,entry) (soc-finished))
					  `(socpgm (bindings ) (call ,entry)))
			      (nodepgm (tokens ,@tokenbinds)
				       				       
				       ;; <TODO> It's the LEAVES that need priming:
				       ,(if (assq entry constbinds)
					    `(startup )
					    ;; How did this make sense:
					;`(startup ,entry)
					    `(startup )
					    )
				       ))))
	 ]))))

;;;  <Pgm> ::= (program <SOCPgm> <NodePgm>)
;;;  <SOCPgm> ::= <Statement*>
;;;  <NodePgm> ::= (nodepgm <Entry> (bindings <Decl>*) (tokens <TokBinding>*))



;========================================

(define these-tests 
  `(
;    [(lazy-letrec () '3) unspecified]

    [(mvlet ([(a b) (get-names 'x)]) (list a b))
     (f_token_x m_token_x)]

    [(deglobalize '(lang '(program 
			   (props [result_1 final local])
			   (lazy-letrec ((result_1 '3)) result_1))))
     unspecified]

    [(deglobalize '(lang '(program 
			   (props [b local]
				  [a local]
				  [anch distributed anchor]
				  [circ distributed final region]
				  )
			   (lazy-letrec
			    ((b (cons '2 '()))
			     (a (cons '1 b))
			     (anch (anchor-at a))
			     (circ (circle anch '50)))
			    circ))))
     unspecified]
  ))


(define test-this (default-unit-tester
		    "Pass10: Pass to convert global to local program."
		    these-tests))  

(define test10 test-this)
(define tests10 these-tests)

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