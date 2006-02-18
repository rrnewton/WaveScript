;;; TODO: Make it so that returning a distributed value actually
;;; collects and returns all the values therein, rather than just
;;; returning the corresponding membership token.


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

;; NOTE: To add code for a new primitive, the most important things to do are
;; to add an entry to explode-primitive and to primitive-return.


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


;;; Output grammar = Input grammar to cleanup-token-machine = messy

(define deglobalize_output_grammar
   `([code (statement ...)]
     [statement basic_tml]
     [statement gexpr]
     ))

;=========================================
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
(define RMAP-NUM 'RMAP) ;39)  ;; [2005.10.20] Why numbers?  Jeez...
(define ANCH-NUM 'ANCH) ;49)
(define CIRC-NUM 'CIRC) ;59)


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
    (if entry 
	;; The given symbol is either in the list of properties, or
	;; its the index to an association pair.
	(or (memq p (cdr entry))
	    (assq p (filter list? (cdr entry))))
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
	       `((greturn this              ;; Value
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
	       `((greturn ,formal              ;; Value
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


;; This produces a list of token bindings.
;; It generates code for one node in the dataflow graph.  It uses
;; get_membership_name to figure out from where control flow will come
;; on incoming edges.  
;; Below, "parent" won't be available if we don't have the full stream-graph...
(define explode-primitive
;; Inputs:
;;   form - the name of the formation token that sparks this node
;;   memb - the name of the membership token to which our output flows
;;   prim - the name of the distributed primitive (rmap, etc)
;;   args - the arguments to the primitive (they're simple: names/constants)
;;   heartbeat - the rate at which this primitive beats, if any.
;; Output: TokenBinds
  (lambda (form memb prim args heartbeat)
;	(disp "Explode primitive" name prim args)
	  (case prim
	    [(sparsify) (void)]
	    
	    ;; -=<TODO>=- UNSURE OF THIS
	    ;; SEEMS PRETTY USELESS!
	    ;[(sense)
	    ;`([,form () (sync-sense)]
					;;[,memb () ]
	    ;)]

	    [(rmap) 
	     (let* ([rator_tok (car args)]   ; The function
		    [region_tok (cadr args)] ; The region
		    [parent (get-membership-name region_tok)])

	     ;; We must check the type of the return value of our function.
	     (let ((rettype (check-prop 'returns rator_tok)))
	       (if (not rettype)
		   (error 'deglobalize:explode-primitive 
			  "rmap: Could not find the return value/type of function: ~s" region_tok))	       
	       (if (check-prop 'distributed (cadr rettype))
		   ;; First case: we're dealing with a function that returns a distributed value.

		   ;; All we can do here is trigger the formation token and wire the "return" (membership token)
		   ;; to our return.
		   ;; TODO: FIXME: HAVE WE MADE THE CLOSURE NON-REENTRANT?
		   (let ((form_child (get-formation-name (cadr rettype)))
			 (memb_child (get-membership-name (cadr rettype))))
		     ; We wire our control-flow parent directly to our child's formation token:
		     `([,parent (v) (call ,form_child v)]
		       [,memb_child (v) (call ,memb v)]))

		   ;; Second case: it's a local function, we simply subcall to it.
		   (if (not (check-prop 'region region_tok)) ; Is it push?
		       ;; In this case membership in the parent drives the rmap.
		       `([,parent (v) (call ,form v)]
			 [,form (v)
				(call ,memb
				      (subcall ,rator_tok v))])
		       ;; In this case rmap must run it's own heartbeat to keep it alive.
		       `([,parent () (activate ,form)]
			 [,form () "rmap with heartbeat"
				(call ,memb (subcall ,rator_tok this))
				(timed-call ,heartbeat ,form)]))
		   )))]

	    ;; This is always push.
	    [(light-up)
	     (let* ([region_tok (car args)]
		    [parent (get-membership-name region_tok)])
	       `([,parent (v) (call ,form v)]
		 [,form (v)
			(leds on green)
			(call ,memb v)]))]

	    [(rwhen-any)
	     (let* ([rator_tok (car args)]
		    [region_tok (cadr args)]
		    [parent (get-membership-name region_tok)]
		    ;[push? (not (check-prop 'region region_tok))]
		    )
	       ;; When we are a member of the input region, we locally try to detect the event:
	       `([,parent (v)
		   ;; If the predicate holds:
		   (if (subcall ,rator_tok v)
		       ;; Then we fire an event:
		       (begin 
			 ;(printf "Node ~s detected event ~s at time ~s!\n" (my-id) v (my-clock))
			 (call ,memb v)))]
		 [,form (v) (void)] ;; The formation token does nothing!
		 ))]

	    ;; [2005.10.20] We might want to think about doing some routing here.
	    ;; You can perform the smap anywhere between the stream source and destination.
	    ;; (In fact, you could even distribute it, if the computation were expensive!)
	    ;; But for now I just perform the computation at the source.
	    ;; So this is currently almost the same as rmap:
	    [(smap)
	     (let* ([rator_tok (car args)]
		    [region_tok (cadr args)]
		    [parent (get-membership-name region_tok)])
	       `([,parent (v) (call ,form v)]
		 [,form (v) (call ,memb (subcall ,rator_tok v))]))]

	    ;; [2005.11.27] smap2 is more tricky. 
	    ;; For now I'm just joining signals at the base-station.
	    [(smap2)
	     (DEBUGASSERT (= (length args) 3))
	     (let* ([rator_tok (car args)]
		    [sig1_tok (cadr args)]
		    [sig2_tok (caddr args)]
		    [parent1 (get-membership-name sig1_tok)]
		    [parent2 (get-membership-name sig2_tok)]
		    [catcher (new-token-name 'smap2_catcher)]
		    )
	       ;; Both parents return values to the base station:
	       `(
		 ; No binding for formation token!
		 [,parent1 (v) 
			   ;(printf "PARENT1: ~s\n" v)
			   (greturn (vector 1 v) (to ,catcher) (via global-tree))]
		 [,parent2 (v) 
			   ;(printf "PARENT2: ~s\n" v)
			   (greturn (vector 2 v) (to ,catcher) (via global-tree))]
		 ;; This catcher receives both values, right now it does a lame merge where on every update it outputs a both.
		 [,catcher (vec)
			   (stored [left #f] [right #f])
			   (printf "~a: SMAP2: left:~s right:~s\n" (my-id) left right)
			   (if (= 1 (vector-ref vec 0))
			       (set! left (vector-ref vec 1))
			       (if (= 2 (vector-ref vec 0))
				   (set! right (vector-ref vec 1))
				   (error ',catcher "smap2 catcher got a bad flag: ~s" (vector-ref vec 0))))
			   ;; Update the membership whenever we get a new left or new right:
			   (call ,memb (subcall ,rator_tok left right))
			   ]
		 [,form (v) (call ,memb (subcall ,rator_tok v))]))]

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
	       ;(inspect region_tok)
	       (let ([parent (get-membership-name region_tok)]     
		     [push? (not (check-prop 'region region_tok))])
		 `([,parent ,(if push? '(v) '()) 
			    ,(if push? 
				 `(call ,form v)
				 `(activate ,form v))]
		   [,form ,(if push? '(v) '())
			  (greturn
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
	    [(rrcluster)
	     (DEBUGASSERT (= (length args) 1))
	     (let  ([region_tok (car args)])
	       (let ([parent (get-membership-name region_tok)]
		     ;[spread-tok (new-token-name 'rrcluster-spread-tok)]
		     [leader-tok (new-token-name 'rrcluster-leader-tok)]
		     ;[push? (not (check-prop 'region region_tok))]
		     )
		 
		 ;; <TODO> <FIXME> ALLOW VARIABLE ARGUMENTS FOR TOKENS!!!
		 `(	    
		   [,parent (v) (call ,form v)]
		   [,form (v)
			  (stored [heldval #f])
			  ;; Freeze the value until we've formed the clusters
			  (set! heldval v)
			  (if (token-present? ,parent)
			      ;; TODO: Need to use a bounding function here.
			      (elect-leader (tok ,leader-tok 0))
			      ;(begin (grelay) (call ,leader-tok))
			      ;; Here we remove ourselves if we've overflowed?
					;(remove-tok ,spread-cluster)
			      )]
		   ;; Use the ID of the leader node to index this sub-region:
		   [,leader-tok (ldr val)
				(printf "WOOT: Cluster formed: index ~a, membership: \n" ldr )
				;; Unfreeze the value and declare membership
				;; NOTE: Currently no way that the spanning tree is revealed.
				(call (tok ,memb ldr) (ext-ref ,form heldval))]
		   )))]

	    
	    [(rfilter)
	     (match args
	       [(,pred_tok ,region_tok)
		(let ([parent (get-membership-name region_tok)])
		  `( [,parent (v) (call ,form v)] ;; member of that area
		     [,form (v) (if (subcall ,pred_tok v)
				    (call ,memb v))] ))])]

            ;; Does this work with flickering?
	    ;; Ignores timing properties and just considers any
	    ;; subregion firing a firing of the union region.
	    [(runion)
	     (let ([mem_a (get-membership-name (car args))]
		   [mem_b (get-membership-name (cadr args))])	       
	       `([,mem_a (v) (call ,memb v)]
		 [,mem_b (v) (call ,memb v)]
		 ))]
	    ;; UNFINISHED:
	    [(rintersect)
	     (let ([mem_a (get-membership-name (car args))]
		   [mem_b (get-membership-name (cadr args))])	       
	       `([,form (v) (if (and (token-present? ,mem_a)
				     (token-present? ,mem_b))
				(call ,memb this)
				(evict ,memb))]
		 [,mem_a (v) (call ,form v)]
		 [,mem_b (v) (call ,form v)]
		 ))]
	    
	    [(rrflatten)
	     (let ([parent (get-membership-name (car args))])	       
	       `([,parent (v) (call ,memb v)]))]
			 
	    ;; This is not a region. It's a signal.  
	    ;; The value on its membership token is the node-id of the leader.
	    ;; (But this token only fires on that leader node anyway...)
	    [(anchor-at) ;; 
	     (let ([consider (new-token-name 'cons-tok)]
		   [leader (new-token-name 'leader-tok)]
		   [calcdist (new-token-name 'calc-dist)]
		   [amwinner (new-token-name 'am-winner?)]
		   [target `(list ,@args)] ;; Location is an X/Y pair
		   )
	       `([,form () (elect-leader ,amwinner ,calcdist)] ;(flood ,consider)]
;		 [,consider () 
;			    (if (< (locdiff (loc) ,target) 10.0)
			    ;(if (< (locdiff (loc) ,target) 10)
;				(elect-leader ,memb)
				;'#f)
;			    ]
		 ;; Returns our score:
		 [,calcdist ()
;			    (printf "\nOurscore: ~a\n" (-. 0. (locdiff (loc) ,target)))
			    (-. 0. (locdiff (loc) ,target))]
		 [,form () 
			(draw-mark ,target)
			(leds on blue)
			]
		 ;; DEBUGGING
		 ;; This just lights up the node when it becomes anchor, for visualization:
		 [,amwinner (ldr val) 
			;; Note that we are an anchor.
;;			(set-simobject-homepage! 
;;			 this (cons 'anchor (simobject-homepage this)))
			;(light-up 0 255 255)
			(if (= ldr (my-id))
			    (begin (leds on red)
				   (call ,memb ldr)))
			]
		 [,memb (v) 
			;(if simalpha-visualize-anchors
			(leds on green) 
			(printf "Anchor-at WINNER! ~a\n" (my-id))
			]
		 ))]

	    ;; This is not a region; it carries no value on its membership token!
	    [(anchor-maximizing)
	     (let-match ([(,fun_tok ,refresh_rate) args])
	       (let ([consider (new-token-name 'cons-tok)]	     
		     [leader (new-token-name 'leader-tok)])
		 
	     `([,form () (elect-leader ,consider ,fun_tok)] ;(flood ,consider)]
;	       [,consider () (elect-leader ,memb ,fun_tok)]


;	       [,form () (draw-mark ,target (make-rgb 0 100 100))]
	       ;; DEBUGGING
	       ;; This just lights up the node when it becomes anchor, for visualization:
	       [,consider (ldr val)
		      ;; Note that we are an anchor.
;			(set-simobject-homepage! 
;			 this (cons 'anchor (simobject-homepage this)))
			  (if (= (my-id) ldr)
			      (begin
				(leds on red)
				(light-node 0 255 255)
				(call ,memb)))]
	       )))]

	    ;; [2005.11.23] This does essentially nothing.  It's
	    ;; formed at a node, it's membership is that node.
	    [(node->anchor)
	     `([,form (v) (call ,memb)])]

	    [(circle)
	     (let ([anch (car args)]
		   [rad (cadr args)])
;		   (arg (unique-name 'arg)))
	       `(;; Anchor membership carries no arguments:
		 [,(get-membership-name anch) () (call ,form)]
;		 [,form () (gemit ,memb this)]
		 [,form (v) (gemit ,memb)]
		 ;; Display stuff:
;		 [,form () (draw-circle (loc) 20)]
		 [,memb ()
			;; Note that we are inside a "hood".
;			(set-simobject-homepage! 
;			 this (cons 'circle (simobject-homepage this)))
			(begin 
			  ;(light-node 0 100 100)
			  )]
		 [,memb () (if (< (gdist) ,rad) (grelay))]
		 )
	       )]

	    [(khood)
	     (let ([anch (car args)]
		   [rad (cadr args)]
		   [spread (new-token-name 'spread-khood)]
		   [temp (new-token-name 'delay)])
;		   (arg (unique-name 'arg)))
	       `(;; Anchor membership carries no arguments:
		 [,(get-membership-name anch) () (call ,form)]
;		 [,form () (emit ,memb this)]

		 ;; Khood's are distinguished by their origin ID.
		 ;; TODO: FIXME: This is not sufficiently general.
		 [,form (v) (gemit (tok ,spread (my-id)))]
		 ;; Display stuff:
;		 [,form () (draw-circle (loc) 20)]
		 [,memb ()
			;; Note that we are inside a "hood".
;			(set-simobject-homepage! 
;			 this (cons 'circle (simobject-homepage this)))
			(leds on red)
			;(light-node 0 100 100)
			(void)
			]


;		 [,memb () (if (< (ghopcount) ,rad) (grelay))]

		 ;; TEMP:
		 [,spread id () (call ,memb (my-id)) ;; The "value" caried in this area is node-id.
			  (if (< (ghopcount) ,rad) (timed-call 1000 (tok ,temp id)))]
		 [,temp id () (grelay (tok ,spread id))]
		 
		 )
	       )]


#;	    
	    [(circle-at)
	     (let ([rad (cadr args)]
		   [loc (car args)])
	       `(
		 [,form () (gemit ,memb)]
		 [,memb () (if (< (gdist ,form) ,rad) (grelay))]
		 ))]	   		 

	    [(when-any)
	     (let* ([rator_tok (car args)]
		    [region_tok (cadr args)]
		    [mem_reg (get-membership-name region_tok)])
	       ;; If we pass, fire that event!
	       `([,mem_reg (v) (if (subcall ,rator_tok v)
				   (call ,memb v))]))]

	    [else `([UNHANDLED-EXPLODE-PRIM (,prim) (void)])])))


;  ======================================================================
;; LetrecExpr -> (Entry, Cbinds, TokenBinds)
;; This produces a list of constant bindings, token bindings, and
;; a token entry point of zero arguments.  The entrypoint is the
;; root, or finally returned edge of the data flow graph...
;;   <br><br>
;; [2005.10.27] This was blatantly incorrect before.  It just
;; turned the lazy letrec into a normal let*.  Now I'm making a
;; first attempt to reprocess the lazy-letrec bindings into a
;; valid let* binding using delazy-bindings.
(define process-letrec
      (lambda (expr)
        (match expr
	       [(lazy-letrec ([,lhs* ,type* ,heartbeat* ,formplace ,membplace ,rhs*] ...) ,body)
		(if (symbol? body)
		    (let loop ((lhs* lhs*) (heartbeat* heartbeat*) (rhs* rhs*)
			       (cacc '()) (tacc '()))
		      (if (null? lhs*)

			  ; [2005.11.23] Why do we return the formation tok for a distributed value?

			  ; Making this return the membership token, idea being that you can use that 
			  ; membership token to observe the value of the distributed prim (if you're 
			  ; in the right place!).
			  (values (if (check-prop 'distributed body)
				      (mvlet (((form memb) (token-names body)))
					     memb)
				      body)
				  (delazy-bindings cacc (list body))
				  ; Because we're returning the name of the member, 
				  ; we need to insure that there's some binding for it.
				  (append 
				   (if (check-prop 'distributed body)
				       (mvlet (((form memb) (token-names body)))
					 `((,memb () (void))))
				       ())
				   tacc))
			  ;; UHH TODO: membership or formation?
					;(map get-formation-name lhs*) 
			  (mvlet ([(cbinds tbinds) (process-expr (car lhs*) (car heartbeat*) (car rhs*))])
;				 (disp "GOT CBINDS TBINDS:" cbinds tbinds)
				 (loop (cdr lhs*) (cdr heartbeat*) (cdr rhs*)
				       (append cbinds cacc) 
				       (append tbinds tacc)))))
		    (error 'deglobalize "Body of letrec should be just a symbol at this point."))]
	  )))

; ======================================================================
;; Takes a set of lazy-letrec bindings and orders it (verifying that
;; there are no cycles) so that it will work with eager evaluation.
(define delazy-bindings
  (lambda (origbindings otherrefs)
    ;; Takes a list of (,name ,const), (,name ,name), (,name (,prim ,args ...)), or (,name (if ,x ,y ,z))
    ;; FIXME: TODO: check for cycles.
    
    (define (ref-counts binds)
      (let ((hash (make-default-hash-table)))
	;; First take into account any "external" refs to this block of bindings.
	(for-each 
	    (lambda (s) (let ((cur (hashtab-get hash s)))
			  (hashtab-set! hash s (if cur (fx+ 1 cur) 1))))
	  otherrefs)
	(for-each (lambda (b)
		    ;; Make sure there's an entry for every bound symbol:
		    (if (not (hashtab-get hash (car b))) (hashtab-set! hash (car b) 0))

		    (let-match ([(,name ,rhs) b])
		      (match rhs
				   [,sym (guard (symbol? sym))
					 (let ((entry (hashtab-get hash sym)))
					   (if entry
					       (hashtab-set! hash sym (fx+ 1 entry))
					       (hashtab-set! hash sym 1)))]
				   [(quote ,const) (guard (or (constant? const) (symbol? const))) (void)]
				   ;; This is node-local code now, check against TML prims:
				   [(,prim ,[args] ...)
				    (guard (or (token-machine-primitive? prim)
					       (basic-primitive? prim)))
				    (void)]
				   [(if ,[x] ,[y] ,[z]) (void)]
				   [,other (error 'deglobalize:delazy-bindings "trying to refcount, bad rhs subexpr: ~s" other)])))
	  binds)
	hash))

    (define firstcounts (ref-counts origbindings))

    ;; Simply inline everything that has only one reference:
    (define substituted-binds
	(begin ;(hashtab-for-each (lambda (k c) (printf "  Got count: ~s ~s\n" k c)) firstcounts)
	(let loop ((curbinds origbindings))
	  (if (null? curbinds) '()
	      (let-match ([(,name ,rhs) (car curbinds)])
		;; This inlines varrefs and creates a new rhs:
		(let ((newrhs (let inner ((xp rhs))
					 (match xp
					   [,sym (guard (symbol? sym))
						 ;(printf "Doing it to ~s... ~s ~s\n" sym (hashtab-get firstcounts sym) (assq sym origbindings))
						 (if (eq? 1 (hashtab-get firstcounts sym)) ;; Could be #f
						     (let ((entry (assq sym origbindings)))
						       (if entry
							   (inner (cadr (assq sym origbindings)))
							   ;; Otherwise it's a free variable!  Nothing to do with that.
							   sym))
						     sym)]
					   [(quote ,c) (guard (or (constant? c) (symbol? c))) `(quote ,c)]
					   [(if ,[x] ,[y] ,[z]) `(if ,x ,y ,z)]
					   [(,prim ,[args] ...) 
					    (guard (or (token-machine-primitive? prim)
						       (basic-primitive? prim)))
					    `(,prim ,args ...)]
					   [,other (error 'deglobalize:delazy-bindings 
							  "trying to inline, bad rhs subexpr: ~s" other)]
					   ))))
		  (cons `(,name ,newrhs) 
			(loop (cdr curbinds)))))))))

    (define newcounts (ref-counts substituted-binds))
    
    ;; Now recount the refs and remove anything that is unreferenced.
    (define pruned-binds
      (begin ;(hashtab-for-each (lambda (k c) (printf "Got new counts: ~s ~s\n" k c)) newcounts)
      (filter (match-lambda ((,name ,rhs))
		;; HACK: FIXME..
		;; We don't nix it unless WE made it unreferenced via our inlining.
		(not (and (eq? 0 (hashtab-get newcounts name))
			  (not (eq? 0 (hashtab-get firstcounts name))))))
	  substituted-binds)))

    ;; Check if IF is fixed up sufficiently.
    (for-each (match-lambda ((,name ,rhs))
		(define (nono rhs)
		  (match rhs
		    [,sym (guard (symbol? sym))
			  ;; Cannot currently delazify this, and we won't generate incorrect code:
			  (if (assq sym origbindings)
			      (error 'deglobalize:delazy-bindings
				     "IF construct currently cannot have lazy references in conseq/altern: reference to ~s in ~s"
				     sym rhs))]
		    [(quote ,const) (guard (or (symbol? const) (constant? const))) (void)]
		    [(,prim ,[args] ...) (guard (or (token-machine-primitive? prim)
						    (basic-primitive? prim))) (void)]
		    [(if ,[x] ,[y] ,[z]) (void)]))
		(define (ok rhs)
		  (match rhs
		    [,sym (guard (symbol? sym)) (void)]
		    [(quote ,const) (guard (or (symbol? const) (constant? const))) (void)]
		    [(,prim ,[args] ...) (guard (or (token-machine-primitive? prim)
						    (basic-primitive? prim))) (void)]
		    [(if ,[x] ,[nono -> y] ,[nono -> z]) (void)]))
		(ok rhs))
      pruned-binds)
		
    ;; Check if ordering is satisfied.
    (let ((edges (map (match-lambda ((,name ,rhs))
			(apply list name (filter (lambda (v) (assq v pruned-binds))
					   (tml-free-vars rhs))))
		   pruned-binds)))
      (if (cyclic? edges)
	  (error 'deglobalize:delazy-bindings
		 "Not allowed to have cycles in node-code let bindings: \n~s"
		 pruned-binds))
      
      ;; Return the appropriately sorted bindings:
      (map (lambda (v) (assq v pruned-binds))
	(reverse! (tsort edges))))
;    pruned-binds
    ))

; ======================================================================


    ;; Conditionals need to assert constraints on the ordering:
;     (let ((edges ;; Could use hashtable, but this shouldn't get too big. 
; 	   (map (lambda (bind)
; 		  (match bind
; 		    [(,name (if ,x ,y ,z))
; 		     ;; The test has to happen before the if.
; 		     ;; And the conseq/altern have to be inlined below or it's not a branch!
; 		     `((,x ,y))]
		     
; 		    [(,name (,prim ,names ...))
; 		     (filter symbol? names)
	   


;; This produces code for returning a value to the SOC 
 ;; from a particular primitive application.
(define primitive-return
  (lambda (prim tokname) ;; The tokname is a membership-name
    (case prim
      [(anchor-at anchor-maximizing node->anchor)
       ;; Node->anchor actually takes zero args.  But that should be fine.
       `([,tokname ()
		   ;; At each formation click, we output this node [id].
		   ,(if (deglobalize-markup-returns)
			`(call reg-return (list 'ANCH (my-id)))
			`(call reg-return (my-id)))
		   ])]

      [(circle circle-at)     
       `([,tokname 
	  ; FIXME FIXME FIXME: This matches by coincidence only with the other names..
	  ()
	  ;; At each formation click, we output this circle: 
	  ;;   For now this just lists the tokname, this should be the
	  ;; membership tokname for the circle.  Later we'll put some
	  ;; other hack in.
	  ;(call reg-return '(CIRC ,tokname this))
	  (call reg-return ',CIRC-NUM)
	  ])]

      [(rfilter)     
       `([,tokname (v)
		   ,(if (deglobalize-markup-returns)
			`(call reg-return (list (list 'FILTRATION ',tokname (my-id)) v))
			`(call reg-return v))
		   ])]

      ;; The membership for a fold means we're at the single point
      ;; that aggregates data.
      [(rfold)
       `([,tokname (v) ;; This value is a sample in the stream
		   ,(if (deglobalize-markup-returns)
			`(call reg-return (list (list 'RFOLD ',tokname (my-id)) v))
			`(call reg-return v))])]
      
      [(khood khood-at)
       `([,tokname () 
		   (leds on red)
		   (call reg-return 
			 ,(if (deglobalize-markup-returns)
			      `(list (list ',(symbol-uppercase prim) ',tokname (my-id)) #f)
			      (my-id))
			 )])]

      [(rmap light-up smap smap2 runion rrflatten rrcluster)
       `([,tokname (v) (call reg-return 
			     ,(if (deglobalize-markup-returns)
				  `(list (list ',(symbol-uppercase prim) (my-id)) v)
				  'v)
			     )])]

      ;; When the membership token has fired, the event has fired!
      ;; TODO: Need to implement greturn with retries!!
      [(rwhen-any)
       `([,tokname (v) ;; Event value		
		   ,(if (deglobalize-markup-returns)
			`(call reg-return (list (list 'EVENT (my-id)) v))
			'(call reg-return v))
		   ])]

      [else (error 'deglobalize:primitive-return 
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
			 `([,(get-formation-name name) () (call ,(get-membership-name name) ;,TMNULL
								)]
			   [spark-world () (call ,(get-membership-name name) ;,TMNULL
						 )]))]

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
	  [(lambda ,formalexp ,types ,[process-letrec -> entry primbinds tokenbinds])
;	   (if (not (null? tokenbinds))
;	       (error 'deglobalize 
;		      "Should not get any tokens from internal letrec right now!: ~s"
;		      tokenbinds))	   
	   (values '() 
		   (cons `[,name ,formalexp (let* ,primbinds ,entry)];(call ,entry))]
			 tokenbinds))]


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
			   (explode-primitive form memb prim rand* heartbeat)
			   (if finalname
			       (primitive-return prim memb)
			       '()))
			  ))]
	  
          [,unmatched
            (error 'deglobalize "invalid expression: ~s"
                   unmatched)]))))

; ==============================================================================

;; This is the main procedure for the pass itself.
(define deglobalize
  (let ()

    (lambda (prog)
;      (pretty-print prog) (newline)
      (match prog
        [(,lang ;add-places-language 
	  (quote (program (props ,table ...) (control-flow ,cfg ...)
			  (lazy-letrec ,binds ,fin)
			  ,type)))
	 ;; This is essentially a global constant for the duration of compilation:
	 (set! proptable table)
	 	 
	 ;; Make sure the final value is tagged simple:
;	 (if (memq 'local (assq fin proctable))
;	 (let ((temp (rfilter (lambda (ls) (memq 'final ls)
	 (let* ([leaves (map car (filter (lambda (ls) (memq 'leaf (cdr ls))) table))]
		[leaftoks (map (lambda (name) (symbol-append 'leaf-pulsar_ name)) leaves)])

	   (mvlet ([(entry constbinds tokenbinds) (process-letrec `(lazy-letrec ,binds ,fin))])
		
   ;	       (disp "Got the stuff " entry constbinds tokenbinds (assq entry constbinds))
		;; This pass uses the same language as the prior pass, lift-letrec
					;`(,input-language '(program ,body))
		`(deglobalize-lang 
		  '(program 
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
							      (timed-call ,(default-slow-pulse) ,leaftokname)])
					  leaves leaftoks)
				      ;; Pulse the global gradient at one hertz:
				      [spread-global ()
						     (gemit global-tree)
						     (timed-call 1000 spread-global)]
				      [global-tree () (grelay)]
				      ;[catcher (v) (soc-return v)]
				      [reg-return (v)
						  (if (= (my-id) ,BASE_ID)
						      (soc-return v)
						      ;; Otherwise use the global tree:
						      (greturn v
							       (to SOC-return-handler)
							       (via global-tree)))]

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

; ==============================================================================
;;; Unit tests.

(define these-tests 
  `(
;    [(lazy-letrec () '3) unspecified]

    ["Test get-names" 
     (mvlet ([(a b) (get-names 'x)]) (list a b))
     (f_token_x m_token_x)]

    ["Deglobalize simple program... (don't check result)"
     (deglobalize '(lang '(program 
			   (props [result_1 final local])
			   (control-flow )
			   (lazy-letrec ((result_1 _ #f _ _ '3)) result_1)
			   notype
			   )))
     unspecified]

    ["Deglobalize region-returning program: circle around anchor (don't check result)."
     (deglobalize '(lang '(program 
			   (props [b local]
				  [a local]
				  [anch distributed anchor]
				  [circ distributed final region]
				  )
			   (control-flow soc anch circ)
			   (lazy-letrec
			    ((b _ #f _ _ (cons '2 '()))
			     (a _ #f _ _ (cons '1 b))
			     (anch _ 0.5 _ _ (anchor-at a))
			     (circ _ 1.0 _ _ (circle anch '50)))
			    circ)
			   notype)))
     unspecified]


    ["Test delazy-bindings #1"
     (,delazy-bindings '((fooobo '3985) (barbar fooobo)) '(barbar))
     ((barbar '3985))]

    ["Test dalazy-bindings #2."
     (,delazy-bindings '((result_369 (if tmpbasic_373 '0 tmpbasic_376))
			 (tmpbasic_373 (= tmpbasic_372 '0))
			 (tmpbasic_372 (cdr v_364))
			 (tmpbasic_376 (/ tmpbasic_375 tmpbasic_374))
			 (tmpbasic_375 (car v_364)) 
			 (tmpbasic_374 (cdr v_364))
			 (fooobo '3985) )
		       '(result_369 fooobo))
     ([result_369 (if (= (cdr v_364) '0) '0 (/ (car v_364) (cdr v_364)))]
      [fooobo '3985])]

    ["Test delazy-bindings #3"
     (,delazy-bindings '((a '1) (b a) (c a)) '(c))
     ((a '1) (b a) (c a))
     ;((a '1) (c a))  ;; This depends on my nasty hack above...
     ]

    ["Test delazy-bindings #4"
     (,delazy-bindings '((a b) (b a)) '(a b))
     error]
    ["Test delazy-bindings #5"
     (,delazy-bindings '((a b) (b a)) '(a))
     error]

    ["Test delazy-bindings #6 (w free vars)"
     (,delazy-bindings '((result_7 (+ a_5 b_4))) '(result_7))
     ((result_7 (+ a_5 b_4)))]


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
		 [form_r () (gemit memb_r)]
		 [memb_r () (begin (if (< (gdist) 50) 
				       (grelay))
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
   (f_token_result_2 () (gemit m_token_result_2))
   (m_token_result_2
    ()
    (if (< (gdist f_token_result_2) '50) (grelay)))
   (f_token_tmp_3 () (flood token_6))
   (token_6
    ()
    (if (< (locdiff (loc) tmp_1) 10.0)
	(elect-leader m_token_tmp_3)))))

