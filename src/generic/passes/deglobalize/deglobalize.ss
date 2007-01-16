 ;;;; TODO: Make it so that returning a distributed value actually
;;;; collects and returns all the values therein, rather than just
;;;; returning the corresponding membership token.


;; INCOMPLETE: this doesn't actually use the CFG right now.

;;;; Pass 20: deglobalize
;;;; April 2004
;===============================================================================

;;;; This pass represents the biggest jump in the compiler.  It
;;;; transforms my simplified global language into a local program to
;;;; be run in each node in the sensor network.

;;;; We introduce a simple imperative language for the token-handlers
;;;; within each node programs.  In this case, it's naturally a subset
;;;; of scheme, but it gets reduced further in the next pass 
;;;; (cleanup-token-machine).

;; NOTE: To add code for a new primitive, the most important things to do are
;; to add an entry to explode-primitive and to primitive-return.


;;;; Input grammar:

;;;; <Pgm>  ::= <Let>
;;;; <Let>  ::= (lazy-letrec ([<Var> <Type> <Exp>]*) <Var>)
;;;; <Exp>  ::= <Simple>
;;;;          | (if <Simple> <Simple> <Simple>)
;;;;          | (lambda (<Var>*) (<Type>*) <Let>)
;;;;          | (<primitive> <Simple>*)
;;;; <Simple> ::= (quote <Lit>) | <Var>


;;;; Output grammar = Input grammar to cleanup-token-machine = messy

;;;;<br><br> [2006.03.15] 
;;;;
;;;;  Each "edge" in the query circuit, that is, each name bound in
;;;;  the program, will either have a "distributed" (monadic) or a
;;;;  "local" type.  Currently, each distributed name has
;;;;  corresponding membership/formation names.  It's operational
;;;;  realization will be some firing of membership tokens by that
;;;;  name. 
;;;;
;;;;  This is tricky with nested regions.  The type has two occurences
;;;;  of "Region" in it, but the compiler can only give one name to
;;;;  the thing (there would be no way to statically know how many
;;;;  names to give it).  This is where token indices come in.  The
;;;;  code generator here in deglobalize must cleverly index the
;;;;  membership and formation tokens so as to not allow undesirable
;;;;  collisions.

;===============================================================================
;; Some CHANGES (not keeping a complete log):

;;[2004.06.09] RRN: Adding a 'soc-return' form.  Anything

;;[2004.06.09] RRN: Adding implicit 'I-am-SOC' boolean variable for
;; use by node programs.. This is only for generated code, or my
;; handwritten test cases.

;; [2004.06.13] RRN: Moved functions for dealing with token names into helpers.ss


;=========================================
;; EXAMPLE:

;; This program just returns 3.  It has a generic binding defining a
;; single constant, it has no startup tokens (startups must be
;; *tokens* not other bindings.)  It has no socpgm-exclusive bindings,
;; and the socprogram merely returns the single value, then finishes.
#;
(program
  (bindings (result '3))
  (socpgm (bindings ) (soc-return result) (soc-finished))
  (nodepgm (tokens ) (startup )))


;===============================================================================

(module deglobalize mzscheme
  (require "../../../plt/common.ss"
           "../../../plt/hashtab.ss"
           (all-except "../../compiler_components/tml_generic_traverse.ss" test-this these-tests)
           (all-except "../../util/tsort.ss" test-this these-tests)
	   )

#;
  (require (lib "include.ss")
	   (lib "trace.ss")
	   "../generic/constants.ss"
           "../plt/iu-match.ss"
           "../plt/hashtab.ss"
	   "../plt/prim_defs.ss"
           (all-except "../plt/hm_type_inference.ss" test-this these-tests)
           (all-except "../plt/tsort.ss" test-this these-tests)
           (all-except "../plt/tml_generic_traverse.ss" test-this these-tests)
           (all-except "../../util/helpers.ss" test-this these-tests)
           (all-except "../plt/regiment_helpers.ss" test-this these-tests))
  
  (provide deglobalize test20 tests20 test-deglobalize tests-deglobalize
           delazy-bindings ;; [2006.04.02] Exposing this for use in pass17, should make its own pass if its going to stick around.        

	   lambda->heads
           )

  (chezimports )

#;
(define deglobalize_output_grammar
   `([code (statement ...)]
     [statement basic_tml]
     [statement gexpr]
     ))


;; THESE ARE TEMPORARY... using numbers to signify return values that are regions..
(define RMAP-NUM 'RMAP) ;39)  ;; [2005.10.20] Why numbers?  Jeez...
(define ANCH-NUM 'ANCH) ;49)
(define CIRC-NUM 'CIRC) ;59)

;; NOTREE is an object identifying that this region or signal has no routing tree.
(define NOTREE 'NO-TREE)
(define WORLDTREE 'WORLD-TREE)




;; THISOB is a piece of TML code that generates a representation of the current object.
;; Must be deterministic when run multiple times on the same node.
;(define THISOB ''(THISNODE))
;(define THISOB ''THISNODE)
(define THISOB '(cons 'THISNODE (cons (my-id) '())))

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

;; This takes a lambda expression and returns all its heads.
;; (All subexpressions of its body in head position.)
;; Then the membership token will be wired to the formation tokens of
;; each of these heads.
;;   Lambda's have no free variables, so there shouldn't be a problem here.
(define (lambda->heads expr)
  (match expr
    [(lambda ,v* ,ty* (lazy-letrec ,binds ,tail))
     (ASSERT (symbol? tail))
     (let ([types (append (map list v* ty*)
			  (map list (map car binds) (map cadr binds)))])
       ;; TODO: Should refer to the control flow graph here!!
       (let loop ([current (list (ASSERT (assq tail binds)))])
	 (match current
	   [() '()]
	   [([,name ,ty ,ants  (,prim ,rands ...)] . ,rest)
	    (guard (regiment-primitive? prim))
	    (if (not (distributed-type? ty))
	       (loop rest) ;; Local value computations needn't be "heads" in this sense.
	       (let ([deps (filter (lambda (v) 
				     (and (symbol? v)
					  (distributed-type? (cadr (ASSERT (assq v types))))
					  ;; Don't count the function arguments to rmap/rfold/etc!
					  (not (arrow-type? (cadr (ASSERT (assq v types)))))))
			     rands)])
		 ;; Do we have any dependencies on distributed values (parents)?
		 (match deps 
		   ;; This must be world or anchor. 
		   [() (cons (car current) (loop rest))] 		   

		   ;; This depends only on the input value, return it as a head.
		   [(,v) (guard (memq v v*))
		    (cons (car current) (loop rest))]

		   ;; This depends on another parent in the list.  Recur on that parent.
		   [(,v) (guard (assq v binds))
		    ;; Better not be cycles!
		    (loop (cons (assq v binds) rest))]

		   [,ls (error 'lambda->heads 
			       "Not handling prims with multiple distributed deps right now: ~s, deps ~s" 
			       `(prim ,@rands) ls)]
		   )))]
	  [,other (error 'lambda->heads "bad bind: ~s" other)]
	  )))]
    [,else (error 'lambda->heads "bad input:" )]))


;; This produces a list of token bindings.
;; It generates code for one node in the dataflow graph.  It uses
;; get_membership_name to figure out from where control flow will come
;; on incoming edges.  
;; Below, "parent" won't be available if we don't have the full stream-graph...
(define explode-primitive
;; Inputs:
;; .param  form - the name of the formation token that sparks this node
;; .param  memb - the name of the membership token to which our output flows
;; .param  prim - the name of the distributed primitive (rmap, etc)
;; .param  args - the arguments to the primitive (they're simple: names/constants)
;; .param  heartbeat - the rate at which this primitive beats, if any.
;; .param  tenv - the type environment
;; Output: TokenBinds
  (lambda (form memb prim args annots tenv dfg)
    (define heartbeat (cadr (ASSERT (assq 'heartbeat annots))))
    
;	(disp "Explode primitive" name prim args)
	  (case prim
	    [(sparsify) (void)]
	    
	    [(rmap) 
	     (let* ([rator_tok (car args)]   ; The function
		    [region_tok (cadr args)] ; The region
		    [parent (get-membership-name region_tok)])

	       ;; TODO FIXME:  DO ALL THIS STUFF BASED ON TYPE. [2006.04.04]
	       ;; STOP USING THE PROP LIST ENTIRELY!!

	     ;; We must check the type of the return value of our function.
	     (let ((rettype (check-prop 'returns rator_tok)))
	       (if (not rettype)
		   (error 'deglobalize:explode-primitive 
			  "rmap: Could not find the return value/type of function: ~s" region_tok))
	       (if (check-prop 'distributed (cadr rettype))
		   ;; First case: we're dealing with a function that returns a distributed value.

		   ;; NESTED REGIONS:
		   ;; All we can do here is trigger the formation token and wire the "return" (membership token)
		   ;; to our return.
		   ;; TODO: FIXME: HAVE WE MADE THE CLOSURE NON-REENTRANT?
		   (let ((form_child (get-formation-name (cadr rettype)))
			 (memb_child (get-membership-name (cadr rettype))))
		     ;; [2006.04.04] NOTE: it looks like this form_child invocation
		     ;; will ONLY work if the lambda's code has been set up to be PULL based.

;		     (error 'explode-exploded "this part doesn't work: ~s" memb)

		     ;; [2006.04.05] The thought back when this code was written was that the code
		     ;; for the lambda would be generated in a pull style.  So that we'd just pull the 
		     ;; result of the lambda and wait for it to manifest.
		     ; We wire our control-flow parent directly to our child's formation token:
; 		     `([,parent (v t) (call ,form_child v t)]
; 		       [,memb (v t)
; 			      (printf "Member of RMAP ~s: v:~s t:~s\n" ,memb v t)]
; 		       [,memb_child (v t) (call ,memb v t)])

		     (printf "Here's lambda:\n")
		     (pretty-print (cadr (assq rator_tok dfg)))
		     ;(inspect (assq rator_tok dfg))

		     (match (cadr (ASSERT (assq rator_tok dfg)))
		       [[,name ,ty ,ants (lambda ,v* ,vty* ,bod)]
			(let ([heads (lambda->heads `(lambda ,v* ,vty* ,bod))])
			  ;; For now we assume there is exactly one expression in head position.
			  (ASSERT (= 1 (length heads)))
			  `(;; We wire our parent to start up the lambda:
			    [,parent (v t) 
				     (dbg "~s: Wired ~s into start of lambda ~s!!" 
					     (my-id) ',parent ',(get-formation-name (caar heads)))
				     (call ,(get-formation-name (caar heads)) v t)]
			    ;; We wire the return pointer from that lambda to our own membership tok.
			    [,memb_child (v t) 
					 (dbg "~s: RETURNED from lambda tail ~s: v:~s t:~s" (my-id) ',memb_child v t)
					 (call ,memb v t)]
			    ))]
		       [,other (error 'explode-primitive "rmap couldn't find lambda arg in dfg: ~s" other)]
		       )
		     )

		   ;; Second case: it's a local function, we simply subcall to it.
		   (if (not (check-prop 'region region_tok)) ; Is it pushed by the parent?
		       ;; In this case membership in the parent drives the rmap.
		       `([,parent (v t) (call ,form v t)]
			 [,form (v t) (call ,memb (subcall ,rator_tok v) t)])
		       ;; In this case rmap must run it's own heartbeat to keep it alive.
		       (begin 
			 (warning 'explode-primitive ;fprintf (current-error-port)
				  "Note: rmap with own heartbeat: ~s\n" memb)
			 ;; This code only works for Region types,
			 ;; that's the only place where an rmap can
			 ;; drive itself with no outside input.
			 (ASSERT (types-compat? (recover-type region_tok tenv) 'Region))
			 
			 `([,parent (v t) 
;				    (if (not (equal? v ,THISOB))
;					(error 'rmap-with-heartbeat "didn't get THISOB in value position, instead: ~s" v))
				    (activate ,form v t)]
			   ;; Value should be NULL_ID
			   [,form (v t) "rmap with heartbeat"  
				  ;; Effectively ise the "stale" value (which is the node 
				  ;; itself in this case )when calling the membership:
				  (call ,memb (subcall ,rator_tok ,THISOB) t)
				  ;; [2006.04.01] Just keep that same stale value!
				  (timed-call ,heartbeat ,form v t)]
			   			  
			   )))
		   )))]

	    ;; Liftsig doesn't mean anything operationally.
	    ;; When the membership token gets called, we push out another call to our membership token.
	    [(liftsig)
	     (let* ([region_tok (car args)]
		    [parent (get-membership-name region_tok)])
	       `([,parent (v t) (call ,memb v t)]
		 
		 [,form (v t) (call ,memb v t)]))]

	    ;; This is always pushed by the parent operator.
	    [(light-up)
	     (let* ([region_tok (car args)]
		    [parent (get-membership-name region_tok)])
	       `([,parent (v t) (call ,form v t)]
		 [,form (v t)
			(leds on green)
			(dbg "~s: LightUP: v:~s t:~s" (my-id) v t)
			(call ,memb v t)]))]

	    ;; [2006.10.27] Hack, really, I should do this within deglobalize2:
	    ;; Just trying this out for a little test:
	    [(gossip)
	     (let* ([region_tok (car args)]
		    [parent (get-membership-name region_tok)])
	       `([,parent (v t) (call ,form v t)]
		 ;; Just bounce it onward with the same value & tree... (dunno about tree)
		 [,form (v t) (bcast ,memb v t)]))
	     ]

	    ;; Similar, but for signals:
	    [(slight-up)
	     (let* ([region_tok (car args)]
		    [parent (get-membership-name region_tok)])
	       `([,parent (v t) (call ,form v t)]
		 [,form (v t)
			(leds on green)
			(dbg "~s: SigLightUP: v:~s t:~s" (my-id) v t)
			(call ,memb v t)]))]


	    [(rwhen-any)
	     (let* ([rator_tok (car args)]
		    [region_tok (cadr args)]
		    [parent (get-membership-name region_tok)]
		    ;[push? (not (check-prop 'region region_tok))]
		    )
	       ;; When we are a member of the input region, we locally try to detect the event:
	       `([,parent (v t)
		   ;; If the predicate holds:
		   (if (subcall ,rator_tok v)
		       ;; Then we fire an event:
		       (begin 
			 ;( printf "Node ~s detected event ~s at time ~s!\n" (my-id) v (my-clock))
			 (call ,memb v t)))]
		 [,form (v t) (void)] ;; The formation token does nothing!
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
	       `([,parent (v t) 
			  ;; TODO: ASSERT: TREE SHOULD BE NULL FOR A SIGNAL FIXME FIXME FIXME
			  (call ,form v t)]
		 [,form (v t) (call ,memb (subcall ,rator_tok v) t)]))]
	    
	    
	    ;; [2006.10.27]
	    ;; Note : rintegrate has the same implementation as integrate.
	    [(integrate rintegrate)
	     (let* ([rator_tok (car args)]
		    [init_state (cadr args)]
		    [region_tok (caddr args)]
		    [parent (get-membership-name region_tok)])
	       `([,parent (v t) 
			  ;; TODO: ASSERT: TREE SHOULD BE NULL FOR A SIGNAL FIXME FIXME FIXME
			  (call ,form v t)]
		 [,form (v t) 
			(stored [state ,init_state])
			;; Arguments to function are <This> <Elem> <State>
			;; This == Null value.
			(let ([vec (subcall ,rator_tok '#() v state)])
			  (printf "INTEGRATE: old:~s  new:~s  emitted:~s\n" 
				  state (vector-ref vec 1) (vector-ref vec 0))
			  (set! state (vector-ref vec 1))
			  (call ,memb (vector-ref vec 0) t)
			  )]))]

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
		 [,parent1 (v t) ;,@(assert-tree 't)

			   ;( printf "PARENT1: ~s\n" v)
			   (greturn (vector 1 v) (to ,catcher) (via global-tree))]
		 [,parent2 (v t) ;,@(assert-tree 't)
			   ;( printf "PARENT2: ~s\n" v)
			   (greturn (vector 2 v) (to ,catcher) (via global-tree))]
		 ;; This catcher receives both values, right now it does a lame merge where on every update it outputs a both.
		 [,catcher (vec)
			   (stored [left #f] [right #f])
			   (dbg "~a: SMAP2: left:~s right:~s\n" (my-id) left right)
			   (if (= 1 (vector-ref vec 0))
			       (set! left (vector-ref vec 1))
			       (if (= 2 (vector-ref vec 0))
				   (set! right (vector-ref vec 1))
				   (error ',catcher "smap2 catcher got a bad flag: ~s" (vector-ref vec 0))))
			   ;; Update the membership whenever we get a new left or new right:
			   (call ,memb (subcall ,rator_tok left right) ',NOTREE)
			   ]
		 [,form (v t) (call ,memb (subcall ,rator_tok v) t)]))]

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
	     (let*  ([rator_tok (car args)]
		     [seed_val (cadr args)]
		     [region_tok (caddr args)]
;		    [return_handler (new-token-name 'rethand-tok)]
                    ;; The tree goes with the membership name for the khood expression.
                    ;; But we also need to worry about getting the correct subtok id.
                    ;; So this won't work right now.
		     [tempmemb (new-token-name 'returntotemp)]
		     ;; If we fail to find a tree, we use the global tree:
		     ;; [2006.04.04] TEMP FIXME: FOR NOW I INSIST THAT A TREE BE RESOLVED.
		     [tree (or (ASSERT (cadr (ASSERT (assq 'tree annots))))
			       'world)]
		     [treespreadsym (symbol-append (get-membership-name tree) '_gradspread)]
		    )
	       
;	       (if (eq? tree 'world) ;WORLDTREE)
;		   (inspect tree))
;	       (inspect (vector tree treespreadsym))

	       (let ([parent (get-membership-name region_tok)]     
		     [push? 
		      (not (check-prop 'region region_tok))
		      ;(not (eq? (recover-type region_tok tenv) 'Region))
		      ])
		 ;; FIXME: NEED TO FACTOR OUT THIS CODE WITH EXPLICIT HEARTBEAT OPS:
		 ;; Self driving should only be allowed for (Area Node) aka Region
		 (unless push? 
		   (let ([type (recover-type region_tok tenv)])
		     (ASSERT (eq? 'Region type))))
		 
		 `(

;		   ,(generate-edge parent form push?)

		   [,parent (v t)
			    (dbg "Fold parent value... v:~s t:~s \n" v t )
;			    ,@(REGIMENT_DEBUG (ASSERT (or (eq? t WORLDTREE))))
			    ,(if push? 
				 `(call ,form v t)
				 `(activate ,form v t))]
		   ;; Just inserts the extra argument.
		   [,tempmemb (v) (call ,memb v ',NOTREE)]
 		   [,memb (v t) (dbg "Member of fold result! ~s ~s\n" v t)]
		   [,form (v t)

			  (dbg "FORMING fold... v:~s t:~s  parent ~s PUSH: ~s\n" v t ,parent ,push?)
			  "This is a strange compromise for now."
			  "I statically compute which VIA token to use, but dynamically pass the subtok ind."
			  
;			  (fprintf (current-error-port) "Tree: ~s\n" t)

#;
			  (if (and (not (eq? t ',WORLDTREE))
				   (not (token-present? (tok ,treespreadsym (token->subid t)))))
			      (warning 'rfold "via token is not present at node ~s: ~s" 
				     (my-id) (tok ,treespreadsym (token->subid t))))

 			  (greturn
			     ,(if push? 'v THISOB)                     ;; Value
			     (to ,tempmemb)             ;; To
			     (via ;; Via
			      ;; If we know statically that it's the global-tree, just do that:
	 		      ,(if (eq? tree 'world);WORLDTREE)
		 		   'global-tree				   
				   `(tok ,treespreadsym (token->subid t)))
			      )

			  (seed ,seed_val)       ;; With seed
			  (aggr ,rator_tok))     ;; and aggregator

			  ;; If it's not driven by the parent, then we need to beat our heart:
			  ,@(if push? '()
				`((timed-call ,(/ 1000 heartbeat) ,form ,THISOB t)))]
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
		   [,parent (v t) (call ,form v t)]
		   [,form (v t)
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
				(dbg "WOO: Cluster formed: index ~a, membership: \n" ldr )
				;; Unfreeze the value and declare membership
				;; NOTE: Currently no way that the spanning tree is revealed. FIXME FIXME
				(call (tok ,memb ldr) (ext-ref ,form heldval) ',NOTREE)]
		   )))]

	    
	    [(rfilter)
	     (match args
	       [(,pred_tok ,region_tok)
		(let ([parent (get-membership-name region_tok)])
		  `( [,parent (v t) (call ,form v t)] ;; member of that area
		     [,form (v t) (if (subcall ,pred_tok v)
				    (call ,memb v t))] ))])]

            ;; Does this work with flickering?
	    ;; Ignores timing properties and just considers any
	    ;; subregion firing a firing of the union region.
	    [(runion)
	     (let ([mem_a (get-membership-name (car args))]
		   [mem_b (get-membership-name (cadr args))])	       
	       `([,mem_a (v t) (call ,memb v ',NOTREE)]
		 [,mem_b (v t) (call ,memb v ',NOTREE)]
		 ))]
	    ;; UNFINISHED:
	    [(rintersect)
	     (error 'explode-primitive "unfinished prim: rintersect")
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
	     ;; The result of flattening no longer has a single spanning tree.
	     ;;
	     ;; If we were cool we would maybe lace together the roots
	     ;; of the individual clusters as a shortcut to making a
	     ;; spanning tree.  But that's a complex optimization that
	     ;; is absolutely not worth it (and may perform worse).
	     (let ([parent (get-membership-name (car args))])	       
	       `([,parent (v t) (call ,memb v ',NOTREE)]))]
			 
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
;			    ( printf "\nOurscore: ~a\n" (-. 0. (locdiff (loc) ,target)))
			    (-. 0. (locdiff (loc) ,target))]

		 ;; Extra visualization code:
		 [,form () 
			(draw-mark ,target) ;,(IF_GRAPHICS `(draw-mark ,target) '(void))
			(leds on blue)
			]
		 ;; DEBUGGING
		 ;; This just lights up the node when it becomes anchor, for visualization:
		 [,amwinner (ldr val)
			;; Note that we are an anchor.
;;			(set-simobject-homepage! 
;;			 this (cons 'anchor (simobject-homepage this)))
			(if (= ldr (my-id))
			    (begin (leds on red)
				   (call ,memb ldr ',NOTREE))) ;; FIXME: THIS SHOULD GET A VALID TREE.
			]
		 [,memb (v t)  ;; FIXME : DETERMINE WHAT THIS TREE VALUE SHOULD BE.
			;(if simalpha-visualize-anchors
			(leds on green)
			(dbg "Anchor-at WINNER! ~a\n" (my-id))
			]
		 ))]

	    ;; This is not a region; it carries no value on its membership token!
	    [(anchor-maximizing)
	     (let-match ([(,fun_tok ,parent_region) args])
	       (let ([consider (new-token-name 'cons-tok)]	     
		     [tmpfun (new-token-name 'tmpfun)]
		     [leader (new-token-name 'leader-tok)]
		     [parent-memb (get-membership-name parent_region)])
		 
	     `(
	       ;; TODO: FIXME:
	       ;; The leader election is currently not bounded, so it will flood the whole network.
	       [,parent-memb (v t) (call ,form v t)]

	       [,form (v t) (elect-leader ,consider ,tmpfun)] ;(flood ,consider)]
;	       [,consider () (elect-leader ,memb ,fun_tok)]

	       ;; This is a wrapped that has zero arity.
	       [,tmpfun () (subcall ,fun_tok ,THISOB)]

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
				(call ,memb ,THISOB ',NOTREE)))]
	       )))]

	    ;; [2005.11.23] This does essentially nothing.  It's
	    ;; formed at a node, it's membership is that node.
	    [(node->anchor) `([,form (v t) (call ,memb v t)])]

	    [(circle)
	     (warning 'explode-primitive "circle not correctly implemented currently")
	     (let ([anch (car args)]
		   [rad (cadr args)])
;		   (arg (unique-name 'arg)))
	       `(;; Anchor membership carries no arguments:
		 [,(get-membership-name anch) () (call ,form ,THISOB ',NOTREE)]

		 ;; FIXME: Unnecessary argument passing through the network:
		 [,form (v t) (gemit ,memb v t)]
		 ;; Display stuff:
		 [,memb (v t)
			;; Note that we are inside a "hood".
;			(set-simobject-homepage! 
;			 this (cons 'circle (simobject-homepage this)))
			(begin 
			  ;(light-node 0 100 100)
			  )]
 
		 ;; [2006.03.27] This isn't right:  We need the euclidean dist.
		 ;; I might have used gdist for this at some point, but no longer.
		 [,memb (v t) (if (< (gdist) ,rad) (grelay))]
		 )
	       )]


	    [(khood)
	     (let ([anch (car args)]
		   [rad (cadr args)]
		   [spread (symbol-append memb '_gradspread)]  ;(new-token-name 'spread-khood)]
		   [temp (new-token-name 'delay)])
;		   (arg (unique-name 'arg)))
	       `(;; Anchor membership carries no arguments:
		 [,(get-membership-name anch) (v t) (call ,form v t)]
;		 [,form () (emit ,memb this)]

		 ;; Khood's are distinguished by their origin ID.
		 ;; TODO: FIXME: This is not sufficiently general.
		 [,form (v t) (gemit (tok ,spread (my-id)))]

		 [,memb (v t)
			;; Note that we are inside a "hood".
;			(set-simobject-homepage! 
;			 this (cons 'circle (simobject-homepage this)))
			(leds on red)
			;(light-node 0 100 100)
			(void)
			]

		 ;; The "value" caried in this area is the node itself.
		 [,spread id () (call ,memb ,THISOB (tok ,spread id))
			  (if (< (ghopcount) ,rad) (timed-call 1000 ;,(default-fast-pulse) 
							       (tok ,temp id)))]		 
		 ;; This is the continuation of spread, it just keeps on spreadin'
		 [,temp id () (grelay (tok ,spread id))]
		 ))]


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
	       `([,mem_reg (v t) (if (subcall ,rator_tok v)
				     (call ,memb v t))]))]

	    [else 
	     (error 'explode-primitive "unhandled prim: ~s\n" prim)
	     `([UNHANDLED-EXPLODE-PRIM (,prim) (void)])
	     ])))

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
      (lambda (expr tenv dfg)
        (match expr
	       [(lazy-letrec ([,lhs* ,type* ,annots* ,rhs*] ...) ,body)
		(let ([tenv (tenv-extend tenv lhs* type* #t)])
		  (if (symbol? body)
		    (let loop ((lhs* lhs*) (annots* annots*) (rhs* rhs*)
			       (cacc '()) (tacc '()))
		      (if (null? lhs*)

			  ; [2005.11.23] Why do we return the formation tok for a distributed value?

			  ; Making this return the membership token, idea being that you can use that 
			  ; membership token to observe the value of the distributed prim (if you're 
			  ; in the right place!).
			  (values (if (check-prop 'distributed body)
				      (mvlet (((form memb) (token-names body))) memb)
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
			  (mvlet ([(cbinds tbinds) 
				   (process-expr (car lhs*) (car annots*) (car rhs*) tenv dfg)])
;				 (disp "GOT CBINDS TBINDS:" cbinds tbinds)
				 (loop (cdr lhs*) (cdr annots*) (cdr rhs*)
				       (append cbinds cacc) 
				       (append tbinds tacc)))))
		    (error 'deglobalize "Body of letrec should be just a symbol at this point."))
		  )]
	  )))

; ======================================================================
;; Takes a set of lazy-letrec bindings and orders it (verifying that
;; there are no cycles) so that it will work with eager evaluation.
;; .param origbindings  A list of lazy-letrec declarations.
;; .param otherrefs  A list of symbols, these are external references to the list of bindings.  
;; Used for reference counting.
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

		    (let ([name (car b)] [rhs (rac b)] [extras (rdc (cdr b))])
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
					       ;(regiment-primitive? prim)
					       (basic-primitive? prim)))
				    (void)]
				   [(if ,[x] ,[y] ,[z]) (void)]
				   ;[(tupref ,n ,m ,[x]) (void)]
				   ;[(tuple ,[args] ...) (void)]

				   [,other (error 'deglobalize:delazy-bindings
						  "trying to refcount, bad rhs subexpr: ~s" other)])))
	  binds)
	hash))

    (define firstcounts (ref-counts origbindings))

    ;; Simply inline everything that has only one reference:
    (define substituted-binds
	(begin ;(hashtab-for-each (lambda (k c) ( printf "  Got count: ~s ~s\n" k c)) firstcounts)
	(let loop ((curbinds origbindings))
	  (if (null? curbinds) '()
	      ;; [2006.04.02] Having problems with this usage of ... currently:
;	      (let-match ([(,name ,extras ... ,rhs) (car curbinds)])
	      (let* ([b (car curbinds)] [name (car b)] [rhs (rac b)] [extras (rdc (cdr b))])
		;; This inlines varrefs and creates a new rhs:
		(let ((newrhs (let inner ((xp rhs))
					 (match xp
					   [,sym (guard (symbol? sym))
						 ;( printf "Doing it to ~s... ~s ~s\n" sym (hashtab-get firstcounts sym) (assq sym origbindings))
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
		  (cons `(,name ,@extras ,newrhs) 
			(loop (cdr curbinds)))))))))

    (define newcounts (ref-counts substituted-binds))
    
    ;; Now recount the refs and remove anything that is unreferenced.
    (define pruned-binds
      (begin ;(hashtab-for-each (lambda (k c) ( printf "Got new counts: ~s ~s\n" k c)) newcounts)
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
       `([,tokname (v t)
		   ;; At each formation click, we output this node [id].
		   ,(if (deglobalize-markup-returns)
			`(call reg-return (list 'ANCH (my-id)))
			`(call reg-return (my-id)))
		   ])]

      [(circle circle-at)     
       `([,tokname 
	  ; FIXME FIXME FIXME: This matches by coincidence only with the other names..
	  (v t)
	  ;; At each formation click, we output this circle: 
	  ;;   For now this just lists the tokname, this should be the
	  ;; membership tokname for the circle.  Later we'll put some
	  ;; other hack in.
	  ;(call reg-return '(CIRC ,tokname this))
	  (call reg-return ',CIRC-NUM)
	  ])]

      [(rfilter)     
       `([,tokname (v t)
		   ,(if (deglobalize-markup-returns)
			`(call reg-return (list (list 'FILTRATION ',tokname (my-id)) v))
			`(call reg-return v))
		   ])]

      ;; The membership for a fold means we're at the single point
      ;; that aggregates data.
      [(rfold)
       `([,tokname (v t) ;; This value is a sample in the stream
		   ,(if (deglobalize-markup-returns)
			`(call reg-return (list (list 'RFOLD ',tokname (my-id)) v))
			`(call reg-return v))])]
      
      [(khood khood-at)
       `([,tokname (v t)
		   (leds on red)
		   (call reg-return 
			 ,(if (deglobalize-markup-returns)
			      `(list (list ',(symbol-uppercase prim) ',tokname (my-id)) #f)
			      `(my-id))
			 )])]

      ;; For all these primitives, returning just means sending the
      ;; argument of the membership token to the base-station.
      [(rmap light-up slight-up smap smap2 runion rrflatten rrcluster liftsig gossip integrate rintegrate)
       `([,tokname (v t) 
		   (call reg-return 
			 ,(if (deglobalize-markup-returns)
			      `(list (list ',(symbol-uppercase prim) (my-id)) v)
			      'v)
			 )])]

      ;; When the membership token has fired, the event has fired!
      ;; TODO: Need to implement greturn with retries!!
      [(rwhen-any)
       `([,tokname (v t) ;; Event value		
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
      (lambda (name annots expr tenv dfg)	
	(let ((finalname (check-prop 'final name)))
        (match expr

	  ;; The result of the 'world' primitive just wires the world
          ;; spark to whatever value-name this is, and wires the
          ;; formation token straight to the membership.
          [world (values '() 
			 ;; This wires the formation token for this name to its membership token.
			 `([,(get-formation-name name) () 
;			    (warning 'world-prim "formation token called: ~s " ',name)
			    (call ,(get-membership-name name) ,THISOB ',WORLDTREE)]
			   ;; [2006.03.30] This was pattently wrong.
			   ;; This along with the above binding can double-stimulate the membership token.
			   ;; Passes unit tests without this:
			   ;[spark-world () (call ,(get-membership-name name) ,THISOB ',WORLDTREE)]
			   ))]

          ;; The possibility that the final value is local is
	  ;; handled in 'deglobalize' so we don't worry about it here:
	  [,x (guard (simple? x))      
	      (values `([,name ,expr]) ;`([,name (begin (return ,x))])
		      '())]
	  
	  ;; NOTE: FIXME FIXME:
	  ;; This won't work if we're inside a signal monad (or shouldn't!).
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

	  [(sense (quote ,type) ,node) ;; Transforms into a local sense.  Doesn't use node...
	   (guard (string? type))
	   (if (equal? type "clock")
	       ;; [2006.02.15] For now we have a cheater clock provided for us by the simulator:
	       (values `([,name (my-clock)]) '())
	       (values `([,name (sync-sense (quote ,type))]) '())
	       )]

	  [(sense . ,other)
	   (error 'deglobalize "invalid sense syntax: ~s" `(sense . ,other))]

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
			       [(tupref ,a ,b ,x) `(vector-ref ,x ' ,a)]
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
	  (quote (program (props ,table ...)
		   (control-flow ,cfg ...)
		   (data-flow ,dfg ...)
		   (lazy-letrec ,binds ,fin)
		   ,type)))
	 ;; This is essentially a global constant for the duration of compilation:
	 (set! proptable table)
	 	 
	 ;; Make sure the final value is tagged simple:
;	 (if (memq 'local (assq fin proctable))
;	 (let ((temp (rfilter (lambda (ls) (memq 'final ls)
	 (let* ([leaves (map car (filter (lambda (ls) (memq 'leaf (cdr ls))) table))]
		[leaftoks (map (lambda (name) (symbol-append 'leaf-pulsar_ name)) leaves)])

	   (mvlet ([(entry constbinds tokenbinds) (process-letrec `(lazy-letrec ,binds ,fin) 
								  (empty-tenv) dfg)])
		
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
						     (timed-call ,(default-slow-pulse) spread-global)]
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
			    `(startup ,@leaftoks 
				      ;; [2006.03.30] Killing spark-world, leaf-pulsar handles all.
				      ;; Might need to think about this more with programs with trickier control flow.
				      ;,@(if (assq 'spark-world tokenbinds)
					;    (list 'spark-world)
					;    '())
				      )))
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
			   (data-flow )
			   (lazy-letrec ((result_1 Int ([heartbeat #f] [formplace _] [membplace _]) '3)) result_1)
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
			   (data-flow )
			   (lazy-letrec
			    ((b (List Int) ([heartbeat #f] [formplace _] [membplace _]) (cons '2 '()))
			     (a (List Int) ([heartbeat #f] [formplace _] [membplace _]) (cons '1 b))
			     (anch Anchor ([heartbeat 0.5] [formplace _] [membplace _]) (anchor-at a))
			     (circ Region ([heartbeat 1.0] [formplace _] [membplace _]) (circle anch '50)))
			    circ)
			   notype)))
     unspecified]


    ["Test delazy-bindings #0" (delazy-bindings '() '()) ()]

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


    ["Lambda->heads"
     (caar (lambda->heads
     '(lambda (n_354)
	(Node)
	(lazy-letrec
	 ((resultofonehop_356           Region
           ((heartbeat 1000))
           (khood tmpnodeanchor_361 '1))
	  (tmpnodeanchor_361	   Anchor
	   ((heartbeat 1000))
	   (node->anchor n_354)))
	 resultofonehop_356))))
     tmpnodeanchor_361]

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

) ;; End module


#;
(t '(letrec ((a (anchor-at '(30 40)))
		(r (circle-at 50 a))
		(f (lambda (tot next)
		     (cons (+ (car tot) (sense next))
			   (+ (cdr tot) 1))))
		(g (lambda (tot) (/ (car tot) (cdr tot))))
		(avg (smap g (rfold f (cons 0 0) r)))
		)
	 3))

#;
(deglobalize-lang
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




#;
(program ;;result???
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

#;
(f_token_result_2
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



;(require pass20_deglobalize)

