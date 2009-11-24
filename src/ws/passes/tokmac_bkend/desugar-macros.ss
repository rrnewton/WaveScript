
;;;; .title Desugar macros (pass22_desugar-macros.ss)

;;;; This removes various sugar:    <br>
;;;;  *) soc-return                 <br>
;;;;  *) elect-leader               <br>
;;;;  *) flood

; =======================================================================
;;;; NOTES: <br><br>

;;;; [2005.04.20] <br>
;;;; Soc-return's are a strange beast. <br><br>

;;;; [2005.10.02] I was just doing this in cleanup-tokmac, but I'm going to move it here.
;;;; <br><br>

;;;; [2005.10.12] <br>
;;;; For now we're only allowing soc-returns from the base-station node,
;;;; there is no implicit "global tree" in TML.  (Regiment does generate
;;;; code for such a global tree, but TML makes no such assumption.)

;;; Main procedure.

(module desugar-macros mzscheme
  (require "../../../plt/common.ss"
	   (all-except "../../compiler_components/tml_generic_traverse.ss" test-this these-tests))
  (provide desugar-macros
	   test-desugar-macros)
  (chezimports )

;; This is the compiler pass.
(define desugar-macros
  (let ()

  (define (process-expr expr)
       (tml-generic-traverse
	;; Driver:
	(lambda (x autoloop)
	  (define (tokonly t)
	    (match t
	      [(tok ,name ,n) (guard (integer? n))
	       (vector `(tok ,name ,n) ())]
	      [(tok ,name ,e) 
	       (match (autoloop e)
		 [#(,v ,tbs) (vector `(tok ,name ,v) tbs)])]
	      [,other (error 'desugar-macros:tokonly "bad token: ~a" other)]))

	  (match x
	     ;; For now this is just syntactic sugar for routing on the global tree:   
	     ;; return-retry indi
	     [(soc-return ,[x])
	      (match x
		[#(,v ,tbs)
		 (vector
		  (let ([socretval (unique-name 'socretval)])
		    `(let ([,socretval ,v])
		       (if (= (my-id) ',BASE_ID)
			   (begin 
			     ,@(DEBUGMODE `(dbg '"Soc return on basenode, returning directly: %d" ,socretval))
			     (call (tok SOC-return-handler 0) ,socretval))
			   (dbg '"ERROR: soc-return called on node other than base station, node id: %d" (my-id))
			   ;; Disabling for now, don't want to assume global-tree
			   #;
			   (greturn ,socretval 
				   (to (tok SOC-return-handler 0)) 
				   (via (tok global-tree 0))
				   (seed '#f)
				   (aggr #f)
				   ))))
		  tbs)])]
	     ;; Sending to subtok 1 indicates that we're finished.
;	     [(soc-return-finished ,x)
;	      (loop `(return ,x (to (tok SOC-return-handler 1)) (via (tok global-tree 0))))]

	     [(flood ,[tokonly -> tok])
	      (let-match ([#(,t ,tbs) tok])
		 (let ((newtok (unique-name 'floodtok)))
		   (vector
		    `(gemit (tok ,newtok (my-id)))
		    `([,newtok subid () 
			       (grelay (tok ,newtok subid))
			       (call ,t)]
		      ,@tbs))))]
	    


	     ;; Elect leader form: 
	     ;;   (elect-leader target-token [criterion-evaluator] [criterion-comparator] [bounding-function])
	     ;; .param target-token: A token to fire when the leader is determined.
	     ;; .param criterion-evaluator:  A function to score each node.
	     ;; .param criterion-comparator:  A function for comparing scores, by default picks greater.
	     ;; .param bounding-function: A function for detecting when we fall over the boundary.
	     ;;    Currently [2005.12.02] bounding functionality is not implemented.
 	     ;;   
	     ;; The target-token is called on all nodes participating,
	     ;; it's passed the ID of the leader as argument.  If the
	     ;; ID matches your ID, you win!

	     [(elect-leader ,t) (process-expr `(elect-leader ,t #f #f #f))]
	     [(elect-leader ,t ,c) (process-expr `(elect-leader ,t ,c #f #f))]
	     [(elect-leader ,t ,c ,comp) (process-expr `(elect-leader ,t ,c ,comp #f))]

	     ;; TODO: Make this work better for constrained regions.
	     ;; The token part has to be totally static.
	     ;; Otherwise we have to TRANSMIT information along during the competition,
	     ;; telling everyone who to elect if you win.
	     [(elect-leader ,[tokonly -> t] ,[c] ,[comp] ,[b])
	      ;; Also maybe want to name the ^^ compete token, so that you can return vals to the leader.
	      (let-match ([#(,t        ,tbs0) t]
			  [#(,criteria ,tbs1) c]
			  [#(,comparison ,tbs2) comp] ;; Comparison returns -1 0 1 for less,equal,greater.
			  [#(,bounding ,tbs3) b])  ;; NOT USED CURRENTLY
		(let* ((compete      (unique-name 'compete))
		       (storagename  (unique-name 'leaderstorage))
		       (cur-leader   (unique-name 'cur-leader))
		       (ldr-criteria  (unique-name 'my-cred))
		       (tmp          (unique-name 'tmp))
		       (tmp0          (unique-name 'tmp))
		       (tmp1          (unique-name 'ldr-crit))
		       (tmp2          (unique-name 'ldr-id))
		       (tmp3          (unique-name 'tmp))
		       (check-winner (unique-name 'am-i-winner))
		       (id           (unique-name 'subtokid))
		       (val           (unique-name 'val))
		       (storage `(tok ,storagename 0)))
		(vector
		 `(begin 
		    (subcall ,storage) ;; First allocate storage locally.
;		    (printf '" (Launching: ~s) \n" (ext-ref ,storage ,ldr-criteria))
		    (gemit (tok ,compete (my-id)) (ext-ref ,storage ,ldr-criteria))
		    (timed-call 1000 ,check-winner)
		    )
		 `([,storagename () 
				 (stored [,cur-leader 'leader-uninitialized]
					 [,ldr-criteria 'mycreds-uninitialized])
				 (begin 
				   (set! ,cur-leader (my-id))
				   ;; We only evaluate ourselves once and store the result. That is here:
				   ,(if criteria
					;; Now we compute our local score ONCE:
					;; (Once per call to elect-leader.)
					`(let ((,tmp (subcall ,criteria)))					   
					   (begin 
;					     (printf '" (INIT ~s) \n" ,tmp)
					     (set! ,ldr-criteria ,tmp)))
					;; Otherwise the default criteria for election is node ID:
					`(set! ,ldr-criteria (- 0 (my-id)))))]
		   [,compete ,id (,val)
		    (begin	
		      ;; TEMP: 
		      ;(leds toggle blue)
;		      ,@(REGIMENT_DEBUG
;			 `(if (token-present? ,storage) (void)
;			      (dbg '"~s.~s ERROR: leader election, was expecting ~s storage token to exist." 
;				   (my-clock) (my-id) ',storage)))
		      '"If storage has not been allocated, do it now."
		      '"This means the message just got to a new node."
		      (if (token-present? ,storage ) 
			  (void)
			  (subcall ,storage))  ;; Control-wise, this is a yield.

		      '"A temporary variable stores our current 'leadership value':"
		      (if (let ((,tmp1 (ext-ref ,storage ,ldr-criteria)))
			    ;; There may or may not be a user comparison function that guides the competition.
			    ,(if comparison
				 `(let ([,tmp3 (subcall ,comparison ,val ,tmp1)])
				    (if (= ,tmp3 1) '#t ;; They beat us
				    (if (= ,tmp3 -1) '#f ;; They lost
					(< (ext-ref ,storage ,cur-leader) ,id) ; Otherwise tie-breaker on ids
					)))
				 ;; Default comparison: Either they beat us flat out, or 
				 ;; they tie us and beat us on the ID based tie-breaker:
				 `(if (> ,val ,tmp1) '#t
				     (if (= ,val ,tmp1)
					 (begin
					;(printf '"\nAt <~a> does node ~a beat ~a in tie breaker?\n" (my-id) ,id (ext-ref ,storage ,cur-leader))
					   ;; Prefer lower ID numbers:
					   (< (ext-ref ,storage ,cur-leader) ,id))
					 '#f))))
; ;				(printf '"Comparing: ~s\n" (list (subcall ,criteria) (ext-ref ,storage ,cur-leader)))
			  (begin '"If they beat the local leader, then the new winner is them."
;				 (printf '"(~a ~a) " ,val (ext-ref ,storage ,ldr-criteria))
				 (let ((,tmp2 (ext-ref ,storage ,cur-leader))) ; The loser's id.
				   (begin 
				     (ext-set! ,storage ,cur-leader ,id)
				     (ext-set! ,storage ,ldr-criteria ,val)
				     '"And since they won, we bear their flag onward:"
				     (grelay (tok ,compete ,id) ,val)
;				     '"We also evict the loser to reclaim memory." ;; [2005.12.04]
;				     (evict (tok ,compete ,tmp2))
				     )))
			  (begin 
			      '"If they don't change our mind about who's leading, we do nothing."
;			      (printf '"~a "(ext-ref ,storage ,ldr-criteria))
			      ;; TEMP: FIXME: I am temporarily: RECOMPETING in this case:
			      ;(gemit (tok ,compete (ext-ref ,storage ,cur-leader)) (ext-ref ,storage ,ldr-criteria))

			      '"Unless this is the root of the gradient, in that case we need to grelay anyway to get things started."
			      (if (= (my-id) (gorigin (tok ,compete ,id)))
				  (grelay (tok ,compete ,id) ,val)
				  (void))

			      ))
		      )]
		   [,check-winner ()
				  '"First we clear out all the competition tokens."
				  (evict-all (tok ,compete 0))
				  ;; Call the users hook with leader ID and leader criteria.
				  (call ,t (ext-ref ,storage ,cur-leader)
					   (ext-ref ,storage ,ldr-criteria))
				  #;
				  (begin
				    (if (= (ext-ref ,storage ,cur-leader) (my-id))
					(begin 
;					  (printf '"\n(winner ~a at time ~a)\n" (my-id) (my-clock))
					  (call ,t))
					(void)))]
		   ,@tbs0
		   ,@tbs1
		   ,@tbs2
		   )
		 )))]

	     ;; [2005.10.28]
	     [(sync-sense)
	      (vector
	       `(subcall (tok SenseTok 0))
	       ())]
	     
	     [,other (autoloop other)]))
	;; Fuser:
	(lambda (subresults reconstruct)
;	  (vector (apply reconstruct (map (lambda (x) (vector-ref x 0)) subresults))
;		  (apply append (map (lambda (x) (vector-ref x 1)) subresults)))
	  ;; This pattern doesn't work in the PLT-port of match: 
	  ;; Could consider fixing it. (FIXED IT)
	  (match subresults
	    [(#(,arg* ,newtbs*) ...)
	     (vector (apply reconstruct arg*)
		     (apply append newtbs*))])
	  )
	;; Expression:
	expr))

  (define (process-tokbind tb)
    (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
      (match (process-expr body)
	[#(,newbod ,tbs)
	 (cons `[,tok ,id ,args (stored ,@stored) 
;		      (begin 
			;; TEMP: display message count:
;			(if (token-present? (tok mgcount 0))
;			    (setlabel ".~a." (ext-ref (tok msgcount 0) count)))
			,newbod]
	       tbs)]
	[,other (error 'desugar-macros:process-tokbind 
		       "BUG: invalid returned val from process-expr: ~a" other)])))


  (lambda (prog)
    (match prog
      [(,lang '(program (bindings ,constbinds ...)
		 (nodepgm (tokens ,toks ...))))
       `(desugar-macros-lang
	 '(program (bindings ,constbinds ...)
	    (nodepgm (tokens 
;		       (msgcount _ () (stored (count 0)) (void))
;		       (node-start _ () (stored) (call msgcount))
		       ,@(apply append (map process-tokbind toks))))))]))
  ))

;;; Unit tests.

(define-testing these-tests
  `(
    ,@(let ((randomprog
	     '(desugar-macros-lang
	      '(program
		(bindings)
		(nodepgm
		 (tokens
		  (node-start subtok_ind () (stored) (void))
		  (SOC-start subtok_ind () (stored)
		   (begin
		     (printf '"~a " (token-scheduled? (tok tok1 0)))
		     (timed-call 500 (tok tok1 0))
		     (timed-call 100 (tok check 0))
		     (timed-call 800 (tok check 0))))
		  (tok1 subtok_ind () (stored) (printf '"tok1 "))
		  (check subtok_ind () (stored)
		   (printf '"~a " (token-scheduled? (tok tok1 0))))))))))
       
      `(
	["Just make sure we get the same thing back for a prog without soc-return:"
	 (desugar-macros ',randomprog)
	 ,randomprog]
	))   

    ))

(define-testing test-this (default-unit-tester
		    "22: Desugar-Macros: expand various macros"
		    these-tests))

;; The usual unit tester.
(define test22 test-this)
(define tests22 these-tests)
(define test-desugar-macros test-this)

) ; End module
