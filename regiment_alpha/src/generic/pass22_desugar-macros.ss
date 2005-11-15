
;; This removes various sugar:
;;  *) soc-return
;;  *) elect-leader
;;  *) flood

;; ======================================================================

;; [2005.04.20]
;; Soc-return's are a strange beast.

;; [2005.10.02] I was just doing this in cleanup-tokmac, but I'm going to move it here.

;; [2005.10.12]
;; For now we're only allowing soc-returns from the base-station node,
;; there is no implicit "global tree" in TML.  (Regiment does generate
;; code for such a global tree, but TML makes no such assumption.)

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
	    

	     [(elect-leader ,t) (process-expr `(elect-leader ,t #f #f))]
	     [(elect-leader ,t ,c) (process-expr `(elect-leader ,t ,c #f))]



	     ;; elect-leader: The first argument is a token to fire when the leader is determined.
	     ;; The token is called on all nodes participating, it's passed the ID of the leader as argument.
	     ;; If the ID matches your ID, you win!

	     ;; TODO: Make this work better for constrained regions.
	     ;; The token part has to be totally static.
	     ;; Otherwise we have to TRANSMIT information along during the competition,
	     ;; telling everyone who to elect if you win.
	     [(elect-leader ,[tokonly -> t] ,[c] ,[b])
	      ;; Also maybe want to name the ^^ compete token, so that you can return vals to the leader.
	      (let-match ([#(,t        ,tbs0) t]
			  [#(,criteria ,tbs1) c]
			  [#(,bounding ,tbs2) b])
		(let* ((compete      (unique-name 'compete))
		       (storagename  (unique-name 'leaderstorage))
		       (cur-leader   (unique-name 'cur-leader))
		       (ldr-criteria  (unique-name 'my-cred))
		       (tmp          (unique-name 'tmp))
		       (tmp0          (unique-name 'tmp))
		       (tmp1          (unique-name 'ldr-crit))
		       (tmp2          (unique-name 'ldr-id))
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
				   ,(if criteria
					;; Now we compute our local score ONCE:
					;; (Once per call to elect-leader.)
					`(let ((,tmp (subcall ,criteria)))					   
					   (begin 
					     (printf '" (INIT ~s) \n" ,tmp)
					     (set! ,ldr-criteria ,tmp)))
					`(set! ,ldr-criteria (- 0 (my-id)))))]
		   [,compete ,id (,val)
		    (begin	
		      ;; TEMP: 
		      (leds toggle blue)
;		      ,@(REGIMENT_DEBUG
;			 `(if (token-present? ,storage) (void)
;			      (dbg '"~s.~s ERROR: leader election, was expecting ~s storage token to exist." 
;				   (my-clock) (my-id) ',storage)))
		      '"If storage has not been allocated, do it now."
		      '"This means the message just got to a new node."
		      (if (token-present? ,storage ) (void)
			  (subcall ,storage))
		      
		      (if (let ((,tmp1 (ext-ref ,storage ,ldr-criteria)))
			    ;; Either they beat us flat out, or they tie us and beat us on the ID based tie-breaker:
			    (if (> ,val ,tmp1) '#t
				(if (= ,val ,tmp1)
				    (begin
				      ;(printf '"\nAt <~a> does node ~a beat ~a in tie breaker?\n" (my-id) ,id (ext-ref ,storage ,cur-leader))
				      ;; Prefer lower ID numbers:
				      (< (ext-ref ,storage ,cur-leader) ,id))
				    '#f)))
;				(printf '"Comparing: ~s\n" (list (subcall ,criteria) (ext-ref ,storage ,cur-leader)))
			  (begin '"If they beat the local leader, then the new winner is them."
				 (printf '"(~a ~a) " ,val (ext-ref ,storage ,ldr-criteria))
				 (ext-set! ,storage ,cur-leader ,id)
				 (ext-set! ,storage ,ldr-criteria ,val)
				 '"And since they won, we bear their flag onward:"
				 (grelay (tok ,compete ,id) ,val))
			  (begin 
			      '"If they don't change our mind about who's leading, we do nothing."
			      (printf '"~a "(ext-ref ,storage ,ldr-criteria))
			      ;; TEMP: FIXME: I am temporarily: RECOMPETING in this case:
			      ;(gemit (tok ,compete (ext-ref ,storage ,cur-leader)) (ext-ref ,storage ,ldr-criteria))
			      ))
		      )]
		   [,check-winner ()
				  ;; Call the users hook with leader ID and leader criteria.
				  (call ,t (ext-ref ,storage ,cur-leader)
					   (ext-ref ,storage ,ldr-criteria))
				  #;
				  (begin
				    (if (= (ext-ref ,storage ,cur-leader) (my-id))
					(begin 
					  (printf '"\n(winner ~a at time ~a)\n" (my-id) (my-clock))
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
	 (cons `[,tok ,id ,args (stored ,@stored) ,newbod]
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
			 ,@(apply append (map process-tokbind toks))))))]))
  ))




(define these-tests
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

(define test-this (default-unit-tester
		    "22: Desugar-Macros: expand various macros"
		    these-tests))

(define test22 test-this)
(define tests22 these-tests)
