;; simulator_nought.ss
;;  -Ryan Newton [2004.05]

;; This is a simulator for the output of pass10_deglobalize.  

;; This file uses and abuses top-level bindings (like mad), because it
;; uses eval, so it's not very well encapsulated right now.

;; NOTE: Unlike some of the files in the chez/ directory, this expects
;; to be loaded from its own parent directory.

;; <TODO>: Make the simulator not use global state for the graph!!

;;============================================================
;; REQUIRES: This file requires that the slib 'tsort module be loaded
;; providing the topological-sort function.

;; REQUIRES: Also on hash tables from SLIB.

;; REQUIRES: This file requires the "flat_threads.ss" interface, which
;; is a simple interface over engines or threads.


;===============================================================================
;; Some CHANGES (not keeping a complete log):


;; NOT FINISHED:

;; [2004.06.09] (soc-return v) Will make v *one of* the return values (a
;; whole stream of them can be provided). (soc-finished) will terminate
;; the computation if there has been a returned value, 
;; ???? WHAT IF ITS NOT SOC CALLING? ????

;;otherwise it
;; will just terminate the thread for the processor



;;[2004.06.09] RRN: Adding implicit 'I-am-SOC' boolean variable for
;; use by node programs.. This is only used by generated code, or my
;; handwritten test cases.  Along with it is a SOC-processor binding
;; which is bound to the simobject for the SOC.


;===============================================================================
;; GLOBAL VARIABLES AND TYPES:

;; NOTE!  The simulator dynamically defines top level variables:
;;  total-messages, soc-return, soc-finished, stop-nodes

;; This is the simplest simulator ever.  Takes the output of pass "deglobalize".
(define this-unit-description 
  "simulator_nought.ss: simplest simulator for nodal language")

;; This uses a lame sort of text display instead of the graphics display:
(define simulator-output-text (make-parameter #f (lambda (x) x)))

;; These are the virtual coordinate bounds of the world.
(define world-xbound 60)
(define world-ybound 60)
(define radius 20) ;; And the comm radius.
(define numprocs 20) ;; And the total # processors.

;; Positions are just 2-element lists.
(define-structure (node id pos))
;; Incoming is a list of messages.
;; Redraw is a boolean indicating whether the object needs be redrawn.
;; [2004.06.11] Added homepage just for my internal hackery.
;; [2004.06.13] Be careful to change "cleanse-world" if you change
;; this, we don't want multiple simulation to be thrashing eachother.
(define-structure (simobject node incoming redraw gobj homepage))

;; This record holds the info that the token cache needs to maintain
;; per each token name.
;; NOTE: The lack of a *parent* indicates that the message is a local call:
(define-structure (msg-object token origin parent count args))

;; Here's a helper to check the invariants on a msg-object
(define (valid-msg-object? mo)
  (and (msg-object? mo)
       (let ([token  (msg-object-token  mo)]
	     [origin (msg-object-origin mo)]
	     [parent (msg-object-parent mo)]
	     [count  (msg-object-count  mo)]
	     [args   (msg-object-args   mo)])
	 (and (token? token)
	      (or (not parent) (simobject? parent))
	      (or (not origin) (simobject? origin))
	      (integer? count)
	      (list? args)))))


;; These global vars start off uninitialized and are initialized with
;; "init-world", below.
(define graph #f) ;; Graph of 'node'
(define object-graph #f) ;; Graph of 'simobject'
(define all-objs #f) ;; List of 'simobject' 

;; This globally defined functions decides the sensor values.
;; Here's a version that makes the sensor reading the distance from the origin:
(define (sense loc)
  (let ([x (car loc)] [y (cadr loc)])
    (sqrt (+ (expt x 2) (expt y 2)))))

;;========================================

(define (id x) x)

(define (random-node) 
  (make-node 
   (random 100);(expt 2 32))
   (list (random world-xbound)
	 (random world-ybound))
   ))

;; I thought the primitive equal? did this by default?  This is just a
;; version that accepts any number of arguments.
(define (myequal? . args)
  (if (null? args) #t
      (let ((first (car args)))
	(let myeqloop ((args (cdr args)))
	  (cond
	   [(null? args)  #t]
	   [(equal? first (car args)) (myeqloop (cdr args))]
	   [else #f])))))
  
(define (dotted-append ls ob)
  (let loop ((ls ls))
    (if (null? ls) ob
	(cons (car ls) (loop (cdr ls))))))

;; Increment a top-level binding.  Totally dynamic.
(define (incr-top-level! v)
  (set-top-level-value! v (add1 (top-level-value v))))

;; Helper to determine the distance between two 2d positions.
(define (posdist a b)
  (sqrt (+ (expt (- (car a) (car b)) 2)
	   (expt (- (cadr a) (cadr b)) 2))))

(define structure-copy  vector-copy)

;; TODO, returns all the nodes in the graph that are connected to the
;; given simobject.  Gonna use this for unit testing oracles.
(define (all-connected simob)
  '())

;; This generates the default, random topology: 
;; (There are more topologies in "network_topologies.ss"
(define (make-object-graph g) (graph-map (lambda (nd) (make-simobject nd '() #f #f '())) g))
(define (init-world)
  (set! graph   
	(let ((seed (map (lambda (_) (random-node)) (iota numprocs))))
	  ;; Connect the graph:
	  (set! seed
					;      (let ((ids (map node-id graph)))
					;	(map 
		(map (lambda (node)
		       (cons node 
			     (filter (lambda (n) 
				       (and (not (eq? node n))
					    (< (posdist (node-pos node) (node-pos n)) radius)))
				     seed)))
		     seed))
	  ;; If there's a prexisting world, clean the screen.
	  (and all-objs 
	       (not (null? all-objs))
	       (simobject-gobj (car all-objs))
	       (clear-buffer))
	  seed))
  (set! object-graph (make-object-graph graph))
  (set! all-objs (map car object-graph)))

;; Call it now as we start up:
(init-world)

(define (destroy-world)
  (set! graph #f)
  (set! object-graph #f)
  (set! all-objs #f))

;; init-world allocates the world, but we also want a procedure to cleanse it.
(define (cleanse-world)
  (if object-graph
      (for-each (lambda (entry)
		  (let ((simob (car entry)))
		    (set-simobject-redraw! simob #f)
		    (set-simobject-homepage! simob '())
		    (set-simobject-incoming! simob '())))
		object-graph)
      (error 'cleanse-world "world must not be allocated object-graph is false.")))

;;========================================  

(define (free-vars expr)
  (let loop ((env ()) (expr expr))
    (match expr	 
	   [,var (guard (symbol? var)) (if (memq var env) '() (list var))]   
	   [(quote ,x) '()]
	   [(,prim ,rand* ...) (regiment-primitive? prim)
	    (let ((frees (map (lambda (x) (loop env x)) rand*)))
	      (apply append frees))]
	   [(lambda (,formals) ,expr)
	    (loop (append formals env) expr)]
	   [,else (error 'free-vars "not simple expression: ~s" expr)])))

(define process-statement 
  (letrec ([process-expr 
	 (lambda (expr)
	   (match expr
		  ;; This is a little wider than the allowable grammar to allow
		  ;; me to do test cases:
		  [,x (guard (or (symbol? x) (constant? x))) x]
		  [(quote ,x) `(quote ,x)]
		  [(call ,rator ,rand* ...)
		   (DEBUGMODE
		    (if (not (token? rator))
			(error 'simulator_nought:process-statement
			       "call form expects rator to be a token name: ~s"
			       rator)))
		   ;; Could add the message to incoming instead!
		   ;`(handler (make-msg-object ',rator #f #f 0 ',rand*))
		   ;; [2004.06.16] For now I'm raising an error... 
		   ;; don't know what the correct behaviour should be:
		   `(let ((call-result 
			   (sendmsg (make-msg-object ',rator #f #f 0 ',rand*) this)))
		      (if (eq? call-result 'multiple-bindings-for-token)
			  (error 'call "cannot perform a local call when there are token handlers for: ~s"
				 ,rator)
			  call-result))
		   ]

;		  [(if ,a ,b ,c)
;		   (disp "IF" a b c)]
		  
		  [(if ,[test] ,[conseq] ,[altern])
		   `(if ,test ,conseq ,altern)]

		  [(flood ,tok) `(sim-flood (quote ,tok))]
		  [(emit ,opera ...) 
		   ;; This is an original emission, and should get a count of 0.
		   `(sim-emit (quote ,(car opera)) (list ,@(cdr opera)) 0)]

		  [(dist) '(sim-dist)]		
  		  ;; <TODO> WHY NOT QUOTED:
		  [(dist ,tok) `(sim-dist ',tok)]
		  [(loc) '(sim-loc)]		
		  [(locdiff ,[l1] ,[l2]) `(sim-locdiff ,l1 ,l2)]
	 	  
		  [(relay) `(sim-relay)]		  
		  [(relay ,rator ,rand* ...) '(VOID-FOR-NOW) ]

		  [(elect-leader ,tok) `(sim-elect-leader ',tok)]

;		   [(elect-leader ,tok ,tokproc) ]

		  ;; Right now I'm recurring on the argument, this
		  ;; shouldn't be required by code generated from my
		  ;; compiler currently. [2004.06.09]
		  [(return ,[x]) `(sim-return ,x)]

		  [(light-up ,r ,g ,b) `(sim-light-up ,r ,g ,b)]		  
		  
		  ;; We're letting them get away with other primitives because
		  ;; we're being lenient, as mentioned above.
		  [(,rator ,[rand*] ...)
		   ;; Don't want to be too lenient tho:
		   (guard (not (token-machine-primitive? rator)))
		   `(,(process-expr rator) ,rand* ...)]
		  
		  [,otherwise (error 'simulator_nought.process-expr 
				"don't know what to do with this: ~s" otherwise)])
	   )])
    (lambda (stmt)
      (process-expr stmt))))

(define (process-binds binds expr)
;  (disp "processbinds" binds expr)
  (let* ([graph (map (lambda (bind)
		       (cons (car bind)
			     (free-vars (cadr bind))))
		     binds)]
	 [flat (reverse (topological-sort graph eq?))]
	 [binds (map (lambda (sym) (assq sym binds))
		     flat)])
    (if (cyclic? graph)
	(error 'process-binds "for now can't take a cyclic graph: ~s" binds))
    `(let* ,binds ,expr)))

;; This removes duplicates among the token bindings.
;; <TOOPTIMIZE> Would use a hash-table here for speed.
(define (remove-duplicate-tokbinds tbinds)
;  (disp "Removing token dups" tbinds)
  (let loop ((ls tbinds))
    (if (null? ls) '()
	(let* ([tok (caar ls)]
	       [dups (filter (lambda (entry) (eq? tok (car entry))) (cdr ls))])
	  (if (null? dups)
	      (cons (car ls) (loop (cdr ls)))	      	      
	      (let* ([all-handlers (cons (car ls) dups)]
		     [formals* (map cadr all-handlers)]
		     [body* (map caddr all-handlers)]
		     [thunks (map (lambda (bod) `(lambda () ,bod)) body*)])
;		(disp "GOT DUPS " tok formals*)
		(DEBUGMODE
		 (if (not (apply myequal? formals*))
		     (error 'simulator_nought:remove-duplicate-tokbinds
			    "handlers for token ~s don't all take the same arguments: ~s" 
			    tok formals*))
		 )
		(let ([mergedhandler
		       `[,tok ,(car formals*)
			      (begin (for-each 
				      (lambda (th) (th))
				      (randomize-list ,(cons 'list thunks)))
				     'multiple-bindings-for-token)
;		             (begin ,@body* 'multiple-bindings-for-token)
			     ]])
;		  (disp "MERGED:" mergedhandler)
		(cons mergedhandler
		      (loop (filter (lambda (entry) (not (eq? tok (car entry)))) (cdr ls)))))
		  ))))))
	 

;; Takes token bindings and a body expression:
(define (process-tokbinds tbinds extradefs expr)
  (let ([binds 
	 (map
	  (lambda (tbind)
	    (match tbind 
		   [(,tok (,args ...) ,expr* ...)
		    `[,tok (lambda ,args 
			     (DEBUGPRINT2 
			      (disp "Token " ',tok 
				    "running at" (node-id (simobject-node this)) 
				    " with message: " ',args))
			     ,@(map process-statement expr*))]]))
	  (remove-duplicate-tokbinds tbinds))]
	;; These inputs to handler must be the *child* message-object
	;; (that is, already updated to have an incremented count, the
	;; correct parent, etc).  So we can shove it right in our cache.
	[handler `(lambda (themessage) ;(origin parent count tok args)
;		    (disp "Handling! msg" (msg-object-token themessage) 
;			  " parent? " (if (msg-object-parent themessage) #t #f)
;			  " Soc? " I-am-SOC)
		    (let ([origin (msg-object-origin themessage)]
			  ;[parent (msg-object-parent themessage)]
			  [count  (msg-object-count  themessage)]
			  [tok    (msg-object-token  themessage)]
			  [args   (msg-object-args   themessage)])
		      
;		    (disp "HANDLER at" (node-id (simobject-node this)) ": " tok args)
		    ;; Redraw every time we handle a message, our state might have changed.
		    (set-simobject-redraw! this #t)
		    ;; This refers to the token cache for this processor:
		    (let ([entry (hashtab-get token-cache tok)]
			  [handle-it (lambda (newentry)
				       (fluid-let ((this-message newentry))
					 (case tok
					   ,@(map (lambda (tok)
						    `[(,tok) 
						      (apply ,tok args)])
						  (map car tbinds))
					   [else (error 'node_program "Unknown message: ~s" tok)]
					   )))])

		      (if (not origin) ;; This is a local call.
			  (handle-it (make-msg-object tok #f #f #f args))
			  ;; TODO: Could optimize a *wee* bit by mutating instead of recreating here.
			  ;; BUT! No premature optimization.		      
			  (if (or (not entry) ;; There's no entry for that token name.
				  (= 0 count)
				  (< count (msg-object-count entry))) ;; This could be <=, think about it. TODO
			      (let ((newentry themessage))
				(if (= 0 local-recv-messages)
				    (sim-light-up 200 0 0))
				(set! local-recv-messages (add1 local-recv-messages))
				(hashtab-set! token-cache tok newentry)
				(handle-it newentry))
			      (begin 
				(incr-top-level! 'total-fizzles)
				;(disp "Message fizzle(no backflow) " tok " to " (node-id (simobject-node this)))
				(void))
			      )
			  ;; fizzle
			  ))))])
    `(let () ,@(map (lambda (x) (cons 'define x)) binds)
	  [define handler ,handler] 
	  [define this-message #f];)
	  ,@extradefs ;; <TODO> THIS IS WEIRD AND LAME <TOFIX>
    ,expr)))


'(define-syntax remove-last!
  (syntax-rules ()
    [(_ id)
     (cond 
      [(null? id) (error 'remove-last "can't remove last from null")]
      [(null? (cdr id)) (set! id '())]
      [else
       (let loop ([prior id]
		  [next (cdr id)])
	 (if (null? (cdr next))
	     (set-cdr! prior '())
	     (loop next (cdr next))))])]))

(define f 0)
(define np 0)
(define sp 0)


;; Takes: a program in the language of pass10_deglobalize
;; Returns: a vector #(thunk (thunk ...)) with SOC and node programs respectively.
(define (compile-simulate-nought prog)
  (match prog
    [(program (bindings ,nodebinds ...)
	      (socpgm (bindings ,socbinds ...) ,socstmts ...)
	      (nodepgm (tokens ,nodetoks ...) (startup ,starttoks ...)))
     (let* (

       [generic-defs
	
	`([define local-sent-messages 0] ;; We should stick this in the simobject...
	  [define local-recv-messages 0]
	  [define token-cache (make-default-hash-table)]

	   ;; MAKE SURE NOT TO INCLUDE OURSELVES:
	  [define neighbors (lambda (obj)
			(let ((entry (assq obj object-graph)))
;			  (disp "ENTRY : " entry)
			  (if (null? entry)
			      (error 'neighbors "generated code.. .cannot find obj in graph: ~s ~n ~s"
				     obj object-graph)
			      (begin 
				(if (memq obj (cdr entry))
				    (error 'neighbors "we're in our own neighbors list"))
				(cdr entry)))))]

	  [define sendmsg (lambda (data ob)
;		   (disp (list 'sendmsg data (node-id (simobject-node ob))))
		   (set-simobject-incoming! ob
		    (cons data (simobject-incoming ob)))
		   ;(set-simobject-redraw! ob #t)
		   )]

	   [define sim-emit (lambda (t m count)
		    ;; Count messages at this node
		    (set! local-sent-messages (add1 local-sent-messages))
		    ;; Count total messages sent.
		    (incr-top-level! 'total-messages)
;		    (newline)(disp "  " (list 'sim-emit t m count))

		    (let ([ourentry   (make-msg-object t this #f count m)]
			  [childentry (make-msg-object t this this (add1 count) m)])
		      ;; Is it fair for emitting to overwrite our cache entry?
		      ;; I need to do it so that the return-handler can figure things out.
		      (hashtab-set! token-cache t ourentry)
		      (for-each (lambda (nd) (sendmsg childentry nd))
				(neighbors this))))]

	   ;; These should be macros, but now I'm cheesily hacking
	   ;; these to work automagically and instantly:
	   [define (sim-flood t . m)
	     ;; FOR NOW THIS TELEPORTS THE MESSAGE EVERYWHERE IN THE NETWORK.
					;		    (disp (list "FLOODING" t m))
	     (let ((msg (if (null? m) '() (car m))))
	       (for-each (lambda (nd) (sendmsg (make-msg-object t this this 0 m) nd))
			 all-objs))]

	   ;; This is just a cludge for now.
	   [define (sim-elect-leader t)
	     (DEBUGPRINT (disp "sim-elect-leader!!" t))
	     (set-simobject-homepage! this (cons (list 'inside t) (simobject-homepage this)))
	     (let ((possible
		    (filter (lambda (simob)
			      (member (list 'inside t) (simobject-homepage simob)))
			    all-objs)))
	       (DEBUGPRINT
		(disp "election: electing from" (node-id (simobject-node this))
		      " number candidaties " (length possible)))
	       ;; This is horribly yucky.
	       ;; Proceed if there are any candidates...a
;	       (if (not (null? possible))
		   (let ([leader (list-get-random possible)])
		     (DEBUGPRINT
		      (disp "election: got leader" (node-id (simobject-node leader))))
		     ;; Tell the message it's leader.  This might happen
		     ;; at multiple nodes.  Need to make sure there's a
		     ;; dependency relationship here, and that a killing a
		     ;; false leader kills all its subsequent doings.
		     (sendmsg (make-msg-object t #f #f 0 '()) leader)))]

	   [define (sim-light-up r g b)
	     (if (simobject-gobj this)
		 (change-color! (simobject-gobj this) (rgb r g b))
		 ;; We're allowing light-up of undrawn objects atm:
		 ;(error 'sim-light-up "can't change color on undrawn object!: ~s" this)
		 )]

	   [define (sim-relay . tok)
	     (if (not (null? tok))
		 (error 'relay "Can't handle optional argument yet"))
	     ,(DEBUGMODE '(if (not this-message) 
			      (error 'inside-node-prog "this-message was #f")))
	     (if (not (msg-object-parent this-message))
		 (error 'simulator_nought.relay 
			"inside simulator. can't relay a message that was~a"
			" sent by a local 'call' rather than an 'emit'"))
	     ;; This is a replicated emission and should copy the existing count:
	     (sim-emit (msg-object-token this-message) 
		       (msg-object-args this-message) 
		       (msg-object-count this-message))]
	   
	   [define (sim-dist . tok)
	     (if (null? tok)
		 (begin 
		   ,(DEBUGMODE '(if (not this-message)
				    (error 'simulator_nought.process-statement 
					   "broken")))
		   (if (msg-object-count this-message)
		       (msg-object-count this-message)
		       (error 'simulator_nought.process-statement:dist
			      "inside simulator (dist) is broken!")))
		 (let ((entry (hashtab-get token-cache (car tok))))
		   (if (and entry (msg-object-count entry))
		       (msg-object-count this-message)
		       (error 'simulator_nought.process-statement:dist
			      "inside simulator (dist ~s) but ~s has not been received!"
			      (car tok) (car tok))
		       )))]
	   
	   [define (sim-loc) ;; Return this nodes location.
	     (node-pos (simobject-node this))]
	   [define (sim-locdiff a b)
	     (sqrt (+ (expt (- (car a) (car b)) 2)
		      (expt (- (cadr a) (cadr b)) 2)))]
	   
	   [define (sim-return retval)
	     ,(DEBUGMODE '(if (not this-message)
			      (error 'simulator_nought.sim-return
				     "broken")))
	       (if (eq? (msg-object-origin this-message) this)
		   ;; Call the return handler:
		   (let ([tok (msg-object-token this-message)])
		     (let ([return_tok (symbol-append tok '_return)])
		       ;; This might be a little sketchy:
		       ,(process-statement `(call return_tok retval))))
		   ;; Send it up to our parent:
		   
	       
	       (void)
	       ;; For now we zap this straight back to the sender.
	       ;; BUT we need a record of who sent what...
					;		     (let ((loop ((
	       )]
	   
	   )]
       
       [socprog
	 `(lambda (SOC-processor this object-graph all-objs)
;	    (printf "CALLING SocProg: ~s~n" this)
	    (let ([I-am-SOC #t]) 
	      ;; We have to duplicate the tokbinds here...
	      ,(process-binds 
		nodebinds
		(process-binds 
		 socbinds
		(process-tokbinds 
		 nodetoks generic-defs			
		 `(begin ,@(map process-statement 
				socstmts)
			 'soc_finished))))))]
       
       [nodeprog
	`(lambda (SOC-processor this object-graph all-objs)
;	    (printf (format "CALLING Nodeprog: ~s~n" (if (simobject-gobj this) #t #f)))

	   ;; This is a little weird, but even the node program
	   ;; running on the SOC has to know that it's physically on
	   ;; the SOC:
	    (let ([I-am-SOC (eq? this SOC-processor)])
;	      ,@generic-defs	      
	      ,(process-binds 
		nodebinds 
		(process-tokbinds 
		 nodetoks generic-defs
		 `(begin 
		    ;; <TODO>: FIX THIS UP, MAKE SURE NO ARGS IS OK!?:
		    ;; Call all the starting tokens with no arguments:
		   ,@(map list starttoks)
		   (let main-node-loop ([incoming (simobject-incoming this)])
		     (cond
		      [stop-nodes 
		       (display "*") ;(disp "Node stopped by external force!")
		       'node-stopped]
		      [(null? incoming)
		       ;; No good way to wait or stop the engine execution?
		       (yield-thread)
		       (main-node-loop (simobject-incoming this))]
		      [else
		       ;; This might introduce message loss (because of no
		       ;; semaphores) but I don't care:					
		       (let ((msg (last incoming)))
			 
			 (DEBUGPRINT
			  ;; Print out the process number when we handle a message:
			  (printf "~s." (node-id (simobject-node this))))

			 ;; Pop that message off:
			 (if (null? (cdr incoming))
			     (set-simobject-incoming! this '())
			     (list-remove-last! incoming))
			 
			 (DEBUGMODE
			  (if (not (valid-msg-object? msg))
			      (error 'node-handler 
				     "invalid message to node, should be a valid msg-object: ~s ~nall messages: ~s"
				     msg incoming)))
			 (handler msg)
			 (main-node-loop (simobject-incoming this))
			 )])
		     ))))))])
       (DEBUGPRINT
	(printf "~nGOT TOKEN HANDLERS:~n" )
	(pretty-print nodetoks)(newline))

;       (disp "Socprog")
;       (pretty-print socprog)
;       (newline)
;       (disp "Nodeprog")
;       (pretty-print nodeprog)
       (set! f socprog)
       (set! sp socprog)
       (set! np nodeprog)
;       (for-each eval generic-defs)       
       (list socprog nodeprog))]))

;; Makes thunks for the simulation:
;; Returns a vector with the socthunk and a list of nodethunks.
(define (build-simulation progs)
  (let ([socprog (car progs)]
	[nodeprog (cadr progs)])
    (let ([socfun (eval socprog)]
	  [nodefun (eval nodeprog)]
	  [socnode (car all-objs)])
      (vector 
       (lambda () (socfun socnode socnode object-graph all-objs))
       (map (lambda (nd) 
	      (lambda () 
		(nodefun socnode nd object-graph all-objs)))
	    all-objs))
      )))

;; This is the "language definition" which will use the compiler nd
;; simulator to evaluate the expression.  It'll be hard to write test
;; cases with meaningful results, though.
(define (simulator-nought-language expr)
  (void))

;;===============================================================================

;; [2004.06.16] This is the shared structure of both the text
;; simulator and the graphical one.  It serves the role of a parent
;; object from which run-simulation and graphical-simulation inherit.
;; ITS A MESS.  The two arguments to this function don't make any
;; sense, very weak abstraction.

;; [2004.06.17] - Modifying this so that it returns an actual stream...
(define (generate-simulator threadrunner fun)
  (lambda (thunks . timeout)
    (let ([return-vals '()])

    ;; First, set up a global counter for communication cost:
  ;; This is defined dynamically so as to avoid PLTs module system. 
  (define-top-level-value 'total-messages 0)
  ;; This one count all the messages that bounce because of
  ;; backpropogation prevention mechanisms:
  (define-top-level-value 'total-fizzles 0)
  ;; This is a global flag which can be toggled to shut down all the
  ;; running processors.
  (define-top-level-value 'stop-nodes #f)
  ;; Define global bindings for these so that we can do fluid-let on them.
  (define-top-level-value 'soc-return ;'unbound-right-now-soc-return)
    (lambda (x)
;      (disp "OLD SOC-RETURN DAMMIT.")
      ;; Collect return vals in a local variable.
      (set! return-vals (cons x return-vals))))
  (define-top-level-value 'soc-finished ;'unbound-right-now-soc-finish)
    (lambda () (set-top-level-value! 'stop-nodes #t)))

  ;;-------------------------------
  ;; We've done our job by initializing.  Apply the given function,
  ;; this gives us back new thunks:
  (let ([newthunks (apply fun (cons thunks timeout))])
   
    ;; Kinda lame to use fluid-let here, but we don't have the
    ;; relevent continuation at the time we build the thunks.
    (fluid-let (
		#;[soc-return (lambda (x)
			      (disp "OLD SOC-RETURN DAMMIT.")
			      ;; Collect return vals in a local variable.
			      (set! return-vals (cons x return-vals)))]
    )
      (let ([result (if (null? timeout)
			(threadrunner newthunks)
			(threadrunner newthunks (car timeout)))])
	(if (null? return-vals)
	    result
	    return-vals)
	  ))))))


;; This one didn't work because nested engines are not allowed.
      ;; Return an unopened stream must return an [improper] list to be a valid stream.
#;      (let stream-loop ((eng theeng))
	(delay
	  (eng 1000
	       (lambda (v rem) 
		 (if (null? return-vals)
		     (list v) 
		     return-vals))
	       (lambda (eng2)
		 (if (null? return-vals)
		     (stream-loop eng2)
		     (let ((temp return-vals))
		       (set! return-vals '())  ;; <TODO> : Use semaphore.
		       ;; Return a stream:
		       (dotted-append temp (stream-loop eng2))))))))

;; I'm essentially doing a lot of ad-hoc inheritance here.  There are
;; different simulators that all share some parts, excuse the mess.
(define generic-text-simulator-core
   (lambda (thunks . timeout)
     (let ([soceng (vector-ref thunks 0)]
	   [nodeengs (vector-ref thunks 1)]
	   [display_engine
	    (lambda ()
	      (let loop ()
		(for-each (lambda (simob)
			    (let ((len (length (simobject-incoming simob))))
			      (cond 
			       [(< len 10) (display len)]
			       [else (display "#")])))
			  all-objs)
		(yield-thread)
		(loop)))])       
       (if (simulator-output-text)
	   (cons display_engine (cons soceng nodeengs))
	   (cons soceng nodeengs))
       )))

(define run-simulation 
  (generate-simulator 
   ;; Thread runner:
   run-flat-threads
   ;; Build engines and possibly display driver:
   generic-text-simulator-core))

;;===============================================================================

;; Include some example programs used by the tests.
(include "simulator_nought.examples.ss")

(define these-tests
  `(
    [ (free-vars '(cons (quote 30) x)) (x) ]

    [ (process-statement '(emit foo 2 3)) (sim-emit 'foo (list 2 3) 0)]
    [ (process-statement '(flood foo)) (sim-flood 'foo)]
    
    ;; Making sure that it recurs on arguments to return, even though
    ;; this is not necessary for code generated by my compiler.
    [ (process-statement '(return (dist))) (sim-return (sim-dist))]

    ;; Just to make sure erros work with my unit-tester:
    [ (process-statement '(return))  error]

    [ (let ((x (make-simobject (make-node 34 '(1 2)) '() #f #f '())))
	(let ((y (structure-copy x)))
	  (and (eq? (simobject-node x) 
                    (simobject-node y))
               (eq? (simobject-incoming x) 
                    (simobject-incoming y))
               (eq? (simobject-gobj x) 
                    (simobject-gobj y))
	       (not (eq? x y))))) #t]

    ;; Generic tests for both this and the graphical sim:

    ,@(include "simulator_nought.tests")

  [ "Compile Flood lights program..."
     (build-simulation (compile-simulate-nought ',example-nodal-prog1))
     unspecified ]

  [ "Run Flood lights program..."
    (run-simulation
     (build-simulation (compile-simulate-nought ',example-nodal-prog1))
     2.0)
    unspecified ]

#;    [ (let ((s (open-output-string)))
	(parameterize ([current-output-port s])
	   (list 
	    (run-simulation (vector (make-engine (lambda () 3))
				    (list (make-engine (lambda () 4))
					  (make-engine (lambda () 5))))
			    .5)
	    (get-output-string s))))
	   
	Simulation_Done]


    ))

(define (wrap-def-simulate test)
  `(begin (define simulate run-simulation) ,test))

;; Use the default unit tester from helpers.ss:
;; But this also makes sure the world is initialized before doing unit tests:
(define test-this
  (let ((tester (default-unit-tester 
		  this-unit-description 
		  ;; Make sure that the world is clean before each test.
		  (map (lambda (test)
			 (match test
			   [(,prog ,res) `((begin (cleanse-world) ,prog) ,res)]
			   [(,name ,prog ,res) 
			    `(name (begin (cleanse-world) ,prog) ,res)]))
			 these-tests)
		  tester-eq?
		  wrap-def-simulate)))
    (lambda args
      ;; First init world:
      (init-world)
      (apply tester args))))

(define testsim test-this)
(define testssim these-tests)

;;===============================================================================
;; JUNK 

(define csn  compile-simulate-nought)

(define temprog
  '(program
    (socpgm (bindings) (emit tok1))
    (nodepgm
;       result_2
       (bindings)
       (tokens
	[tok1 () (flood tok2)]
	[tok2 () (light-up 255 0 100)])
       ()
       )))

(define (tt) (csn temprog))
(define a (car all-objs))
(define b object-graph)
(define c all-objs)
;(dsis g ((eval f) a b c))
;(define (g) (run-simulation (tt) .5))


(define (ttt)
  (run-simulation
   (build-simulation 
    (compile-simulate-nought 
     example-nodal-prog4))
   1.7))

