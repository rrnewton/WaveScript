
;; simulator_alpha.ss
;;  -Ryan Newton [2005.02.25]
;===============================================================================

;; This will be a second attempt simulator.
;; However, it will support only core tml (no gradients).

;; It will have a single thread of control and a queue of simulator
;; events sorted by virtual clock times.

;; Later, it may serve as a place to test scheduling algorithms so
;; that we may actually implement the atomic action model.


(define this-unit-description 
  "simulator_alpha.ss: event-queue simulator for nodal language")


;; Positions are just 2-element lists.
(define-structure (node id pos))

;; Incoming is a list of messages (or return-objs).
;; Redraw is a boolean indicating whether the object needs be redrawn.
;; [2004.06.11] Added homepage just for my internal hackery.
;; [2004.06.13] Be careful to change "cleanse-world" if you change
;; this, we don't want multiple simulation to be thrashing eachother.
;; [2004.07.08] I don't know why I didn't do this, but I'm storing the
;; token-cache in the structure too
(define-structure (simobject node incoming timed-tokens redraw gobj homepage 
			     token-cache local-sent-messages local-recv-messages
			     ))

;; This record holds the info that the token cache needs to maintain
;; per each token name.
;; NOTE: The lack of a *parent* indicates that the message is a local call:
(define-structure (msg-object token 
			      timestamp ;; when it was sent 
			      origin ;; :: simobject - original source of message
			      parent ;; :: simobject - who I got it from
			      count
			      args))


;; ======================================================================

;; Safer version:
#;(define (construct-msg-object token timestamp origin parent count args)
  (if (eq? token SPECIAL_RETURN_TOKEN)
      (error 'construct-msg-object "cannot manually make a return message")
      (begin
	(unless (token? token) (error 'construct-msg-object "bad token: ~s" token))
	(unless (or (number? timestamp) (not timestamp))
	  (error 'construct-msg-object "bad timestamp: ~s" timestamp))
	(unless (or (simobject? origin) (not origin))
		(error 'construct-msg-object "bad origin: ~s" origin))
	(unless (or (simobject? parent) (not parent))
		(error 'construct-msg-object "bad parent: ~s" origin))
	(unless (number? count)
		(error 'construct-msg-object "bad count: ~s" count))
	(unless (list? args)
		(error 'construct-msg-object "bad args: ~s" args))))
  (make-msg-object token timestamp origin parent count args))



;; [2004.06.28] This is a helper to construct the locally used
;; messages that don't have a parent, timestamp, etc.
#;(define (bare-msg-object rator rands)
  (if (eq? rator SPECIAL_RETURN_TOKEN)
      (error 'bare-msg-object "Can't build a return msg!!"))
  (construct-msg-object rator ;; token
		   #f    ;; timestamp
		   #f    ;; origin
		   #f    ;; parent
		   0     ;; count
		   rands))

;; Here's a helper to check the invariants on a msg-object
#;(define (valid-msg-object? mo)
  (and (msg-object? mo)
       (let ([token  (msg-object-token  mo)]
	     [timestamp (msg-object-timestamp mo)]
	     [origin (msg-object-origin mo)]
	     [parent (msg-object-parent mo)]
	     [count  (msg-object-count  mo)]
	     [args   (msg-object-args   mo)])	
	 (or 
	  #;(and (return-msg? mo) ;; Things are different for return-messages.
		  (list? count) (andmap integer? count)
		  (list? timestamp) (andmap integer? timestamp)
		  (or (not parent) (simobject? parent))
		  (or (not origin) (simobject? origin))
		  (list? args)		  
		  )
	     (and (token? token)
		  (or (not timestamp) (integer? timestamp))
		  (or (not parent) (simobject? parent))
		  (or (not origin) (simobject? origin))
		  (integer? count)
		  (list? args)))))


;; These global vars start off uninitialized and are initialized with
;; "init-world", below.
(define graph #f) ;; Graph of 'node'
(define object-graph #f) ;; Graph of 'simobject'
(define all-objs #f) ;; List of 'simobject' (this is just computed
		     ;; from object-graph; its existence is merely an
		     ;; optimization


;; ======================================================================

;; These are used by compile-simulate-nought.  They are the helper
;; functions used by the generated code.  Right now I'm actually just
;; defining these functions globally so that I may test them.  They
;; make liberal use of our three primary global variables (object-graph, all-objs).
(define generic-defs	
  `(
    ;; Is set to a list of all the leds that are toggled on.    
    [define led-toggle-state '()]

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
		   (set-simobject-incoming! ob
		    (cons data (simobject-incoming ob)))
		   ;(set-simobject-redraw! ob #t)
		   )]
    

    [define (sim-light-up r g b)
      ((sim-debug-logger) "~n~a: light-up ~a ~a ~a"
       (node-id (simobject-node this)) r g b)
      (if (simobject-gobj this)
	  (change-color! (simobject-gobj this) (rgb r g b))
	  ;; We're allowing light-up of undrawn objects atm:
	   ;(error 'sim-light-up "can't change color on undrawn object!: ~s" this)
	  )]

    ;; INCOMPLETE (we don't yet draw the leds directly.)
    [define (sim-leds what which)
      (let* ([colors 
	      (case which
		[(red)   '(255 0 0)]
		[(green) '(0 255 0)]
		[(blue)  '(0 0 255)]
		[else (error 'sim-leds "bad color: ~a" which)])]
	     ;; INCOMPLETE:
;	     [oldcolors '(0 0 0)]
	     )
	(let ((string (format "~a: (time ~s) (Leds: ~a ~a ~a)~n" 	
	 (node-id (simobject-node this)) (cpu-time) which what
	 (case what
	   [(on) 
	    (set! led-toggle-state (list->set (cons which led-toggle-state)))
	    (apply sim-light-up colors)
	    "" ]
	  [(off)
	   (set! led-toggle-state (remq which led-toggle-state))
	   (sim-light-up '(0 0 0))
	   "" ]
	  [(toggle)
	   (if (memq which led-toggle-state)
	       (begin 
		 (set! led-toggle-state (remq which led-toggle-state))
		 (sim-light-up 0 0 0)
		 "off")
	       (begin 
		 (set! led-toggle-state (list->set (cons which led-toggle-state)))
		 (apply sim-light-up colors)
		 "on")
	       )]
	  [else (error 'sim-leds "bad action: ~a" what)]))))
	  ;((sim-debug-logger) string)
	  (logger string)
	))]
	  
    [define (sim-dist . tok)
	     (if (null? tok)
		 (begin 
		   (if (msg-object-count this-message)
		       (msg-object-count this-message)
		       (error 'simulator_nought.process-statement:dist
			      "inside simulator (dist) is broken!")))
		 (let ((entry (hashtab-get (simobject-token-cache this) (car tok))))
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

    ))

;; ======================================================================


(define (process-statement tokbinds)
  (letrec ([get-arg-index
	    (lambda (tok argname)
	      (let ([entry (assq tok tokbinds)])
		(if (not entry)
		    (error 'simulator_nought:get-arg-index
			   "No entry for token! ~a" tok))
		(list-find-position argname (cadr entry))))]

	   [process-expr 
	 (lambda (expr)
;	   (disp "Process expr" expr)
	   (match expr
		  ;; This is a little wider than the allowable grammar to allow
		  ;; me to do test cases:
		  [,x (guard (or (symbol? x) (constant? x))) x]
		  [(quote ,x) `(quote ,x)]
		  ;; NOTE! These rands ARE NOT simple.
		  [(call ,rator ,[rand*] ...)
		   (DEBUGMODE
		    (if (not (token? rator))
			(error 'simulator_nought:process-statement
			       "call form expects rator to be a token name: ~s"
			       rator)))
		   (build-call `(quote ,rator)
			       rand*)]
		  [(activate ,rator ,rand* ...)
		   (build-activate-call `(quote ,rator) rand*)]
		  [(timed-call ,delay ,rator ,rand* ...)
		   ;; Delay is in milleseconds.
		   (build-timed-call delay `(quote ,rator) rand*)]		  

		  [(reject)
		   '(set! reject-incoming-token #t)]

		  [(cache ,tok)
		   `(hashtab-get (simobject-token-cache this) ',tok)]

		  [(cache ,tok ,field)
		   `(let ([entry (hashtab-get (simobject-token-cache this) ',tok)])
		      (if entry			  
			  (list-ref (msg-object-args entry)
				    ,(get-arg-index tok field))))]


		  ;; It's like call but doesn't add quotes.
		  [(internal-call ,rator ,rand* ...)
;;		   (disp "processing internal-call" rator rand*)
		   ;; Could add the message to incoming instead!
		   ;`(handler (construct-msg-object ',rator #f #f 0 ',rand*))
		   ;; [2004.06.16] For now I'm raising an error... 
		   ;; don't know what the correct behaviour should be:
		   ;; NOTE: there is the possibility of variable capture with 'call-result'
		   `(let ((call-result-9758
			   (sendmsg (bare-msg-object ,rator (list ,@rand*)) this)))
		      (if (eq? call-result-9758 'multiple-bindings-for-token)
			  (error 'call "cannot perform a local call when there are multiple token handlers for: ~s"
				 ,rator)
			  call-result-9758))
		   ]

;		  [(if ,a ,b ,c)
;		   (disp "IF" a b c)]
		  
		  [(if ,[test] ,[conseq] ,[altern])
		   `(if ,test ,conseq ,altern)]

		  [(flood ,tok) `(sim-flood (quote ,tok))]
		  [(emit ,opera ...)
		   ;; This is an original emission, and should get a count of 0.
		   `(sim-emit (quote ,(car opera)) (list ,@(cdr opera)) 0)]

		  [(my-id) '(node-id (simobject-node this))]		
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
		  [(return ,[x] ,optional ...)
		   (let ([totok (if (assq 'to optional)
				    `(quote ,(cadr (assq 'to optional)))
				    '(string->symbol
				      (string-append 
				       (symbol->string (msg-object-token this-message))
				       "_return")))]
			 [via (if (assq 'via optional)
				  `(quote ,(cadr (assq 'via optional)))
				  '(msg-object-token this-message))]
			 [seed (if (assq 'seed optional)
				   (cadr (assq 'seed optional))
				   #f)]
			 [aggr (if (assq 'aggr optional)
				   (cadr (assq 'aggr optional))
				   #f)])
;		     (DEBUGMODE
;		      (if (not (and (token? via)
;				    (token? totok)))
;			  (error 'process-statement:return
;				 "One of these is not a token: via: ~s, totok: ~s" via totok)))			       
		     ;; FIXME: does seed need a quote!!!???
		     `(sim-return ,x ,totok ,via ,seed ',aggr))]

		  [(light-up ,r ,g ,b) `(sim-light-up ,r ,g ,b)]		  
		  [(leds ,which ,what) `(sim-leds ',which ',what)]
		  [(dbg ,str ,[args] ...)
		   ;; TODO FIX ME: would be nice to print properly
		   `(begin (display (format ,str ,@args)) (newline))]

		  [(,prim ,[rand*] ...)
		   (guard (token-machine-primitive? prim))
		   `(,prim ,rand* ...)]


		  ;; We're being REAL lenient in what we allow in token machines being simulated:
		  [(let ((,lhs ,[rhs]) ...) ,[bods] ...)
		   `(let ((,lhs ,rhs)  ...) ,bods ...)]

		  ;; We're letting them get away with other primitives because
		  ;; we're being lenient, as mentioned above.
		  [(,rator ,[rand*] ...)
		   ;; Don't want to be too lenient tho:
		   (guard (not (token-machine-primitive? rator))
			  (not (memq rator '(emit call timed-call activate relay return))))
;;		   (disp "processing app:" rator rand*)
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
	 [binds 
	  (map (lambda (sym) 
		 (let ([bind (assq sym binds)])
		   (if bind bind
		       (error 'simulator_nought:process-binds
			      "no entry for sym '~s' in constant binds ~s" sym binds))))
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
			     ,@(map (process-statement tbinds) expr*))]]))
	  (remove-duplicate-tokbinds tbinds))]
	[handler (build-handler tbinds)]
	)

    (printf "~n;  Converted program for Simulator:~n")
    (printf "<-------------------------------------------------------------------->~n")
    (pretty-print binds)
    

    `(let () ,@(map (lambda (x) (cons 'define x)) binds)
	  [define handler ,handler] 
	  [define this-message #f];)
	  ,@extradefs ;; <TODO> THIS IS WEIRD AND LAME <TOFIX>
    ,expr)))

;; This builds the function that handles incoming tokens.
(define (build-handler tbinds)
	;; These inputs to handler must be the *child* message-object
	;; (that is, already updated to have an incremented count, the
	;; correct parent, etc).  So we can shove it right in our 
	`(lambda (themessage) ;(origin parent count tok args)
		    (logger "~a: (time ~s) (ProcessMsg ~a ~a ~a ~a ~a ~a ~a ~a)~n" 
			    (node-id (simobject-node this))
			    (cpu-time) 
			    (msg-object-token themessage)
			  " parent? " (if (msg-object-parent themessage)
					  (node-id (simobject-node (msg-object-parent themessage)))
					  #f)
			  (msg-object-count themessage)
			  " Soc? " I-am-SOC 
			  "Args:" (msg-object-args   themessage))
		    (let ([origin (msg-object-origin themessage)]
			  ;[parent (msg-object-parent themessage)]
			  [count  (msg-object-count  themessage)]
			  [tok    (msg-object-token  themessage)]
			  [args   (msg-object-args   themessage)])
		      
;		    (disp "HANDLER at" (node-id (simobject-node this)) ": " tok args)
		    ;; Redraw every time we handle a message our state might have changed.
		    (set-simobject-redraw! this #t)
		    ;; This refers to the token cache for this processor:
		    (let ([entry (hashtab-get (simobject-token-cache this) tok)]
			  [handle-it (lambda (newentry)
				       (fluid-let ((this-message newentry))
					 (case tok
					   ,@(map (lambda (tok)
						    `[(,tok) 
						      (apply ,tok args)])
						  (map car tbinds))
					   [else (error 'node_program "Unknown message: ~s" tok)]
					   )))])
			  
			  (if (not origin) 
			      ;; This is a local call, don't touch the cache:
			      (handle-it (bare-msg-object tok args))

			      ;; TODO: Could optimize a *wee* bit by mutating instead of recreating here.
			      ;; BUT! No premature optimization.		      
			      (if (or (not entry) ;; There's no entry for that token name.
				      (= 0 count)
				      (< count (msg-object-count entry))) ;; This could be <=, think about it. TODO

				  (begin
				    (if (= 0 local-recv-messages)
					(silently (sim-light-up 200 0 0)))
				    (set! local-recv-messages (add1 local-recv-messages))
				    
				    ;; This is where I'm gonna cut in support for "reject".. [2004.10.23]
				    (fluid-let ([reject-incoming-token #f])
				      (handle-it themessage)
				      ;; Only add it to the cache if it hasn't been rejected:
				      (if (not reject-incoming-token)
					  (hashtab-set! (simobject-token-cache this) tok themessage)))
				    )

				  (begin 
				    (incr-top-level! 'total-fizzles)
				;(disp "Message fizzle(no backflow) " tok " to " (node-id (simobject-node this)))
				    (void))
				  )))
		    )))

;; ======================================================================

(define (compile-simulate-alpha prog)
  ;; Accept either with or without the language wrapper:
  (let ((prog (match prog 
		     [(program ,_ ...) prog]
		     [(,input-lang '(program ,stuff ...)) `(program ,stuff ...)])))
    (match prog
      [(program (bindings ,nodebinds ...)
		(socpgm (bindings ,socbinds ...) ,socstmts ...)
		(nodepgm (tokens ,nodetoks ...) (startup ,starttoks ...)))
       (let* (
       
#;       [socprog
	 `(lambda (soc-return soc-finished SOC-processor this object-graph all-objs)
	    (let ([I-am-SOC #t]) 
	      ;; We have to duplicate the tokbinds here...
	      ,(process-binds 
		nodebinds
		(process-binds 
		 socbinds
		(process-tokbinds
		 nodetoks generic-defs			
		 `(begin ,@(map (process-statement nodetoks)
				socstmts)
			 'soc_finished))))))]
              
       [nodeprog
	`(lambda (soc-return soc-finished SOC-processor this object-graph all-objs)
;	    (printf (format "CALLING Nodeprog: ~s~n" (if (simobject-gobj this) #t #f)))

	   ;; This is a little weird, but even the node program
	   ;; running on the SOC has to know that it's physically on
	   ;; the SOC:
	    (let ([I-am-SOC (eq? this SOC-processor)]
		  [local-sense (lambda ()
				 ((current-sense-function)
				  (node-pos (simobject-node this))))])

	      ,(process-binds 
		nodebinds 
		(process-tokbinds 
		 nodetoks generic-defs
		 `(begin 
		    ;; <TODO>: FIX THIS UP, MAKE SURE NO ARGS IS OK!?:
		    ;; Call all the starting tokens with no arguments:
		   ,@(map list starttoks)
		   ;; [2004.06.28] The simulator is asynchronous, yet
		   ;; communication should be vaguely synchronized by
		   ;; the fact that this loop handles a batch of
		   ;; incoming messages at a time before yielding.
		   ;; Hopefully "return" messages from all the
		   ;; children will make their way into a single one
		   ;; of these batches.  We'll see.
		   (let main-node-loop ([incoming (simobject-incoming this)]
					[returns-next-handled (+ (cpu-time) return-window-size)]
					[returns '()])
		     (if (not (andmap return-obj? returns))
			 (error 'main-node-loop "Hmm, got a non return-obj: ~s" returns))
			 

;		     (if (> (length returns) 0)
;			 (disp "loop" (cpu-time) returns-next-handled (length returns)))

		     (let ([current-time (cpu-time)]
			   [triggers (simobject-timed-tokens this)])
		       (let ([fired-triggers 
			      (filter (lambda (trigger) (>= current-time (car trigger)))
				      triggers)])
			 ;; This is a little weird because it gives
			 ;; timed calls priority over other incoming
			 ;; tokens:
			 (if (not (null? fired-triggers))
			     (begin 
			       (critical-section
				(set-simobject-incoming! this
			         (append (map cdr fired-triggers)
					 (simobject-incoming this))))
			       (set-simobject-timed-tokens! this
					 (difference triggers fired-triggers))
			       (main-node-loop (simobject-incoming this)
					       returns-next-handled
					       returns))
		     (cond
		      [stop-nodes 
		       (display "*") ;(disp "Node stopped by external force!")
		       'node-stopped]
		      
		      ;; If the time has come, handle those returns!
		      [(>= current-time returns-next-handled)
		       ;; This progress indicator goes to stdout regardless of where the logger goes.
		       (if (> (length returns) 1)
			   (display #\!) 
			   (display #\.))
		       (flush-output-port)
; 		       (if (not (null? returns))
; 			   (with-logger
; 			    (display (list "Handling rets from" (node-id (simobject-node this)) 
; 					   "got " (length returns)))
; 			    (newline)))

		       ;; Handle all the returns to date (either
		       ;; locally generated or from neighbors). 
		       (if (not (null? returns))
			   (handle-returns returns))
		       ;(yield-thread)
		       ;; Should I give us a time bonus for the time
		       ;; we spent *handling* the returns?  Or try to
		       ;; hold us to a fixed frequency?
		       (main-node-loop incoming
				       (+ returns-next-handled return-window-size)
				       '())]

		      [(null? incoming)
		       ;; No good way to wait or stop the engine execution?
		       (yield-thread)
		       (main-node-loop (simobject-incoming this) returns-next-handled returns)]

		      [else
		       ;; This might introduce message loss (because of no
		       ;; semaphores) but I don't care:					
		       (let ((msg 'NOT-SET-YET))
			       
			 (DEBUGPRINT
			  ;; Print out the process number when we handle a message:
			  (printf "~s." (node-id (simobject-node this))))
			 ;; Pop that message off:
			 (critical-section
			  (set! msg (last incoming))
			  (if (null? (cdr incoming))
			      (set-simobject-incoming! this '())
			      (list-remove-last! incoming)))

			 (if (return-obj? msg)
			     (main-node-loop (simobject-incoming this) 
					     returns-next-handled
					     (cons msg returns))
			     (begin 
			       (DEBUGMODE
				(if (not (valid-msg-object? msg))
				    (begin
				      (define-top-level-value 'x msg)
				      (error 'node-handler 
				     "invalid message to node, should be a valid msg-object: ~s "
				     (pop-msg-object msg )))))
			       (handler msg)
			       (main-node-loop (simobject-incoming this)
					       returns-next-handled 
					       returns))
			 ))]))
		     ))))))))])

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
       ;; DANGEROUS:
       (list socprog nodeprog))])))
