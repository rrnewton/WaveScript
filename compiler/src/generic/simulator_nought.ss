;; simulator_nought.ss
;;  -Ryan Newton [2004.05]
;; This is a simulator for the output of pass10_deglobalize.  
;; A simple simulator.
;; This file uses and abuses top-level bindings (like mad).
;; It should be loaded inside a module that only exports:
;;   (provide simulator-nought-language)

;; NOTE: Unlike some of the files in the chez/ directory, this expects
;; to be loaded from its own parent directory.

;;============================================================
;; DEPENDS: This file requires that the slib 'tsort module be loaded
;; providing the topological-sort function.

;; DEPENDS: Also on hash tables from SLIB.

;; DEPENDS: This file requires the "graphics_stub.ss" interface be loaded
;; so that it may draw the simulation upon the screen.

;; DEPENDS: This file requires the "flat_threads.ss" interface, which
;; is a simple interface over engines or threads.
;;============================================================

(define (id x) x)

;; This is the simplest simulator ever.  Takes the output of pass "deglobalize".
(define this-unit-description 
  "\"simulator_nought.ss\"a: simplest simulator for nodal language")

;; This uses a lame sort of text display instead of the graphics display:
(define simulator-output-text (make-parameter #f id))

;; These are the virtual coordinate bounds of the world.
(define world-xbound 60)
(define world-ybound 60)
(define radius 30) ;; And the comm radius.
(define numprocs 10) ;; And the total # processors.

;; This counts total messages sent.
;;(define total-messages 0)
;; Can't do this here because of the plt module system.

;;========================================
;; Positions are just 2-element lists.
(define-structure (node id pos))
;; Incoming is a list of messages.
;; Redraw is a boolean indicating whether the object needs be redrawn.
(define-structure (simobject node incoming redraw gobj))

;; This record holds the info that the token cache needs to maintain
;; per each token name.
(define-structure (cache-entry token parent count args))

(define (random-node) 
  (make-node 
   (random 100);(expt 2 32))
   (list (random world-xbound)
	 (random world-ybound))
   ))

(define (dist a b)
  (sqrt (+ (expt (- (car a) (car b)) 2)
	   (expt (- (cadr a) (cadr b)) 2))))

(define (unfold-list lst)
  (let loop ((lst lst))
    (if (null? lst) '()
	(cons lst (loop (cdr lst))))))

(define structure-copy  vector-copy)

;; TODO, returns all the nodes in the graph that are connected to the
;; given simobject.  Gonna use this for unit testing oracles.
(define (all-connected simob)
  '())


;; This generates the default, random topology: 
;; (There are more topologies in "network_topologies.ss"
;;========================================
;; After the start of the program this doesn't change:
(define graph 
  (let ((seed (map (lambda (_) (random-node)) (iota numprocs))))
    ;; Connect the graph:
    (set! seed
					;      (let ((ids (map node-id graph)))
					;	(map 
	  (map (lambda (node)
		 (cons node 
		       (filter (lambda (n) 
				 (and (not (eq? node n))
				      (< (dist (node-pos node) (node-pos n)) radius)))
				    seed)))
	       seed))
    seed))

(define (make-object-graph g) (graph-map (lambda (nd) (make-simobject nd '() #f #f)) g))

;; Nor does this:
(define object-graph (make-object-graph graph))
(define all-objs (map car object-graph))
;;========================================


#;(define (draw)
  (init-graphics)
  (for-each draw-point (map node-pos graph))
  (for-each draw-line (map (lambda (node) 
			     (map (lambda (neighb) 
				    (list (node-pos node) (node-pos neighb)))))
			   graph)))
  

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

(define (process-statement stmt)
  (match stmt
	 [(call ,rator ,rand* ...)	  
	  ;(error 'process-statement "call not supported from SOC")] 
	  `(handler #f #f ',rator ',rand*)]
	 [(flood ,tok) `(flood (quote ,tok))]
	 [(emit ,opera ...) 
	  ;; This is an original emission, and should get a count of 0.
	  `(sim-emit (quote ,(car opera)) ,(cdr opera) 0)]

	 [(dist) `(begin 
		    ,(DEBUGMODE '(if (not this-message)
				     (error 'simulator_nought.process-statement 
					    "broken"))a)
		    (if (cache-entry-count this-message)
		      (cache-entry-count this-message)
		      (error 'simulator_nought.process-statement:dist
			     "inside simulator, (dist) is broken!")))]
	 [(dist ,tok) `(let ((entry (hashtab-get token-cache ',tok)))
			(if (and entry (cache-entry-count entry))
			    (cache-entry-count this-message)
			    (error 'simulator_nought.process-statement:dist
				   "inside simulator (dist ~s) but ~s has not been received!")))]
	 	  
	 [(relay)
	  `(begin 
	     ,(DEBUGMODE '(if (not this-message) 
			      (error 'inside-node-prog "this-message was #f")))
	     (if (not (cache-entry-parent this-message))
		 (error 'simulator_nought.relay 
			"inside simulator, can't relay a message that was~a"
			" sent by a local 'call' rather than an 'emit'"))
	     ;; This is a replicated emission, and should copy the existing count:
	     (sim-emit (cache-entry-token this-message) (cache-entry-args this-message) 
		       (cache-entry-count this-message)))]
	 
	 [(relay ,rator ,rand* ...) '(VOID-FOR-NOW) ]

	 [(light-up ,r ,g ,b)
	  `(begin
;	     (disp "trying to light")
	     (if (simobject-gobj this)
	       (begin 
;		 (disp "SETTING LIGHT" ,r ,g ,b)
		 (change-color! (simobject-gobj this) 
				(rgb ,r ,g ,b)))))]
	 [,else stmt])
  )

(define (process-binds binds expr)
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

(define (process-tokbinds tbinds expr)
  (let ([binds (map
		(lambda (tbind)
		  (match tbind 
			 [(,tok (,args ...) ,expr* ...)
			  `[,tok (lambda ,args 
;				   (disp "Token " ',tok "running at" (node-id (simobject-node this)) " with message: " ',args)
				   ,@(map process-statement expr*))]]))
		tbinds)]
	[handler `(lambda (parent count tok args)
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

		      (if (not parent) ;; This is a local call.
			  (handle-it (make-cache-entry tok #f #f args))
			  ;; TODO: Could optimize a *wee* bit by mutating instead of recreating here.
			  ;; BUT! No premature optimization.		      
			  (if (or (not entry) ;; There's no entry for that token name.
				  (= 0 count)
				  (< count (cache-entry-count entry))) ;; This could be <=, think about it. TODO
			      (let ((newentry (make-cache-entry tok parent (add1 count) args)))
				(hashtab-set! token-cache tok newentry)
				(handle-it newentry))
			      (disp "Ignored message " tok " to " (node-id (simobject-node this)))) ;; fizzle
			  )))])
    `(letrec (,@binds [handler ,handler] [this-message #f]) ,expr)))


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
    [(program (socpgm (bindings ,socbinds ...) ,socstmts ...)
	      (nodepgm (bindings ,nodebinds ...) (tokens ,nodetoks ...) ,starttoks))
     (let* (

       [generic-defs

	 '(
	   [define token-cache (make-default-hash-table)]
	   ;; MAKE SURE NOT TO INCLUDE OURSELVES:
	   [define neighbors (lambda (obj)
;			(disp 'neighbors obj)
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
		    ;; Count total messages sent.
		    (set! total-messages (add1 total-messages))
		    (newline)(disp "  " (list 'sim-emit t m count))
		   (map (lambda (nd) (sendmsg (make-cache-entry this count t m) nd))
			(neighbors this)))]
	   [define flood (lambda (t . m)
		    ;; FOR NOW THIS TELEPORTS THE MESSAGE EVERYWHERE IN THE NETWORK.
;		    (disp (list "FLOODING" t m))
		    (let ((msg (if (null? m) '() (car m))))
		      (map (lambda (nd) (sendmsg (make-cache-entry this 0 t m) nd))
			   all-objs)))])]

       [socprog
	 `(lambda (this object-graph all-objs)
;	    (printf "CALLING SocProg: ~s~n" this)
	    (let () ,@generic-defs
	      ,(process-binds socbinds
			      `(begin ,@(map process-statement 
					     socstmts)
				      'soc_finished))))]

       [nodeprog
	`(lambda (this object-graph all-objs)	   
;	    (printf (format "CALLING Nodeprog: ~s~n" (if (simobject-gobj this) #t #f)))
	    (let () ,@generic-defs	      
	      ,(process-binds 
		nodebinds 
		(process-tokbinds 
		 nodetoks
		 `(begin 
		   ,@(map list starttoks)
		    (let loop ([incoming (simobject-incoming this)])
		      (if (null? incoming)
			  ;; No good way to wait or stop the engine execution?
			  (begin (yield-thread)
				 (loop (simobject-incoming this)))
			  
			  ;; This might introduce message loss (because of no
			  ;; semaphores) but I don't care:					
			  (let ((msg (last incoming)))
			    ;; Pop that message off:
			    (if (null? (cdr incoming))
				(set-simobject-incoming! this '())
				(list-remove-last! incoming))
			    
			    (DEBUGMODE
			     (if (not (cache-entry? msg))
				 (error 'node-handler 
					"invalid message to node, should be a cache-entry: ~s ~nall messages: ~s"
					msg incoming)))
			    (handler (cache-entry-token msg)
				     (cache-entry-parent msg)
				     (cache-entry-count msg)
				     (cache-entry-args msg))))))))))])

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
	  [nodefun (eval nodeprog)])
      (vector 
       (lambda () (socfun (car all-objs)
			  object-graph all-objs))
       (map (lambda (nd) 
	      (lambda () 
		(nodefun nd object-graph all-objs)))
	    all-objs))      
      )))

;; This is the "language definition" which will use the compiler nd
;; simulator to evaluate the expression.  It'll be hard to write test
;; cases with meaningful results, though.
(define (simulator-nought-language expr)
  (void))

;;===============================================================================

(define (run-simulation thunks . timeout)
  (eval '(define total-messages 0))
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
    (let ([thunks (if (simulator-output-text)
		       (cons display_engine (cons soceng nodeengs))
		       (cons soceng nodeengs))])      

      (if (null? timeout)
	  (run-flat-threads thunks)
	  (run-flat-threads thunks (car timeout))
	  ))))


;;===============================================================================

;; Include some example programs used by the tests.
(include "simulator_nought.examples.ss")

(define these-tests
  `(
    [ (free-vars '(cons (quote 30) x)) (x) ]

    [ (process-statement '(emit foo 2 3)) (emit 'foo 2 3)]
    [ (process-statement '(flood foo)) (flood 'foo)]

    [ (let ((x (make-simobject (make-node 34 '(1 2)) '() #f #f)))
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
(define test-this (default-unit-tester 
		    this-unit-description 
		    these-tests
		    tester-eq?
		    wrap-def-simulate))

(define testsim test-this)
(define testssim these-tests)

;;===============================================================================
;; JUNK 

(define csn  compile-simulate-nought)

(define t1
  '((shirt tie belt)
    (tie jacket)
    (belt jacket)
    (watch)
    (pants shoes belt)
    (undershorts pants shoes)
    (socks shoes)))

(define t2
  '((a b) (b c) (c a)
    (d e) (e f)))

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
(define (g) (run-simulation (tt) .5))

