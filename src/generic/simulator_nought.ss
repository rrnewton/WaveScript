;; simulator_nought.ss
;;  -Ryan Newton [2004.05]
;; This is a simulator for the output of pass10_deglobalize.  
;; A simple simulator.
;; This file uses and abuses top-level bindings (like mad).
;; It should be loaded inside a module that only exports:
;;   (provide simulator-nought-language)

;;============================================================
;; DEPENDS: This file requires that the slib 'tsort module be loaded
;; providing the topological-sort function.

;; DEPENDS: Also on hash tables from SLIB.

;; DEPENDS: This file requires the "graphics_stub.ss" interface be loaded
;; so that it may draw the simulation upon the screen.

;; DEPENDS: This file requires the "flat_threads.ss" interface, which
;; is a simple interface over engines or threads.
;;============================================================

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

;;========================================
;; Positions are just 2-element lists.
(define-structure (node id pos))
;; Incoming is a list of messages.
;; Redraw is a boolean indicating whether the object needs be redrawn.
(define-structure (simobject node incoming redraw gobj))

(define (random-node) 
  (make-node 
   (random 100);(expt 2 32))
   (list (random world-xbound)
	 (random world-ybound))
   ))

(define hashtab-get (hash-inquirer eq?))
(define hashtab-set! (hash-associator eq?))

(define (dist a b)
  (sqrt (+ (expt (- (car a) (car b)) 2)
	   (expt (- (cadr a) (cadr b)) 2))))

(define (unfold-list lst)
  (let loop ((lst lst))
    (if (null? lst) '()
	(cons lst (loop (cdr lst))))))

(define structure-copy  vector-copy)

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
;; Nor does this:
(define object-graph (graph-map (lambda (nd) (make-simobject nd '() #f #f)) graph))
(define all-objs (map car object-graph))
;;========================================


;(list-get-radom 


(define (draw)
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
	 [(call ,opera ...) 
	  
	  (error 'process-statement "call not supported from SOC")] ;opera]
	 [(flood ,tok) `(flood (quote ,tok))]
	 [(emit ,opera ...)
	  `(emit (quote ,(car opera)) ,@(cdr opera))]
	 [(light-up ,r ,g ,b)
	  `(begin
	     (disp "trying to light")
	     (if (simobject-gobj this)
	       (begin 
		 (disp "SETTING LIGHT" ,r ,g ,b)
		 (set-color! (simobject-gobj this) ,r ,g ,b))))]
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
			  `[,tok (lambda ,args ,@(map process-statement expr*))]]))
		tbinds)]
	[handler `(lambda (msg args)
		    (disp "HANDLER at" (node-id (simobject-node this)) ": " msg args)
		    ;; Redraw every time we handle a message, our state might have changed.
		    (set-simobject-redraw! this #t)
		    ;; This refers to the token cache for this processor:
		    (hashtab-set! token-cache msg args)
;		    ,(if (null? tbinds) '(void)
		    (case msg
		       ,@(map (lambda (tok)
				`[(,tok) 
				  (apply ,tok args)])
			      (map car tbinds))
		       [else (error 'node_program "Unknown message: ~s" msg)]
		       ))])
    `(letrec (,@binds [handler ,handler]) ,expr)))


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

;; Takes: a program in the language of pass10_deglobalize
;; Returns: a vector #(thunk (thunk ...)) with SOC and node programs respectively.
(define (compile-simulate-nought prog)
  (match prog
    [(program (socpgm (bindings ,socbinds ...) ,socstmts ...)
	      (nodepgm (bindings ,nodebinds ...) (tokens ,nodetoks ...) ,starttoks))
     (let* (

       [generic-defs

	 '(
	   [define token-cache (make-hash-table 50)]
	   [define neighbors (lambda (obj)
;			(disp 'neighbors obj)
			(let ((entry (assq obj object-graph)))
;			  (disp "ENTRY : " entry)
			  (if (null? entry)
			      (error 'neighbors "generated code.. .cannot find obj in graph: ~s ~n ~s"
				     obj object-graph)
			      (cdr entry))))]
	   [define sendmsg (lambda (data ob)
		   (disp 'sendmsg data (node-id (simobject-node ob)))
		   (set-simobject-incoming! ob
		    (cons data (simobject-incoming ob)))
		   ;(set-simobject-redraw! ob #t)
		   )]
	   [define emit (lambda (t . m)
		   (disp 'emit t m)
		   (let ((msg (if (null? m) '() (car m))))
		     (map (lambda (nd) (sendmsg (list t m) nd))
			  (neighbors this))))]
	   [define flood (lambda (t . m)
		    (disp "FLOODING" t m)
		    (let ((msg (if (null? m) '() (car m))))
		      (map (lambda (nd) (sendmsg (list t msg) nd))
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
	    (printf "CALLING Nodeprog: ~s~n" (if (simobject-gobj this) #t #f))
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
			  (loop (simobject-incoming this))
			  
			  ;; This might introduce message loss (because of no
			  ;; semaphores) but I don't care:					
			  (let ((msg (last incoming)))
			    (if (null? (cdr incoming))
				(set-simobject-incoming! this '())
				(list-remove-last! incoming))
			    (handler (car msg) (cadr msg)))
			  )))
		 ))))])

;       (disp "Socprog")
;       (pretty-print socprog)
;       (newline)
;       (disp "Nodeprog")
;       (pretty-print nodeprog)
       (set! f socprog)
       (set! sp socprog)
       (set! np nodeprog)
;       (for-each eval generic-defs)


       (let ([socfun (eval socprog)]
	     [nodefun (eval nodeprog)])
       (vector ;(make-engine 
		(lambda () (socfun (car all-objs)
				   object-graph all-objs))
	       (map (lambda (nd) 
		      ;(make-engine
		       (lambda () 
;			 (disp "run node engine for node" nd)
			 (nodefun nd object-graph all-objs)))
		    all-objs))
       
       ))]))

;; This is the "language definition" which will use the compiler and
;; simulator to evaluate the expression.  It'll be hard to write test
;; cases with meaningful results, though.
(define (simulator-nought-language expr)
  (void))

;;===============================================================================

(define (run-simulation thunks . timeout)
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
	     (engine-block)
	     (loop)))])
    (let ([thunks (if (simulator-output-text)
		       (cons display_engine (cons soceng nodeengs))
		       (cons soceng nodeengs))])      

      (if (null? timeout)
	  (run-flat-threads thunks)
	  (run-flat-threads thunks (car timeout))
	  ))))


;;===============================================================================

(define example-nodal-prog0
  '(program
    (socpgm  (bindings) (emit tok1))
    (nodepgm (bindings) (tokens) ())))

(define these-tests
  `(
    [ (free-vars '(cons (quote 30) x)) (x) ]

    [ (process-statement '(emit foo 2 3)) (emit 'foo 2 3)]
    [ (process-statement '(flood foo)) (flood 'foo)]

    [ (let ((x (make-simobject (make-node 34 '(1 2)) '() #f #f)))
	(let ((y (structure-copy x)))
	  (and (equal? x y)
	       (not (eq? x y))))) #t]

    [ "First just with a trivial SOC program"
      (run-simulation (vector (lambda () 3) '()))
      All_Threads_Returned ]

    [ "Now we throw in a couple trivial nodeprograms" 
      (let ((s (open-output-string)))
	(run-simulation (vector (lambda () 3)
				(list (lambda () 4)
				      (lambda () 5)))
			10))
      All_Threads_Returned ]
    

    [ "Run two threads each with a display" 
      (let ((s (open-output-string)))
	(parameterize ([current-output-port s])
		      (run-simulation (vector (lambda () (display 3))
					      (list (lambda () (display 4))))
				      10)
		      (get-output-string s)))
      ;; Oracle to tell if the answers good:
      ,(lambda (res) (member res (list "34" "43"))) ]

    [ "Now actually run the translator ..."
      (run-simulation
       (compile-simulate-nought
	'(program
	  (socpgm (bindings) );(emit tok1))
	  (nodepgm (bindings) (tokens) () )))
       10)
      unspecified ]


#;    [ (let ((s (open-output-string)))
#;	(parameterize ([current-output-port s])
	   (list 
	    (run-simulation (vector (make-engine (lambda () 3))
				    (list (make-engine (lambda () 4))
					  (make-engine (lambda () 5))))
			    .5)
	    (get-output-string s))))
	   
	Simulation_Done]


    ))

;; Use the default unit tester from helpers.ss:
(define test-this (default-unit-tester this-unit-description these-tests))

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

(dsis tt (csn temprog))
(define a (car all-objs))
(define b object-graph)
(define c all-objs)
;(dsis g ((eval f) a b c))
(dsis g (run-simulation tt .5))

