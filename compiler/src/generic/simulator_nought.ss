;; simulator_nought.ss
;;  -Ryan Newton [2004.05]
;; This is a simulator for the output of pass10_deglobalize.  
;; A simple simulator.
;; This file uses and abuses top-level bindings (like mad).
;; It should be loaded inside a module that only exports:
;;   (provide simulator-nought-language)

;; DEPENDS: This file requires that the slib 'tsort module be loaded
;; providing the topological-sort function.


(define this-unit-description 
  "simplest simulator for nodal language")

;; This is the simplest simulator ever.  Takes the output of pass "deglobalize".

(define-structure (node id pos))
(define-structure (simobject node incoming))

(define world-xbound 60)
(define world-ybound 60)
(define radius 30)
(define numprocs 10)

(define (random-node) 
  (make-node 
   (random 100);(expt 2 32))
   (list (random world-xbound)
	 (random world-ybound))
   ))

(define (dist a b)
  (sqrt (+ (expt (- (car a) (car b)) 2)
	   (expt (- (cadr a) (cadr b)) 2))))
	
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
(define object-graph (graph-map (lambda (nd) (make-simobject nd '())) graph))
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
	 [(call ,opera ...) (error 'process-statement "call not supported from SOC")] ;opera]
	 [(emit ,opera ...) 
	  `(emit (quote ,(car opera)) ,@(cdr opera))]
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
			  `[,tok (lambda ,args ,expr* ...)]]))
		tbinds)]
	[handler `(lambda (msg args)
		    (case msg
		      ,(map (lambda (tok)
			      `[(,tok) (apply ,tok args)])
			    (map car tbinds))))])
  `(letrec (,@binds [handler ,handler]) ,expr)))



(define-syntax remove-last!
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

(define (compile-simulate-nought prog)
  (match prog
    [(program (socpgm (bindings ,socbinds ...) ,socstmts ...)
	      (nodepgm (bindings ,nodebinds ...) (tokens ,nodetoks ...) ,starttok))
     (let* (

       [generic-defs
	 '([define neighbors (lambda (obj)
			(disp 'neighbors obj)
			(let ((entry (assq obj object-graph)))
			  (disp "ENTRY : " entry)
			  (if (null? entry)
			      (error 'neighbors "generated code.. .cannot find obj in graph: ~s ~n ~s"
				     obj object-graph)
			      (cdr entry))))]
	   [define send (lambda (data ob)
		   (disp 'send data ob)
		   (set-simobject-incoming! ob
		    (cons data (simobject-incoming ob))))]
	   [define emit (lambda (t . m)
		   (disp 'emit t m)
		   (let ((msg (if (null? m) '() (car m))))
		     (map (lambda (nd) (send (list t m) nd))
			  (neighbors this))))]
	   [define flood (lambda (t . m)
		    (disp 'flood t m)
		    (let ((msg (if (null? m) '() (car m))))
		      (map (lambda (nd) (send (list t msg) nd))
			   all-objs)))])]

       [socprog
	 `(lambda (this object-graph all-objs)
	    (printf "CALLING SocProg: ~s~n" this)
	    (let () ,@generic-defs
	      ,(process-binds socbinds
			      `(begin ,@(map process-statement 
					     socstmts)
				      'soc_finished))))]

       [nodeprog
	`(lambda (this object-graph all-objs)	    
	    (printf "CALLING Nodeprog: ~s~n" this)
	    (let () ,@generic-defs	      
	      ,(process-binds 
		nodebinds 
		(process-tokbinds 
		 nodetoks
		 `(begin 
		    (,starttok)
		    (let loop ([incoming (simobj-incoming this)])
		      (if (null? incoming)
			  ;; No good way to wait or stop the engine execution?
			  (loop (node-incoming this))
			  
			  ;; This might introduce message loss (because of no
			  ;; semaphores) but I don't care:					
			  (let ((msg (last incoming)))
			    (remove-last! incoming)
			    (handler (car msg) (cadr msg))))))	
		 ))))])

       (disp "Socprog")
       (pretty-print socprog)      8 
       (set! f socprog)
       (for-each eval generic-defs)


       (let ([socfun (eval socprog)]
	     [nodefun (eval nodeprog)])	 
       (vector (make-engine 
		(lambda () (socfun (car all-objs) 
				   object-graph all-objs)))
	       (map (lambda (nd) 
		      (make-engine
		       (lambda () 
			 (disp "run node engine for node" nd)
			 (nodefun nd object-graph all-objs))))
		    all-objs))
       
       ))]))

(define (run-simulation engines . rounds)
  (let ([soceng (vector-ref engines 0)]
	[nodeengs (vector-ref engines 1)])
    (let loop ([engs (cons soceng nodeengs)]
	       [acc '()]
	       [rounds (if (null? rounds) -1 (car rounds))])
;      (disp "looping " engs acc rounds)
      (cond
       [(= rounds 0) 'Simulation_Done]
       [(null? engs)
	(begin 
	; (printf "Beginning loop around ~s engines.~n" (length acc))
	  (loop (reverse acc) '() (- rounds 1)))]
       [else 
	((car engs) 100
	 (lambda (remaining ret) 
	   ;(error 'run-simulation "engine shouldn't return.  Values were: ~n~s~n" ret))
	   (printf "Engine returned!: ~s~n" ret)
	   (loop (cdr engs) acc rounds))
	 (lambda (nexteng)
	   (loop (cdr engs) (cons nexteng acc) rounds)))]
      ))))


(define (simulator-nought-language expr)
  (void))

;========================================

(define these-tests
  `(
    [ (free-vars '(cons (quote 30) x)) '(x) ]
    ))

(define test-this
  (let ((these-tests these-tests))
    (lambda args 
      (let ([verbose (memq 'verbose args)]
	    [tests (map car these-tests)]
	    [intended (map cadr these-tests)]
	    [success #t])
	  (if verbose 
	      (printf "Testing ~s\n" this-unit-description))
	  (let ((results (map eval tests)))
	    (for-each 
	     (lambda (expr intended result) 
	       (display-constrained "  " `(,expr 40) 
				    " -> " `(,intended 20)
				    ": ")
	       (if (tester-equal? intended result)
		   (if verbose (display "#t\n"))
		   (begin (set! success #f)
			  (newline)
			  (display "FAIL: ")
			  (display-constrained `(,intended 40))
			  (newline) (newline))))
	     tests intended results))))))

(define testsim test-this)
(define testssim these-tests)


;;===============================================================================


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


(define p
  '(program
    (socpgm (bindings) (emit result_2))
    (nodepgm
;       result_2
       (bindings (tmp_4 (cons '40 '())) (tmp_1 (cons '30 tmp_4)))
       (tokens
	[f_token_tmp_3 () (flood token_6)]
	[token_6
            ()
            (if (< (locdiff (loc) tmp_1) 10.0)
                (elect-leader m_token_tmp_3))]
	[m_token_tmp_3 () (call f_token_result_2)]
	[f_token_result_2 () (emit m_token_result_2)]
	[m_token_result_2
            ()
            (if (< (dist f_token_result_2) '50) (relay))])
       f_token_tmp_3
       )))

(define csn  compile-simulate-nought)

(dsis tt (csn p))
(define a (car all-objs))
(define b object-graph)
(define c all-objs)
(dsis g ((eval f) a b c))

