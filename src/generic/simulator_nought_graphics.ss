;; [2004.05.25]

;; This depends on simulator_nought.ss and runs a similar simulation
;; but with a graphical interface.

;; DEPENDS ON INCLUDE SYNTAX

(define this-unit-description 
  "\"simulator_nought_graphics.ss\"a: simple simulator with graphics.")

;;========================================

(define (unfold-list lst)
  (let loop ((lst lst))
    (if (null? lst) '()
	(cons lst (loop (cdr lst))))))

;;===============================================================================

(define edge-table (make-default-hash-table))
(define proc-table (make-default-hash-table))

;; This assumes a static comm graph for now:
;; What's more, this is lame and assumes that the *allocated object-graph*
;; will not change, it depends on the physical identity of edges within this 
;; graph.  SO, no using graph-map on object-graph or anything!  We're going full
;; imperative style here...

;;   NOTE: This takes *functions* rather than thunks, they are expected 
;; to take their own graphical objects as input, giving them handles for
;; changing their color and so forth.
(define graphical-simulation 
  (lambda (funcs . timeout)
    (eval '(begin 
	     ;; First, set up a global counter for communication cost:
	     ;; (This is defined with eval because otherwise the module system gets angrye
	     (define total-messages 0)
	   ;; This is a global flag which can be toggled to shut down all the
	     ;; running processors.
	     (define stop-nodes #f)	   
	     ;; Define global bindings for these so that we can do fluid-let on them.
	     (define soc-return 'unbound-right-now)
	     (define soc-finished 'unbound-right-now)))
        
    ;; These "edges" are distinct objects for each comm link (directionally):
    (let ([edges (apply append (map unfold-list (map cdr object-graph)))]
	  [soceng (vector-ref funcs 0)]
	  [nodeengs (vector-ref funcs 1)])

      ;; Contains a graphics object, and the last drawn state.
      (define-structure (edgestate gobj oldstate))
    
      ;; This will associate edges with graphics objects:


      (define positions (map node-pos (map simobject-node all-objs)))
;      (draw-procs positions)

      ;; Fill up our two hash tables with drawn objects.
      (for-each (lambda (graph-entry)
;		  (disp "DOing graph entry" (length graph-entry) graph-entry )
		  (let ([proc (car graph-entry)]
			[edges (unfold-list (cdr graph-entry))])
;		    (disp "EDGES" (length edges)edges)

		    (let ([origpos (node-pos (simobject-node proc))])

		      (let ((gobj (draw-proc origpos)))
			(hashtab-set! proc-table proc gobj)
			(set-simobject-gobj! proc gobj))
		      
;; Not done yet.
;; This just draws the edges once... need to take responsibility for
;; changing them:
		      (for-each 
		       (lambda (edgeob)
;			 (disp "DOING edge" edgeob)
			 (hashtab-set! edge-table edgeob
			    (make-edgestate
			     (draw-edge origpos (node-pos (simobject-node (car edgeob))))
			     (structure-copy (car edgeob)))))
		       edges)

		      )))
		object-graph)
		       
      (if (null? timeout)
	  (run-flat-threads (cons soceng nodeengs))
	  (run-flat-threads (cons soceng nodeengs) (car timeout)))
;      'Grahpical_Simulation_Done
      )))

(define gsim graphical-simulation)

;;===============================================================================

;; UNIT TESTS:

;(load "simulator_nought.examples.ss")
;(include "simulator_nought.examples.ss")

(define these-tests
  `( 
    
    [ "First test display by bringing it up and then closing it down." 
      (begin (init-graphics) (sleep-me 0.3) (close-graphics))
      unspecified ]
          
    [ "Now we test color changing"
      (begin 
	(init-graphics)	
	(let ((res 
	       (graphical-simulation 
		  (vector (lambda () 
			    (for-each (lambda (ob) 
					(change-color!
					 (simobject-gobj ob)
					 (rgb 0 255 0)))
				      all-objs))
			  '())
		  1.0)))
	  (sleep-me 0.7)
	  (close-graphics)
	  res))
      All_Threads_Returned]
;    Grahpical_Simulation_Done ]

    [ "Color changing from nodepgm instead of soc."
      (begin 
	(init-graphics)
	(graphical-simulation 
	 (vector (lambda () 3)
		 (list (lambda () 
			 (for-each (lambda (ob)
				     (change-color!
				      (simobject-gobj ob)
				      (rgb 0 255 0)))
				   all-objs))
		       ))
	 1.0)
	(sleep-me 0.7)
	(close-graphics))
      unspecified ]

    [ "Flood lights program..."
      (begin 
	(init-graphics)
	(graphical-simulation
	 (build-simulation (compile-simulate-nought ',example-nodal-prog1)))
	(sleep-me 1.0)
	(close-graphics))
      unspecified]
      
  ;; This really should be graphical only.
  [ "Build Spread lights gradually..."
    (build-simulation (compile-simulate-nought ',example-nodal-prog2))
    ,(lambda (x)
       (procedure? (vector-ref x 0))
       (andmap procedure? (vector-ref x 1)))]

  [ "Run Spread lights gradually..."
    (simulate
     (build-simulation (compile-simulate-nought ',example-nodal-prog2))
     5.0)
    unspecified]

  [ "Run Spread lights gradually with display distance..."
    (begin (init-world)
	   (simulate
	    (build-simulation (compile-simulate-nought ',example-nodal-prog3))
	    1.7))
    unspecified]
  
;    ,@(include "simulator_nought.tests")
  ))


(define (wrap-def-simulate test)
  `(begin (define (simulate . args)
	    (init-graphics)
	    (let ((res (apply graphical-simulation args)))
	      (sleep-me 0.3)
	      (close-graphics)
	      res))
	  ,test))

(define test-this (default-unit-tester 
		    this-unit-description 
		    these-tests
		    tester-eq?
		    wrap-def-simulate))

(define testgsim test-this)
(define testsgsim these-tests)

;;===============================================================================
;; JUNK 

'(define csn  compile-simulate-nought)

'(define t1
  '((shirt tie belt)
    (tie jacket)
    (belt jacket)
    (watch)
    (pants shoes belt)
    (undershorts pants shoes)
    (socks shoes)))

'(define t2
  '((a b) (b c) (c a)
    (d e) (e f)))


'(dsis tt (csn example-nodal-prog1))
'(define a (car all-objs))
'(define b object-graph)
'(define c all-objs)
;(dsis g ((eval f) a b c))
;a (dsis g )

'(define temp_temp
  '(program
    (socpgm (bindings) 
	    (disp "SOCPROG" )
	    (light-up 0 255 255)
	    (emit tok1))
    (nodepgm
;       result_2
       (bindings)
       (tokens
	[tok1 () 
	      (disp "Firing Tok1")
	      (flood tok2)]
	[tok2 () 
	      (disp "Firing Tok2 and lighting up")
	      (light-up 255 255 0)])
       () ;; seed tokens
       )))

'(dsis g
      (begin (init-graphics)
	     (graphical-simulation
	      (build-simulation 
	       (csn example-nodal-prog1))
	      5.0)))

(define (ttt)
  (run-simulation
   (build-simulation 
    (compile-simulate-nought 
     example-nodal-prog4))
   1.7))

(set! these-tests
  `([ "Simple test"
    (begin (init-world)
;	   (simulate
	   (ttt))
    unspecified])
  )
