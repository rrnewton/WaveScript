;; [2004.05.25]

;; This depends on simulator_nought.ss and runs a similar simulation
;; but with a graphical interface.  This file might also depend on all
;; the resources that simulator_nought.ss depends on.

;; REQUIRES: This file requires the "graphics_stub.ss" interface be loaded
;; so that it may draw the simulation upon the screen.

(define this-unit-description 
  "simulator_nought_graphics.ss: simple simulator with graphics.")

;;===============================================================================

;; <<TODO>> THESE ARE REDUNDANT!! DO AWAY WITH:
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

;; This just uses the global variables storing the simulator state to
;; draw it up on the screen.
(define (draw-sim)
  ;; This is a promise so as to be called only once.
  (define wipe-screen (delay clear-buffer))

  ;; Contains a graphics object, and the last drawn state.
  (define-structure (edgestate gobj oldstate))

  (if object-graph
  ;; Fill up our two hash tables with drawn objects.
      (for-each (lambda (graph-entry)		   
		  (let ([proc (car graph-entry)]
			[edges (unfold-list (cdr graph-entry))])
		    (let ([origpos (node-pos (simobject-node proc))])
		      ;; If the processor has already been drawn, we trigger a screen wipe.
		      (if (simobject-gobj proc) (force wipe-screen))
		      (let ((gobj (draw-proc origpos)))
			(hashtab-set! proc-table proc gobj)
			(set-simobject-gobj! proc gobj))
		      ;; Not done yet.
		      ;; This just draws the edges once... need to take responsibility for
		      ;; changing them:
		      (for-each 
		       (lambda (edgeob)
			 (hashtab-set! edge-table edgeob
				       (make-edgestate
					(draw-edge origpos (node-pos (simobject-node (car edgeob))))
					(structure-copy (car edgeob)))))
		       edges)
		      )))
		object-graph)))

(define graphical-simulation   
  (generate-simulator
   ;; THREADRUNNER:
   run-flat-threads

   ;; SIMCORE:
   ;; Build engines and display driver:
   (lambda (funcs . timeout)
                 
     ;; These "edges" are distinct objects for each comm link (directionally):
     (let ([edges (apply append (map unfold-list (map cdr object-graph)))]
	   [soceng (vector-ref funcs 0)]
	   [nodeengs (vector-ref funcs 1)])      
    
       ;; This will associate edges with graphics objects:
       (define positions (map node-pos (map simobject-node all-objs)))
;      (draw-procs positions)
      
       (draw-sim)

       ;; Return the thunks:
       (cons soceng nodeengs)))))

(define gsim graphical-simulation)

;;===============================================================================

;; UNIT TESTS:

;(load "simulator_nought.examples.ss")
;(include "simulator_nought.examples.ss")

(define these-tests
  `( 
    ;; 0
    [ "First test display by bringing it up and then closing it down." 
      (begin (init-graphics) (sleep-me 0.3) (close-graphics))
      unspecified ]

    ;; 1
    [ "Now we test color changing"
      (begin 
	(init-graphics)	
	(let ((res 
	       (graphical-simulation 
		(lambda (soc-ret soc-fin)
		  (vector (lambda () 
			    (for-each (lambda (ob) 
					(change-color!
					 (simobject-gobj ob)
					 (rgb 0 255 0)))
				      all-objs))
			  '()))
		  1.0)))
	  (sleep-me 0.7)
	  (close-graphics)
	  res))
      All_Threads_Returned]
;    Grahpical_Simulation_Done ]

    ;; 2 
    [ "Color changing from nodepgm instead of soc."
      (begin 
	(init-graphics)
	(graphical-simulation 
	 (lambda (soc-ret soc-fin)
	   (vector (lambda () 3)
		   (list (lambda () 
			   (for-each (lambda (ob)
				       (change-color!
					(simobject-gobj ob)
					(rgb 0 255 0)))
				     all-objs))
			 )))
	 1.0)
	(sleep-me 0.7)
	(close-graphics))
      unspecified ]

    ;; 3
    [ "Flood lights program..."
      (begin 
	(init-graphics)
	(graphical-simulation
	 (build-simulation (compile-simulate-nought ',example-nodal-prog1))
	 0.5)
;	(sleep-me 1.0)
	(close-graphics))
      unspecified]
      
  ;; 4: This really should be graphical only.  
  [ "Build Spread-lights-gradually..."
    (build-simulation (compile-simulate-nought ',example-nodal-prog2))
    ,(lambda (sim)
       (let ((x (sim (lambda (v) (void)) (lambda (v) (void)))))
	 (procedure? (vector-ref x 0))
	 (andmap procedure? (vector-ref x 1))))]

  ;; 5: 
  [ "Run Spread-lights-gradually..."
    (simulate
     (build-simulation (compile-simulate-nought ',example-nodal-prog2))
     0.5)
    unspecified]

  ;; 6: FIXME: Why should init-world vs. cleanse-world make a difference!! 
  [ "Run Spread-lights-gradually with display distance..."
    (begin (cleanse-world); (init-world)
	   (simulate
	    (build-simulation (compile-simulate-nought ',example-nodal-prog3))
	    0.7))
    unspecified]

  ;; 7:  
  [ "Return a value via the gradient..."
    (begin (cleanse-world) ;(init-world)
	   (simulate
	    (build-simulation 
	     (compile-simulate-nought ',example-nodal-prog4))
	    1.7))
    ,(lambda (ret)
       (disp "GOT RETURN VALS: " ret)
       (not (null? ret)))]
  
  ;; What do we expect the current directory to be??
  ,@(include "simulator_nought.tests")
  ))


(define (wrap-def-simulate test)
  `(begin (define (simulate . args)
	    (init-graphics)
	    (let ((res (apply graphical-simulation args)))
	      (sleep-me 0.3)
	      (close-graphics)
	      res))
	  ,test))

;; This makes sure the world is initialized before doing unit tests:
(define test-this
  (let ((tester (default-unit-tester 
		  this-unit-description 
		  (map (lambda (test)
			 (match test
			   [(,prog ,res) `((begin (cleanse-world) ,prog) ,res)]
			   [(,name ,prog ,res) 
			    `(name (begin (cleanse-world) ,prog) ,res)]))
			 these-tests)
		  tester-eq?
		  wrap-def-simulate)))
    (lambda args
      ;; First init world
      (init-world)
      ;; Then bring in tests:
      (apply tester args))))

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

#;(dsis g
      (begin (init-graphics)
	     (graphical-simulation
	      (build-simulation 
	       (csn example-nodal-prog1))
	      5.0)))

