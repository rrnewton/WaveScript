;; [2004.05.25]

;; This depends on simulator_nought.ss and runs a similar simulation
;; but with a graphical interface.

(define this-unit-description 
  "\"simulator_nought_graphics.ss\"a: simple simulator with graphics.")

;;========================================

(define (unfold-list lst)
  (let loop ((lst lst))
    (if (null? lst) '()
	(cons lst (loop (cdr lst))))))

;;===============================================================================

      (define edge-table (make-hash-table 500))
      (define proc-table (make-hash-table 500))

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

      'Grahpical_Simulation_Done
      )))

(define gsim graphical-simulation)


;;===============================================================================

;; This simplest of programs just lights up all the nodes.
;; Really this only makes sense on the graphical simulator.
;; See simulator_nought_graphics.
(define example-nodal-prog1
  '(program
    (socpgm (bindings) 
	    (emit tok1))
    (nodepgm
;       result_2
       (bindings)
       (tokens
	[tok1 () (flood tok2)]
	[tok2 () (light-up 255 0 100)])
       () ;; seed tokens
       )))

;; This program floods the network with a token, then elects a leader
;; near a point, finally creating a gradient from there.
;;
(define example-nodal-prog99
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


;;===============================================================================

;; UNIT TESTS:

(define these-tests
  `( 

    [ "First test display by bringing it up and then closing it down." 
      (begin (init-graphics) (thread-sleep 300) (close-graphics))
      unspecified ]
    
    ;; Here I repeat some of the tests from the non-graphical simulator:
    [ "Now we start just with a trivial SOC program"
      (begin 
	(init-graphics)
	(graphical-simulation (vector (lambda () 3) '()))
	(thread-sleep 300)
	(close-graphics))
      unspecified]

    [ "Now we throw in a couple trivial nodeprograms" 
      (begin 
	(init-graphics)
	(let ((res 
	       (graphical-simulation (vector (lambda () 3)
					     (list (lambda () 4)
						   (lambda () 5)))
				     10)))
	  (thread-sleep 300)
	  (close-graphics) 
	  res))
      Grahpical_Simulation_Done ]


    [ "Now we test color changing"
      (begin 
	(init-graphics)	
	(let ((res 
	       (graphical-simulation 
		  (vector (lambda () 
			    (for-each (lambda (ob) 
					(set-fill-color! 
					 (simobject-gobj ob)
					 (make <rgb> 0 255 0)))
				      all-objs))
			  '())	  
		  1.0)))
	  (thread-sleep 700)
	  (close-graphics)
	  res))
    Grahpical_Simulation_Done ]

    [ "Color changing from nodepgm instead of soc."
      (begin 
	(init-graphics)
	(graphical-simulation 
	 (vector (lambda () 3)
		 (list (lambda () 
			 (for-each (lambda (ob)
				     (set-fill-color! 
				      (simobject-gobj ob)
				      (make <rgb> 0 255 0)))
				   all-objs))
		       ))
	 1.0)
	(thread-sleep 700)
	(close-graphics))
      unspecified ]

    
    [ "Next actually run the translator ..."
      (begin 
	(init-graphics)
	(graphical-simulation
	 (compile-simulate-nought
	  '(program
	    (socpgm (bindings) );(emit tok1))
	    (nodepgm (bindings) (tokens) () )))
	 1.0)
	(thread-sleep 300)
	(close-graphics))
      unspecified]

    [ "Run the translator on a spreading lights program..."
      (compile-simulate-nought ',example-nodal-prog1)
      unspecified
#;      (lambda (x)
	(procedure? (vector-ref x 0))
	(andmap procedure? (vector-ref x 1)))]

    [ "Then simulate the spreading lights program..."
      (begin 
	(init-graphics)
	(graphical-simulation
	 (compile-simulate-nought ',example-nodal-prog1)
	 1.0)
	(thread-sleep 300)
	(close-graphics))
      unspecified ]

#;    [ "Next we run a program through the translator"
      (begin 
	(init-graphics)
	(let ((res 
	       (graphical-simulation 
		(vector (lambda () 
			  (for-each (lambda (ob) 
				      (set-fill-color! 
				       (simobject-gobj ob)
				       (make <rgb> 0 255 0)))
				    all-objs))
			'())	  
		1.0)))
	  (thread-sleep 700)
	  (close-graphics)
	  res))
      Grahpical_Simulation_Done ]

    
   

    ))

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


(dsis tt (csn example-nodal-prog1))
(define a (car all-objs))
(define b object-graph)
(define c all-objs)
;(dsis g ((eval f) a b c))
(dsis g
      (begin (init-graphics)
	     (graphical-simulation 
	      (csn '(program
		     (socpgm (bindings) );(emit tok1))
		     (nodepgm
		      (bindings)
		      (tokens)
		      () ;; seed tokens
		      )))
	      10)))

(define temp_temp
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

(dsis g
      (begin (init-graphics)
	     (graphical-simulation
	      (csn temp_temp)
	      5.0)))