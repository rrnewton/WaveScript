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

(define structure-copy  vector-copy)

;;===============================================================================

;; This assumes a static comm graph for now:
;; What's more, this is lame and assumes that the *allocated object-graph*
;; will not change, it depends on the physical identity of edges within this 
;; graph.  SO, no using graph-map on object-graph or anything!  We're going full
;; imperative style here...
(define graphical-simulation 
  (lambda (thunks . timeout)
    ;; These "edges" are distinct objects for each comm link (directionally):
    (let ([edges (apply append (map unfold-list (map cdr object-graph)))]
	  [soceng (vector-ref thunks 0)]
	  [nodeengs (vector-ref thunks 1)])

      ;; Contains a graphics object, and the last drawn state.
      (define-structure (edgestate gobj oldstate))
    
      ;; This will associate edges with graphics objects:
      (define edge-table (make-hash-table 500))
      (define proc-table (make-hash-table 500))

      ;; Fill up our two hash tables with drawn objects.
      (for-each (lambda (graph-entry)
		  (let ([proc (car graph-entry)]
			[edges (unfold-list (cdr graph-entry))])
		    (let ([origpos (node-pos (simobject-node proc))])
		      (hashtab-set! proc-table proc
				    (draw-procs origpos))
;; Not done yet.
'		      (for-each 
		       (lambda (edgeob)
			 (hashtab-set! edge-table edgeob
			    (make edgestate
			      (draw-edge origpos (node-pos (simobject-node (car edgeob))))
			      (structure-copy (car edgeob)))))
		       edges)
		      )))
		object-graph)
		       
      (if (null? timeout)
	  (run-flat-threads (cons soceng nodeengs))
	  (run-flat-threads (cons soceng nodeengs) (car timeout)))
	  )))

(define gsim graphical-simulation)



;;===============================================================================

;; This simplest of programs just lights up all the nodes.
;; Really this only makes sense on the graphical simulator.
;; See simulator_nought_graphics.
(define example-nodal-prog0
  '(program
    (socpgm (bindings) (emit tok1))
    (nodepgm
;       result_2
       (bindings)
       (tokens
	[tok1 () (flood tok2)]
	[tok2 () (light-up 255 0 100)])
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



(define these-tests
  `( 

    [ "First test display by bringing it up and then closing it down." 
      (begin (init-graphics) (thread-sleep 500) (close-graphics))
      unspecified ]
    
    ;; Here I repeat some of the tests from the non-graphical simulator:
    [ "Now we start just with a trivial SOC program"
      (graphical-simulation (vector (lambda () 3) '()))
      All_Threads_Returned ]

    ))


(define test-this (default-unit-tester this-unit-description these-tests))


(define testsim test-this)
(define testssim these-tests)



#;    [ "Now we throw in a couple trivial nodeprograms" 
      (let ((s (open-output-string)))
	(graphical-simulation (vector (lambda () 3)
				(list (lambda () 4)
				      (lambda () 5)))
			10))
      All_Threads_Returned ]
    

#;    [ "Run two threads each with a display" 
      (let ((s (open-output-string)))
	(parameterize ([current-output-port s])
		      (graphical-simulation (vector (lambda () (display 3))
					      (list (lambda () (display 4))))
				      10)
		      (get-output-string s)))
      ;; Oracle to tell if the answers good:
      ,(lambda (res) (member res (list "34" "43"))) ]
