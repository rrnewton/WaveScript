;;;; .title SimAlpha Roll World(s) -- Simulatio & topology creation

;;;; [2006.03.01] <br>
;;;; I have factored this file out from the complex simulator_alpha.ss
;;;; (Which has spun off many-a-file at this point.)

; =================================================================================

;;; Constructing and maintaining world objects.

;; This is the function (of no arguments) for creating a new world
;; simulation.  It takes no arguments because all the decisions
;; affecting the kind of topology to create are made based upon the
;; values of global regiment system parameters. <br><br>
;;
;;<br>  Relevent parameters include (as of [2006.03.01]):
;;<br>    simalpha-placement-type
;;<br>    simalpha-connectivity-function
;;<br>    simalpha-channel-model
;;<br>    simalpha-outer-radius
;;<br>    simalpha-inner-radius
;;<br>    simalpha-consec-ids
;;<br>    simalpha-world-xbound
;;<br>    simalpha-world-ybound
;;<br>    simalpha-max-gridlike-perturbation
;;
;; <br><br>
;; Parameterize these before calling to control the nature of the created simworld object.
;;
;; <br><br>
;;  This subroutine generates randomzied topology: 
;; (There are more topologies in "network_topologies.ss")
;;
;;   .returns A 'simworld' record.
(define (fresh-simulation)  

  ;; Make a graph of simobjects out of the graph of nodes.
  (define (make-object-graph g world)
    (graph-map (lambda (n) (node->simobject n world)) g))
  
  ;; This is our helper function that checks for collisions.
  (define (collide?  n1 n2)
    (let ((connectivity ((simalpha-connectivity-function)
			 (node-pos n1) (node-pos n2))))
      (if (not (memq connectivity '(0 100))) (printf "connectivity: ~s\n" connectivity))
      (not (eqv? 0 connectivity))))
 
  ; --------------------------------------------------
  ;; Randomly places nodes.
  (define (make-random-topology)    
    ;; Old method, doesn't produce connected graph:
    (let ((seed (map (lambda (_) (random-node)) (iota (sim-num-nodes)))))
      (if (simalpha-consec-ids)
	  (for-each set-node-id! 
		    seed (iota (length seed))))
      ;; Now we just SET the first node to have the BASE_ID and serve as the SOC.
      (set-node-id! (car seed) BASE_ID)
      ;; Connect the graph:
      ;; TODO: Make the graph representation better.  Should cache the connectivity functions on the edges.
      (set! seed
	    (map (lambda (node)
		   (cons node 
			 (filter (lambda (n) 
				   (and (not (eq? node n))
					(let ((connection ((simalpha-connectivity-function) (node-pos node) (node-pos n))))
					  ;; We establish a connection unless we get zero-reception
					  (not (eqv? connection 0))
					  )))
				 seed)))
		 seed))
      seed))

  ; --------------------------------------------------
  ;; Places nodes randomly, but each node must be connected to the nodes placed before.
  (define (make-connected-topology)
    ;; This might have some serious biases in the layout and distribution of degree.
    (let ((start-node (let ((x (random-node)))				   
			(set-node-id! x BASE_ID)
			x)))
      (let loop ((graph (list (list start-node))) (count (sub1 (sim-num-nodes))))
					;		 (printf "\nLooping: graph:\n")
					;		 (pretty-print graph)
	(if (<= count 0)
	    graph
	    (let ((newnode (random-node)))
	      ;; Here we number them as we go.  
	      ;; This will result in a non-random spatial distribution of node-ids.
	      (if (simalpha-consec-ids)
		  (set-node-id! newnode count))
	      (let ((nbr-rows (filter (lambda (row) (collide? newnode (car row))) graph)))
		(if (null? nbr-rows)
		    (loop graph count)
		    (begin 
		      (for-each (lambda (row)
				  ;; Splice in this as a neighbor:
				  (set-cdr! row (cons newnode (cdr row))))
				nbr-rows)
		      (loop (cons (cons newnode (map car nbr-rows))
				  graph)
			    (sub1 count))))))))))

    ; --------------------------------------------------
  ;; A perturbed grid of nodes.
  (define (make-gridlike-topology perfect?)
    (let* ([num-nodes (sim-num-nodes)]
	   [seed (make-vector num-nodes)]
	   ;; Calculate dimensions
	   [height (inexact->exact (floor (sqrt num-nodes)))]
	   [width (ceiling (/ num-nodes height))]
	   [step (fxmin (fx/ (simalpha-world-xbound) width)
			(fx/ (simalpha-world-ybound) height))]
	   [offsetx (fx/ (fx- (simalpha-world-xbound) (fx* (fx- width 1) step)) 2)]
	   [offsety (fx/ (fx- (simalpha-world-ybound) (fx* (fx- height 1) step)) 2)])

      ;; For now we use a simple linear perturbation.
      (define (perturb coord)
	(if perfect? coord
	    (if (zero? (simalpha-max-gridlike-perturbation)) coord
		(+ coord (reg:random-int 
			  (inexact->exact 
			   (floor (* step (simalpha-max-gridlike-perturbation)))))))))

      (for i = 0 to (sub1 num-nodes) ;; Allocate the nodes.
	   (vector-set! seed i (random-node)))
      (if (simalpha-consec-ids)
	  (begin (set-node-id! (vector-ref seed 0) BASE_ID)
		 (for i = 1 to (sub1 num-nodes)
		      (set-node-id! (vector-ref seed i) i))
		 ;; Have to mix it up after that
		 (randomize-vector seed))
	  ;; Otherwise a random node becomes base:
	  (set-node-id! (vector-ref seed (reg:random-int num-nodes)) BASE_ID))
      ;; Now we tile them in a gridlike fashion, and perturb them.
      (for i = 0 to (sub1 num-nodes) 
	   (set-node-pos! (vector-ref seed i)
			  (let ((pos (list (fx+ offsetx (fx* step (quotient i height)))
					   (fx+ offsety (fx* step (remainder i height))))))
			    ;; Then we perturb them randomly off that position:
			    (map perturb pos))))
      ;; Finally, connect the graph:
      (let ((seed (vector->list seed)))
	(map (lambda (node)
	       (cons node 
		     (filter (lambda (n) 
			       (and (not (eq? node n))
				    (let ((connection ((simalpha-connectivity-function) (node-pos node) (node-pos n))))
				      ;; We establish a connection unless we get zero-reception
				      (not (eqv? connection 0))
				      )))
				 seed)))
	     seed))))

  ; --------------------------------------------------
  (define (make-topology) ;; Returns the graph	  
    (case (simalpha-placement-type)
      [(random) (make-random-topology)]
      [(connected) (make-connected-topology)]
      [(gridlike) (make-gridlike-topology #f)]
      [else (error 'simulator_alpha:fresh-simulation 
		   "unknown node placement strategy: ~s" (simalpha-placement-type))]))
  
  (printf "Rolling fresh simulator world (current srand seed ~a).\n" (reg:get-random-state))
  ;; Set global parameter:
  ;; This is a function which takes two locations and returns either
  ;;  1) a number, representing the loss percentage
  ;;  2) a function of time, representing the loss percentage over time
  (simalpha-connectivity-function
    (case (simalpha-channel-model)
      [(lossless)
       (lambda (p1 p2) ;x1 y1 x2 y2)
	 (let ((dist (posdist p1 p2))) ;(sqrt (^ (- x2 x1) 2) (^ (- y2 y1) 2))))
	   (if (< dist (simalpha-outer-radius))
	       100
	       0)))]
      [(linear-disc)
       (printf "Got linear disc...\n")
       (lambda (p1 p2)	
	 (let ((dist (posdist p1 p2))
	       (outer (simalpha-outer-radius))
	       (inner (simalpha-inner-radius)))
;	   (printf "Considering ~s (~s, ~s) between ~s and ~s\n" dist p1 p2 inner outer)
	   (cond
	    [(<= dist inner) 100]
	    [(> dist outer) 0]
	    [else
	     (inexact->exact (floor (* 100 (/ (- outer dist) (- outer inner)))))])))]))
   
  ;; TODO, FIXME, need to use a real graph library for this!
  ;; (One that treats edges with due respect, and that uses hash tables.)
   (let* ([theworld (make-simworld #f #f #f #f #f #f #f)]
	  [graph (make-topology)]
;	  [soc (caar graph)]
          [obgraph (make-object-graph graph theworld)]
          [allobs  (map car obgraph)]
	  [hash
	   (let ([h (make-default-hash-table)])
	     (for-each (lambda (ob)
			 (hashtab-set! h (node-id (simobject-node ob)) ob))
		       allobs)
	     h)]
	  [scheduler-queue '()])

     (DEBUGMODE
      (andmap (lambda (row) (andmap node? row))
	      graph))
     ;; Set I-am-SOC
     (for-each (lambda (ob)
		 (set-simobject-I-am-SOC! ob		  
		  (= (node-id (simobject-node ob)) BASE_ID)))
	       allobs)
     (set-simworld-graph! theworld graph)
     (set-simworld-object-graph! theworld obgraph)
     (set-simworld-all-objs! theworld allobs)
     (set-simworld-obj-hash! theworld hash)
     (set-simworld-scheduler-queue! theworld scheduler-queue)
     (set-simworld-vtime! theworld 0)
     
     (set-simworld-led-toggle-states! theworld (make-default-hash-table))
     theworld))  ;; End fresh-simulation


;; This scrubs all the transient state off an existing simworld.
;; .returns The cleaned simworld.
(define clean-simworld!
  (lambda (sim) ;(sim : simworld? -> simworld?)

  (set-simworld-scheduler-queue! sim '())
  (set-simworld-vtime! sim 0)

  ; Flip off all the LEDs
    (IF_GRAPHICS
     (hashtab-for-each 
      (lambda (id flipped)
        (let ((ob (hashtab-get (simworld-obj-hash sim) id)))
          (for-each (lambda (x) (sim-leds 'toggle x ob)) flipped)))
      (simworld-led-toggle-states sim)))
  (DEBUGMODE ; Now make sure that we actually turned off all the LEDs:
   (hashtab-for-each (lambda (id flipped) (DEBUGASSERT (null? flipped)))
		     (simworld-led-toggle-states sim)))

  ; Flip off all the edge highlights:
    (IF_GRAPHICS
     (let ((lines
            (list->set ;; eq? based list->set
             (apply append
                    (map (lambda (simob)
                           (map cadr (gobject-edgelist (simobject-gobj simob))))
                         (simworld-all-objs sim))))))
       (for-each unhighlight-edge lines)))
  
    ; Set all labels to the empty string:
    (IF_GRAPHICS
     (for-each (lambda (x) (sim-setlabel "" x)) (simworld-all-objs sim)))

  ; Reset all the transient state on the simobjects:
  (for-each (lambda (so)
	      (set-simobject-token-store! so (make-default-hash-table 100))
	      (set-simobject-token-table! so (make-default-hash-table 100))
	      (set-simobject-incoming-msg-buf! so '())
	      (set-simobject-outgoing-msg-buf! so '())
	      (set-simobject-local-msg-buf! so '())
	      (set-simobject-timed-token-buf! so '())
	      (set-simobject-local-sent-messages! so 0)
	      (set-simobject-local-recv-messages! so 0)
	      (set-simobject-redraw! so #f)
	      (set-simobject-homepage! so '())
;	      (set-simobject-scheduler! so #f)
;	      (set-simobject-meta-handler! so #f)
	      (set-simobject-worldptr! so sim)
	      so)
    (simworld-all-objs sim))
  sim))

;; And this cleans the global parameters that go along with a simulation.
;; (Does nothing right now.)
(define (clean-simalpha-counters!)
  ;(simalpha-total-messages 0)
  ;(simalpha-total-tokens 0)
  (void)
  )
