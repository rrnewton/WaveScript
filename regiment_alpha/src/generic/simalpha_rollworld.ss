;;;; .title SimAlpha Roll World(s) -- Simulatio & topology creation

;;;; [2006.03.01] <br>
;;;; I have factored this file out from the complex simulator_alpha.ss
;;;; (Which has spun off many-a-file at this point.)

; =================================================================================

;;; Constructing and maintaining world objects.

;; Utility for generating a node with random id in random location.
;; This is done safely so that it cannot conflict with the BASE_ID or NULL_ID.
(define (random-node) 
  (make-node 
   (let loop ((id (reg:random-int 1000)))
     (if (or (eq? id BASE_ID) 
	     (eq? id NULL_ID))
	 (loop (reg:random-int 1000))
	 id))
   (list (reg:random-int (simalpha-world-xbound))
	 (reg:random-int (simalpha-world-ybound)))
   ))

;; Here we build a new connectivity function based on the current parameter settings.
;; This is a function which takes two locations and returns either
;;  1) a number, representing a fixed loss percentage
;;  2) a function of time, representing the loss percentage over time
(define (build-connectivity-fun)
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

;; This is the function (of no arguments) for creating a new world
;; simulation.  It takes no arguments because all the decisions
;; affecting the kind of topology to create are made based upon the
;; values of global regiment system parameters. <br><br>
;;
;;<br>  Relevent parameters include (as of [2006.03.01]):
;;<br>    simalpha-placement-type
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
;;
;; <br><br>
;;  This subroutine generates randomzied topology: 
;; (There are more topologies in "network_topologies.ss")
;;
;;   .returns A 'simworld' record.
(define (fresh-simulation)    

  (define connectivity-fun (build-connectivity-fun))

  ;; This is our helper function that checks for collisions.
  (define (collide?  n1 n2)
    (let ((connectivity (connectivity-fun
			 (node-pos n1) (node-pos n2))))
      (if (not (memq connectivity '(0 100))) (printf "connectivity: ~s\n" connectivity))
      (not (eqv? 0 connectivity))))
  
  ;; This generates a valid set of node-ids.  Either randomly or in order.
  ;; [2006.03.01] Added the restriction for now that ids be unique.
  (define (roll-node-ids)
    ;; Now we just SET the first node to have the BASE_ID and serve as the SOC.
    (cons BASE_ID 
	  (if (simalpha-consec-ids)
	      ;; Is BASE_ID in this 1-n span?  If so have to ensure no dups.
	      (if (and (>= BASE_ID 1) (< BASE_ID (sim-num-nodes)))
		  (remq BASE_ID (cdr (iota (add1 (sim-num-nodes)))))
		  (cdr (iota (sim-num-nodes))))
	      (let loop ((n (sub1 (sim-num-nodes))) (acc ()))
		(if (zero? n) acc
		    (let ([x (reg:random-int 1000)])
		      (if (or (memq x acc) (= x BASE_ID) (= x NULL_ID))
			  (loop n acc)
			  (loop (sub1 n) (cons x acc)))))))))
  
  ; --------------------------------------------------
  ;; Function that randomly places nodes.
  (define (make-random-topology)    
    ;; Old method, doesn't produce connected graph:
    (let ((seed (map (lambda (_) (random-node)) (iota (sim-num-nodes)))))
      (for-each set-node-id! seed (roll-node-ids)) 

      ;; Connect the graph:
      ;; TODO: Make the graph representation better.  Should cache the connectivity functions on the edges.
      (set! seed
	    (map (lambda (node)
		   (cons node 
			 (filter (lambda (n) 
				   (and (not (eq? node n))
					(let ((connection (connectivity-fun (node-pos node) (node-pos n))))
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
    (let ((start-node (random-node)))
      (let loop ((graph (list (list start-node))) (count (sub1 (sim-num-nodes))))
	(if (<= count 0)
	    (begin 
	      ;; Randomly assign node-ids
	      (for-each set-node-id! (randomize-list (map car graph)) (roll-node-ids))
	      ;; Then the graph is ready to return.
	      graph)
	    (let ((newnode (random-node)))
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

      ;; Set node ids randomly:
      (for-each set-node-id! (randomize-list (vector->list seed))
		(roll-node-ids))

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
				    (let ((connection (connectivity-fun (node-pos node) (node-pos n))))
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

  (let ([theworld (make-simworld (make-topology) #f #f #f #f #f #f #f)])
    (animate-world! theworld)
    theworld)

)  ;; End fresh-simulation


; ======================================================================
;; This scrubs all the transient state off an existing simworld.
;; But it keeps the topological structure of the world the same.
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


; ================================================================================
;;; Marshalling world objects.

;;; I would like to save simworld's to disk so that I may use the same
;;; one repeatedly for many experiments.

;; This removes all the hash-tables and procedures (unmarshalable
;; things) from the simworld object.  Thereafter we can write it to a file.
;; 
;; .param mode Either 'full or 'topo-only.  We either want to store
;; just the layout of the nodes, or we want to flash-freeze the whole
;; world (even in-flight messages!).
(define freeze-world
  (case-lambda 
    [(world) (freeze-world 'topo-only)]
    [(world mode)
     9]
    ))

;; This takes a dessicated simworld and reanimates its lively state
;; from whatever dead-state remains.  The bare minimum is that the
;; node-graph exist.
;;
;; Missing fields in the record are assumed to be represented by #f.
;;
;; TODO FIXME: Currently the connectivity function is reanimated using the
;; CURRENT value of the global parameter.  This state needs to be
;; encapsulated within the frozen world object.  (Along with bindings
;; for BASE_ID and other parameters.)
(define animate-world!
  (lambda (world)
    (let ([world (cond
		  [(simworld? world) world]
		  [(string? world) (animate-world (car (file->slist world)))]
		  [(input-port? world) (animate-world (read world))]
		  [else (error 'animate-world "bad input: ~a\n" world)])])

      ;; TODO, FIXME: One day need to use a real graph library for this!
      ;; (One that treats edges with due respect, and that uses hash tables.)
      (define graph
	(or (simworld-graph world)
	    (error 'animate-world "need to at least have the node-graph to start with!\n")))

      (define scheduler-queue '())

      (DEBUGMODE  (andmap (lambda (row) (andmap node? row)) graph))
      ;; There had better be no duplicate identifiers.  We don't allow this for now.
      (DEBUGASSERT (set? (map node-id (map car graph))))

      (unless (simworld-object-graph world)
	(set-simworld-object-graph! world
	    (graph-map (lambda (n) (node->simobject n world)) graph))
	;; Thus need to rebuild the hash table:
	;(set-simworld-obj-hash! world #f)
	)

      ;; Regardless of whether its present, rebuild
      (set-simworld-all-objs! world (map car (simworld-object-graph world)))

      ;(unless (simworld-obj-hash world)
	(set-simworld-obj-hash! world 
	      (let ([h (make-default-hash-table)])
		(for-each (lambda (ob)
			    (hashtab-set! h (node-id (simobject-node ob)) ob))
		  (simworld-all-objs world))
		h))

      ;; Set I-am-SOC on every node:
      (for-each (lambda (ob)
		  (set-simobject-I-am-SOC! ob		  
					   (= (node-id (simobject-node ob)) BASE_ID)))
	(simworld-all-objs world))
	
      (set-simworld-vtime! world 0)
      
      (unless (simworld-led-toggle-states world)
	(set-simworld-led-toggle-states! world (make-default-hash-table)))
      
      (unless (and (simworld-connectivity-function world)
		   (procedure? (simworld-connectivity-function world)))
	(set-simworld-connectivity-function! world (build-connectivity-fun)))

	;; Make sure the world's sane before we release it!
      (DEBUGMODE (invcheck-simworld world))

      (void))))
