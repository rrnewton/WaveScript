;;;; .title SimAlpha UI -- Printing and displaying the state of the simulator.

;;;; [2006.03.01] <br>
;;;; I have factored this file out from the complex simulator_alpha.ss
;;;; (Which has spun off many-a-file at this point.)


; =================================================================================

;;; Graphics related.

;; This takes a simworld object and draws it on the screen.  It uses
;; (or should) the graphics interface defined in graphics_stub.ss
(define (simalpha-draw-world world)
  (IF_GRAPHICS
   (let ()
;     (define edge-table (make-default-hash-table))
;     (define proc-table (make-default-hash-table))
     
     ;; This is a promise so as to be called only once.
;     (define wipe-screen (delay clear-buffer))
     ;; Contains a graphics object, and the last drawn state.

     (if (not the-win) (init-graphics))
     (clear-buffer)

     ;; This temporarily stores all edges:
     (let ((edge-table (make-default-hash-table)))
	 (for-each (lambda (graph-entry)
		     (hashtab-set! 
		      edge-table (node-id (car graph-entry))
		      (let ([here (node-pos (car graph-entry))]
			    [id (node-id (car graph-entry))])
			;; This forms a list of line objects:
			(map (lambda (nbr)
			       (let ([line 
				      ; If there's already an entry in the other direction, use it.
				      (let ((entry (hashtab-get edge-table (node-id nbr))))
					(if (and entry (assq id entry))
					    (begin 
					      ;(inspect (list id (node-id nbr)))
					      (cadr (assq id entry)))
					    (draw-edge here (node-pos nbr))))])
				 (list (node-id nbr) line)))
			  (cdr graph-entry)))))
	   (simworld-graph world))

       ;; Make sure the edge table is the correct type:
       ;; It binds ids to nbr-alists.
       (DEBUGMODE 
	(hashtab-for-each
	 (lambda (k alst)
	   (DEBUGASSERT
	    (and (integer? k)
		 (andmap (lambda (pr)
			   (and (list? pr) (= (length pr) 2)
				(integer? (car pr))
				(gobj? (cadr pr))))
			 alst))))
	 edge-table))

     ;; This is not a good abstraction boundary.
     ;; We just drew the edges, and now we call "draw network" just to draw nodes:
     ;; Associate with each simobject the resultant graphics object "gobj".
     (let* ((all-objs (simworld-all-objs world))
	    (nodes (map simobject-node all-objs)))
       (let ((gvecs (draw-network (map node-pos nodes)
				  (map node-id nodes))))
	 (for-each (lambda (simob vec)
		     (match vec
		       [#(,circ ,r ,g ,b ,text ,label)
			;; Construct a graphics object:
			;(set-title! label "TEST")
			(set-simobject-gobj! simob
			 (make-gobject circ r g b text label 
				       (hashtab-get edge-table 
						    (node-id (simobject-node simob)))))]))
	   all-objs gvecs)
	 ))))
  (error 'simalpha-draw-world "graphics not loaded.")))


; ================================================================================

;;; Printing simulator state.

(define (simalpha-total-messages . sim)
  (let ((sim (if (null? sim) (simalpha-current-simworld) (car sim))))
    (apply + (map simobject-local-sent-messages (simworld-all-objs sim)))))

;; This prints all the simalpha counters: how many tokens fired, messages broadcast, etc.
(define print-stats 
  (case-lambda 
    [() (print-stats "")]
    [(prefix) (print-stats prefix (current-output-port))]
    [(prefix port) (print-stats prefix port (simalpha-current-simworld))]
    [(prefix port sim)    
     (if (not sim)
	 (printf "\nCouldn't print statistics, no value for simalpha-current-simworld!\n")
	 (let* ([measured (map car (graph-label-dists BASE_ID (graph-map node-id (simworld-graph sim))))]
		[sorted (sort (lambda (x y) 
				(cond 
				 [(and (cdr x) (cdr y)) (< (cdr x) (cdr y))]
				 [(cdr x) #t] [else #f]))
			      measured)]
		[obs (map (lambda (pr) (hashtab-get (simworld-obj-hash sim) (car pr))) sorted)]
		
		[total-messages (simalpha-total-messages sim)
					;(apply + (map simobject-local-sent-messages (simworld-all-objs sim)))
				]
		)	
	(printf "\nStatistics for most recent run of SimAlpha.\n")
	(printf "  Values returned         : ~s\n" (length (soc-return-buffer)))

	(let ([isum 0])
	  (for-each (lambda (ob)
		      (hashtab-for-each
		       (lambda (k vec) (set! isum (+ isum (vector-ref vec 0))))
		       (simobject-token-table ob)))
	    obs)
	  (printf "  Total tokens fired      : ~a (per-node ~a)\n" 
		  (pad-width 5 isum)
		  (round-to 2 (exact->inexact (/ isum (sim-num-nodes))))))
	
	(printf "  Total messages broadcast: ~a (per-node ~a)\n" 
		(pad-width 5 total-messages)
		(round-to 2 (exact->inexact (/ total-messages (sim-num-nodes)))))
	(printf "  Max sent / Max received : ~a / ~a\n"
		(foldl max 0 (map simobject-local-sent-messages obs))
		(foldl max 0 (map simobject-local-recv-messages obs)))
	(printf "  Msg distribution (sorted based on hops from base):\n")
	
	(printf "sent: ~a\n" 
		(map (lambda (x) (pad-width 3 (simobject-local-sent-messages x))) obs))
	(printf "rcvd: ~a\n" 
		(map (lambda (x) (pad-width 3 (simobject-local-recv-messages x))) obs))
	(printf "dsts: ~a\n" 
		(map (lambda (x) (pad-width 3 (cdr x))) sorted))
	(printf " ids: ~a\n" (map (lambda (x) (pad-width 3 (node-id (simobject-node x)))) obs))
	))]))


;; [2005.11.28] <br>
;; This is mainly for use with GUI, click on a node to get the node statistics.
(define (print-node-stats id . sim)
  (let ((sim (if (null? sim) (simalpha-current-simworld) (car sim))))
    (if (not sim)
	(error 'print-node-stats "got uninitialized simulator."))
    (let ((ob (hashtab-get (simworld-obj-hash sim) id)))
      (if (not ob)
	  (error 'print-node-stats "could not find node ~a in simworld's object hash." id))
      (printf "\nStatistics for node ~a, pos:~a\n" id (node-pos (simobject-node ob)))
;      (printf "  Total sent messages: ~a\n" (simobject-local-sent-messages ob))
;      (printf "  Total rcvd messages: ~a\n" (simobject-local-recv-messages ob))
      (printf "  Token breakdown:                 invoked  sent  received\n")

	(hashtab-for-each 
	 (lambda (k vec)
	   (printf "    ~a ~a ~a ~a\n"
		   (pad-width 30 k)
		   (pad-width 8 (vector-ref vec 0))
		   (pad-width 5 (vector-ref vec 1))
		                (vector-ref vec 2)))
	 (simobject-token-table ob))

      (let ([isum 0] [ssum 0] [rsum 0])
	(hashtab-for-each
	 (lambda (k vec)
	   (set! isum (+ isum (vector-ref vec 0)))
	   (set! ssum (+ ssum (vector-ref vec 1)))
	   (set! rsum (+ rsum (vector-ref vec 2))))
	 (simobject-token-table ob))
	(printf "  Total:                           ~a ~a ~a\n"
		(pad-width 8 isum)
		(pad-width 5 ssum)
		             rsum))

      (printf "  Token Store: \n")
      (parameterize ([print-gensym #f])
	(hashtab-for-each
	 (lambda (k pr)	   
	   (let ((tk (key->token k)))
	     (display-constrained "    " (pad-width 37 (cons (simtok-name tk) (simtok-subid tk)))
				  " | "  (list (cdar pr) 80)  "\n")))
	 (simobject-token-store ob)))
      )))

;; Helper function to print out a table listing which nodes are
;; connected to eachother (any two without provably zero connectivity).
(define (print-connectivity . sim)
  (define connects '()) ;; Just accumulates all the numeric connectivities.
  (let ((world (if (null? sim) (simalpha-current-simworld) (car sim))))    
  (printf "\nCurrent network connectivity at vtime ~a.\n" (simworld-vtime world))
  (parameterize ([print-gensym #f])
    (let ((edges 0))
      (for-each (lambda (row)
					;	      (printf "Bang : ~s \n" (map node-id row))
		  (printf "  ~s: ~s\n" (car row)
			  (map (lambda (nbr) 			     
					;			     (printf "Woot ~s ~s ~s ~s \n" 
					;				     (node? (car row)) (node? nbr)
					;				     (car row) nbr ;(node-pos (car row)) (node-pos nbr)
					;				     )
				 (let ((connectivity ((simworld-connectivity-function world)
						      (node-pos (car row))
						      (node-pos nbr))))
				   (if (number? connectivity) (set! connects (cons connectivity connects)))
				   connectivity))
			    (cdr row)))
		  (set! edges (+ edges (length (cdr row)))))
	(simworld-graph world))
      (printf "Total edges: ~a\n" (/ edges 2))
      (printf "Average degree: ~a\n" (exact->inexact (/ edges (length (simworld-graph world)))))
      (let ((avg (exact->inexact (average connects))))
	(printf "Average connectivity: ~a\n" avg)
	avg)
      ))))

;; [2006.02.01] This is a simple routine that displays the connectivity distribution.
(define (plot-connectivity . sim)
  (define world (if (null? sim) (simalpha-current-simworld) (car sim)))
  (define connects '()) ;; Just accumulates all the numeric connectivities.
  (for-each (lambda (row)
	      (map (lambda (nbr) 			     
		     (let ((connectivity ((simworld-connectivity-function world)
					  (node-pos (car row))
					  (node-pos nbr))))
		       (if (number? connectivity) (set! connects (cons connectivity connects)))
		       connectivity))
		(cdr row)))
    (simworld-graph world))
  (let* ([binsize 5]
	 [hist (histogram connects 5)]
	 [start (apply min connects)])
    (gnuplot
     (map list 
       (map (lambda (n) (+ start (* binsize n)))
	 (iota (length hist)))
       hist)
     'boxes
     )))

; =================================================================================
