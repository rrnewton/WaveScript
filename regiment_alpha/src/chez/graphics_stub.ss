;; chez/graphics_stub.ss
;; Implements the GRAPHICS_STUB interface, described in "generic/graphic_stub.ss".
;; Should provide the same functionality as plt/graphics_stub.ss

;; I've moved this over to a "persistent object" kind of drawing interface.
;; The "simobject" structure holds a "gobj", which is an
;; implementation specific thingy that supports "get-state" for 
;; retrieving properties and "change-color!".
;;  The simobject also has a redraw flag that can be set to refresh
;;  the processor during the periodic redraw.


;; THERE IS NO ABSTRACTION BOUNDARY BETWEEN THIS AND BASIC_GRAPHICS.SS
;; FIXME TODO: MERGE WITH BASIC_GRAPHICS.SS


;; TODO FIXME: REMOVE eval's FROM THIS AND BASIC_GRAPHICS.ss
;; (HAVING PROBLEMS WITH RECORD-TYPES CURRENTLY. evals ARE A HACK)

;; TODO IMPLEMENT GET-STATE


; ======================================================================
;; [2004.06.05] Adding "get-state" to this interface so that the
;; simulator can ask the graphics object, say, what color it is.

;; [2004.05.23]

;; This will depend on the "basic_graphics" abstraction, but will not
;; respect the abstraction.  It's kinda silly, right now I have two
;; layers of astraction that I prolly don't need.  Both the
;; "basic_graphics" for basic drawing primitives, and "graphics_stub"
;; for high level routines used by my system.  SO, if this continues
;; to be unneccessary I will prolly move this file to the src/generic/
;; subdirectory and just use the basic_graphics interface.

;; REQUIRES: on world-xbound and world-ybound from simulator_nought.ss, 
;; it uses these to scale the world coordinates to the display.


;; NOTE: Right now the "redraw" flag isn't used yet... [2005.02.25]
;; Change-color redraws immediately, and it's the only way to state-change...


;(load "basic_graphics.ss")

(module graphics_stub (draw-network draw-proc draw-edge draw-mark draw-circle
				  clear-buffer delete-gobj
;				  Starting-Node-Color
				  change-color! ;set-color!
				  ;get-state ;; [2004.11.13] including
				  change-text!
				  highlight-edge
				  highlight-parent-routes
				  highlight-all-neighbors
				  highlight-child-gradients
				  unhighlight-edge

				  these-tests test-this 
				  test-graphics-stub)
  (import basic_graphics)


;; GLOBAL BINDINGS:

;; These three keep track of the SWL widgets.  Could use a hash.
;; Basically we use these to clear the screen by destroying everything mentioned in these lists.
(define processor-screen-objs '())
(define edge-screen-objs '())
(define other-objs '())

;===============================================================================
;; Utils:

;; Returns a fixnum or flonum, 
;; Maps a coordinate in one box to the anologous coordinate in another.
(define scale2d 
  (let ((prep (lambda (x)
		(if (not (integer? x))
		    (exact->inexact x)
		    x))))
    (lambda (pos box1 box2)
					;  (disp "SCALING" (list pos box1 box2))
      (match (list pos box1 box2)
	     [((,x ,y) (,a1 ,b1 ,a2 ,b2) (,c1 ,d1 ,c2 ,d2))
	      (values 
	       (prep (+ (* (/ (- x a1) (- a2 a1)) (- c2 c1)) c1))
	       (prep (+ (* (/ (- y b1) (- b2 b1)) (- d2 d1)) d1)))]
	     [,otherwise (error 'scale2d "bad arguments: ~s ~s ~s"
				pos box1 box2)]))))


;; This makes use of the global constants for world and screen bounds,
;; and uses them to convert a coordinate.
(define (coord:sim->screen pr)
  (scale2d pr (list 0 0 world-xbound world-ybound)
  	      (list 0 0 window-width window-height)))

;; [2004.06.22]
;; The values returned by the draw-* procedures in this file are my
;; "graphics-objects" or gobjs used by the simulator.  They can be
;; uniformly destroyed with this procedure:
(define (delete-gobj g)
  (if (list? g) (map destroy g) (destroy g)))

;; Deletes all the existing SWL widgets on the canvas..
(define (clear-buffer)
  (for-each destroy other-objs)
  (for-each destroy processor-screen-objs)
  (for-each destroy edge-screen-objs) 
  (set! other-objs '())
  (set! processor-screen-objs '())
  (set! edge-screen-objs '())
  ;; If the simulation has pointers over here, get rid of em.
  (and (top-level-bound? 'all-objs)
       all-objs
       (for-each (lambda (simob)
		   (set-simobject-gobj! simob #f))
		 all-objs)))


;================================================================================

;; We don't keep track of which objects need to be redrawn, that's all
;; handled by tcl/tk.
(define (change-color! ob c)
  (set-fill-color! ob
		   (make <rgb>
		     (rgb-red c)
		     (rgb-green c)
		     (rgb-blue c))))

(define change-text! set-title!)

(define (get-state sym ob)
  (case sym
    [(color) (get-fill-color ob)]
    [(loc) (get-coords ob)] ;; WHAT TYPE DOES THIS RETURN??
    [else (error 'chez/graphics_stub.ss "unknown state property: ~s" sym)]))

;(define links '())

;; Highlights a line gobj, optionally unhighlighting after a period of time.
(define highlight-edge   
  (case-lambda 
    [(line) (highlight-edge line Default-Line-Highlight-Color)]   
    [(line color) (send line highlight-edge color)]
    [(line color time) (send line highlight-edge color time)]))
#;    [(line color)
     (change-color! line color)
     (set-line-thickness! line 2)]
#;    [(line color time)
     (let ((oldcolor (get-fill-color line))
	   (oldthickness (get-line-thickness line))
	   (col (rec->rgb color)))
       ;(when (not (number? oldthickness)) (printf "Bad oldthickness! \n")(inspect oldthickness))
       (when (and oldcolor oldthickness)
	 (set-fill-color! line col)
	 (set-line-thickness! line 2)
	 (thread-fork 
	  (lambda ()	  
	    (thread-sleep time)
	    ;(when (and (eq? oldcolor (get-fill-color line))
	    (set-fill-color! line oldcolor) 
	    (set-line-thickness! line (inexact->exact oldthickness))
	    (thread-sleep 500)
	    (show line)
	    ))))]

;; Turns the line gobj back to default appearance.
(define (unhighlight-edge line)
  ; ;(change-color! line Default-Edge-Full-Color)
  ; ;(set-line-thickness! line 1)
  ;(send line restore-defaults)
  (send line unhighlight-edge)
  )


; ----------------------------------------------------------
; These two are helper functions used by the bindings below:
; The helper "retrieve-grad-args" is in desugar-gradients to localize
; things dealing with the add-to-end/add-to-beginning tradeoff.

(define (get-grads ob)
  (let ((grads '()))
    (hashtab-for-each 
     (lambda (k pr)
       (let ([tok (caar pr)] [rec (cdar pr)])
	 (let* ([lst (reg:struct->list rec)]
		[len (length lst)])
	   (if (>= len 4)
	       (mvlet ([(parent origin hops version) (retrieve-grad-args lst)])
		 (let ([subid (simtok-subid tok)])				
		 (when (and (or (symbol? parent) (nodeid? parent)) ;; Could be 'atroot
			    (nodeid? origin)
			    (number? hops)
			    (integer? version))		     
		   (set! grads (cons (vector (simtok-name tok) (simtok-subid tok)
					     parent origin hops version) grads)))))))))
     (simobject-token-store ob))
    grads))
(define trace-grad-up 
  (match-lambda (,id #(,tokname ,tokid ,parent ,origin ,hops ,version))
    (let ([hash (simworld-obj-hash (simalpha-current-simworld))])
    (let loop ([id id] [visited '()])
      (cond 
       [(eqv? id origin)
	(printf "  Reached origin at ~a for grad on tok <~a:~a>\n"
		origin tokname tokid)]
       [(memq id visited)
	(warning 'highlight-parent-routes
		 "Found loop in parent pointers! Hit this node twice: ~a, all visited were: ~a"
		 id visited)]
       [else
	(printf "Tracing gradient through ~a. on tok <~a:~a>, with parent:~a orig:~a hops:~a ver:~a\n" 
		id tokname tokid parent origin hops version)
					;		(sleep 1000)
	(let ((ob (hashtab-get hash id)))
					;		     (if (not (simobject? ob))
					;			 (error 'highlight-parent-routes "bad simobjet: \n~a" ob))
	  (let ((grads (filter (match-lambda (#(,tn ,tid ,p ,o ,h ,v)) 
				 (and (eq? tn tokname)
				      (eqv? tid tokid)
				      (eqv? o origin)))
			 (get-grads ob))))
	    (cond
	     [(null? grads) (void)]
	     [(null? (cdr grads))
	      (let ((parent (vector-ref (car grads) 2)))
		(when (nodeid? parent)
		  ;;(printf "Highlighting Suspected gradient from node ~a: tokname,tokid,orig,prnt,hops,ver:~s\n" 
		  ;;	  id (car grads))
		  (eval `(parameterize ([current-simobject ,ob])
			   (sim-highlight-edge ',parent (make-rgb 0 255 0) 2000)))
		  (loop parent (cons id visited))))]
	     [else (warning 'highlight-parent-routes 
			    "Found multiple token entries for one gradient! ~s" grads)])))])))))
(define trace-grad-down
  (match-lambda (,id ,color #(,tokname ,tokid ,parent ,origin ,hops ,version))
    (let* ([sim (simalpha-current-simworld)]
	   [hash (simworld-obj-hash sim)])
    (define (neighbors id)
      (cdr (assq (hashtab-get hash id)
		 (simworld-object-graph sim))))
    (let loop ((objs (neighbors id)) (visited (list id)))
      (for-each (lambda (ob)
		  (unless (memq ob visited)
		    (for-each
			(match-lambda (#(,tn ,tid ,p ,o ,h ,v)) 
			  (when (and (eq? tn tokname)
				     (eqv? tid tokid)
				     (eqv? o origin)
				     (nodeid? p))
			    (eval `(parameterize ([current-simobject ,ob])
				     (sim-highlight-edge ',p ,color 2000)))))
		      (get-grads ob))))
	objs)
      (loop (apply append (map neighbors (map node-id (map simobject-node objs))))
	    (append objs visited))))))
; ----------------------------------------------------------

;; This takes a node-id and momentarily highlights all the
;; "parent-routes" from the gradients in which this node participates.
;; Currently it just uses a cheesy, unsound heuristic to guess which
;; tokens in the memory are gradients.
(define highlight-parent-routes 
  (lambda (id)
    (let ([hash (simworld-obj-hash (simalpha-current-simworld))])
      ;; Now trace all gradients from the current node:
      (let ((these-grads (get-grads (hashtab-get hash id))))
	(for-each (lambda (x) (trace-grad-up id x)) these-grads)))))

;; This one highlights the full tree for all the gradients rooted at the current node.
;; It uses different colors for each:
(define highlight-child-gradients 
  (lambda (my-id)
    (let ([hash (simworld-obj-hash (simalpha-current-simworld))])
      ;; Now trace all gradients from the current node:
      (let* ([these-grads (get-grads (hashtab-get hash my-id))]
	     [my-grads (filter (match-lambda (#(,tn ,tid ,p ,orig ,h ,v))
				 (eq? orig my-id))
			 these-grads)])
	(for-each (lambda (x c) (trace-grad-down my-id c x))
	  my-grads
	  (make-repeats (list (make-rgb 0 0 255)
			      (make-rgb 0 255 0)
			      (make-rgb 255 0 0)
			      (make-rgb 255 255 0)
			      (make-rgb 0 255 255)
			      (make-rgb 255 0 255))
			(length my-grads)))))))

;; This one simply highlights all the edges connected to a node.
(define highlight-all-neighbors
  (lambda (id)
    (let* ([sim (simalpha-current-simworld)]
	   [ob (hashtab-get (simworld-obj-hash sim) id)]
	   [entry (assq ob (simworld-object-graph sim))]
	   [nbrids (map node-id (map simobject-node (cdr entry)))])
      (disp "ME" (node-id (simobject-node ob)) nbrids)
      (eval `(parameterize ([current-simobject ,ob])
	       (for-each (lambda (id)
			   (sim-highlight-edge id (make-rgb 0 255 255) 1500))
		 ',nbrids)))
      )))



;; This function bears an onus to destroy old screen objects and post up new ones.
;; It might be called whenever the processor set changes.
;; This one DOES SHOW the processors.
;; Input: list of pairs (coordinates).
(define (draw-network procs ids)
  (DEBUGMODE ;; Invariant checking:
   (if (not the-win) (error 'draw-network "graphics window is not initialized"))
   (for-each (lambda (proc)
		(if (not (and (list? proc)
			      (= (length proc) 2)
			      (number? (car proc))
			      (number? (cadr proc))))
		    (error 'draw-network
			   "Invalid processor coordinates: ~s among processors ~n~s~n" 
			   proc procs)))
	     procs))
  ;; This is not an efficient use of the SWL library:
  ;; We don't clear the entire buffer, we're just drawing new procs and edges 
  ;; and we leave whatever else was drawn on the screen.
  (begin 
    (for-each destroy processor-screen-objs)
    ;(for-each destroy edge-screen-objs)
    ;(set! edge-screen-objs '())
    (set! processor-screen-objs '()))

  ;; Draw nodes:
  (let ((results (map draw-proc procs ids)))
    (for-each show processor-screen-objs) 
    results))


;; This *does not show* the screen object (gobj) that it creates.
;; Screen objects are SWL objects.  Their represention is opaque to us.
;; [2005.11.07] Now this returns a vector with four graphics: circle + three led boxes
(define draw-proc 
  (let ()
    (define (thread-eval exp)
      (thread-fork (lambda () (eval exp) (thread-kill))))
    (define-class (<proc-oval> prnt x1 y1 x2 y2) (<oval> prnt x1 y1 x2 y2)
      (ivars (nodeid #f))
      (inherited) (inheritable)
      (private) (protected)
      (public
       [set-node-id! (nid)
		     (set-fill-color! self (rec->rgb Default-Node-Color))
		     (set! nodeid nid)
		     (send self set-border)]

       [set-border ()
		   (if (eqv? nodeid BASE_ID)
		       (set-outline-color! self (rec->rgb Default-Base-Border-Color))
		       (set-outline-color! self (rec->rgb Default-Proc-Border-Color)))]

;       [init (prnt x1 y1 x2 y2)
;	     (send-base self prnt x1 y1 x2 y2)]

       [mouse-enter (x y mods)
		    (set-line-thickness! self 2)
		    (set-outline-color! self (rec->rgb Default-Mouse-Highlight-Color))]
       [mouse-leave (x y mods)
		    (set-line-thickness! self 1)
		    ;(set-outline-color! self (rec->rgb Default-Proc-Border-Color))
		    (send self set-border)]
       [mouse-release (x y mods)
		      (event-case ((modifier= mods))
			[([left-button]) (eval `(print-node-stats ,nodeid))
			                 (thread-eval `(highlight-all-neighbors ,nodeid))]
			[([right-button]) (thread-eval `(highlight-parent-routes ,nodeid))]
			[([middle-button]) (thread-eval `(highlight-child-gradients ,nodeid))]
			)]
       ))
    (define-class (<child-rect> prnt x1 y1 x2 y2) (<rectangle> (get-parent prnt) x1 y1 x2 y2)
      (ivars (prnt prnt))
      (inherited) (inheritable) (private) (protected)
      (public ; This is transparent, just acts as part of the parent:
       [mouse-enter   (x y mods) (send prnt mouse-enter x y mods)]
       [mouse-leave   (x y mods) (send prnt mouse-leave x y mods)]
       [mouse-press   (x y mods) (send prnt mouse-press x y mods)]
       [mouse-release (x y mods) (send prnt mouse-release x y mods)]
       ))

    (lambda (pr nodeid)
      (mvlet ([(x y) (coord:sim->screen pr)])
	;(set-procesor-screen-radius!) ;; This needn't happen so often. TODO FIXME
	(let* ([radius (processor-screen-radius)]
	       [boundx1 (- x radius)]
	       [boundy1 (- y radius)]
	       [boundx2 (+ x radius)]
	       [boundy2 (+ y radius)]
	       [third (/ (* 2 radius) 3)])
	  
	  (DEBUGASSERT (flonum? radius))
	  (DEBUGASSERT (flonum? boundx1))
	  (DEBUGASSERT (flonum? boundy1))
	  (DEBUGASSERT (flonum? boundx2))
	  (DEBUGASSERT (flonum? boundy2))

	  (let ((circ (create <proc-oval> the-win
;			     50 50 
;			     100 100)))
			      boundx1 boundy1 boundx2 boundy2 
			      with (node-id: nodeid))))
;			     (flonum->fixnum (- x radius))
;			     (flonum->fixnum (- y radius))
;			     (flonum->fixnum (+ x radius))
;			     (flonum->fixnum (+ y radius)))))	  

	   (let ([box1 (create <child-rect> circ
			       boundx1                 (+ boundy1 (* .8 third))
			       (+ boundx1 third)       (- boundy2 (* .8 third)))]
		 [box2 (create <child-rect> circ 
			       (+ boundx1 third)       (+ boundy1 (* .8 third))
			       (+ boundx1 (* 2 third)) (- boundy2 (* .8 third)))]
		 [box3 (create <child-rect> circ
                               (+ boundx1 (* 2 third)) (+ boundy1 (* .8 third))
			       (+ boundx1 (* 3 third)) (- boundy2 (* .8 third)))]
		 [defled (rec->rgb Default-LED-Off-Color)]
		 [text (create <canvas-text> the-win (+ boundx1 (* 0.1 radius))
			                             (- boundy1 (* 0.3 radius))
			       with (title: (format "~s" nodeid ))
			       (fill-color: (rec->rgb Default-Supertext-Color)))]
		 [text2 (create <canvas-text> the-win (+ boundx1 (* 0.1 radius))
			                             (+ boundy2 (* 0.35 radius))
						     with (title: "") ;(format "~s" nodeid ))
						     (fill-color: (rec->rgb Default-Subtext-Color)))]
		 )
	     (show text)
	     (show text2)

	     ;(set-foreground-color! text2 (make <rgb> 255 0 0))
	     ;(set-color! text2 (make <rgb> 255 0 0))

	     (set-fill-color! box1 defled)
	     (set-fill-color! box2 defled)
	     (set-fill-color! box3 defled)
	   
;	   (show circ)
	   (set! processor-screen-objs
		 (append (list circ box1 box2 box3 text text2)
			 processor-screen-objs))
	   (vector circ box1 box2 box3 text text2))))))))

;; This returns nothing.
(define (draw-mark pr . color)
  (set! color (if (null? color) Default-Mark-Color (car color)))
  (mvlet ([(x y) (coord:sim->screen pr)])
    (let ((len 10)) ;; shouldn't be constant.
      (let ([l1 (create <line> the-win (- x len) (- y len) (+ x len) (+ y len))]
	    [l2 (create <line> the-win (- x len) (+ y len) (+ x len) (- y len))]
	    [c  (make <rgb> (rgb-red color) (rgb-green color) (rgb-blue color))])
	(set-fill-color! l1 c)
	(set-fill-color! l2 c)
	(set! other-objs (cons l1 (cons l2 other-objs)))
	(list l1 l2)))))

;; This draws an empty circle:
(define (draw-circle pr rad)
  (mvlet ([(x y) (coord:sim->screen pr)]
	  [(radx rady) (coord:sim->screen (list rad rad))])
	 (let ([obj (create <oval> the-win
			    (- x radx) (- y rady)
			    (+ x radx) (+ y rady))])
	   (set! other-objs (cons obj other-objs))
	   obj)))

;; This draws a new edge and adds that graphics object to the global list, 
;; as well as returning it.
(define (draw-edge pos1 pos2)
  (define (interpolate per c1 c2)
    (define (->fixnum n) (inexact->exact (floor n)))
    (let ((frac (/ per 100)))
      (make <rgb> 
	(->fixnum (+ (rgb-red c1) (* frac (- (rgb-red c2) (rgb-red c1)))))
	(->fixnum (+ (rgb-green c1) (* frac (- (rgb-green c2) (rgb-green c1)))))
	(->fixnum (+ (rgb-blue c1) (* frac (- (rgb-blue c2) (rgb-blue c1))))))))

  (let ([box1 (list 0 0 world-xbound world-ybound)]
	[box2 (list 0 0 window-width window-height)]
	[connectivity ((simalpha-connectivity-function) pos1 pos2)])

    (define-class (<my-line> prnt x1 y1 x2 y2) (<line> prnt x1 y1 x2 y2)
      (ivars [orig-color 'uninit-color]
	     [orig-thickness 'unit-thickness]
	     [currently-flashing #f]
	     [highlight-stack '()]
	     )
      (inherited) (inheritable)
      (private) (protected)
      (public
       [mouse-enter  (x y mods) (send self trigger-higlight)]
       [mouse-leave  (x y mods) (send self trigger-higlight)]
       [mouse-motion (x y mods) (send self trigger-higlight)]

       [restore-defaults ()
			 (set-fill-color! self orig-color)
			 (set-line-thickness! self 1)
			 (void)]

       ;; Destroy the highlight-stack and permanently set a color:
       [highlight-edge (color)
		       (set! highlight-stack ())
		       (change-color! self color)
		       (set-line-thickness! self 2)]
       [highlight-edge (color time)
		       ;; (printf "Highlight stack: len ~a: ~a ~a\n" (length highlight-stack) (real-time) (map car highlight-stack))
		       ;; Add a frame to the highlight stack:
		       (critical-section
			(set! highlight-stack
			      (cons (list (+ (real-time) time)
					  (get-fill-color self)
					  (get-line-thickness self))
				    highlight-stack )))			            
		       (set-fill-color! self (rec->rgb color))
		       (set-line-thickness! self 2)
		       (thread-fork 
			(lambda ()	  
			  (thread-sleep time)
			  ;; Unwind the highlight-stack:
			  (send self unhighlight-edge)))]

       ;; Pops off the stack.
       [unhighlight-edge ()
			 (critical-section
			   (let ((t (real-time)))
			     ;(printf "Unwinding stack of len: ~a ~a ~a\n" (length highlight-stack) t (map car highlight-stack))
			     (let ((next (let loop ((ls highlight-stack) (mostrecent #f))
					   (cond
					    [(null? ls) (set! highlight-stack ()) mostrecent]
					    ; If this next frame has already expired we pop it and keep going.
					    [(< (caar ls) t)  (loop (cdr ls) (car ls))]
					    ; If we reach a frame in the future we stop.
					    [else (set! highlight-stack ls) mostrecent]))))
			       ;(printf "Unwound to  len: ~a: ~a\n" (length highlight-stack) (map car highlight-stack))
			       (if next
				   (begin 
				     (when (not (number? (caddr next)) )
				       (printf "Bad oldthickness! \n");(inspect oldthickness))
				       )
				     (set-fill-color! self (cadr next))
				     (set-line-thickness! self (inexact->exact (caddr next))))
				   ; Fizzle, we're already gone:
				   (void)
				   ;(send self restore-defaults)
				   ))))]
	
       [init (prnt x1 y1 x2 y2)
	     (send-base self init prnt x1 y1 x2 y2)
	     (set! orig-color (interpolate connectivity
					   Default-Edge-Dead-Color
					   Default-Edge-Full-Color))
	     (set! orig-thickness
		   (if (and (not (eq? (simalpha-channel-model) 'lossless))
			    (number? connectivity) (> connectivity 50))
		       2 1))
	     (set-fill-color! self orig-color)
	     (set-line-thickness! self orig-thickness)
	     ]

       [trigger-higlight ()
		(when (or (not currently-flashing)
			  (< 1000 (- (real-time) currently-flashing)))
		  (set! currently-flashing (real-time))
		  (highlight-edge self Default-Mouse-Highlight-Color 300)
		  ;(printf "Edge ~a->~a Connectivity: ~a\n" pos1 pos2 connectivity)
		  (flash-text (format "Edge ~a->~a len: ~a Connectivity: ~a" 
				      pos1 pos2 (round-to 1 (posdist pos1 pos2)) connectivity) 1500))
		]
       ))

    (DEBUGMODE
     (if (not (and (list? pos1) (= 2 (length pos1))
		   (list? pos2) (= 2 (length pos2))))
	 (error 'chez/graphics_stub.draw-edge
		"Invalid input positions: ~s ~s" pos1 pos2)))

    (mvlet ([(x1 y1) (scale2d pos1 box1 box2)]
	    [(x2 y2) (scale2d pos2 box1 box2)])
	   (let ((line (create <my-line> the-win x1 y1 x2 y2)))
	     (set! edge-screen-objs (cons line edge-screen-objs))
	     (show line)
	     line))))


;================================================================================

;; Get the tests for this unit:
(include "generic/graphics_stub.tests")

(define test-this (default-unit-tester "GraphicsStub: common graphics interface for PLT and chez" 
		    these-tests))
(define test-graphics-stub test-this)

) ;; End module.
