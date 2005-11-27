;; [2004.05.24]
;; This implements my simple "basic graphics" interface, which I will
;; document somewhere as soon as it's done.  Erg, you'd think this
;; would be part of slib?

;; THERE IS NO ABSTRACTION BOUNDARY BETWEEN THIS AND GRAHPICS_STUB.SS

(module basic_graphics (window-height window-width  
			init-graphics close-graphics  
			rec->rgb ;; This converts rgb records into system specific (SWL) rgb values.
;			make-rgb rgb? rgb-red rgb-green rgb-blue
			the-win 
			;; current-drawing-color 
			;; current-filling-color
			;; current-background-color
			;; draw-ellipse ....
			)


;; This defines window-width and window-height presets:
;(include "../generic/basic_graphics.ss")
;; Sadly scheme's path system makes no sense, so this loads from the 
;; "Regionstreams/compiler/src" directory where compiler_chez.ss is:
(include "generic/basic_graphics.ss")

(define the-winframe #f)

;; [2005.11.25] The button panel:
(define the-panel  #f)
(define the-panel2 #f)
(define the-viewer-thread #f)

;; [private] A single line of text used for messages to the user.
(define the-text-readout #f)

(define current-drawing-color (rec->rgb Default-Drawing-Color))
(define current-filling-color #f)
(define current-background-color (rec->rgb Default-Background-Color))

;; Here's where we keep objects that are to be drawn:
(define object-buffer '())
;; And here's where we keep the objects that are on the screen:
(define screen-buffer '())

; ======================================================================

#;
(define (flash ob ms)
  (thread-fork
   (lambda ()
     (show ob)
     (thread-sleep ms)
     (destroy ob))))

(define (flash-text txt ms)
  (if (not the-text-readout)
      (error 'flash-text "the-text-readout is null, graphics must not be initialized."))
  (thread-fork
   (lambda ()
     (let ((snapshot (get-title the-text-readout)))
       (set-title! the-text-readout txt)
       (thread-sleep ms)
       ;; If we haven't been displaced, reset text:
       ;(if (equal? txt (get-title the-text-readout)) (set-title! the-text-readout snapshot))
       (set-title! the-text-readout "")
       (thread-kill)
       ))))

;; Init the graphics system, bring up a window.<br> 
;; [2005.11.25] Man I'm having some serious annoyance figuring out how to do proper layout with SWL.
;; <br>
;; <br> [2005.11.26] NOTE: Having problems with modules/imports/records here.  Using 'eval' as a hack: (FIXME)
(define (init-graphics)
  
  ;; This entry calls its action every time a key is typed:
  (define-class (<better-entry> parent) (<entry> parent)
    (ivars)
    (inherited) (inheritable)
    (private)   (protected)
    (public
      ;[mouse-leave (x y mods) ((get-action self) self)]
     [key-release (k mods)     ((get-action self) self)]
     ))

  ;; We set up a single additional thread for evaluation.  It receives
  ;; expressions via this queue.
;  (define eval-queue (thread-make-msg-queue 'graphics-eval-queue))
;  (define (thread-eval exp) (thread-send-msg eval-queue exp))
  (define sim-busy #f)
  (define (thread-eval exp)
    (thread-fork (lambda () (let ((snap sim-busy))
			      (set! sim-busy #t) 
			      (eval exp)
			      (set! sim-busy snap)
			      (flash-text "Simulation Finished." 900)))))

;  (printf "Running graphical interface to simple simulator.~n")
  (if the-win 
      (printf "Graphics already open!!~n")
      (let ([pause-queue (thread-make-msg-queue 'pause-queue)]
	    [group #f])

	(set! the-winframe (create <toplevel> with 
			      (title: "Region Streams Demo")))

	(set! group (create <frame> the-winframe))

	(set! the-panel (create <frame>  group ;the-winframe
				with
				;(width: (in->pixels 1)) ;(+ window-width 200))
				;(height: window-height)
				;(background-color: (make <rgb> 100 50 50))
				))
	(set! the-panel2 (create <frame>  the-winframe
				with
				;(width: (in->pixels 1)) ;(+ window-width 200))
				;(height: window-height)
				;(background-color: (make <rgb> 100 50 50))
				))
	(set! the-win (create <canvas> group ;the-winframe
				 with
				 (width: window-width) ;(in->pixels 5))
				 (height: window-height) ;(in->pixels 5))
				 (background-color: (make <rgb> 215 215 255))
				 ))

	(set! the-text-readout (create <canvas-text> the-win (- (/ window-width 2) 10) (- window-height 13)
					with (title: "")))

	;; We start up a single additional thread for evaluation:
#;
	(thread-fork (lambda ()
		       (let ((winsnap the-win))
			 (let graphics-eval-loop ()
			   ;; Run until the-win changes.
			   (when (eq? winsnap the-win)
			     (let ((exp (thread-receive-msg eval-queue)))
			       (unless (eq? exp 'kill)
				 (eval exp)
				 (graphics-eval-loop))))))))

	(let* (
	       [pause-button (create <button> the-panel with (title: "Pause")
				(action:
				   (lambda (self)
				     (if (simalpha-pause-hook)
					 (begin (set-title! self "Pause")
						(printf "Simulation Unpaused.\n")
						(flash-text "Simulation Unpaused." 600)
						(set! sim-busy #t)
						(simalpha-pause-hook #f)
						(thread-send-msg pause-queue 'continue)
						)
					 (begin (set-title! self "Play")
						(flash-text "Simulation Paused." 600)
						(set! sim-busy #f)
						;; This tells the simulator thread to pause and wait for a msg:
						(simalpha-pause-hook (lambda () 
								       (if (eq? 'kill (thread-receive-msg pause-queue))
									   (thread-kill))
								       ))
						))
				     )))]
	       ; Wow, I can't believe that there doesn't seem to be a way to get the current state of a checkbutton. [2005.11.26]
	       [realtime-state #f]
	       [realtime-button (create <checkbutton> the-panel2 with (title: "Realtime")
;				(draw-indicator: #f)
				(action:				
				   (lambda (_)
				     (set! realtime-state (not realtime-state))
				     (simalpha-realtime-mode (not (simalpha-realtime-mode))))))]
	       [msgcounts-state #f]
	       [msgcounts-button (create <checkbutton> the-panel2 with (title: "Show MsgCounts")
				(action: (lambda (self)
					   (set! msgcounts-state (not msgcounts-state))
					   (simalpha-label-msgcounts (not (simalpha-label-msgcounts)))
					   (unless msgcounts-state
					     (eval '(for-each (lambda (x) (sim-setlabel "" x))
						      (simworld-all-objs (simalpha-current-simworld)))))
					   )))]

	       ;; UNFINISHED:
	       [showedges-state #t]
	       [showedges-button (create <button> the-panel2 with (title: "Hide Edges")
					 (action: (lambda (self)
						    (set! showedges-state (not showedges-state))
						    (let ((edges 
							   (list->set 
							    (apply append 
								   (map (lambda (x) 
									  (map cadr (gobject-edgelist (simobject-gobj x))))
								     (simworld-all-objs (simalpha-current-simworld)))))))
						    (if showedges-state
							(for-each show edges)
							(for-each hide edges))))))]

	       [num-nodes-widget 
		(let ((f (create <frame> the-panel2)))
		  (let ((e (create <better-entry> f with (width/char: 4)			
				   (action: (lambda (self)
					      (flash-text (format "Setting sim-num-nodes: ~s\n" (get-string self)) 1000)
					      ;(printf "Setting sim-num-nodes: ~s\n" (get-string self))
					      (sim-num-nodes (string->number (get-string self))))))))
		    (insert e (number->string (sim-num-nodes)))
		    (pack e (side: 'left))
		    (pack (create <label> f with (title: "Nodes")) (side: 'right))
		    f))]
	       
	       [placement-widget
		(let* ((f (create <frame> the-panel2))
		       (b1 (create <radiobutton> f with (title: "Connected")
				(action: (lambda _ (simalpha-placement-type 'connected)))))
		       (b2 (create <radiobutton> f with (title: "Gridlike")
				(action: (lambda _ (simalpha-placement-type 'gridlike)))))
		       (b3 (create <radiobutton> f with (title: "Random")
				(action: (lambda _ (simalpha-placement-type 'random))))))
		  (pack b1 (anchor: 'w))
		  (pack b2 (anchor: 'w))
		  (pack b3 (anchor: 'w))
		  (case (simalpha-placement-type)
		    [(connected) (send b1 select)]
		    [(gridlike)  (send b2 select)]
		    [(random)    (send b3 select)])
		  f)]

	       [radio-widget
		(let* ((f (create <frame> the-panel2))
		       (b1 (create <radiobutton> f with (title: "Lossless")
				   (action: (lambda _ (simalpha-channel-model 'lossless)))))
		       (b2 (create <radiobutton> f with (title: "LinearDisc")
				   (action: (lambda _ (simalpha-channel-model 'linear-disc))))))
		  (pack b1 (anchor: 'w))
		  (pack b2 (anchor: 'w))
		  (case (simalpha-channel-model)
		    [(lossless)    (send b1 select)]
		    [(linear-disc) (send b2 select)])
		  
		  (let* ((f2 (create <frame> f))
			 (inner (create <better-entry> f2 with (width/char: 3)
					(action: (lambda (self) (simalpha-inner-radius (string->number (get-string self))))))))
		    (insert inner (number->string (simalpha-inner-radius)))
		    (pack inner (side: 'left))
		    (pack (create <label> f2 with (title: "Inner radius")) (side: 'left))
		    (pack f2 (anchor: 'w)))
		  (let* ((f3 (create <frame> f))
			 (outer (create <better-entry> f3 with (width/char: 3)
					(action: (lambda (self) (simalpha-outer-radius (string->number (get-string self))))))))
		    (insert outer (number->string (simalpha-outer-radius)))
		    (pack outer (side: 'left))
		    (pack (create <label> f3 with (title: "Outer radius"))(side: 'left))
		    (pack f3 (anchor: 'w)))
		  f)]

	       [clock-readout (create <canvas-text> the-win 
				      30 ;(- (/ window-width 2) 10) 
				      (- window-height 13)
				      with (title: "t = ")
				      (font: (create <font> 'times 16 '(bold))))]


	       [rerun-button (create <button> the-panel with (title: "Rerun Sim")
				     (action: (lambda (self)
						(unless sim-busy
						  (set-title! the-text-readout "")
					          ; Kill existing evaluations if they exist:
						  (when (simalpha-pause-hook) (simalpha-pause-hook #f) 
							(thread-send-msg pause-queue 'kill) (set! sim-busy #f))
						  (set-title! pause-button "Pause")
						  (printf "\nRerunning Simulator!\n")
						  
						  (thread-eval '(rerun-simulator-alpha 'use-stale-world)))
						)))]
	       [restart-button (create <button> the-panel with (title: "Reroll network")
				       (action: (lambda (self)
						  (unless sim-busy
						    (set-title! the-text-readout "")
						    (when (simalpha-pause-hook) (simalpha-pause-hook #f)
							  (thread-send-msg pause-queue 'kill) (set! sim-busy #f))
						    (set-title! pause-button "Pause")
						    (printf "\nRerunning Simulator!\n")
						    (thread-eval '(rerun-simulator-alpha))
						    (thread-fork 
						     (lambda () ((get-action pause-button) pause-button)))
						    ))))]
	       [printstats-button (create <button> the-panel with (title: "Print Stats")
					  (action: (lambda (_) 
						     ;; This runs asynchronously:
						     (eval '(print-stats)))))]
	       [printconn-button (create <button> the-panel with (title: "Print Connectivity")
					  (action: (lambda (_) 
						     ;; This runs asynchronously:
						     (eval '(print-connectivity (simalpha-current-simworld))))))]

	       )

	  ;; This is the thread that periodically updates the view by polling various state.
	  (set! the-viewer-thread 
		(thread-fork (let ([win the-win]
				   ;[sim (simalpha-current-simworld)]
				   )
			       ;(inspect (list (simworld? sim) ((top-level-value 'simworld?) sim) sim))
			       ;(DEBUGASSERT (simworld? sim))
			       (lambda ()
				 (let viewer-update-loop ()
				   ;; If the window has changed or been destroyed, we kill this thread.
				   (if (not (eq? win the-win)) (thread-kill))

				   (DEBUGASSERT (simworld? (simalpha-current-simworld)))
				   ;(printf "View update!\n")(flush-output-port)
				   (set-title! clock-readout 
					       (format "t = ~s" (simworld-vtime (simalpha-current-simworld))))
				   (show clock-readout)

				   (unless (eq? realtime-state (simalpha-realtime-mode))
				     (set! realtime-state (not realtime-state))
				     (send realtime-button toggle))

				   (unless (eq? msgcounts-state (simalpha-label-msgcounts))
				     (set! msgcounts-state (not msgcounts-state))
				     (send msgcounts-button toggle))

				   ;; TODO: num-nodes
				   ;; TODO: placement-widget

				   (if (simalpha-label-msgcounts)
				       (thread-eval 
					'(for-each (lambda (ob)						   
						   (sim-setlabel (format "~a->~a"
									 (simobject-local-recv-messages ob)
									 (simobject-local-sent-messages ob)) ob))
					   (simworld-all-objs (simalpha-current-simworld)))))

				   (thread-sleep 250)
				   (viewer-update-loop))))))

;	  (send showedges-button toggle)
	  	
	  (show the-winframe)

	  (pack pause-button      (side: 'left))
	  (pack restart-button    (side: 'left))
	  (pack rerun-button      (side: 'left))
	  (pack printstats-button (side: 'left))
	  (pack printconn-button  (side: 'left))


	  (pack (create <label> the-panel2 with (title: "Topology")) (anchor: 'w))
	  (pack placement-widget );(anchor: 'w)) ;(side: 'left))
	  (pack (create <label> the-panel2 with (title: "Radio")) (anchor: 'w))
	  (pack radio-widget )
	  (pack realtime-button  (anchor: 'w)) ;(side: 'left) (anchor: 'se))
	  (pack msgcounts-button (anchor: 'w)) ;(side: 'left) (anchor: 'se))
	  (pack num-nodes-widget (anchor: 'w)) ;(side: 'left))

	  (pack showedges-button );(anchor: 'w)) ;(side: 'left) 

	  (pack the-win    (side: 'top))
	  (pack the-panel  (side: 'bottom))
	  (pack group      (side: 'left))
	  (pack the-panel2 (side: 'right))
	  
	))))

(define (close-graphics)
  (if the-win
      (begin
	(destroy the-win)
	(destroy the-winframe)
	(set! the-win #f)
	(set! the-winframe #f))
      (printf "Graphics already closed!~n")))

#;(define (clear-buffer)
  (for-each destroy object-buffer)
  (set! object-buffer '())
  )

;; [2005.11.25] Not used right now:
#; 
;; This may optionally destroy the object buffer as well:
(define (paint-buffer)
  ;; This requires them be seperate in memory:
  (for-each destroy screen-buffer)
  (for-each show object-buffer)
  (set! screen-buffer object-buffer)
  (set! object-buffer '())
  )

(define draw-ellipse
  (let ((drawit (lambda (x1 y1 x2 y2 draw fill)		  
		  (let ((circ (create <oval> the-win x1 y1 x2 y2)))
		    (set-outline-color! circ 
                      (make <rgb> 
			(rgb-red draw) (rgb-green draw) (rgb-blue draw)))
		    (if fill (set-fill-color! 
			      circ 
			      (make <rgb> (rgb-red fill) (rgb-green fill) (rgb-blue fill))))
		    ;(show circ)
		    (hide circ)
		    (set! object-buffer (cons circ object-buffer))
		    ))))
  (case-lambda
   [(x1 y1 x2 y2) (drawit x1 y1 x2 y2 current-drawing-color current-filling-color)]
   [(x1 y1 x2 y2 c1) (drawit x1 y1 x2 y2 c1 current-filling-color)]
   [(x1 y1 x2 y2 c1 c2) (drawit x1 y1 x2 y2 c1 c2)])))



);; End module

