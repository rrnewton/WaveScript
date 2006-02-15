;; [2004.05.24]
;; This implements my simple "basic graphics" interface, which I will
;; document somewhere as soon as it's done.  Erg, you'd think this
;; would be part of slib?

;; THERE IS NO ABSTRACTION BOUNDARY BETWEEN THIS AND GRAHPICS_STUB.SS

;; TODO FIXME: REMOVE eval's FROM THIS AND GRAPHICS_STUB.ss
;; (HAVING PROBLEMS WITH RECORD-TYPES CURRENTLY. evals ARE A HACK)

; ======================================================================

;; [2004.06.21] Adding "draw-mark" to the interface, I use this for
;; drawing 'X' marks for additional debugging info.

(module basic_graphics (
			init-graphics close-graphics  
			rec->rgb ;; This converts rgb records into system specific (SWL) rgb values.
;			make-rgb rgb? rgb-red rgb-green rgb-blue
			the-win 
			;; current-drawing-color 
			;; current-filling-color
			;; current-background-color
			;;;;; draw-ellipse 

			flash-text ;thread-eval
			)

 ;; This global variable points to the drawing surface itself.
(define the-win #f) 

(define the-winframe #f)

;; [2005.11.25] The button panel:
(define the-panel  #f)
(define the-panel2 #f)
(define the-viewer-thread #f)

;; [private] A single line of text used for messages to the user.
(define the-text-readout #f)

;; [Chez] This simply makes a SWL rgb value out of our standard record representation.
(define (rec->rgb rec)
  (make <rgb> (rgb-red rec) (rgb-green rec) (rgb-blue rec)))

; Module local bindings:
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

;(define (new-timer t th) (thread-fork (lambda () (thread-sleep ms) (th))))

;; Flashes a bit of text at the bottom of the GUI window for a given period of time.
(define flash-text 
  ; Currently this list should only contain one flash:
  (let ((current-flashes '()))
    (lambda (txt ms)
      (if (not the-text-readout)
	  (error 'flash-text "the-text-readout is null, graphics must not be initialized."))
      (critical-section 
       (for-each thread-kill current-flashes)
       (set-title! the-text-readout "")
       (let ((newthread (thread-fork
			 (lambda ()
			   (let ((snapshot (get-title the-text-readout)))
			     (set-title! the-text-readout txt)
			     (thread-sleep ms)
			     ;; If we haven't been displaced, reset text:
					;(if (equal? txt (get-title the-text-readout)) (set-title! the-text-readout snapshot))
			     (set-title! the-text-readout "")
			     (critical-section
			      (set! current-flashes (remq (thread-self) current-flashes)))
			     (thread-kill)
			     )))))
	 (set! current-flashes (cons newthread current-flashes)))))))

;; Init the graphics system, bring up a window.<br> 
;; [2005.11.25] Man I'm having some serious annoyance figuring out how to do proper layout with SWL.
;; <br>
;; <br> [2005.11.26] NOTE: Having problems with modules/imports/records here.  Using 'eval' as a hack: (FIXME)
(define (init-graphics)
  
  ;; [internal] A list of thunks to call when the view is being updated.
  (define view-update-hooks '())

  ;; [internal] This entry calls its action every time a key is typed:
  (define-class (<better-entry> parent) (<entry> parent)
    (ivars (parent parent))
    (inherited) (inheritable)
    (private)   (protected)
    (public
      ;[mouse-leave (x y mods) ((get-action self) self)]
     [key-release (k mods)     ((get-action self) self)]

     [key-press (c mods) 
		(send-base self key-press c mods)
		(send parent key-press c mods)
		]
     ))

  ;; [internal] This class displays a label next to the entry, and
  ;; monitors the associated numeric parameter for changes, updating
  ;; the view accordingly.
  (define-class (<numeric-param-entry> name label param parent) (<frame> parent)
    (ivars (label-obj #f) (entry-obj #f) (counter 0))
    (inherited)  (inheritable)  (private)   (protected)
    (public
      ;[mouse-leave (x y mods) ((get-action self) self)]
     
     [key-press (c mods) 
		(set! counter 8)]

     [init (nm lab prm prnt)
	   (send-base self init prnt)	   
	   (set! entry-obj 
		 (create <better-entry> self with (width/char: 4)			
			 (action: (lambda (self)
				    (let ((num (string->number (get-string self))))
				      (when num 
					(flash-text (format "Setting ~a: ~s\n" nm (get-string self)) 1000)
					(prm num)))))
			 ;(fill-color: Default-Window-Text-Color)
			 ))
	   (insert entry-obj (number->string (prm)))
	   (set! label-obj (create <label> self with (title: lab)))
	   (pack entry-obj (side: 'left))
	   (pack label-obj (side: 'right))

	   (set-background-color! self      (rec->rgb Default-Window-Color))
	   (set-background-color! entry-obj (rec->rgb Default-Window-Color))
	   (set-background-color! label-obj (rec->rgb Default-Window-Color))
	   
	   ;; Add an update hook for this:
	   (set! view-update-hooks 
		 ;; I don't have a good way to tell when the user is editing this entry.
		 ;; (It's very annoying when we reset over what they're trying to type.)
		 ;; So what I do is I set the counter above when a key
		 ;; is pressed, and it must count down to zero before we'll touch the contents.
		 (cons (lambda ()
			 (if (zero? counter)
			     (let ((curval (string->number (get-string entry-obj))))
			       (unless (equal? (prm) curval)
				 (delete-all entry-obj)
				 (insert entry-obj (number->string (prm)))))
			     (set! counter (sub1 counter))))
		       view-update-hooks))
	   ]
     [set-width/char! (n) (set-width/char! entry-obj n)]
     ))

  ;; [internal] This class is a check-button that also manages the book-keeping.
    (define-class (<bool-param-button> name label param parent) (<checkbutton> parent)
    (ivars) 
    (inherited) (inheritable) (private) (protected)
    (public
      ;[mouse-leave (x y mods) ((get-action self) self)]
     [init (nm lab prm prnt)
	   (send-base self init prnt)
	   (set-title! self lab)
	   (if (prm) (send self select) (send self deselect))

	   (set-action! self
	    (lambda (self)
	      ;(set! realtime-state (not realtime-state))
	      (prm (not (prm)))
	      (flash-text (format "Setting ~a: ~a\n" nm (prm)) 1000)
	      (if (prm) (send self select) (send self deselect))))

	   (set-background-color! self (rec->rgb Default-Window-Color))

	   (set! view-update-hooks
		 (cons (lambda ()
			 (if (prm) (send self select) (send self deselect)))
		       view-update-hooks))]
     ))
  
    ;; [internal] This class provides a set of radio buttons for a Regiment parameter. <br>
    ;; 
    ;; .param name - The symbolic name of the parameter
    ;; .param label - The user-visible label for the widget.
    ;; .param param - The parameter closure itself
    ;; .param options - An association list binding option title
    ;; (string) to a thunk for creating a new object to bind to the
    ;; parameter when the corresponding button is pressed.
    ;; .param parent - As usual with SWL, the parent widget.
    (define-class (<radio-param-selector> name label param options parent) (<frame> parent)
      (ivars) 
      (inherited) (inheritable) (private) (protected)
      (public
       [init (name label param options parent)
	     (send-base self init parent)
	     (set-background-color! self (rec->rgb Default-Window-Color))	     
	     (let ((buttons 
		    (map (lambda (option)
			   (list (cadr option)
				 (create <radiobutton> self with (title: (format "~a" (car option)))
					 (action: (lambda _ (param ((cadr option)))))
					 (background-color: (rec->rgb Default-Window-Color)))))
		      options)))
	       
	       (pack (create <label> self with (title: label)
			     (background-color: (rec->rgb Default-Window-Color))) (anchor: 'w))
	       
	       (for-each (lambda (b) (pack (cadr b) (anchor: 'w))) buttons)

	       ;; Look up the current value of the parameter in the list we just created.
	       (let ((entry (assq (param) buttons)))
		 (if (not entry)
		     (warning '<radio-param-selector>
			      "Parameter ~a has unknown current value: ~a" name (param))
		     ;; Otherwise select the correct button:
		     (send (cadr entry) select))))
	     ]))

    ;; [internal] This is the regiment drawing surface.
    (define-class (<regiment-canvas> parent) (<canvas> parent)
      (ivars) 
      (inherited) (inheritable) (private) (protected)
      (public
       [key-press (c mods)
		  (send-base self key-press c mods)
		  (flash-text (format "Key pressed: ~s" c) 1000)]
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
			      (printf "Simulation Finished.\n")
			      (flash-text "Simulation Finished." 900)
			      (thread-kill)))))

;  (printf "Running graphical interface to simple simulator.~n")
  (if the-win 
      (printf "Graphics already open!!~n")
      (let ([pause-queue (thread-make-msg-queue 'pause-queue)]
	    [group #f])

	(set! the-winframe (create <toplevel> with 
				   (title: "Region Streams Demo")
				   (background-color: (rec->rgb Default-Window-Color))
				   ))

	(set! group (create <frame> the-winframe with
			    (background-color: (rec->rgb Default-Window-Color))))

	(set! the-panel (create <frame>  group ;the-winframe
				with
				;(width: (in->pixels 1)) ;(+ window-width 200))
				;(height: window-height)
				;(background-color: (make <rgb> 100 50 50))
				(background-color: (rec->rgb Default-Window-Color))
				))
	(set! the-panel2 (create <frame>  the-winframe
				with
				;(width: (in->pixels 1)) ;(+ window-width 200))
				;(height: window-height)
				;(background-color: (make <rgb> 100 50 50))*
				(background-color: (rec->rgb Default-Window-Color))
				))
	(set! the-win (create <regiment-canvas> group ;the-winframe
				 with
				 (width: window-width) ;(in->pixels 5))
				 (height: (+ window-height 25)) ;(in->pixels 5))
				 (background-color: (rec->rgb Default-Background-Color))
				 ))

	(set! the-text-readout (create <canvas-text> the-win (- (/ window-width 2) 10) (- window-height -15);13)
					with (title: "")
					(fill-color: (rec->rgb Default-Canvas-Text-Color))
					))

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

	       [realtime-button (create <bool-param-button> 'simalpha-realtime-mode 
					"Realtime"           simalpha-realtime-mode the-panel2)]
	       [msgcounts-button (create <bool-param-button> 'simalpha-label-msgcounts 
					 "Show MsgCounts"     simalpha-label-msgcounts the-panel2)]
	       [sensorshow-button (create <bool-param-button> 'simalpha-label-sensorvals 
					 "Show SensorVals"     simalpha-label-sensorvals the-panel2)]

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
		(create <numeric-param-entry> 'sim-num-nodes "NumNodes" sim-num-nodes the-panel2
			with (width/char: 4))]
	       [timeout-widget 
		(create <numeric-param-entry> 'sim-timeout "Timeout" sim-timeout the-panel2
			with (width/char: 6))]
	       
	       ;; TODO: Abstract to use the generic <radio-param-selector> class for this:
	       [placement-widget
		(let* ((f (create <frame> the-panel2 with
				  (background-color: (rec->rgb Default-Window-Color))))
		       (b1 (create <radiobutton> f with (title: "Connected")
				   (action: (lambda _ (simalpha-placement-type 'connected)))
				   (background-color: (rec->rgb Default-Window-Color))))
		       (b2 (create <radiobutton> f with (title: "Gridlike")
				   (action: (lambda _ (simalpha-placement-type 'gridlike)))
				   (background-color: (rec->rgb Default-Window-Color))))
		       (b3 (create <radiobutton> f with (title: "Random")
				   (action: (lambda _ (simalpha-placement-type 'random)))
				   (background-color: (rec->rgb Default-Window-Color)))))

		  (pack b1 (anchor: 'w))
		  (pack b3 (anchor: 'w))
		  (pack b2 (anchor: 'w))
		  (pack (create <numeric-param-entry> 'simalpha-max-gridlike-perturbation 
				"GridPerturb"          simalpha-max-gridlike-perturbation the-panel2
				with (width/char: 4))) ;(anchor: 'w))

		  (case (simalpha-placement-type)
		    [(connected) (send b1 select)]
		    [(gridlike)  (send b2 select)]
		    [(random)    (send b3 select)])

		  ;(let ((f2 (create <frame> f)))
; 		  (let ((e (create <better-entry> f with (width/char: 4)			
; 				   (action: (lambda (self)
; 					      (flash-text (format "Setting simalpha-max-gridlike-perturbation: ~s\n" (get-string self)) 1000)
; 					      (let ((num (string->number (get-string self))))
; 						(if num (simalpha-max-gridlike-perturbation num))))))))
; 		      (insert e (number->string (simalpha-max-gridlike-perturbation)))
; 		      (pack e (side: 'left))
; 		      (pack (create <label> f with (title: "GridPerturbation")) (side: 'right)))

		  f)]

	       ; Haha radio button for the radio widget...
	       [radio-widget
		(let* ((f (create <frame> the-panel2 with
				  (background-color: (rec->rgb Default-Window-Color))))
		       (b1 (create <radiobutton> f with (title: "Lossless")
				   (action: (lambda _ (simalpha-channel-model 'lossless)))
				   (background-color: (rec->rgb Default-Window-Color))))
		       (b2 (create <radiobutton> f with (title: "LinearDisc")
				   (action: (lambda _ (simalpha-channel-model 'linear-disc)))
				   (background-color: (rec->rgb Default-Window-Color)))))
		  (pack b1 (anchor: 'w))
		  (pack b2 (anchor: 'w))
		  (case (simalpha-channel-model)
		    [(lossless)    (send b1 select)]
		    [(linear-disc) (send b2 select)])

		  (pack (create <numeric-param-entry> 'simalpha-inner-radius "Inner radius" simalpha-inner-radius f
				with (width/char: 3)) (anchor: 'w))
		  (pack (create <numeric-param-entry> 'simalpha-outer-radius "Outer radius" simalpha-outer-radius f
				with (width/char: 3)) (anchor: 'w))

		  f)]

	       [sensor-widget
		(create <radio-param-selector> 'simalpha-sense-function-constructor "Sensor Readings"
			simalpha-sense-function 
			`(["Sine Wave" ,sense-sine-wave]
			  ["Noise-Rising" ,sense-noisy-rising]
			  ["Random 1-100" ,sense-random-1to100]
			  ["Dist to Origin" ,sense-dist-from-origin]
			  ["Spatial Sine Wave" ,sense-spatial-sine-wave])
			the-panel2)]

	       [clock-readout (create <canvas-text> the-win 
				      60 ;(- (/ window-width 2) 10) 
				      (- window-height -10) ;13)
				      with (title: "t = ")
				      (fill-color: (rec->rgb Default-Canvas-Text-Color))
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

				   ; Draw the clock in the corner of the screen.
				   (set-title! clock-readout 
					       (format "t = ~s" (simworld-vtime (simalpha-current-simworld))))
				   (show clock-readout)

				   ; This updates all the message labels.
				   (if (simalpha-label-msgcounts)
				       (eval ; Was using thread-eval for this.  How long should it take?
					'(for-each (lambda (ob)						   
						   (sim-setlabel (format "~a->~a"
									 (simobject-local-recv-messages ob)
									 (simobject-local-sent-messages ob)) ob))
					   (simworld-all-objs (simalpha-current-simworld)))))
				   ;; And this updates the labels to reflect sensor values.
				   (if (simalpha-label-sensorvals)
				       (eval
					'(let ((vtime (simworld-vtime (simalpha-current-simworld))))
					   (for-each 
					       (lambda (ob)
						 (let ((reading ((simalpha-sense-function)
								 (node-id (simobject-node ob))
								 (car (node-pos (simobject-node ob)))
								 (cadr (node-pos (simobject-node ob)))
								 vtime)))
						   (sim-setlabel 
						    (if (number? reading)							
							(number->string (round-to 2 reading))
							(format "~a" reading))
						    ob)))
					     (simworld-all-objs (simalpha-current-simworld))))))

				   ;; Now call any other update hooks:
				   ;; (Right now [2005.11.27] this is used for the text entry fields.)
				   (for-each (lambda (th) (th)) view-update-hooks)

				   (thread-sleep 250)
				   (viewer-update-loop))))))

	  ;; Let's add a bit more to wipe those msg count readouts when we turn the param off:
	  (let ((oldac (get-action msgcounts-button)))
	    (set-action! msgcounts-button
			 (lambda (self)
			   (oldac self)
			   (if (not (simalpha-label-msgcounts))
			       (eval '(for-each (lambda (x) (sim-setlabel "" x))
					(simworld-all-objs (simalpha-current-simworld))))))))
	  ;; Same for showing the sensor vals:
	  (let ((oldac (get-action sensorshow-button)))
	    (set-action! sensorshow-button
			 (lambda (self)
			   (oldac self)
			   (if (not (simalpha-label-sensorvals))
			       (eval '(for-each (lambda (x) (sim-setlabel "" x))
					(simworld-all-objs (simalpha-current-simworld))))))))


;	  (send showedges-button toggle)
	  	
	  (show the-winframe)

	  (pack pause-button      (side: 'left))
	  (pack restart-button    (side: 'left))
	  (pack rerun-button      (side: 'left))
	  (pack printstats-button (side: 'left))
	  (pack printconn-button  (side: 'left))

	  (pack sensor-widget)

	  (pack (create <label> the-panel2 with (title: "Topology")
			(background-color: (rec->rgb Default-Window-Color))) (anchor: 'w))
	  (pack placement-widget );(anchor: 'w)) ;(side: 'left))
	  (pack (create <label> the-panel2 with (title: "\nRadio")
			(background-color: (rec->rgb Default-Window-Color))) (anchor: 'w))
	  (pack radio-widget )
	  (pack realtime-button  (anchor: 'w)) ;(side: 'left) (anchor: 'se))
	  (pack msgcounts-button (anchor: 'w)) ;(side: 'left) (anchor: 'se))
	  (pack sensorshow-button (anchor: 'w)) ;(side: 'left) (anchor: 'se))
	  (pack num-nodes-widget (anchor: 'w)) ;(side: 'left))
	  (pack timeout-widget (anchor: 'w)) ;(side: 'left))

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

#;(define draw-ellipse
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

