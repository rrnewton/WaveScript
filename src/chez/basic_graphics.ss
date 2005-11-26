;; [2004.05.24]
;; This implements my simple "basic graphics" interface, which I will
;; document somewhere as soon as it's done.  Erg, you'd think this
;; would be part of slib?

;; THERE IS NO ABSTRACTION BOUNDARY BETWEEN THIS AND GRAHPICS_STUB.SS

(module basic_graphics (window-height window-width  
			init-graphics close-graphics  
			make-rgb rgb? rgb-red rgb-green rgb-blue
			the-win 
			;; current-drawing-color 
			;; current-filling-color
			;; current-background-color
			;; draw-ellipse ....
			)

;; CONSTANTS:
(define Default-Drawing-Color (make <rgb> 0 255 0))
(define Default-Background-Color (make <rgb> 200 200 200))

;; This defines window-width and window-height presets:
;(include "../generic/basic_graphics.ss")
;; Sadly scheme's path system makes no sense, so this loads from the 
;; "Regionstreams/compiler/src" directory where compiler_chez.ss is:
(include "generic/basic_graphics.ss")

(define the-winframe #f)

;; [2005.11.25] The button panel:
(define the-panel #f)
(define the-viewer-thread #f)

(define current-drawing-color Default-Drawing-Color)
(define current-filling-color #f)
(define current-background-color Default-Background-Color)

;; Here's where we keep objects that are to be drawn:
(define object-buffer '())
;; And here's where we keep the objects that are on the screen:
(define screen-buffer '())

;; Init.
;; [2005.11.25] Man I'm having some serious annoyance figuring out how to do proper layout with SWL.
;; <br>
;; <br> [2005.11.26] NOTE: Having problems with modules/imports/records here.  Using 'eval' as a hack: (FIXME)
(define (init-graphics)

;  (printf "Running graphical interface to simple simulator.~n")
  (if the-win 
      (printf "Graphics already open!!~n")
      (begin
	(set! the-winframe (create <toplevel> with 
			      (title: "Region Streams Demo")))
	(set! the-panel (create <frame>  the-winframe
				with
				;(width: (in->pixels 1)) ;(+ window-width 200))
				;(height: window-height)
				;(background-color: (make <rgb> 100 50 50))
				))
	(set! the-win (create <canvas> the-winframe
				 with
				 (width: window-width) ;(in->pixels 5))
				 (height: window-height) ;(in->pixels 5))
				 (background-color: (make <rgb> 215 215 255))
				 ))

#;
	(set! the-panel (create <canvas> the-panel 
				with (width: 200) (height: window-height)
				(background-color: (make <rgb> 50 50 100))))

	(let* ([pause-button (create <button> the-panel with (title: "Pause")
				(action:
				 (let ((state #f))
				   (lambda (self)
				     (if state 
					 (begin (set-title! self "Pause")
						(let ((unpause (simalpha-pause-hook)))					       
						  (if (procedure? unpause)
						      (begin 
							;; First restart the saved computation:
							(unpause)
							;; Then release our hook:
							(simalpha-pause-hook #f))
						      (error 'pause-button "could not unpause.")))
						)
					 (begin (set-title! self "Play")
						;; This tells the simulator to pause and save its continuation:
						(simalpha-pause-hook #t)))
				     (set! state (not state))))))]

	       [printstats-button (create <button> the-panel with (title: "Print Stats")
					  (action: (lambda (_)(print-stats))))]

	       [rerun-button (create <button> the-panel with (title: "Rerun Sim")
				       (action: (lambda (self)
						  (printf "\nRerunning Simulator!\n")
						  (eval '(rerun-simulator-alpha 'use-stale-world))
						  )))]
	       [restart-button (create <button> the-panel with (title: "Reroll network")
				       (action: (lambda (self)
						  (printf "\nRerunning Simulator!\n")
						  (eval '(rerun-simulator-alpha))
						  )))]
	       
	       ; Wow, I can't believe that there doesn't seem to be a way to get the current state of a checkbutton. [2005.11.26]
	       [realtime-state #f]
	       [realtime-button (create <checkbutton> the-panel with (title: "Realtime")
;				(draw-indicator: #f)
				(action:				
				   (lambda (_)
				     (set! realtime-state (not realtime-state))
				     (simalpha-realtime-mode (not (simalpha-realtime-mode))))))]

	       [msgcounts-state #f]
	       [msgcounts-button (create <checkbutton> the-panel with (title: "Show MsgCounts")
				(action: (lambda (self)
					   (set! msgcounts-state (not msgcounts-state))
					   (simalpha-label-msgcounts (not (simalpha-label-msgcounts))))))]
	       

	       [clock-readout (create <canvas-text> the-win 30 20
				      with (title: "t = ")
				      (font: (create <font> 'times 16 ())))]

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
				   (DEBUGASSERT (simworld? (simalpha-current-simworld)))
				   ;(printf "View update!\n")(flush-output-port)
				   ;; If the window has changed or been destroyed, we kill this thread.
				   (if (not (eq? win the-win)) (thread-kill))
				   (set-title! clock-readout 
					       (format "t = ~s" (simworld-vtime (simalpha-current-simworld))))
				   (show clock-readout)

				   (unless (eq? realtime-state (simalpha-realtime-mode))
				     (set! realtime-state (not realtime-state))
				     (send realtime-button toggle))

				   (unless (eq? msgcounts-state (simalpha-label-msgcounts))
				     (set! msgcounts-state (not msgcounts-state))
				     (send msgcounts-button toggle))

				   (if (simalpha-label-msgcounts)
				       (eval 
					'(for-each (lambda (ob)						   
						   (sim-setlabel (format "~a->~a"
									 (simobject-local-recv-messages ob)
									 (simobject-local-sent-messages ob)) ob))
					   (simworld-all-objs (simalpha-current-simworld)))))

				   (thread-sleep 250)
				   (viewer-update-loop))))))
	  	
	  (show the-winframe)
	  (show the-win)  	  
	  (show the-panel)
;	  (pack the-win (side: 'right))
;	  (pack the-panel (anchor: 'e) (side: 'right))

	  (pack pause-button      (side: 'left))
	  (pack rerun-button      (side: 'left))
	  (pack restart-button    (side: 'left))
	  (pack printstats-button (side: 'left))
	  (pack realtime-button (anchor: 'se) (side: 'left))
	  (pack msgcounts-button (anchor: 'se) (side: 'left))
	  

;	  (show button1)
;	  (show button2)
;	  (show button3)
	  
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

