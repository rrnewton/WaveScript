
;;;; This demo implements a simple tracking example.
;;;; UNFINISHED:


(parameters 
  [simalpha-realtime-mode #t]

  [simalpha-channel-model 'lossless]
  ;[simalpha-placement-type 'gridlike]
  ;[simalpha-inner-radius 6] ;4];6]
  ;[simalpha-outer-radius 8] ;5];8]
  ;[sim-num-nodes 100]

  [simalpha-outer-radius 20]
  [simalpha-placement-type 'connected]
  [sim-num-nodes 3]
   
  [simalpha-failure-model  'none]

  [simalpha-dbg-on #t]

  ;[simalpha-sense-function-constructor sense-noisy-rising]
  ;[simalpha-sense-function-constructor sense-random-1to100]
  [simalpha-sense-function-constructor sense-dist-from-origin]
  [sim-timeout 5000])


;; Main program:

;; Magnetometer threshold.
;(define threshold 50)

;; All the nodes near the object.
; (define readings
;   (light-up ; Identity function that just happens to perform a harmless side-effect.
;    (rfilter (lambda (n) (< n 20))
; 	    (rmap sense world))))

; (define clump (cluster readings))

; clump

(define nodes
  (light-up ; Identity function that just happens to perform a harmless side-effect.
   (rfilter (lambda (n)	    
	      (= (nodeid n) 1)
		 ;(= (nodeid n) 14)
		  )
	    world)))

(define twohop (lambda (n) (khood (node->anchor n) 1)))

(define nbrhoods (rmap twohop nodes))
;nbrhoods

(define sum (lambda (r) (rfold + 0 (rmap sense r))))
(rmap sum nbrhoods)


; ======================================================================
; These are some commands for invoking this file from the interactive REPL:
; (mvlet (((prog _) (read-regiment-source-file "demos/regiment/tracking.rs"))) (set! theprog (rc prog 'verbose 'barely-tokens)))
; (load-regiment "demos/regiment/tracking.rs")
