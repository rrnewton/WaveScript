
;;;; This demo implements a simple tracking example.
;;;; UNFINISHED:


(parameters 
  [simalpha-realtime-mode #t]

  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'gridlike]
  [simalpha-inner-radius 6] ;4];6]
  [simalpha-outer-radius 8] ;5];8]
  [sim-num-nodes 100]
   
  [simalpha-failure-model  'none]

  ;[simalpha-sense-function sense-noisy-rising]
  ;[simalpha-sense-function sense-random-1to100]
  [simalpha-sense-function sense-dist-from-origin]
  [sim-timeout 2000])


;; Main program:

;; Magnetometer threshold.
;(define threshold 50)

;; All the nodes near the object.
(define nodes
  (light-up ; Identity function that just happens to perform a harmless side-effect.
   (rfilter (lambda (n)	    
	      (< (sense n) 20))
	    world)))

nodes
