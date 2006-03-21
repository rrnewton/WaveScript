
(parameters 
  [simalpha-realtime-mode #t]
  
  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function sense-noisy-rising]
  [simalpha-sense-function sense-random-1to100]

  [default-fast-pulse 100]
  [default-slow-pulse 1000]

  [sim-num-nodes 100]
  [simalpha-outer-radius 8]
  [sim-timeout 2000])



;; This is our base region.  It consists of exactly two nodes
;; preselected from the batch.
(define nodes
  (light-up ; Identity function that just happens to perform a harmless side-effect.
   (rfilter (lambda (n)	    
	      (or (= (nodeid n) 6)
		  (= (nodeid n) 14)))
	    world)))

(define twohop (lambda (n) (khood (node->anchor n) 2)))

(define nbrhoods (rmap twohop nodes))

;; Main query:

(rrflatten nbrhoods)

#;(liftsig (rmap (lambda (r) (rfold append '() 
			 (rmap (lambda (n) (cons (nodeid n) '()))
			       r)))
      nbrhoods))

#;(liftsig (rmap (lambda (r) (rfold + 0
				  (rmap (lambda (n) (sense 'default n))
					r)))
	       nbrhoods))



; ======================================================================
; These are some commands for invoking this file from the interactive REPL:
; (mvlet (((prog _) (read-regiment-source-file "demos/regiment/nested_regions.rs"))) (set! theprog (rc prog 'verbose 'barely-tokens)))
; (load-regiment "demos/regiment/nested_regions.rs")


;; JUNK:

; ;(define anchs (rmap node->anchor world))
; ;(define sum (lambda (r) (rfold + 0 r)))
; ;(define valfield (rmap sense world))

; ;(sum valfield)



;(rmap (lambda (n) (vector (nodeid n) (sense n))) nodes)



;(define sums (rmap (lambda (r) (rfold + 0 r)) nbrhoods))
;(define sums (rmap (lambda (n) (rfold + 0 (twohop n))) nodes))
;sums


;(cons (khood (anchor-at 30 40) 1)
;      (cons (anchor-at 50 10) ;(khood (anchor-at 50 10) 1)
;	    '())

;(khood (anchor-at 10 40) 2)

