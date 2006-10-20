
(parameters 
  [simalpha-realtime-mode #f]
  
  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'connected]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function sense-noisy-rising]
  [simalpha-sense-function sense-random-1to100]

  [simalpha-zeropad-args #f]

  [default-fast-pulse 100]
  [default-slow-pulse 1000]

  [simalpha-dbg-on #f] 

  [sim-num-nodes 4]
  [simalpha-outer-radius 8]
  [simalpha-consec-ids #t]
  [sim-timeout 10000])


;; This is our base region.  It consists of exactly two nodes
;; preselected from the batch.
(define nodes
  ;(light-up ; Identity function that just happens to perform a harmless side-effect.
   (rfilter (lambda (n)	    
	      (= (nodeid n) 1)
	      ;(or (= (nodeid n) 1)
		  ;(= (nodeid n) 14))
	      )
	    world))

(define onehop (lambda (n) (khood (node->anchor n) 1)))

(define nbrhoods (rmap onehop nodes))

(define (sumhood reg) 
  (letrec ([thevals (rmap (lambda (n) (cons (nodeid n) '()))
			  reg)])
    (rfold append '() thevals)))

;; Main query:

;nbrhoods ;; works ok

;; [2006.04.05] Currently this one is weird.  It will map straight
;; over all the nodes in the nested region, it essentially ignores the type and flattens. FIXME
;(rmap (lambda (x) 333) nbrhoods) ;; also works

(rmap (lambda (x) 333) (liftsig (rmap sumhood nbrhoods))) ;; also works

;; [2006.04.05] WORKS:
;(define final (rmap sumhood nbrhoods))
;(liftsig final)

;(rfold append () (liftsig (rmap sumhood nbrhoods)))

;(rfold + 0 (rmap (lambda (x) 333) nbrhoods))




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
;(define sums (rmap (lambda (n) (rfold + 0 (onehop n))) nodes))
;sums


;(cons (khood (anchor-at 30 40) 1)
;      (cons (anchor-at 50 10) ;(khood (anchor-at 50 10) 1)
;	    '())

;(khood (anchor-at 10 40) 2)

