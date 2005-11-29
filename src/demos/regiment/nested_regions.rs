
(parameters 
  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function sense-noisy-rising]
  [simalpha-sense-function sense-random-1to100]
  [sim-timeout 2000])


;(define anchs (rmap node->anchor world))
;(define 2hop (lambda (n) (khood (node->anchor n) 2)))
;(define sum (lambda (r) (rfold + 0 r)))
;(define valfield (rmap sense world))

;; Main program
;(sum valfield)

;(rmap 2hop world)
;(cons (khood (anchor-at 30 40) 1)
;      (cons (anchor-at 50 10) ;(khood (anchor-at 50 10) 1)
;	    '())

(khood (anchor-at 10 40) 2)

; ======================================================================
; These are some commands for invoking this file from the interactive REPL:
; (mvlet (((prog _) (read-regiment-source-file "demos/regiment/nested_regions.rs"))) (set! theprog (rc prog 'verbose 'barely-tokens)))
; (load-regiment "demos/regiment/nested_regions.rs")
