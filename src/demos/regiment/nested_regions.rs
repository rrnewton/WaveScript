
(parameters 
  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'connected]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function sense-noisy-rising]
  [simalpha-sense-function sense-random-1to100]
  [sim-timeout 2000])


;(define 2hop (lambda (n) (khood (node->anchor n) 2)))
;(define sum (lambda (r) (rfold + 0 r)))
;(define valfield (rmap sense world))

;; Main program
;(sum valfield)
(rmap node->anchor world)

; ======================================================================
; (mvlet (((prog _) (read-regiment-source-file "demos/regiment/nested_regions.rs"))) (rc prog 'verbose))
