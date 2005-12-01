;;;; This very simple demo program creates an anchor and then a 2 hop
;;;; neighborhood around it.

(parameters 
  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function sense-noisy-rising]
  [simalpha-sense-function sense-random-1to100]
  [sim-timeout 2000])

;; Main program:

(khood (anchor-at 10 40) 2)

; ======================================================================
; These are some commands for invoking this file from the interactive REPL:
; (mvlet (((prog _) (read-regiment-source-file "demos/regiment/khood_anchor_at.rs"))) (set! theprog (rc prog 'verbose 'barely-tokens)))
; (load-regiment "demos/regiment/khood_anchor_at.rs")
