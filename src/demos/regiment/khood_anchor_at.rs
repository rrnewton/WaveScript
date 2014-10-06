;;;; This very simple demo program creates an anchor and then a 2 hop
;;;; neighborhood around it.

(parameters 
  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function-constructor sense-noisy-rising]
  [simalpha-sense-function-constructor sense-random-1to100]
  [sim-timeout 2000])

;; Main program:

(khood (anchor-at 10 40) 2)

; ======================================================================
; These are some commands for invoking this file from the interactive REPL:
; (mvlet (((prog _) (read-wavescript-source-file "demos/wavescript/khood_anchor_at.rs"))) (set! theprog (rc prog 'verbose 'barely-tokens)))
; (load-wavescript "demos/wavescript/khood_anchor_at.rs")
