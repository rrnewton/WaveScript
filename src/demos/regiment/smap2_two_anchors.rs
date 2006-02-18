

;;;; [2005.11.28] <br>
;;;; This simple demo uses smap2 to perform a join in the network
;;;; between two signals, in this case two anchors. a

(parameters 
  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function-constructor sense-noisy-rising]
  [simalpha-sense-function-constructor sense-random-1to100]
  [default-slow-pulse 5000]
  [default-slow-pulse 1000]
  [sim-timeout 2000])


;; Main:

(smap2 (lambda (n1 n2) (tuple n1 n2)) ;(list n1 n2))
       (anchor-at 50 10)
       (anchor-at 30 40))

; ======================================================================
; These are some commands for invoking this file from the interactive REPL:
; (mvlet (((prog _) (read-regiment-source-file "demos/regiment/smap2_two_anchors.rs"))) (set! theprog (rc prog 'verbose 'barely-tokens)))
; (load-regiment "demos/regiment/smap2_two_anchors.rs")

