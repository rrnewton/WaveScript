
;;;; This program demonstrates how static elaboration works in Regiment.

;; Simulation Configuration:
(parameters 
  [sim-timeout 1000]
  [sim-num-nodes 30]
  [simalpha-realtime-mode #f]
  [simalpha-consec-ids #t]  ;; Ids should be 0-29 if BASE_ID=0
  [simalpha-channel-model 'lossless]
  [simalpha-outer-radius 15]
  [simalpha-placement-type 'gridlike] ;'connected]
  [simalpha-failure-model  'none])


;; Main program:
;; Result should be a list that when collapsed to a set is the numbers 100->129.

(define incr (lambda (x) (+ x 10)))

;; This expands into 11 nested applications of rmap.
(define loop 
  (lambda (n)
    (if (= 0 n)
	(rmap nodeid world)
	(rmap incr (app loop (- n 1))))))

(app loop 10)

; ======================================================================
; These are some commands for invoking this file from the interactive REPL:
; (mvlet (((prog _) (read-regiment-source-file "demos/regiment/static_elab.rs"))) (set! theprog (rc prog 'verbose 'barely-tokens)))
; (load-regiment "demos/regiment/static_elab.rs")
