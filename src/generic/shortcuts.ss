

;;;; NOTE: This file is a bit of a mess.  This is the file in which I
;;;; put messy shorthands and entrypoints that I'm using (usually
;;;; temporarily.) -Ryan 



; =============================================================
;;; Shorthands.  
;;; These are just for my convenient usage in interactive invocation of the compiler.

(define-id-syntax rl (reload)) ;; shorthand

#;
(define r  ;; shorthand 
  (letrec ((loop
	    (case-lambda 
	     [(pass x)
	      (let ((prog  x))
		(parameterize ((pass-list (list-remove-after (eval pass) (pass-list)))
			       ;(tracer #t)
			       )
		  (test-one prog #f #f)))]
	     [(x) (loop (rac (pass-list)) x)])))
    loop))

(define-syntax at (identifier-syntax assemble-tokmac)) ;; shorthand

(define-syntax rc (identifier-syntax run-compiler)) ;; shorthand

(define-syntax ct (identifier-syntax compile-to-tokens)) ;; shorthand

(define-syntax ra (identifier-syntax run-simulator-alpha)) ;; shorthand

;; Token and later compiler:
#;
(define (tr x)  ;; shorthand
  (let ((prog  x))
    (parameterize (;(tracer #t)
		   (game-eval (lambda args 'unspecified))
		   (host-eval (lambda args 'unspecified)))
      (parameterize ((pass-list (cdr (list-remove-before 'deglobalize (pass-list)))))
	(test-one prog)))))


; temp:
#;
(define (doit x) ;; shorthand
  (cleanse-world)
  (run-simulation (build-simulation (compile-simulate-nought x))		  
		  20.0))


;; These are some temporary diagnostic functions:
#|
(define (all-incoming) ;; shorthand
  (filter (lambda (ls) (not (null? ls)))
          (map simobject-incoming all-objs)))
(define (all-homepage) ;; shorthand
  (filter (lambda (ls) (not (null? ls)))
          (map simobject-homepage all-objs)))

(define (sim) (build-simulation  ;; shorthand
	     (compile-simulate-nought 
	      (cadadr (run-compiler '(anchor-at '(30 40)))))))
|#


;; Define a bunch of useful shorthands for interacting with the graphical simulator.
(IF_GRAPHICS
 (begin 
   (define-id-syntax ig (identifier-syntax (init-graphics))) ;; shorthand

   (define-id-syntax cg (identifier-syntax (close-graphics))) ;; shorthand
   (define-id-syntax cw (identifier-syntax (close-graphics))) ;; shorthand

;   (define-syntax g  (identifier-syntax (simalpha-draw-world (fresh-simulation))))

   (define-id-syntax debug-grammar (identifier-syntax (analyze-grammar-failure failure-stack)))

   (define-id-syntax edges (identifier-syntax ;; shorthand
			    (list->set 
			     (apply append
				    (map (lambda (x) (map cadr (gobject-edgelist (simobject-gobj x))))
				      (simworld-all-objs (simalpha-current-simworld)))))))
   
   (define-id-syntax demos (begin (if (getenv "REGIMENTD")
				      (cd (++ "REGIMENTD")))
				  (cd "demos/regiment")
				  (printf "Regiment demos:\n")
				  (system "ls")))
				      
   ))

(begin ;; Some convenient syntax shortcuts:
   (define (node id) ;; shorthand
     (simobject-node (hashtab-get (simworld-obj-hash (simalpha-current-simworld)) id)))
   (define (nodeid->neighbors id)
     (map node-id (cdr (assq (node id) (simworld-graph (simalpha-current-simworld))))))
   (define (dist id1 id2) ;; shorthand
     (sim-locdiff (node-pos (simobject-node (node id1)))
		  (node-pos (simobject-node (node id2)))))
   (define-id-syntax world (simalpha-current-simworld)) ;; shorthand   
;   (define (
)



(define-id-syntax tn (begin (simalpha-realtime-mode #t) (rerun-simulator-alpha 'use-stale-world)))

(IFCHEZ
 (define-id-syntax t  ;; uber shorthand
   (let ([start 
	  (begin (collect (collect-maximum-generation))
		 (statistics))]
	 [startobs (oblist)])
     (and 
      (time (test-units))
      (collect (collect-maximum-generation))
      (let ([end (statistics)]
	    [endobs (oblist)])
	(let ([before (- (sstats-bytes start) (sstats-gc-bytes start))]
	      [after (- (sstats-bytes end) (sstats-gc-bytes end))])
	  (printf "\nAfter a thorough collection:\n")
	  (printf "   ~:d additional allocated bytes are left over (~:d before ~:d after).\n"
		  (- after before) before after)
	  (let ([len1 (length startobs)]
		[len2 (length endobs)]
		[diff (difference endobs startobs)])
	    (printf "   ~s syms in oblist before, ~s syms after, diff ~s\n" len1 len2 (- len2 len1))
	    (parameterize ([print-length 20])
	      (printf "Difference: ~s\n" diff))
	    (printf "  Difference with top-level bindings: ~s\n\n" (filter top-level-bound? diff))
	    ))))))
 (define-id-syntax t (time (test-units)))
)


(define-id-syntax mem  ;; shorthand
  (begin (collect (collect-maximum-generation))
	 (let ([stats (statistics)])
	   (printf "Approx dynamic mem usage: ~:d\n" (- (sstats-bytes stats) (sstats-gc-bytes stats))))))


(define-id-syntax cur  ;; shorthand, current test
  (begin (cd "~/wavescript/src/demos/wavescope/")
	 (browse-stream (wsint "demo6_sync.ws"))))



;;; TEMPORARY JUNK:
; =============================================================




'
(parameterize ([pass-list
             '(cleanup-token-machine
                desugar-gradients
                cleanup-token-machine
                desugar-let-stored
                rename-stored
                cps-tokmac
                closure-convert
                cleanup-token-machine)])
  (let ([prog
         (run-compiler
           '(tokens
              (SOC-start () (gemit tok1))
              (tok1 (x) (printf "_ ") (grelay))))])
    (let ([prt (open-output-string)])
      (display "(" prt)
      (run-simulator-alpha prog 'outport prt)
      (display ")" prt)
      (read (open-input-string (get-output-string prt))))))




'
      (parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
      (parameterize ([pass-list
		   '(cleanup-token-machine  desugar-gradients
		     cleanup-token-machine desugar-let-stored
		     rename-stored          cps-tokmac
		     closure-convert        cleanup-token-machine
		     )])
	(let ([prog
	       (run-compiler
		'(tokens
		  (SOC-start () (call tok1 '1))
		  (catcher (v) (printf "~a" v))
		  (tok1 (reps) 
			(gemit tok2)
			(if (> reps 0)
			    (timed-call 500 tok1 (- reps 1))))
		  (tok2 () (greturn 34 ;(my-id) 
				    (to catcher)))
		  ))])
	  (let ((lst 
		 (let ([prt (open-output-string)])
		   (run-simulator-alpha prog)
		   (read (open-input-string (get-output-string prt))))))
	    lst
	    ))))


;(define (test) (eval (caddr (list-ref (maintest 'get-tests) 55))))





(define-id-syntax t1 (begin (close-graphics) b2))
;(define (t2) (parameterize ((simalpha-realtime-mode #f)) (eval (caddr (list-ref (maintest 'get) 60)))))
(define-id-syntax t2 (load-regiment (++ (REGIMENTD) "demos/regiment/nested_regions.rs")))
(define-id-syntax t3 (load-regiment (++ (REGIMENTD) "demos/regiment/simple/events.rs")))
(define-id-syntax t3b (load-regiment (++ (REGIMENTD) "demos/regiment/simple/union.rs")))

(define-id-syntax t4 (load-regiment (++ (REGIMENTD) "demos/regiment/static_elab.rs")))
(define-id-syntax t5 (load-regiment (++ (REGIMENTD) "demos/regiment/anchor_free_localization.rs")))
(define-id-syntax t5b (load-regiment (++ (REGIMENTD) "demos/token_machs/anchor_free_localization.tm")))
(define-id-syntax t6 (load-regiment (++ (REGIMENTD) "demos/regiment/tracking.rs")))

(define-id-syntax t7a (load-regiment (++ (REGIMENTD) "demos/firelightning/simple_lightup.tm")))
(define-id-syntax t7b (load-regiment (++ (REGIMENTD) "demos/firelightning/lightup_video.tm")))
;(define-id-syntax t7 (load-regiment "demos/firelightning/manual_nbrhood.tm"))
;(define-id-syntax t8 (load-regiment "demos/firelightning/nbrhood_alarm0.rs"))
;(define-id-syntax t8 (load-regiment "demos/firelightning/nbrhood_alarm1.rs"))

(define-id-syntax t7 (load-regiment (++ (getenv "HOME") "/firelightning/prog1.rs") 'verbose))
(define-id-syntax t8 (load-regiment (++ (getenv "HOME") "/firelightning/p2/prog2.rs") 'verbose))
(define-id-syntax t9 (load-regiment (++ (getenv "HOME") "/firelightning/p2/prog2_manual.tm") 'verbose))
(define-id-syntax t10 (load-regiment (++ (getenv "HOME") "/firelightning/p2/prog2_manual_batchopt.tm") 'verbose))
(define-id-syntax t11 (load-regiment (++ (getenv "HOME") "/firelightning/test.tm") 'verbose))
(define-id-syntax t12 (load-regiment (++ (getenv "HOME") "/firelightning/p3/p3_hand.tm") 'verbose))

(define-id-syntax tb (load-regiment (++ (REGIMENTD) "demos/regiment/bug.tm")))

;(define-id-syntax t9 (load-regiment "demos/firelightning/deadsimple_alarm.rs"))
;(define-id-syntax t9b (load-regiment "demos/firelightning/deadsimple_alarm.tm"))

(define-id-syntax d1 (load-regiment (++ (REGIMENTD) "demos/regiment/simple_fold.rs")))
(define-id-syntax d2 (load-regiment (++ (REGIMENTD) "demos/regiment/average_temperature2.rs")))
(define-id-syntax d3 (load-regiment (++ (REGIMENTD) "demos/regiment/simple_events.rs")))
(define-id-syntax d4 (load-regiment (++ (REGIMENTD) "demos/regiment/smap2_two_anchors.rs")))
(define-id-syntax d5 (load-regiment (++ (REGIMENTD) "demos/regiment/nested_regions.rs")))
(define-id-syntax d6 (load-regiment (++ (REGIMENTD) "demos/regiment/static_elab.rs")))


(define p3 
  '(let* ([thresh 20]
       [read (lambda (n)
               (tuple
                 (app nodeid n)
                 (app sense "time" n)
                 (app sense "temp" n)))]
       [abovethresh (lambda (#(id t temp)) (> temp thresh))]
       [temps (app rmap read world)]
       [heat_events (app rfilter abovethresh temps)]
       [strms (Area #(Int Int Int)) (app gossip heat_events)]
       [tables (Area #(Int (List #(Int #(Int Int))))) (app rintegrate
                                                           (lambda (this #(id tm temp) table)
                                                             (tuple
                                                               (tuple (app nodeid this) table)
                                                               (app alist_update table id (tuple tm temp))))
                                                           '()
                                                           strms)]
       [table_filt (#(Int (List #(Int #('a Int)))) -> Bool) (lambda (#(thisid
                                                                       table))
                                                              (letrec ([temps (app map (lambda (#(_ #(_ z))) z) table)])
                                                                (letrec ([ids (List Int) (app map
                                                                                              (lambda (#(id _)) id)
                                                                                              table)])
                                                                  (> (app fold + 0 temps) 60))))])
  (app rmap
       (lambda (#(_ tbl)) tbl)
       (app rfilter table_filt tables))))





