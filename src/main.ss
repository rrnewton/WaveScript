;;;; .title Compiler Core (main.ss)

;;;; This contains the core compiler entry points. 
;;;; Loaded by both the Chez and PLT versions.
;;;; (Expects to be loaded from the directory that contains it.)


;======================================  
;(display "Loading main compiler module.  RegionStreams Demo.")
;(newline)

(define-regiment-parameter regiment-version "0.87.2")

(define-regiment-parameter svn-revision
  (and (zero? (system "which svn > /dev/null"))
       (read (open-input-string (system-to-str "svn info | grep Revision | sed s/Revision://")))))

;; This is the global parameter that determines which transformations
;; (passes) the compiler applies and in what order.  We set it here.
(pass-list
  (list
    ;; (1) Type checking comes first, but that happens before these passes
    ;; are run.  Maybe should make it one of the "passes".
    
    ;; (2) Next we verify our input language.
    verify-regiment

    ;; (3) Then we do a little massaging/normalization.
    eta-primitives
    rename-var
    remove-unquoted-constant 

    ;; (4) Then -- here comes the metaprogramming -- we evaluate as much
    ;; of the program as can be evaluated.  The residual had better follow our
    ;; restrictions on implementable Regiment programs.
    static-elaborate

    ;; (5) Now we normalize the residual in a number of ways to
    ;; produce the core query language, then we verify that core.
    reduce-primitives    
    remove-complex-constant  
    uncover-free             
    lift-letrec              
    lift-letrec-body         
    remove-complex-opera*
    verify-core

    ;; (6) Analysis: these passes analyze the query circuit and
    ;; annotate it with various information which may be used in
    ;; "deglobalize" further down the road.  Currently, most of these
    ;; analyses are underdeveloped; more work is warranted.
    classify-names
    add-heartbeats
    add-control-flow
    add-data-flow
    resolve-fold-trees
    add-places      ;; UNNECESSARY CURRENTLY
;    add-routing
    analyze-places

    ;; (7) Finally, the core of the Regiment compiler.  Convert a
    ;; Regiment query into a (albeit high level) node-level
    ;; Token Machine program.
    deglobalize

    ;; (8) There's a large gap from the high-level (human readable) TM
    ;; language and the low-level (actually implemented) TM language.
    ;; Cleanup-token-machine does a lot of the work of desugaring.
    cleanup-token-machine 
    desugar-macros		
;    cleanup-token-machine   ;; TEMP: FIXME

    ;; (9) The next major step is desugaring the gradient
    ;; communication constructs used in TML.  
    find-emittoks
    desugar-gradients
    cleanup-token-machine   ;; Rerun to expand out some stuff.
    ;    analyze-tokmac-recursion    

    ;; (10) Then we desugar another construct: "let-stored".
    desugar-let-stored
    rename-stored

    
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

)

(define-id-syntax t1 (begin (close-graphics) b2))
;(define (t2) (parameterize ((simalpha-realtime-mode #f)) (eval (caddr (list-ref (maintest 'get) 60)))))
(define-id-syntax t2 (load-regiment "demos/regiment/nested_regions.rs"))
(define-id-syntax t3 (load-regiment "demos/regiment/simple/events.rs"))
(define-id-syntax t3b (load-regiment "demos/regiment/simple/union.rs"))

(define-id-syntax t4 (load-regiment "demos/regiment/static_elab.rs"))
(define-id-syntax t5 (load-regiment "demos/regiment/anchor_free_localization.rs"))
(define-id-syntax t5b (load-regiment "demos/token_machs/anchor_free_localization.tm"))
(define-id-syntax t6 (load-regiment "demos/regiment/tracking.rs"))

(define-id-syntax t7a (load-regiment "demos/firelightning/simple_lightup.tm"))
(define-id-syntax t7b (load-regiment "demos/firelightning/lightup_video.tm"))
;(define-id-syntax t7 (load-regiment "demos/firelightning/manual_nbrhood.tm"))
;(define-id-syntax t8 (load-regiment "demos/firelightning/nbrhood_alarm0.rs"))
;(define-id-syntax t8 (load-regiment "demos/firelightning/nbrhood_alarm1.rs"))

(define-id-syntax t7 (load-regiment "../analysis/firelightning/prog1.rs" 'verbose))
(define-id-syntax t8 (load-regiment "../analysis/firelightning/prog2.rs" 'verbose))
(define-id-syntax t9 (load-regiment "../analysis/firelightning/prog2_manual.tm" 'verbose))
(define-id-syntax t10 (load-regiment "../analysis/firelightning/prog2_manual_batchopt.tm" 'verbose))
(define-id-syntax t11 (load-regiment "../analysis/firelightning/test.tm" 'verbose))

(define-id-syntax tb (load-regiment "demos/regiment/bug.tm" ))

;(define-id-syntax t9 (load-regiment "demos/firelightning/deadsimple_alarm.rs"))
;(define-id-syntax t9b (load-regiment "demos/firelightning/deadsimple_alarm.tm"))

(define-id-syntax d1 (load-regiment "demos/regiment/simple_fold.rs"))
(define-id-syntax d2 (load-regiment "demos/regiment/average_temperature2.rs"))
(define-id-syntax d3 (load-regiment "demos/regiment/simple_events.rs"))
(define-id-syntax d4 (load-regiment "demos/regiment/smap2_two_anchors.rs"))
(define-id-syntax d5 (load-regiment "demos/regiment/nested_regions.rs"))
(define-id-syntax d6 (load-regiment "demos/regiment/static_elab.rs"))


(define-id-syntax tn (begin (simalpha-realtime-mode #t) (rerun-simulator-alpha 'use-stale-world)))

(define-id-syntax t  ;; uber shorthand
  (let ([start (begin (collect (collect-maximum-generation))
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


(define-id-syntax mem  ;; shorthand
  (begin (collect (collect-maximum-generation))
	 (let ([stats (statistics)])
	   (printf "Approx dynamic mem usage: ~:d\n" (- (sstats-bytes stats) (sstats-gc-bytes stats))))))
