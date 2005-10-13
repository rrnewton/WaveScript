(tokens

  ;; ======================================================================
  ;; Base code:

  [SOC-start ()
	     (call feed-tree)
	     ]
  [feed-tree () 
	     (printf "~a~a Launching tree...\n" (pad-width 5 (my-clock)) (pad-width 5 (my-id)))
	     (gemit tree) (timed-call 1000 feed-tree)]

  [depthmeasure (d) 
		(stored [maxd 0])
		(if (> d maxd) (set! maxd d))]

  ;; ======================================================================
  ;; Node code:

  [tree () (grelay)]

  [node-start () 
	      (call datafeed)
	      ]

  [datafeed ()
   (if (token-present? tree)
       (begin 
;	 (printf "~a~a Tree here, returning...\n" (pad-width 5 (my-clock)) (pad-width 5 (my-id)))
	 ;; First, report our depth.
	 (greturn (gdist tree)
		  (to depthmeasure)
		  (via tree)
		  (seed 0)
		  (aggr max))

	 ;; Additionally, report data.
	 (greturn (list (my-clock) (my-id))
		  (to catcher)
		  (via tree)
		  (seed '())
		  (aggr f)
		  ))
       (begin 
	; (printf "~a~a Tree not here yet!\n" (pad-width 5 (my-clock)) (pad-width 5 (my-id)))
	 )
       )
   ;; loop
   (timed-call 1000 datafeed)
   ]

  [catcher (v) 
	   (printf "~a~a Catcher, got val at root!: ~a maxdepth: ~a\n" (pad-width 5 (my-clock)) (pad-width 5 (my-id))
		   v (ext-ref depthmeasure maxd))
   ]
  
    
  ;; This is the aggregator function
  [f (x y)
     (stored [buffer (make-vector 5 0)])
     (return (append x y))
     ]

  [max (x y) (if (< x y) y x)]
)