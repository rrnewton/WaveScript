(tokens

  ;; ======================================================================
  ;; Base code: (Runs only on SOC)

  [SOC-start () (call feed-tree)]
  [feed-tree () 
    (printf "~a~a Launching tree...\n" (pad-width 5 (my-clock)) (pad-width 5 (my-id)))
    (gemit tree) (timed-call 1000 feed-tree)]


  ;; ========================================
  ;; Value catchers.  Receive the results of aggregation:
  
  [depthmeasure (d) 
     (stored [maxd 0])
     (if (> d maxd) (set! maxd d))]

  [catcher (v) 
    (printf "~a~a Catcher, got val at root!: ~a maxdepth: ~a\n" (pad-width 5 (my-clock)) (pad-width 5 (my-id))
	    v (ext-ref depthmeasure maxd))
    ]

  ;; ======================================================================
  ;; Node code:

  [tree () (grelay)]
  [node-start () (call datafeed)]

  [datafeed ()
   (if (token-present? tree)
       (begin 
;	 (printf "~a~a Tree here, returning...\n" (pad-width 5 (my-clock)) (pad-width 5 (my-id)))
	 ;; First, report our depth.
	 (greturn (gdist tree)
		  (to depthmeasure)
		  (via tree)
		  (seed 0)
		  (aggr max_aggr))

	 ;; Additionally, report data.
	 (greturn (list (list (my-clock) (my-clock)) ;; Time span
			(list (my-id))) ;; Value
		  (to catcher)
		  (via tree)
		  (seed '#f);(list (list '0 '0) '()))
		  (aggr buffered-aggr)
		  ))
       (begin 
	; (printf "~a~a Tree not here yet!\n" (pad-width 5 (my-clock)) (pad-width 5 (my-id)))
	 )
       )
   ;; loop
   (timed-call 1000 datafeed)
   ]

  ;; ========================================
  ;; Aggregators:
    
  ;; This is the aggregator function
  [buffered-aggr (x y)
     (stored [buffer (make-vector 5 0)])

     (if (not x) y
	 (if (not y) x
	     (let ([span1 (car x)] [v1 (cadr x)]
		   [span2 (car y)] [v2 (cadr y)])
					;(printf "Aggr: ~a ~a\n" (subcall span-length span1) (subcall span-length span2))
	       (printf "Aggr: ~a.~a ~a.~a   ~a ~a  result: ~a  damn: ~a\n"
		       (car span1)(cadr span1)
		       (car span2)(cadr span2)
		       v1 v2
		       (list (list (min (car span1) (car span2))
				   (cons (cadr span1) (cadr span2))
				   (max (cadr span1) (cadr span2))
				   )
			     (append v1 v2))
		       (max 0 9001)
		       )
	       (return (list (list (min (car span1) (car span2))
				   (max (cadr span1) (cadr span2)))
			     (append v1 v2))
		       ))))
     ]
  [span-length (s) (return (- (cadr s) (car s)))]


  [max_aggr (x y) (return (max x y))]



)

