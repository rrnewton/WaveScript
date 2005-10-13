
(tokens

  [SOC-start ()
   (printf "foo\n")
   (call feed-tree)
   ]

  [node-start () 
   ;(soc-return (my-id))
   (if (token-present? tree)
       (greturn (list (my-id))
		(to catcher)
		(seed '())
		(aggr f)))
   ]

  [catcher (v) 
   (printf "Got val at root: ~a\n" v)
   ]
  
  [feed-tree () (gemit tree) (timed-call 1000 feed-tree)]
  [tree () (grelay)]
    
  ;; This is the aggregator function
  [f (x y)
     (stored [buffer (make-vector 5 0)])     
     (return (append x y))
     ]
  	   
)