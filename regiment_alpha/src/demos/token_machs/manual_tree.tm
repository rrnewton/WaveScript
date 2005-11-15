
;; This manually floods down a tree, and then returns back up.
;; Should see correctly staggered timing.
;; With perfect communication the down's should only happen once, 
;; then the ups should happen repeatedly.
`(tokens
 [SOC-start () 
	    (leds on green)
	    (printf "~a: Root spreading...\n" (my-clock))
	    (bcast down (my-id) 1)
	    (timed-call 1000 SOC-start)
	    ]
 [node-start () (call up 0 0) (timed-call 1000 node-start)]
 [down (p h)
       (stored [parent -1] [hops 1000])
       (leds on blue)
       (if (< h hops)
	   (begin 
	     (printf "~a.~a: Down   p:~a  hops:~a\n" (my-clock) (my-id) p h)
	     (set! parent p)
	     (set! hops h)			
	     (bcast down (my-id) (+ hops 1))))
       ]
 [up (dest v)
     (leds on red)
     (if (or (= dest (my-id)) (= dest 0))
	 (if (token-present? down)
	     (if (= (my-id) ,BASE_ID)
		 (begin
		   (printf "~a.~a: Got return: ~a\n" (my-clock) (my-id) v)
		   (soc-return v)
		   )
		 (bcast up (ext-ref down parent) (+ v 1)))
	     (begin 
  	       ;  (printf "Not on tree!"))
	       (printf "."))))
     ]
 )
