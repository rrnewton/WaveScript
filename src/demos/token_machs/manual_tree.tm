
;; This manually floods down a tree, and then returns back up.
;; Should see correctly staggered timing.
`(tokens

 [SOC-start () (call launch-down)] 
 [node-start () (timed-call 1000 launch-ups)]

 ;; Start a downward tree two seconds:
 [launch-down ()
	      (leds on green)
	      (printf "~a: Root spreading...\n" (my-clock))
	      (bcast down (my-id) 1)
	      (timed-call 2000 launch-down)]
 [launch-ups ()
	     (call up ,NULL_ID 0)
	     (timed-call 1000 launch-ups)]

 [down (p h)
       (stored [parent -1] [hops 1000])
       (leds on blue)
       (if (< h hops)
	   (begin 
	     (printf "~a,~a: Down   p:~a  hops:~a\n" (my-clock) (my-id) p h)
	     (set! parent p)
	     (set! hops h)			
	     (bcast down (my-id) (+ hops 1))))
       ]
 [up (dest v)
     (leds on red)
     ;; Was this broadcast message to me?
     (if (or (= dest (my-id)) (= dest ,NULL_ID))
	 (if (token-present? down)
	     (if (= (my-id) ,BASE_ID)
		 (begin
		   (printf "~a,~a: Got return: ~a\n" (my-clock) (my-id) v)
		   (soc-return v)
		   )
		 (bcast up (ext-ref down parent) (+ v 1)))
	     (begin 
  	       ;  (printf "Not on tree!"))
	       (printf "."))))
     ]
 )
