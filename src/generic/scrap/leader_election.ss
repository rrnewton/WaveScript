
;; LEADER ELECTION SHOULD ONLY HAPPEN ONCE.
;; No they should just have VERY slow heartbeats...
;; But how do you do this in a non-stateless way???

;; (elect-leader A)
;; -> (call elect_A)


;; You include some number of expressions to be executed on each
;; potential leader, they
[tok () ...
     (elect A Compr
	    get.x
	    (+ get.y 9))]
;; No, that doesn't make sense, they can just be values to be executed
;; immediately.

     
[Compr (a b)
       (> (+ a b)
	  (+ get.x get.y 9))]


	   




(define example-elect-leader
  '(program
    (bindings )
    (socpgm (bindings ) 
	    (emit tok1))
    (nodepgm
     (tokens
      
      ;; Elect all that fire elect_A: simple version 

      ;; (Well within a certain time window that is:)
      
      [elect_A () 
	       (call compete (my-id))
	       (timed-call 1000 confirm_A)]

      [compete (id)
	       ;; If this is the first call, accept our own id.
	       ;; If the argument beats us, join them.
	       (if (or (= 0 (fire-count)) (< (load.id) id))
		   (begin 
		     (store.id id) 
		     (relay)))]


      ;; A function which confirms, then calls the elected token and
      ;; spawns a suppressor.
      [confirm_call ()
       (if (call confirm)
	   (begin 
	     (call A)
	     ;; If we do succeed, activate a suppressor:
	     (activate suppress_others)
	     ))]
      
      ;; A local function which makes sure we're on top of the heap.
      [confirm ()
       (if (not (token-cache compete)) 
	   #f ;; this should not happen though...
	   (= (token-cache compete id) (my-id)))]
      
      [suppress_others ()
       (if (call confirm)
	   (begin 
	     (emit compete (my-id))
	     (timed-call 10000 suppress_others)))]

      [A () (disp "ELECTED LEADER AT" (my-id))]	    
    )
   (startup elect_A) ;; seed tokens
   )))

;======================================================================
