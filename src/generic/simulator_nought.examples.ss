
;; Here are some example token machines.

(define tm0
  '(program
    (bindings)
    (socpgm  (bindings) (emit tok1))
    (nodepgm 
     (tokens
      [tok1 () (leds toggle red) (relay) (timed-call 500 tok2)]
      [tok2 () (leds toggle green)]
      )
     (startup))))

;; Works in NesC [2005.01.12]
(define tmfact
  '(program
    (bindings)
    (socpgm  (bindings) (emit tok1))
    (nodepgm 
     (tokens
      [tok1 () (timed-call 300 fact 6)]
      [fact (n) (call loop n 1)]
      [loop (n acc) (if (= 0 n)
			(dbg "TM: FACT DONE: %d\\n" acc)
			(call loop (- n 1) (* n acc)))]
      )
     (startup))))

;; Works in NesC [2005.01.12]
(define tmfactrec
  '(program
    (bindings)
    (socpgm  (bindings) (emit tok1))
    (nodepgm 
     (tokens
      [tok1 () (dbg "TM: FACT Recursive returned: %d\\n" (call fact 6))]
      [fact (n) (if (= 0 n) 
		    1
		    (* n (call fact (- n 1))))]
      )
     (startup))))


(define tm1
  '(program
    (bindings )
    (socpgm (bindings)
	    (dbg "SOC Running %d\\n" (my-id))
	    (emit tok1))
    (nodepgm
     (tokens
      [tok1_return (v)
		   (dbg "Got return! at %d, value %d" (my-id) v)
;		   (soc-return v)
		   ]
      [tok1 () (relay)
	    (return (dist)
		    (to tok1_return)
		    (via tok1)
		    )])
     (startup ) ;; seed tokens
     )))


(define (go)
  (eval (cadr (last testssim)))
  (eval '(define z (map simobject-token-cache all-objs)))
  (eval '(define y (map (lambda (tc) (hashtab-get tc 'tok1)) z)))
  ;(eval '(define x (map node-id (map simobject-node (map msg-object-origin y)))))
  )








;; LEADER ELECTION SHOULD ONLY HAPPEN ONCE.
;; No they should just have VERY slow heartbeats...
;; But how do you do this in a non-stateless way???

;; (elect-leader A)
;; -> (call elect_A)

(define example-elect-leader
  '(program
    (bindings )
    (socpgm (bindings ) 
	    (emit tok1))
    (nodepgm
     (tokens
      
      ;; Elect all that fire A: simple version -- each call to A spreads a gradient
      ;; that either 
      
      ;; Well within a certain time window that is:
      
      [elect_A () 
	       (call compete (my-id))
	       (timed-call 1000 confirm_A)]

      [compete (id)      
       (if (and (token-cache compete)
		(< (token-cache compete id) id))
	   (begin 
	   ; If we've got a reigning champ, and its not us, make sure
           ; that that suppressor is turned off.  This is so that if
           ; leadership transfers to another node, their suppression
           ; engine will send out compete messages which will here
           ; kill our suppression engine:
	   ; (if (not (= id (my-id))) (kill suppress_others))
	     (reject))
	   (relay))
       ]

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

;; Example: voting on remote detection

'(program
 (tokens
  [eventDetected () 
		 (return 1
			 (to vote-collector)
			 (via global-tree)
			 (seed '0)
			 (aggr vote-adder))]
  [vote-adder (x y)
	      (let ((sum (+ x y)))
		(if (> sum threshold)
		    (soc-return 'ALARM))
		sum)]
  [vote-collector (v)
		  (if (> sum threshold)
		      (soc-return 'ALARM))]))


'(program
 (tokens
  [eventDetected () (emit-radius 2 addactivation 1)
		    (call addactivation 1)]
  [addactivaton (x)
		(let ((total (+ x (load:x))))
		  (if (> total threshold)
		      (soc-return 'ALARM))
		  (expire-after 2000))]))


'(program
 (tokens
  [eventDetected () (emit tmp1 1)
		    (call addactivation 1)]
  [tmp1 () (call addactivation 1)
	   (if (< (dist) 2) (relay))]
  [addactivaton (x)
		(let ((total (+ x (load:x))))
		  (if (> total threshold)
		      (return 'ALARM
			      (to soc-receiver)
			      (via global-tree)))
		  (timed-call 2000 tmp2))]
  [tmp2 () (evict addactivaton)]))


;;; Slowly expanding group:

;;; ERK should I have some kind of dynamic return context?
;;; It seems like trees get really screwed up using macros.
;(tokens 
; [start () (spread 1 500)]
; [spread (d t) (emit-radius d)
;	 ]
; [memb () (return 1 ...)
;	 [A_ret (depth)


'(program
 (tokens
  [eventDetected () (emit-radius 2 addactivation 1)
		    (call addactivation 1)]
  [addactivaton (x)
		(let ((total (+ x (load:x))))
		  (if (> total threshold)
		      (soc-return 'ALARM))
		  (deschedule expire)
		  (timed-call 2000 expire))]
  [expire () (evict addactivaton)]))
