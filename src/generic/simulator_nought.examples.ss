
(define tm0
  '(program
    (bindings)
    (socpgm  (bindings) (emit tok1))
    (nodepgm 
     (tokens
      [tok1 () (light-up 0 255 0) (relay)]
     (startup)))))

(define example-nodal-prog0
  '(program
    (bindings)
    (socpgm  (bindings) (emit tok1))
    (nodepgm 
     (tokens
      [tok1 () (call tok2 3)
   	       (relay)]
      [tok2 (x) (+ x x)])
     (startup))))

;; This simplest of programs just lights up all the nodes.
;; Really this only makes sense on the graphical simulator.
;; See simulator_nought_graphics.
(define example-nodal-prog1
  '(program
    (bindings )
    (socpgm (bindings ) 
	    (emit tok1))
    (nodepgm
;       result_2
       (tokens
	[tok1 () (flood tok2)]
	[tok2 () (light-up 0 255 0)])
       (startup ) ;; seed tokens
       )))
;; [2004.06.03] TODO BUG.  Sometimes it doesn't flood the whole
;; network and only a few turn green!


;; This is slightly more complex and actually propogates the light
;; messages through the network via relays.]
(define example-nodal-prog2
  '(program
    (bindings)
    (socpgm (bindings) 
	    (emit tok1))
    (nodepgm
;       result_2
       (tokens
	[tok1 () (call tok2) (relay)]
	[tok2 () (light-up 0 255 0)])
       (startup ) ;; seed tokens
       )))

;; This one spreads lights, and prints the distances as it goes.
(define example-nodal-prog3
  '(program
    (bindings )
    (socpgm (bindings ) 
	    (emit tok1))
    (nodepgm	
     (tokens
      [tok1 () 
	    (call tok2)
	    (let ((x (sense_light)))
	      ...)
	    (printf " ~s " (dist))
	    (relay)]
      [tok2 () (light-up 0 255 0)])
     (startup ) ;; seed tokens
     )))

;; This returns the distances between each node and the root.  It
;; spreads a gradient and returns values back.  NO AGGREGATION.
(define example-nodal-prog4
  '(program
    (bindings )
    (socpgm (bindings ) 
;	    (tokens [tok1_return (v)
;		       (disp "Got return!" v)]
	    (disp "SOC Running" (node-id (simobject-node this)))
	    (emit tok1))
    (nodepgm
     (tokens
      [tok1_return (v)
		   ;(disp "Got return!" v)
		   (printf "~s." v)
		   (soc-return v)]
      [tok1 ()
	    (display #\-)
;	    (disp "tok1" (node-id (simobject-node this)))
	    (call tok2)
	    (relay)
	    (return (dist)
		    (to tok1_return)
		    (via tok1)
		    )]
      [tok2 () 
	    (display #\_)
;	    (disp "tok2" (node-id (simobject-node this)))
	    (light-up 0 255 0)])
     (startup ) ;; seed tokens
     )))

;; Now with aggregation
(define example-nodal-prog4b
  '(program
    (bindings )
    (socpgm (bindings ) 
	    (crit-printf "[~s]" (node-id (simobject-node this)))
	    (emit tok1))
    (nodepgm
     (tokens
      [tokret (v) 
	      (crit-printf "<~s>" (node-id (simobject-node this)))
	      (soc-return v)]
      ;; This will return as fast as it goes out in a wave.
      ;; 
      [tok1 () 
	    ;(call tok2) 
	    (relay)
	    (return (dist)
		    (to tokret) (via tok1)
		    (seed 0)  (aggr plus) )]
      [plus (x y) 
;	    (disp "(" x "," y ")")
	    (crit-printf "(~s,~s) " x y)
	    (if (not (and (integer? x) (integer? y)))
		(error 'test "NOT BOTH INTEGERS: ~s ~s~n" x y))
	    (+ x y)]
      [tok2 () (light-up 0 255 0)])
     (startup ) ;; seed tokens
     )))

;; This was produced by my compiler.  It's a compilation of the
;; trivial anchor program.
(define example-nodal-prog5
  '(program
    (bindings (tmp_3 (cons '2 '())) (tmp_1 (cons '1 tmp_3)))
    (socpgm (bindings) (call f_token_result_2))
    (nodepgm
     (tokens
      (f_token_result_2 () (flood token_5))
      (token_5
           ()
           (if (< (locdiff (loc) tmp_1) 10.0)
               (elect-leader m_token_result_2)))
      (m_token_result_2 (soc-return (this))))
     (startup))))


;; This tests the timed-call facility.
(define example-nodal-prog6
  '(program
    (bindings )
    (socpgm (bindings) (call f 0))
    (nodepgm
     (tokens
      ;(start () (call f 0))
      (f (v) (soc-return v)
	     (timed-call 500 f (add1 v))))
     (startup))))

(define THEPROG
  '(program
     (bindings
       (tmpunknpr_13 (cons '40 '()))
       (tmp_4 (cons '30 tmpunknpr_13)))
     (socpgm (bindings) (call spread-global))
     (nodepgm
       (tokens
         (f_token_tmpanch_8 () (flood constok_16))
         (constok_16
           ()
           (if (< (locdiff (loc) tmp_4) 10.0)
               (elect-leader m_token_tmpanch_8)
               '#f))
;         (f_token_tmpanch_8 () (draw-mark tmp_4 (rgb 0 100 100)))
;         (m_token_tmpanch_8 () (light-up 0 255 255))
         (m_token_tmpanch_8 () (call f_token_tmpkhood_9))
         (f_token_tmpkhood_9 () (emit m_token_tmpkhood_9))
;         (m_token_tmpkhood_9 () (light-up 0 100 100))
         (m_token_tmpkhood_9 () (if (< (dist) '10) (relay)))
         (tmpfunc_10
           (a_1)
           (lazy-letrec ((result_5 (local-sense))) result_5))
         (m_token_tmpkhood_9 () (activate f_token_tmpunknpr_11))
         (f_token_tmpunknpr_11
           ()
           (call m_token_tmpunknpr_11 (call tmpfunc_10 this))
           (timed-call 10.0 f_token_tmpunknpr_11))
         (tmpfunc_12
           (a_3 b_2)
           (lazy-letrec ((result_6 (+ a_3 b_2))) result_6))
         (m_token_tmpunknpr_11 (v) (call f_token_result_7 v))
         (f_token_result_7
           (v)
           (return
             v
             (to m_token_result_7)
             (via global-tree)
             (seed '0)
             (aggr tmpfunc_12)))
         (m_token_result_7 (v) (soc-return v))
         (leafpulsar_14
           ()
           (call f_token_tmpanch_8)
           (timed-call 1.0 leafpulsar_14))
         (spread-global
           ()
           (emit global-tree)
           (timed-call 1000 spread-global))
         (global-tree () (relay)))
       (startup leafpulsar_14))))

(define example
  '(program
    (bindings (result_2 '3))
    (socpgm (bindings ) (soc-return result_2) (soc-finished))
    (nodepgm (tokens ) (startup ))))

;; This program floods the network with a token, then elects a leader
;; near a point, finally creating a gradient from there.
;;
(define example-nodal-prog99
  '(program
    (socpgm (bindings) (emit result_2))
    (nodepgm
;       result_2
       (bindings (tmp_4 (cons '40 '())) (tmp_1 (cons '30 tmp_4)))
       (tokens
	[f_token_tmp_3 () (flood token_6)]
	[token_6
            ()
            (if (< (locdiff (loc) tmp_1) 10.0)
                (elect-leader m_token_tmp_3))]
	[m_token_tmp_3 () (call f_token_result_2)]
	[f_token_result_2 () (emit m_token_result_2)]
	[m_token_result_2
            ()
            (if (< (dist f_token_result_2) '50) (relay))])
       f_token_tmp_3
       )))


;; List with Socprog and Nodeprog
;; [2004.06.09] -- this got to be too much trouble to maintain...
(define example-nodal-output0 'unspecified
#;  '((lambda (this object-graph all-objs)
      (let ()
	(define token-cache (make-default-hash-table))
	(define neighbors unspecified)
	(define sendmsg unspecified)
	(define emit unspecified)
	(define flood unspecified)
	(let* () (begin (emit 'tok1) 'soc_finished))))
    unspecified))


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
