
(define example-nodal-prog0
  '(program
    (bindings)
    (socpgm  (bindings) (emit tok1))
    (nodepgm (tokens) (startup))))

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
;; messages through the network via relays.
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
	    (printf " ~s " (dist))
	    (relay)]
      [tok2 () (light-up 0 255 0)])
     (startup ) ;; seed tokens
     )))

;; This takes the maximum distance to a leaf.  It spreads a gradient
;; and returns values back.  NO AGGREGATION.
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
	    (printf "<~s>" (node-id (simobject-node this)))
	    (emit tok1))
    (nodepgm
     (tokens
      [tokret (v) (soc-return v)]
      [tok1 () (call tok2) (relay)
	    (return (dist)
		    (to tokret) (via tok1)
		    (seed 0)  (aggr plus) )]
      [plus (x y) (printf "(~s,~s)" x y) (+ x y)]
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

