
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
;		       (disp "Got return!" v)])
	    (emit tok1))
    (nodepgm
     (tokens
      [tok1_return (v)
		   (disp "Got return!" v)]
      [tok1 ()
	    (call tok2)	    	    
	    (relay)
	    (return (dist))]
      [tok2 () (light-up 0 255 0)])
     (startup ) ;; seed tokens
     )))


(define example
  '(program
    (bindings (result_2 '3))
    (socpgm (bindings ) (soc-return result_2) (finished))
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

