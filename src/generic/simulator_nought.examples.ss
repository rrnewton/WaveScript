
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




