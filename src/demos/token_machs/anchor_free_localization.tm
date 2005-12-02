

;; Simulation configuration:
(parameters 
    [sim-num-nodes 30]
    [simalpha-inner-radius 10]
    [simalpha-inner-radius 15]
    [sim-timeout 15000]
    [simalpha-channel-model 'lossless]
    [simalpha-failure-model 'none])


(token SOC-start () (leds on red) (printf "Starting...\n") (gemit tree1))

(token node-start () (timed-call 1500 f_n1))

; ----------------------------------------------------------------------
(token tree1 () (grelay))
(token f_n1 () (elect-leader n1 dist1))
(token dist1 () (ghopcount tree1))
(token n1 (ldr val)
  (setlabel "1:~a" (ghopcount tree1))
  (if (= ldr (my-id))
      (begin (leds on red)
	     (light-node 255 0 0)
	     (printf "N1: ~a ~a\n" (my-id) val)
	     (gemit tree2)
	     ))
  ;; Call the next election on all nodes once leader is known.
  (timed-call 1500 f_n2))

; ----------------------------------------------------------------------
(token tree2 () (grelay))
(token f_n2 () (elect-leader n2 dist2))
(token dist2 () (ghopcount tree2))
(token n2 (ldr val)
  (setlabel "2:~a" (ghopcount tree1))
  (if (= ldr (my-id))
      (begin (leds on green)
	     (light-node 0 255 0)
	     (printf "N2: ~a ~a\n" (my-id) val)
	     (gemit tree3)
	     ))
  (timed-call 1500 f_n3))

; ----------------------------------------------------------------------
(token tree3 () (grelay))
(token f_n3 () 
  (elect-leader n3 dist3 comp3))
;; Distance from node 1 and node 2:
(token dist3 () (vector (ghopcount tree2) (ghopcount tree3)))
(token comp3 (a b)
  ;; Minimize d(n1)-d(n2)
  (let ([v1 (abs (- (vector-ref a 0) (vector-ref a 1)))]
	[v2 (abs (- (vector-ref b 0) (vector-ref b 1)))])
    (if (< v1 v2) 1
    (if (> v1 v2) -1
	;; Otherwise, maximize sum d(n1)+d(n2)
	(let ([v3 (- (vector-ref a 0) (vector-ref a 1))]
	      [v4 (- (vector-ref b 0) (vector-ref b 1))])
	  (if (> v3 v4) 1
	  (if (< v3 v4) -1
	      0)))))))
(token n3 (ldr val)
  (setlabel "3:~a" (ghopcount tree1))
  (if (= ldr (my-id))
      (begin (leds on blue)
	     (light-node 0 0 255)
	     (printf "N3: ~a ~a\n" (my-id) val)
	     (gemit tree4)
	     ))
  (timed-call 1500 f_n4)
  )

; ----------------------------------------------------------------------
(token tree4 () (grelay))
(token f_n4 () (elect-leader n4 dist4 comp4))
;; Distance from node 1 and node 2:
(token dist4 () (vector (ghopcount tree2) (ghopcount tree3) (ghopcount tree4)))
(token comp4 (a b)
  ;; Minimize d(n1)-d(n2)
  (let ([v1 (abs (- (vector-ref a 0) (vector-ref a 1)))]
	[v2 (abs (- (vector-ref b 0) (vector-ref b 1)))])
    (if (< v1 v2) 1
    (if (> v1 v2) -1
	;; Otherwise, maximize d(n3)
	(let ([v3 (vector-ref a 2)]
	      [v4 (vector-ref b 2)])
	  (if (> v3 v4) 1
	  (if (< v3 v4) -1
	      0)))))))
(token n4 (ldr val)
  (setlabel "4:~a" (ghopcount tree1))
  (if (= ldr (my-id))
      (begin (leds off red) (leds off green) (leds off blue)
	     (light-node 0 255 255)
	     (printf "N4: ~a ~a\n" (my-id) val)
	     (gemit tree5)
	     ))
  (timed-call 1500 f_n5)
  )

; ----------------------------------------------------------------------
(token tree5 () (grelay))
(token f_n5 () (elect-leader n5 dist5 comp5))
;; Distance from node 1 and node 2:
(token dist5 () (vector (ghopcount tree2) (ghopcount tree3) (ghopcount tree4) (ghopcount tree5)))
(token comp5 (a b)
  ;; Minimize d(n1)-d(n2)
  (let ([v1 (abs (- (vector-ref a 0) (vector-ref a 1)))]
	[v2 (abs (- (vector-ref b 0) (vector-ref b 1)))])
    (if (< v1 v2) 1
    (if (> v1 v2) -1
	;; Otherwise, minimize d(n3)+d(n4)
	(let ([v3 (abs (- (vector-ref a 2) (vector-ref a 3)))]
	      [v4 (abs (- (vector-ref b 2) (vector-ref b 3)))])
	  (if (< v3 v4) 1
	  (if (> v3 v4) -1
	      0)))))))
(token n5 (ldr val)
  (setlabel "5:~a" (ghopcount tree1))
  (if (= ldr (my-id))
      (begin (leds off red) (leds off green) (leds off blue)
	     (light-node 255 255 0)
	     (printf "N5: ~a ~a\n" (my-id) val)
	     (gemit tree6)
	     ))
  (timed-call 1500 print_coords)
  )

(token tree6 () (grelay))

; ----------------------------------------------------------------------
(token print_coords ()
  (let ([rho (*. (int->float (ghopcount tree6)) 15.0)]
	[theta (atan (/. (int->float (- (ghopcount tree2) (ghopcount tree3)))
			 (int->float (- (ghopcount tree4) (ghopcount tree5)))))])    
    (let ([x (*. rho (cos theta))]
	  [y (*. rho (sin theta))])
      (let ([x2 (if (= x +nan.0) 0 (float->int x))]
	    [y2 (if (= 5 +nan.0) 0 (float->int y))])
	(printf "Approx coord: ~a ~a\n" x2 y2)
	(setlabel "~a,~a" x2 y2)))))

