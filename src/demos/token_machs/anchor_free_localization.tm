

;; Simulation configuration:
(parameters 
  ;[sim-num-nodes 25]
  [sim-num-nodes 100]  
  [simalpha-max-gridlike-perturbation 0]
  ;[simalpha-outer-radius 17.0]
  [simalpha-outer-radius 8.5]
    [sim-timeout 40000]
    [simalpha-channel-model 'lossless]
    [simalpha-failure-model 'none])

(token SOC-start () ;(leds on red)
       (printf "Starting...\n") (gemit tree1))

(token node-start () (timed-call 1500 f_n1))

; ----------------------------------------------------------------------
(token tree1 () (grelay))
(token f_n1 () (elect-leader n1 dist1))
(token dist1 () (ghopcount tree1))
(token n1 (ldr val)
  (setlabel "1:~a" (ghopcount tree1))
  (if (= ldr (my-id))
      (begin ;(leds on red)
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
      (begin ;(leds on green)
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
      (begin ;(leds on blue)
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
  (timed-call 2000 print_coords)
;  (call orig-loc (loc))
  )

(token tree6 () (grelay))

; (token orig-loc (x)
;   (stored (location #f) (invoked #f))
;   (if (not invoked)
;       (bcast orig-loc x))
;   (set! location x)
;   (set! invoked #t))

; ----------------------------------------------------------------------
; The "preprocessor" evaluates (simalpha-outer-radius) before compiling this program:
(token print_coords ()
  (let* ([rho (*. (int->float (ghopcount tree6)) (exact->inexact (simalpha-outer-radius)))] ;15.0)] ;12.0)]
	 [xdelt (int->float (- (ghopcount tree2) (ghopcount tree3)))]
	 [ydelt (int->float (- (ghopcount tree4) (ghopcount tree5)))]
	 [theta (atan (/. xdelt ydelt))]
	 [x (*. rho (cos theta))]
	 [y (*. rho (sin theta))]
	 ;;[x2 (if (eq? x +nan.0) 0 (float->int x))]
	 ;;[y2 (if (eq? y +nan.0) 0 (float->int y))]
	 [x2 (round-to 1 x)]
	 [y2 (round-to 1 y)]
;	 [relx (- (car  (loc)) (car  (ext-ref orig-loc location)))]
;	 [rely (- (cadr (loc)) (cadr (ext-ref orig-loc location)))]
	 )
    (printf "~a: Approx coord: (~a ~a) real was ~a\n" (my-id) x2 y2 (loc)); relx rely)
    ;(setlabel "~a,~a rad ~a\npos ~a ~a" (float->int xdelt) (float->int ydelt) rho  x2 y2)
    (setlabel "~a,~a" x2 y2)
    ;; Store the coordinates:
    (call coords x2 y2)
    ;; Now we draw a "square" bounded by (-20,-20) (20,20):
    (timed-call 2000 go1)
    (timed-call 4000 go2)
    (timed-call 6000 go3)
    (timed-call 8000 go4)
    ))

(token coords (a b)
  (stored [x #f] [y #f])
  (set! x a)
  (set! y b))

(token go1 () (elect-leader corner1 locdiff1))
(token locdiff1 () (stored [diff #f])
       (let ([score (round-to 1 (-. 0.0 (locdiff '(20 20) (list (ext-ref coords x) (ext-ref coords y)))))])
	 (set! diff score)
	 (if (eqv? score +nan.0) -1000 score)))
(token corner1 (ldr val) (if (= (my-id) ldr) 
			     (begin (leds on green)
				    (setlabel "ONE:~a" (ext-ref locdiff1 diff))
				    (printf "Corner1: ~a \n" (my-id))
				    )))
(token go2 () (elect-leader corner2 locdiff2))
(token locdiff2 () (stored [diff #f])
       (let ([score (round-to 1 (_-. 0.0 (locdiff '(-20 -20) (list (ext-ref coords x) (ext-ref coords y)))))])
	 (set! diff score)
	 (if (eqv? score +nan.0) -1000 score)))
(token corner2 (ldr val) (if (= (my-id) ldr) 
			     (begin (leds on red)
				    (setlabel "TWO:~a" (ext-ref locdiff2 diff))
				    (printf "Corner2: ~a \n" (my-id))
				    )))
(token go3 () (elect-leader corner3 locdiff3))
(token locdiff3 () (stored [diff #f])
       (let ([score (round-to 1 (_-. 0.0 (locdiff '(0 20) (list (ext-ref coords x) (ext-ref coords y)))))])
	 (set! diff score)
	 (if (eqv? score +nan.0) -1000 score)))
(token corner3 (ldr val) (if (= (my-id) ldr) 
			     (begin (leds on red) (leds on green) (leds on blue)
				    (setlabel "THREE:~a" (ext-ref locdiff3 diff))
				    (printf "Corner3: ~a \n" (my-id))
				    )))
(token go4 () (elect-leader corner4 locdiff4))
(token locdiff4 () (stored [diff #f])
       (let ([score (round-to 1 (_-. 0.0 (locdiff '(-20 0) (list (ext-ref coords x) (ext-ref coords y)))))])
	 (set! diff score)
	 (if (eqv? score +nan.0) -1000 score)))
(token corner4 (ldr val) (if (= (my-id) ldr) 
			     (begin (leds off red) (leds off green) (leds off blue)
				    (setlabel "FOUR:~a" (ext-ref locdiff4 diff))
				    (printf "Corner4: ~a \n" (my-id))
				    )))
