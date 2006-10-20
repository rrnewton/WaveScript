`( 
    ;; Urg, this is wrong:
    ;    [(deep-assq 'startup (run-compiler '(circle-at '(30 40) 50))) (startup)]
    
    ["Verify that the trivial program produces no token bindings but the defaults"
     (filter (lambda (tokbind)
	       (not (memq (car tokbind) '(spread-global global-tree #|catcher|# reg-return))))
	     (cdr (deep-assq 'tokens (compile-to-tokens '3))))
     ()]

    ["resolve-trees:  Verify that an rfold over a khood gets the khood tree"
     (parameterize ([pass-list (list-remove-after resolve-fold-trees (pass-list))])
       (run-compiler '(rfold + 0 (rmap nodeid (khood (anchor-at 50 10) 2)))))
     ;; The result had better put a khood in for the "tree" property.
     ,(lambda (prog)
	(let ([name (deep-assq 'tree prog)]
	      [dfg (cdr (deep-assq 'data-flow prog))])
	  (match name
	    [(tree ,s)
	     (match (rac (cadr (assq s dfg)))
	       [(khood . ,_) #t]
	       [,else #f])]
	    [,else #f])
	  ))]

    ["resolve-trees:  Verify that an rfold over a khood gets the CORRECT tree"
     (parameterize ([pass-list (list-remove-after resolve-fold-trees (pass-list))])
       (deunique-name (cadr (deep-assq 'tree
		  (run-compiler 
		   '(rfold + 0 
			   (letrec ([myhood (khood (anchor-at 50 10) 2)])
			     (rmap nodeid myhood))))))))
     myhood]

    ["resolve-trees: Verify that nested regions program uses the khood's tree"
     (parameterize ([pass-list (list-remove-after resolve-fold-trees (pass-list))])
       (run-compiler 
	'(letrec ([nodes (light-up
			(rfilter
			 (lambda (n) (or (= (nodeid n) '6) (= (nodeid n) '14)))
                    world))]
		[twohop (lambda (n) (khood (node->anchor n) '2))]
		[nbrhoods (rmap twohop nodes)]
		[ids (rmap (lambda (r) (rmap nodeid r)) nbrhoods)]
		[sums (rmap (lambda (r) (rfold + '0 r)) ids)]
		)
	 ;(rrflatten nbrhoods)
	   sums
	 )))
     ;; The result had better put a khood in for the "tree" property.
     ,(lambda (prog)
	(let ([name (deep-assq 'tree prog)]
	      [dfg (cdr (deep-assq 'data-flow prog))])
	  (match name
	    [(tree ,s)
	     (match (rac (cadr (assq s dfg)))
	       [(khood . ,_) #t]
	       [,else #f])]
	    [,else #f])
	  ))]

    ["resolve-trees:  An unresolvable fold gets #f for the tree"
     (parameterize ([pass-list (list-remove-after resolve-fold-trees (pass-list))])
       (deep-assq 'tree
		  (run-compiler '(rmap (lambda (n)
			      (rfold + 0 
				     (rmap nodeid 
					   (if (even? (nodeid n))
					       (khood (anchor-at 50 10) 2)
					       (khood (anchor-at 10 30) 2)))))
				       world))))
     (tree #f)]

     ["Testing simple combinations of passes: generate a continuation." 
      (let ((toks (cdr (deep-assq 'tokens 
		 (closure-convert 
		  (sever-cont-state 
		   (cps-tokmac (cleanup-token-machine '(tokens (tok1 () (subcall tok2)))))))))))
	(let ((x (remq 'SOC-start (remq 'node-start (remq 'actual-node-start (map car toks))))))
	  ;; This is the continuation that was added:
	  (length x)))
      1]
         
     ;; These test deglobalize2
     ,@(let () 	
	 ;; Irrespective
	 (define (test-deglob2 p) 
	   (parameterize ([pass-list
			   (snoc deglobalize2 (rdc 
		  (list-remove-after deglobalize
				     (list-remove-after deglobalize2
							(pass-list)))))])
	     (run-compiler p 'verbose)))
	 `(
	   ["deglobalize2: Do a dump over world"
	    (,test-deglob2 '(rdump (rmap nodeid world)))
	    ,(match-lambda ((deglobalize2-lang '(program (commdecls . ,decls))))
	       (and (equal? (map car decls) '(DUMP))
		    (eq? 'world (cadr (deep-assq 'VIA decls)))
		    
		    ))]

	   ["deglobalize2: Aggreate nodeids over a single khood"
	    (,test-deglob2 
	     '(rfold + 0 (rmap nodeid (khood (anchor-maximizing nodeid world) 2))))
	    ,(match-lambda ((deglobalize2-lang '(program (commdecls . ,decls))))
	       (and (set-eq? (map car decls) '(ELECT EMIT AGGR))
		    ;; Do we aggregate using the local tree?
		    (substring? "khood" (symbol->string (cadr (deep-assq 'VIA (deep-assq 'AGGR decls)))))
		    ;; Elect plugs into the Emit:
#;
		    (inspect (list (cadr (deep-assq 'OUTPUT (deep-assq 'ELECT decls)))
				   (cadr (deep-assq 'INPUT (deep-assq 'EMIT decls)))))
		    #;
		    (equal? (cadr (deep-assq 'OUTPUT (deep-assq 'ELECT decls)))
			    (cadr (deep-assq 'INPUT (deep-assq 'EMIT decls))))
		    ;; And EMIT plugs into the Aggregation:
		    (match (list (cadr (deep-assq 'OUTPUT (deep-assq 'ELECT decls)))
				 (deep-assq 'REGEVT (deep-assq 'INPUT (deep-assq 'EMIT decls))))
		      [((REGEVT ,rn1 ,e) (REGEVT ,rn2 ,id))
		       (and (symbol? id) (symbol? rn1) (symbol? rn2)
			    (eq? rn1 rn2)
			    )
		       ]
		      [,other 
		       (void)
		       ;(inspect other)
		       ])
		    ))]

#;
	   ["deglobalize2: Now aggregate over a locally formed nested regions"
	    (,test-deglob2 
	     '(rdump (rmap (lambda (r) (rfold + 0 r))
			   (rmap (lambda (r) (rmap nodeid r))
				 (rmap (lambda (n) (khood (node->anchor n) 2))
				       (rfilter (lambda (n) (= (nodeid n) 22))				  
						world))))))
	    ,(match-lambda ((deglobalize2-lang '(program (commdecls . ,decls))))
	       (inspect decls)
	       
	       )
	    ]

#; ;; Curried version.
'(rdump (rmap (rfold + 0)
			   (rmap (rmap nodeid)
				 (rmap (lambda (n) (khood (node->anchor n) 2))
				       (rfilter (lambda (n) (= (nodeid n) 22))				  
						world)))))

	  
	   )
	 )



     
     ;;================================================================================

    ["Simalpha: Now we test running the Simulator Alpha on a very simple token machine."
     (parameterize ([unique-name-counter 0] 
		    [simalpha-dbg-on #f]
		    ;[simalpha-consec-ids #t]
		    [sim-num-nodes 30])
     (let ((prt (open-output-string)))
       (display "(" prt)
       (run-simulator-alpha 
	(cleanup-token-machine 
	 '(tokens (node-start () (display " ") (display (my-id)))))
	'outport prt)
       (display ")" prt)
       (read (open-input-string (get-output-string prt)))))
     ,(lambda (ls) 	
	(set-equal? (list->set ls)
		    (list->set (cons BASE_ID (cdr (iota 30))))))]


    ["Simalpha: run a simple program that floods lights."
     , (tm-to-socvals
	'(tokens
	   [SOC-start () (leds on green) (gemit lightup) (timed-call 1000 base)]
	   [base () (leds on blue)]
	   [lightup () (printf "~s: lightup \n" (my-id))
		    (leds toggle red) (grelay)])
	'[simalpha-placement-type 'gridlike] ;'connected]
	'[sim-num-nodes 30]
	'[simalpha-graphics-on #t])
       unspecified]
    ["Do a basic test of token-handler argument passing."
     , (tm-to-socvals 
	'(tokens (SOC-start () (call f 1 2 3 4 5)) 
		 (f (a b c d e) (soc-return (list a b c d e)))))
       ((1 2 3 4 5))]
    ["Do a basic test of token-handler argument *padding*."
     , (tm-to-socvals 
	'(tokens (SOC-start () (call f 1 2 3))
		 (f (a b c d e) (soc-return (list a b c d e))))
       '[simalpha-zeropad-args 'warning])
       ((1 2 3 0 0))]

    ["Do a basic test of argument evaluation order."
     , (tm-to-list
	'(tokens 
	   (SOC-start () 
		      (printf "~s ~s \n"
			      (begin (printf "~s " 
					     (begin (printf "3 ")
						    2))
				     1)
			      (begin (printf "b ")
				     'a)))))
       (3 2 b 1 a)]

    ["Respect call order."
     , (tm-to-list
	'(tokens 
	      (SOC-start () 
			 (call a)
			 (call b)
			 (call c))
	    (a () (printf "a "))
	    (b () (printf "b "))
	    (c () (printf "c "))
	    ))
      (a b c)]

     ["Check call scheduling order."
      , (tm-to-list
       '(tokens 
	    (SOC-start () 
		       (call a)
		       (call b)
		       ;(sim-print-queue)
		       )
	  (a () (printf "a ") (call a2))
	  (b () (printf "b ") (call b2))
	  (a2 () (printf "a2 "))
	  (b2 () (printf "b2 "))
	  ))
      ,(lambda (x) ;; All of these are semantically valid.
	 (member x '[(a b a2 b2) ;; This is the straightforward FIFO one
		     (a a2 b b2) ;; This one works like a Stack
		     (a b b2 a2)] ;; This one is kinda weird.
		 ))]

     ["Stress generated code for 'call'."
      , (tm-to-list 
	 '(tokens 
	      [SOC-start () (call tok1 (begin (printf "rand\n") (call tok2)))
					;(printf "~a\n" (call tok2))
			 (printf "~a ~a\n" (token-scheduled? tok1) (token-scheduled? tok2))]
	    [tok1 (v) (printf "~a\n" 1)]
	    [tok2 () (printf "~a\n" 2)]))
	(rand #t #t 2 1)]

     ["Stress scheduler a bit more." 
      , (tm-to-socvals
	 '(tokens
	      (SOC-start () 
			 (call (tok t 0))
			 (call (tok t 1))
			 (call (tok t 2))
			 (call (tok t 3)))
	    (t id () (soc-return id)
	       (call g))
	    (g () (soc-return 'g))))
	;; Might erroneously get:
	;(0 3 1 2 g g g g)
	(0 1 2 3 g g g g)]


     ["Timed tokens: test the simulator with timed tokens."
      , (tm-to-list '(tokens 
			 (SOC-start () 
				    (timed-call 200 tok1)
				    (timed-call 100 tok2))
		       (tok1 () (printf "tok1 "))
		       (tok2 () (printf "tok2 ")
			     (timed-call 50 tok3))
		       (tok3 () (printf "tok3 "))))
	(tok2 tok3 tok1)]

     ["Timed tokens 2: test simultaneous timed/untimed calls to same token, then deschedule one timed"
      , (tm-to-list
	 '(tokens 
	      (SOC-start () 
			 (call tok1) 
			 (timed-call 100 tok1)
			 (call tok2)
			 (timed-call 100 tok2))
	    (tok1 () (printf "tok1 ")
		  (token-deschedule tok1))
	    (tok2 () (printf "tok2 "))))
	(tok1 tok2 tok2)]

     ["Timed tokens 3: make two timed calls to the same token."
      (apply < ,(tm-to-list
	'(tokens
	  (SOC-start () (timed-call 100 tok1)
		        (timed-call 200 tok1))
	  (tok1 () (printf "~a \n" (my-clock))))))
      #t]

     ["Timed tokens 4"
      , (tm-to-list
	 '(tokens 
	      (SOC-start () 
			 (timed-call 200 a)
			 (timed-call 100 b)
			 (timed-call  50 c))
	    (a () (printf "a "))
	    (b () (printf "b "))
	    (c () (printf "c "))
	    ))
	(c b a)]

     ["Timed tokens 5"
      , (tm-to-list 
	 '(tokens 
	      (SOC-start () 
			 (timed-call 200 a)
			 (call b)
			      (timed-call  50 c))
	    (a () (printf "a "))
	    (b () (printf "b "))
	    (c () (printf "c "))
	    ))
	(b c a)]

     ["Timed tokens 6: repeated timed-calls looping 10 times."
      , (tm-to-list 
	 '(tokens 
	      (SOC-start () (call f 0))
	      (f (x) (printf "%d\n" x)
		 (timed-call 100 f (+ x 1))))
	 '[sim-timeout 1090])
	(0 1 2 3 4 5 6 7 8 9 10)]

     ["Timed tokens 7: try looping with two different tokens simultaneously."
      , (tm-to-list 
	 '(tokens 
	    (SOC-start () (call s))
	    (node-start () (call n))
	    (s ()
		       (stored (x 0))
		       (printf "%d\n" x)
		       (timed-call 100 s)
		       (set! x (+ x 1)))
	    (n () 
			(printf "n ")
			(timed-call 100 n)))
	 '[sim-timeout 1090]
	 '[sim-num-nodes 2])
	(n n 0 n n 1 n n 2 n n 3 n n 4 n n 5 n n 6 n n 7 n n 8 n n 9 n n 10)]

     ["Clocks"
      (let ((result  , (tm-to-list
		 '(tokens 
		   (SOC-start () (call tok0))
		   (tok0 () (printf "~a " (my-clock))
			 (call tok1))
		   (tok1 () (printf "~a " (my-clock))
			 (timed-call 100 tok2))
		   (tok2 () (printf "~a " (my-clock))
			 (call tok3))
		   (tok3 () (printf "~a " (my-clock)))
		 ))))
	;; RRN: modified this test to be indiffirent to how much time soc-start takes:
	(map (lambda (x) (- x (car result))) result))
      (0 1 101 102)]

     ["Bcast:"
      , (tm-to-list
	 '(tokens 
	      (SOC-start () (printf "~a" (my-clock)) (bcast tok1 3))
	    (tok1 (x) (printf "(~a ~a)" (my-id) (my-clock)))
	    ))
      ,(lambda (results)
	 (let ((SOCSTRT (car results)) ;; This compensates for whatever time is consumed before soc-start runs.
	       (ls (cdr results)))
	   (printf "Testing ~a\n" (+ SOCSTRT RADIO_DELAY SCHEDULE_DELAY))
	 (and (< (length ls) 30) (> (length ls) 1) ;; There should be "some" responses
	      (andmap (lambda (x) (equal? (cadr x) 
					  (+ SOCSTRT RADIO_DELAY SCHEDULE_DELAY)))
		      ls)
	      (not (memq BASE_ID (map car ls))))))]

     ["Bcast to neighbors, Ucast back to base." 
      , (tm-to-list
	 '(tokens
	    (SOC-start () (bcast down (my-id)))
	    (down (num) (ucast num up (my-id)))
	    (up   (num) (printf "(Received-ucast: ~a ~a)\n" num (my-id))))
	 '[simalpha-placement-type 'connected]
	 )
	,(lambda (ls)	  
	   ; Just require a list of IDs:
	   (andmap (lambda (x) 
		     (match x
		       [(Received-ucast: ,id ,base)
			(and (integer? id) (not (= id BASE_ID)) (= base BASE_ID))]))
		   ls))]

     ;; Need to make this one have a higher probability of success.
     ;; [2006.02.27] Just failed with :
     ;; (read "~? at char ~a of ~a" "unexpected end-of-file reading ~a" ("list") 5775 "./_genned_node_code.ss")
     ;; [2006.03.19] Just returned ((picked-child-w-qual: 10) FAIL).
     ;;   Still need to make the probability of success higher.  But this is ok for now, allowing retry.
     ["Now test Ucast-wACK"
      retry
      , (tm-to-list
	 '(tokens
	    ;; First discover a neighbor.
	    ;; Repeat many times to overcome bad connections.
	    (SOC-start () (call downloop 100))
	    (downloop (n) (if (not (= 0 n))
			      (begin (bcast (tok down) (my-id))
				     (call (tok downloop) (- n 1)))))
	    (down (baseid) 
		  (stored [first_heard #t])
		  ;; We only respond if it's a sub-par connection, and
		  ;; if we haven't already gone into an up-loop.
		  (if (and first_heard 
			   (< (linkqual-from baseid) 90))
		      (begin 
			(set! first_heard #f)
			(call (tok uploop) baseid 100))))
	    ;; When we do respond over our less-than-perfect
	    ;; connection, we do it several times.
	    (uploop (base n)
		    (if (not (= 0 n))
			(begin
			  (ucast base (tok back) (my-id))
			  (call (tok uploop) base (- n 1)))))
	    (back (num) 
		  (stored [first_heard #t])
		  ;(printf "(Back ~a) " num)
		  (if first_heard
		      (begin (set! first_heard #f)
			     ;(printf " calltry ")
			     (printf "(picked-child-w-qual: ~a)\n" (linkqual-to num))
			     (call try_ucast num 10))))
	    ;; Then send it a message.
	    (try_ucast (dest count)
		       ;(printf "Trying...~a ~a\n" dest count)
		       (if (= 0 count)
			   (printf "(FAIL)")
			   (if (not (ucast-wack dest (tok yay 0) count))
			       (call try_ucast dest (- count 1))
			       (if (= count 10)
				   ;; Not allowable to succeed on first try, keep going:
				   (call try_ucast dest count)
					;(printf "Yeah that worked\n")
				   )
			       )))
	    (yay (n) (printf "(SUCCEED ~a)" n)))
	 '[sim-num-nodes 30]
	 '[simalpha-placement-type 'gridlike]
	 '[simalpha-channel-model 'linear-disc]
	 '[simalpha-inner-radius 0]
	 '[simalpha-outer-radius 15]
	 '(simalpha-world-xbound 60)
	 '(simalpha-world-ybound 60))
	,(lambda (ls)
	   ;; Did we get something below 10, some try that took more than one try.
	   (not (null?
		 (filter (lambda (n) (not (= n 10)))
		   (map cadr
		     (filter (lambda (x) (eq? (car x) 'SUCCEED)) ls))))))]

     ["Test fast-call"
      , (tm-to-list
	'(tokens 
	   [SOC-start () 
		      (printf "(start: ~s) \n" (my-clock))
		      (call f)
		      (call-fast g)]
	   [f () (printf "(f ~s) \n" (my-clock))]
	   [g () (printf "(g ~s) \n" (my-clock))]))
	,(lambda (x)
	   (equal? (map car x)
		   '(start: g f)))]

     ["Test fast-call #2: two fast calls"
      , (tm-to-list
	'(tokens 
	   [SOC-start () 
		      (printf "(start: ~s) \n" (my-clock))
		      (call-fast g 'a)
		      (call f)
		      (call-fast g 'b)]
	   [f () (printf "(f ~s) \n" (my-clock))]
	   [g (v) (printf "(g ~s ~s) \n" (my-clock) v)]))
	,(lambda (x)
	   (match x
		  [((start: ,_) 
		    (g ,__ a) 
		    (g ,___ b) 
		    (f ,____)) #t]
		  [,_ #f]))]

     ["Test fast-call #3: try a fast and a timed call, which happens first? (should be fast-call)"
      , (tm-to-list
	'(tokens 
	   [SOC-start () 
		      (printf "(start: ~s) \n" (my-clock))
		      (timed-call 0 f)
		      (call-fast g)]
	   [f () (printf "(f ~s) \n" (my-clock))]
	   [g () (printf "(g ~s) \n" (my-clock))]))
	,(lambda (x)
	   (match x
		  [((start: ,_) 
		    (g ,__) 
		    (f ,___)) #t]
		  [,_ #f]))]

     ;; Before I had a simulator bug wherein call-tokens were going to neighbors. 
     ;; (but bcast tokens did not arrive locally)
     ["Test for interference between calls and bcasts. " 
      (filter (lambda (x) (eq? (car x) 'tok3))
	, (tm-to-list
		'(tokens
		  (SOC-start () (printf "(start ~a ~a)" (my-id) (my-clock)) (call (tok tok3 0)) (bcast tok1))
		  (tok1 () (printf " (tok1 ~a) " (my-id)))
		  (tok3 () (printf " (tok3 ~a) " (my-id)))
		  )))
      ((tok3 ,BASE_ID))]

     ["Test the 'activate' form of token-invocation."
      , (tm-to-socvals
	 '(tokens
	      (SOC-start () 
			 (timed-call 100 tok1)
			 (timed-call 50 tok2))
	      (tok1 () (soc-return (vector 'tok1 (my-clock))))
	      (tok2 () (soc-return (vector 'tok2 (my-clock)))
		       (activate tok1)
		       (activate tok3))
	      (tok3 () (soc-return (vector 'tok3 (my-clock))))))
	,(lambda (v)
	   (match v
	     [(#(tok2 ,x) #(tok3 ,y) #(tok1 ,z))
	      (and ;(= (- y x) SCHEDULE_DELAY) ;; This is not necessarily true with closure-convert
	           (< (- y x) RADIO_DELAY) ;; But we should still be able to bound it.
		   (> z 100))]
	     [,else #f]))
	]

     ["Stored: Test to make sure stored vars on different subtokids don't interfere."
      , (tm-to-socvals
	 '(tokens
	    (SOC-start () 
		       (call (tok t 1) 'init 3)
		       (call (tok t 2) 'init 4)
		       (timed-call 500 p))
	    (t (flag v)
	       (stored (s 0))
	       (set! s v))
	    (p ()
	       (soc-return (ext-ref (tok t 1) s))
	       (soc-return (ext-ref (tok t 2) s)))))
	(3 4)]


     ["Stored: Make sure stored rhs's are only executed once and have access to passed arguments."
      , (tm-to-list
	 '(tokens
	    (SOC-start () (call t 99) (call t 100))
	    (t (v)
	       (stored [s (begin (printf "stored-rhs\n") v)])
	       (printf "(~a ~a)\n" v s))))
	(stored-rhs (99 99) (100 99))]
	       
    ["Test reading sensor values in simulation."
     , (tm-to-socvals
	'(tokens
	   (SOC-start () (call loop 100))
	   (loop (reps) (if (> reps 0)
			    (begin (soc-return (list (my-clock) (sync-sense)))
				   (timed-call 100 loop (- reps 1))))))
	'[simalpha-zeropad-args 'warning]
	'[simalpha-sense-function-constructor sense-sine-wave])
       ,(lambda (ls) 
	  (let ((vals (map cadr ls)))
	    ;; Make sure our sin wave goes from 0 to 255:
	    (and (<= (apply min vals) 10)
		 (>= (apply max vals) 245))))]

     ["Token present?"
      , (tm-to-list
		 '(tokens 		   
		   (SOC-start () 
			      (printf "first: ~a" (token-present? (tok tok1 0)))
			      ;(call tok1)
			      (timed-call 200 tok2)
			      (timed-call 100 tok1)
			      )
		   (tok1 () (printf "tok1 "))
		   (tok2 () (printf "second: ~a" (token-present? (tok tok1 0))))
		 ))
	(first: #f tok1 second: #t)]

     ["Another Token-present? test"
      , (tm-to-list
		 '(tokens 		   
		   (SOC-start () 
			      (printf "first: ~a" (token-present? (tok tok1 0)))
			      (timed-call 200 tok2)
			      (call tok1)
			      (timed-call 100 tok2)
			      )
		   (tok1 () (printf "tok1 "))
		   (tok2 () (printf "second: ~a" (token-present? (tok tok1 0))))
		 ))
	(first: #f tok1 second: #t second: #t)]

     ["Token Evict"
      , (tm-to-list
		 '(tokens 		   
		   (SOC-start () 
			      (call check)
			      (call (tok tok1 0))
			      (call check)
			      (call kickout)
			      (call check))
		   (tok1 () (printf "tok1 "))
		   (check () (printf "~a" (token-present? (tok tok1 0))))
		   (kickout () (evict (tok tok1 0)))
		   ))
	(#f tok1 #t #f)]
     ["Token Evict-All"
      , (tm-to-list
		 '(tokens 		   
		   (SOC-start () 
			      (call check)
			      (call (tok tok1 1))
			      (call check)
			      (call (tok tok1 2))
			      (call check)
			      (call kickout)
			      (call check))
		   (tok1 id () (printf "(tok1 ~a) " id))
		   (check () (printf "(~a ~a) " 
				     (token-present? (tok tok1 1))
				     (token-present? (tok tok1 2))
				     ))
		   (kickout () (evict-all (tok tok1 0))))
		 '(sim-num-nodes 2))
	((#f #f) (tok1 1) (#t #f) (tok1 2) (#t #t) (#f #f))]

     ["token-scheduled? test"
      , (tm-to-list
		'(tokens
		  (SOC-start () 
			     (call tok1 '10)
			     (if (token-scheduled? tok1)
				 (display "yes ")
				 (display "no "))
			     (if (token-scheduled? tok2)
				 (display "yes ")
				 (display "no "))
			     (call tok2 11))
		  (tok1 (x) (void))
		  (tok2 (x) (void))
		  ))
	(yes no)]

     ["token-deschedule test"
      , (tm-to-list
		'(tokens
		  (SOC-start () 
			     (call tok1 '10)
			     (if (token-scheduled? tok1)
				 (display "yes ")
				 (display "no "))
			     (token-deschedule tok1)
			     (if (token-scheduled? tok1)
				 (display "yes ")
				 (display "no "))
			     (token-deschedule tok1)
			     (token-deschedule tok1)
			     (token-deschedule tok1)
			     (if (token-scheduled? tok1)
				 (display "yes ")
				 (display "no "))
			     (call tok1 '10)
			     (if (token-scheduled? tok1)
				 (display "yes ")
				 (display "no "))
			     (if (token-scheduled? tok1)
				 (display "yes ")
				 (display "no "))
			     )
		  (tok1 (x) (void))
		  ))
	(yes no no yes yes)]


     ["Token Timed-Schedule/Deschedule test"
      , (tm-to-list
		'(tokens
		  (SOC-start () 
			     (if (token-scheduled? (tok tok1 0)) (display "yes ") (display "no "))
			     (timed-call 100 tok1 '10)
			     (if (token-scheduled? (tok tok1 0)) (display "yes ") (display "no "))
			     (token-deschedule tok1)
			     (if (token-scheduled? (tok tok1 0)) (display "yes ") (display "no "))
			     (timed-call 100 tok1 '10)
			     (timed-call 200 tok1 '10)
			     (token-deschedule tok1)
			     (if (token-scheduled? (tok tok1 0)) (display "yes ") (display "no "))
			     (call tok1 '10)
			     (call tok1 '10)
			     (timed-call 200 tok1 '10)
			     (timed-call 100 tok1 '10)
			     (token-deschedule tok1)
			     (if (token-scheduled? (tok tok1 0)) (display "yes ") (display "no "))
			     )
		  (tok1 (x) (void))
		  ))
	(no yes no no no)]

     ;; This one will cause token-scheduled? to have to look in the timed-tok buf:
     ["Another Token Scheduled?"
      , (tm-to-list
		 '(tokens 		   
		   (SOC-start () 
			      (printf "~a " (token-scheduled? (tok tok1 0)))
			      (timed-call 500 (tok tok1 0))
			      (timed-call 100 check)
			      (timed-call 800 check))
		   (tok1 () (printf "tok1 "))
		   (check () (printf "~a " (token-scheduled? (tok tok1 0))))
		 ))
	(#f #t tok1 #f)]


     ["LinkQual: test measurement of link quality."
      retry ;; Retry until we get some sub-perfect links.
      , (tm-to-list
	 `(tokens
	    (SOC-start () (bcast tok1 (my-id)))
	    (tok1 (base_id) 
		  (printf "(~a ~a ~a)\n" (my-id)
			  (linkqual-from base_id)  ;; down
			  (linkqual-to base_id)))) ;; up
	 '[simalpha-channel-model  'linear-disc]
	 '[simalpha-placement-type 'gridlike]
	 '[sim-num-nodes 30]
	 '[simalpha-failure-model  'none]
	 '[simalpha-consec-ids #f])
	,(lambda (ls)
	   (let ([hit_hundred #f]
		 [hit_subhundred #f])
	     (and 
	      (andmap
	       (lambda (entry)
		 (match entry
		   [(,id ,down ,up)
		    (if (equal? down 100) (set! hit_hundred #t))
		    (if (< down 100) (set! hit_subhundred #t))
		    (and (integer? down) (<= down 100) (>= down 0)
			 (integer? up) (<= up 100) (>= up 0))]))
	       ls)
	      hit_hundred
	      hit_subhundred)))]

     ["Subcall: Try token-scheduled? with some subcall trickery"
      , (tm-to-list
	 '(tokens 		   
	      (SOC-start ()
			 ;; Use subcall to call it immediately,befoe the rest proceeds.
			 (subcall check)
			 (timed-call 500 (tok tok1 0))
			 (timed-call 100 check)
			 (timed-call 800 check))
	    (tok1 () (printf "tok1 "))
	    (check () (printf "~a " (token-scheduled? (tok tok1 0))))
	    ))
      (#f #t tok1 #f)]

     ["Subcall: Make sure simulator can handle subcalls directly if need be."
      (parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
	  (let ([prog (cleanup-token-machine
		       '(tokens
			  (SOC-start () (printf "a ") (subcall tok1) (printf "c ")
				     (call tok2) (printf "d "))
			  (tok1 () (printf "b "))
			  (tok2 () (printf "e "))))])
	    (let ([prt (open-output-string)])
	      (display "(" prt)
	      (let ([result (run-simulator-alpha prog 'outport prt)])
		(display ")" prt)
		(read (open-input-string (get-output-string prt)))))))
      (a b c d e)]

     ["Subcall: Make sure simulator can handle subcalls directly if need be #2."
      , (tm-to-list
		 '(tokens 		   
		   (SOC-start () (printf "~a " (subcall tok1)))
		   (tok1 () (return 349))
		 ))
	(349)]
     ["Subcall: make the stack three deep."
      , (tm-to-socvals
	 '(tokens
	    (SOC-start () (soc-return (cons 1 (subcall f))))
	    (f () (cons 2 (subcall g)))
	    (g () (cons 3 (subcall h)))
	    (h () (return (cons 4 '())))))
	((1 2 3 4))]
     ["Subcall: Test for sanity wrt to subcall ordering reads/writes."
      , (tm-to-list
	 '(tokens
	    (SOC-start () (printf " (before ~s) \n" (ext-ref storage foo))
		       (subcall storage)
		       (printf " (after ~s) \n" (ext-ref storage foo)))
	    (storage () (stored [foo 'uninit])
		     (set! foo 'yay))))
	((before #f) 
	 (after yay))]
     ["Subcall: combine both the prior two tests make a deep stack and then mess with state."
      , (tm-to-socvals
	 '(tokens
	    (SOC-start () (soc-return (let ((v (subcall f)))
					(cons (ext-ref f foo) v))))
	    (f () (stored (foo 'uninit))
	       (begin 
		 (let ((ret (cons 2 (subcall g))))
		   (set! foo 'yay)
		   ret)))
	    (g () (cons 3 ()))))
	((yay 2 3))]
     ["Subcall: Now make that even a little harder."
      , (tm-to-socvals
	 '(tokens
	    (SOC-start () (soc-return (let ((v (subcall f)))
					(cons (ext-ref f foo) 
					      (cons (ext-ref f bar)
						    v)))))
	    (f () (stored (foo 'uninit) (bar 'baruninit))
	       (begin 
		 (set! bar 'hmm)
		 (let ((ret (cons 2 (subcall g))))
		   (set! foo 'yay)
		   ret)))
	    (g () (cons 3 ())))
	 )
	((yay hmm 2 3))]
     ["Subcall: this is a simple form of a cps bug."
      , (tm-to-socvals
	 '(tokens
	    (SOC-start () (soc-return (subcall f)))
	    (f () 
	       (begin 
		 (printf " a ")
		 (subcall g)))
	    (g () (cons 3 ()))))
	((3))]


;      ["Subcall: combine both the prior two tests make a deep stack and then mess with state."
;       , (tm-to-socvals
; 	 '(tokens
; 	    (SOC-start () (soc-return (let ((v (subcall f)))
; 					(cons (ext-ref f foo) v))))
; 	    (f () (stored (foo 'uninit))
; 	       (begin 
; 		 (let ((ret (cons 2 (subcall g))))
; 		   (set! foo 'yay)
; 		   ret)))
; 	    (g () (cons 3 ()))))
; 	((1 2 3 4))]



     ;; FIXME: Add better oracle
     ["Testing sim: 'manually' propogate a flood"
      ; For this unit test we simply run this long enough to get one round of returns back.
      (load-regiment (++ (REGIMENTD) "/src/demos/token_machs/manual_tree.tm")
		     '[sim-timeout 1500])
      unspecified]


;      ["Testing sim: 'manually' propogate a flood"
;       (parameterize ([unique-name-counter 0] [simalpha-dbg-on #t])
; 	 (let ((prog 
; 		(run-compiler
; 		 `(tokens
; 		   [SOC-start () 
; 			      (printf "~a: Root spreading...\n" (my-clock))
; 			      (bcast down (my-id) 1)
; 			      (timed-call 1000 SOC-start)
; 			      ]
; 		   [down (p h)
; 			 (stored [parent -1] [hops 1000])
; 			 (if (< h hops)
; 			     (begin 
; 			       (printf "~a.~a: Down   p:~a  hops:~a\n" (my-clock) (my-id) p h)
; 			       (set! parent p)
; 			       (set! hops h)			
; 			       (bcast down (my-id) (+ hops 1))))
; 			 ]
; 		   [up (dest v)
; 					;(if (not (token-present? down))
; 					;  (printf "Not on tree!"))
; 		       (if (= dest (my-id))
; 			   (if (token-present? down)
; 			       (if (= (my-id) ,BASE_ID)
; 				   (printf "Got return: ~a\n" v)
; 				   (bcast up (ext-ref down parent) (+ v 1)))))
; 		       ])
		 
; 		 )



     
    ;; [2005.05.29] Note tok1 should be statically called and is currently called dynamically!
    ;; Oh duh, that's because all calls go through the dyndispatch table.
     ;; Works with convert closure:
     ["Subcall: Run simulator on simple subcall program." 
      , (tm-to-list '(tokens 
		 (SOC-start () (printf "result ~a" (subcall tok1 3)))
		 (tok1 (x) (return (+ x 300)))
		 ))
	(result 303)]
     
     ,@(let ([commontest 
	      '(parameterize ([unique-name-counter 0] 
			      [simalpha-dbg-on #f]
			      [simulation-logger #f] ;; Disable logging, continuation closures cant' be logged!
			      [simalpha-zeropad-args 'warning])
		 (let ((prog 
		     (run-compiler
		      '(tokens 
			(SOC-start () (printf "result ~a" (+ (subcall tok1 4) (subcall tok1 3))))
			(tok1 (x) (return (+ x 1000)))
			))))
		(let ((prt (open-output-string)))
		  (display "(" prt)
		  (run-simulator-alpha prog 'outport prt)
		  (display ")" prt)
		  (read (open-input-string (get-output-string prt))))))])
	 `(["Add two subcalls (only through cps-tokmac)"
	    (parameterize ((pass-list (list cleanup-token-machine cps-tokmac )))
	      ,commontest)
	    (result 2007)]
	   ["Same test but now with closure-convert"
	    (parameterize ((pass-list (list cleanup-token-machine cps-tokmac closure-convert cleanup-token-machine)))
	      ,commontest)
	    (result 2007)]))

     ["Direct-subcall: simple test of direct subcall."
      , (tm-to-socvals
	 '(tokens
	    (SOC-start () (call tok1))
	    (tok1 () (stored (x 3))
		  (direct-subcall tok2)
		  (soc-return x))
	    (tok2 ()
		  (ext-set! tok1 x (+ 1 (ext-ref tok1 x)))))
	 '[sim-num-nodes 1])
	(4)]
     ["Direct-subcall: should not be allowed for unknown token target."
      , (tm-to-socvals
	 '(tokens
	    (tok1 (v) (direct-subcall v))))
	error]

     ["Stored vars: Now use a stored var."
      , (tm-to-list
		 '(tokens 
		   (SOC-start () 
			      (call tok1 99) 
			      (call tok1 100)
			      (call tok1 101)
			      (call tok1 999))
		   (tok1 (x) (stored (y 3))
			 (printf "~a " y)
			 (set! y x))))
      (3 99 100 101)]

     ["Stored vars: Now many stored vars."
      , (tm-to-list
		 '(tokens 
		   (SOC-start () 			      
			      (call tok1 1 2 3)
			      (call tok2))
		   (tok1 (a b c) (stored (x 0) (y 0) (z 0))
			 (set! x a)
			 (set! y b)
			 (set! z c))
		   (tok2 () 
			 (printf "(~a ~a ~a)" (ext-ref tok1 x) (ext-ref tok1 y) (ext-ref tok1 z))
			 (ext-set! tok1 y 10)
			 (printf "(~a ~a ~a)" (ext-ref tok1 x) (ext-ref tok1 y) (ext-ref tok1 z))
			 (ext-set! tok1 z 10)
			 (printf "(~a ~a ~a)" (ext-ref tok1 x) (ext-ref tok1 y) (ext-ref tok1 z))
			 (ext-set! tok1 x 10)
			 (printf "(~a ~a ~a)" (ext-ref tok1 x) (ext-ref tok1 y) (ext-ref tok1 z))
			 )))
	((1 2 3) (1 10 3) (1 10 10) (10 10 10))]

     ["Stored vars: Initializations should not be able to reference eachother."
      , (tm-to-list
	'(tokens 
	  (SOC-start () (call tok1) (call tok1) (call tok1))
	  (tok1 () 
		(stored [a (begin (printf "a ") 3)]
			[b (begin (printf "b ") (+ 1 a))])
		(printf "~a " (+ a b)))))
	error]

     ;; [2005.11.09] This jus mutated top-level binding "id"!  Weird.
     ["Now keep stored vars on all 1-hop neighbors, make sure they don't change."  
      , (tm-to-list
	'(tokens 
	  (SOC-start () 			      
		     (bcast tok1)
		     (timed-call 100 tok2)
		     (printf "\n")
		     (timed-call 200 tok2)
		     )
	  (tok2 () (bcast tok1))
	  (tok1 () (stored [count 0])
		(let-stored ([id (my-id)])
		(if (= count 0)
		    (begin
		      (printf "fst ~a ~a \n" (my-id) id)
		      (set! count 1))
		    (printf "snd ~a ~a \n" (my-id) id))))))
      unspecified]
	


     ;; Ok before I was having problems with how I do the counters for
     ;; subtok indices of the continuations.  This double invocation tests that:
     ["Subcall: Test double invocation of a continuation-bearing token." 
      , (tm-to-list
		 '(tokens
		   (SOC-start () (printf "SOCSTART~n")
			      (call tok1 55)
			      (call tok1 66))
		   (tok1 (x) (printf " ~a " (+ x (subcall tok2))))
		   (tok2 () (return 3))))
      (SOCSTART 58 69)]

     ["Cps bug: currently cps is generating non-tail kcall's"
      (cps-tokmac (cleanup-token-machine '(tokens (buffered-aggr
           subtok_ind
           (x y)
           (stored (buffer_26 (make-vector '5 '0)))
           (if (not x)
               (return y)
               (if (not y)
                   (return x)
		   (begin
		     (let ([span1 (car x)])
		       (let ([v1 (cadr x)])
			   (return
			    (list v1)))))))))))
      , (lambda (x)
	  (> 2 (length (deep-assq-all 'kcall x))))]




     ;; This doesn't work after closure-convert because while it does
     ;; maintain orderings of local invocations, that does not include
     ;; subsequent continuation invocations of those local
     ;; invocations...
     ;;   Ok, now it works because I changed the CPS algorithm not to
     ;;   introduce a continuation in this case.
     ,@(let ((common 
	     '(parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
	      (let ((prog 
		(run-compiler
		 '(tokens 
		   (SOC-start () 
			      (call tok1 1)
			      (call tok1 0)
			      (call tok1 1)
			      (call tok1 0)
			      )
		   (tok1 (x) 
;			 (printf "_ ")
			 (if (= x 0)
			     (let-stored ((y (begin (printf "a") 3))) (printf "b ") y)
			     (printf "c "))))
		   )))
		(let ((prt (open-output-string)))
		  (display "(" prt)
		  (run-simulator-alpha prog 'outport prt)
		  (display ")" prt)
		  (read (open-input-string (get-output-string prt))))))))	
	 `(["Now use let-stored:"
	    (parameterize ((pass-list (list cleanup-token-machine 
				      desugar-let-stored  rename-stored
				      cps-tokmac      )))
	      ,common)	   
	    (c ab c b)]
	   ["Same test but with closure-convert."
	    (parameterize ((pass-list (list cleanup-token-machine 
				      desugar-let-stored  rename-stored
				      cps-tokmac sever-cont-state closure-convert   
				      )))
	      ,common)
	    ,(lambda (x)
	      (member x
	        '((c ab c b)
		  (c ac b b) ;; FIXME: With closure convert it's currently producing this ordering.
		  )))
	    ]))


     ,@(let ((common 
	      '(parameterize ((unique-name-counter 0)
			      (simalpha-dbg-on #f)
			      [simalpha-zeropad-args 'warning])
	      (let ((prog 
		(run-compiler
		 '(tokens 
		   (SOC-start () 
			      (call tok1 1)
			      (call tok1 0)
			      (call tok1 1)
			      (call tok1 0)
			      )
		   (tok1 (x) 
			 (printf "_ ")
			 (if (= x 0)
			     (let-stored ((y (begin (printf "a") 3))) (printf "b ") y)
			     (begin (subcall tok2) (printf "c "))))
		   (tok2 () (return 3)))
		   )))
		(let ((prt (open-output-string)))
		  (display "(" prt)
		  (run-simulator-alpha prog 'outport prt)
		  (display ")" prt)
		  (read (open-input-string (get-output-string prt))))))))
	 `(["This time I force a continutaion by using subcall.  Thus the c's are delayed."
	   (parameterize ((pass-list (list cleanup-token-machine 
				     desugar-let-stored  rename-stored
				     cps-tokmac ))
			  ;; Turn off fasl writing because we can't write procedures (continuations):
			  [simulation-logger-fasl-batched #f])
	     ,common)
	   ;(_ _ ab _ _ b c c) ;; [2005.10.31] Enabled call-fast for cps'd code:
	   (_ c _ ab _ c _ b)
	   ]
	   ["And one last time using subcall and closure convert."
	    (parameterize ((pass-list (list cleanup-token-machine 
				      desugar-let-stored  rename-stored
				      cps-tokmac sever-cont-state closure-convert cleanup-token-machine)))
	      ,common)
	    ;(_ _ ab _ b _ c c) ;; [2005.10.31] Enabled call-fast for cps'd code.
	    ;(_ _ a_ _ c b c b) ;; [2005.11.03] Currently let's can generate continuations.
	    (_ _ ab _ _ b c c) ;; [2005.11.03] Ok, but I put in a hack in cps-tokmac that makes it a little better.
	    ]))

     ;; FIXME BUG: I got an error on this test when running all units.
     ;; Unfortunately, it was not repeatable, and I don't know what the error was.
     ;; I should modify the unit tester to save output for erroneous tests. [2005.10.17]
     ["Test gradient ghopcount, gversion, gparent, gorigin."
      (let ((lst ,(tm-to-list
		   '(tokens
		      (SOC-start () (gemit tok1))
		      (tok1 () 			    
			    (printf "(~a : ~a ~a ~a ~a : ~a)" (my-id) (gparent) (gorigin) (ghopcount) (gversion) (my-clock))
			    (if (= (ghopcount) 0)
				(grelay)))
		      )
		   '[simalpha-zeropad-args #f]
		   '[simalpha-channel-model  'lossless]
		   '[simalpha-failure-model  'none]
		   '[simalpha-placement-type 'connected]
		   '[sim-num-nodes 20]
		   '[simalpha-consec-ids #t]
		   '[simalpha-graphics-on #t])
		 ))
	(let ((base (cdr (assq BASE_ID lst)))
	      (others (map cdr (assq-remove-all BASE_ID lst))))
	  (if (all-equal? others)
	      ;; Return something that won't vary based on sim parameters:
	      (list (assq BASE_ID lst) (car others))
	      `(ERROR: ,others))))
      ;; This timing stuff is a bit fragile
      ,(lambda (x)
	(match x 
	  [((,base1 : atroot ,base2 ,basedist 1 : 2)
	    (: ,base3 ,base4 ,gdist 1 : ,time))
	   (and (= base1 base2 base3 base4 BASE_ID)
		(= time (+ RADIO_DELAY (* 2 SCHEDULE_DELAY) 1))
		(= basedist 0)
		(> gdist 0))]
	  [,else #f]))]

     ;; TODO: need to explicitely control tho network parameters for this one:
     ["Gradients: just a gemit and unconditional grelay. (NONDETERMINISTIC)" 
      retry ;; Retry this test if it fails... it's nondeterministic.
      (let ((lst , (tm-to-list
		    '(tokens
		       (SOC-start () (leds on green)
				  (gemit tok1 99))
		       (tok1 (x) 
;			     (printf "(Running on: ~a) \n" (my-id))
			     (setlabel "<~a,~a,~a,~a>" (gdist) (gparent) (gorigin) (gversion))
			     (if (odd? (ghopcount)) (leds on red) (leds on blue))
			     ;(printf "~a " (gdist))
			     (printf "~a " (gdist))
			     (grelay tok1 99)))
		    '[simalpha-zeropad-args #f]
		    '[simalpha-channel-model  'lossless]
;		    '[simalpha-placement-type 'connected]
		    '[simalpha-failure-model  'none]
		    '[sim-num-nodes 30]
		    '[simalpha-consec-ids #t]
		    '[simalpha-graphics-on #t]
		    )))
;	(inspect lst)
	(list (length lst)
	      (= (car lst) 0) ;; Distance at base is zero.
	      (> (cadr lst) 0) ;; "Hopcount" is greater than zero.
	      (> (car (reverse lst)) 1)
	      (equal? lst (sort < lst)))
	) ;; Only true with VERY restricted simulation model.
      (30 #t #t #t #t)]


     ["Gradients: SLOWly spread a gradient through the network.  Try this one in real time with GUI."
      , (tm-to-socvals
	 '(tokens
	    (SOC-start () (leds on green)
		       (gemit tok1))
	    (tok1 () 
		   (if (odd? (ghopcount tok1))
		       (leds on red)
		       (leds on blue))
		   ;(setlabel "~a\n" (ghopcount))
		   
		   (if (integer? (gparent))
		       (highlight-edge (gparent)))
		   
		   (printf "~a.~a.A.~a \n" (pad-width 5 (my-clock)) (pad-width 3 (my-id)) (gdist))
		   (timed-call 1000 tok1b)
		   ;(call tok1b)
		   ;(grelay tok1)
		   )
	    (tok1b ()
		   (printf "~a.~a.B \n" (pad-width 5 (my-clock))  (pad-width 3 (my-id)))
		   (grelay tok1)))
	 '[simalpha-zeropad-args #t]
	 '[simalpha-channel-model  'lossless]
	 '[simalpha-placement-type 'gridlike] ;'connected]
	 '[simalpha-failure-model  'none]
	 '[sim-num-nodes 30]
	 '[simalpha-consec-ids #t]
	 '[simalpha-graphics-on #t])
	unspecified]

     ;; [2005.11.01] Currently there is inconsistency in the order a neighbor receives messages sent by one tokhandler.
     ;; TODO: FIXME
     ["Gradients: Respect emit order just like call order. (FIXME)"
      , (tm-to-list
	 '(tokens 
	      (SOC-start () 
			 (gemit a )
			 (call b )
			 (gemit c))
	    (a () (printf "(~s a) " (my-id)))
	    (b () (printf "(~s b) " (my-id)))
	    (c () (printf "(~s c) " (my-id)))
	    ))
	unspecified]


     ["Gradients: make an (approximately) two hop neighborhood. (NONDETERMINISTIC)"
      retry
      (let ((lst , (tm-to-list
		'(tokens
		  (SOC-start () (gemit tok1))
		  (tok1 () (printf "~a " (gdist)) (if (< (gdist) 20) (grelay))))
		'[simalpha-placement-type 'connected]
		'[simalpha-channel-model 'lossless]
		)))
	(list (< (length lst) 30)
	      (sort < (list->set lst))
	      (equal? lst (sort < lst)))) ;; Only true with VERY restricted simulation model.
      (#t (0 10 20) #t)]

;; This case was too fragile and dependent on the ordering.  I could make it better and bring it back.
;; Problem is that it depends on the aggregator being turned on, because without aggregation we no longer use the timer.
     ["Gradients (inlined): Make sure the timer gets set right. "
      (parameterize ([unique-name-counter 0]
		     [desugar-gradients-mode 'inlined] ;; Only works for this mode.
		     [simalpha-dbg-on #f])
      (parameterize ([pass-list
		   (list cleanup-token-machine  find-emittoks desugar-gradients
		     cleanup-token-machine desugar-let-stored
		     ;rename-stored         
		     cps-tokmac
;		     closure-convert        cleanup-token-machine
		     )])
	(let ([prog
	       (run-compiler
		'(tokens
		  (SOC-start () (gemit tok1))
		  (catcher (x) (void))
		  (tok1 () (greturn (my-id) 
				    (to (tok catcher 0))
				    (via (tok tok1 0))
				    (aggr #f)))
		  ))])
	  ;; Dig out the name of the timeout:
	  (let ((timeout-name
		 (car (rac (rdc (deep-assq 'tokens prog))))))
	    (let ((newprog
		   ;; Insert some more functionality for tok1:
		   (append (deep-assq 'tokens prog)
			   `([tok1 (g_parent g_origin g_hopcount g_version)
				   (printf "~a.~a: tok1: Is time-out set? ~a\n" 
					      (my-clock) (my-id) (token-present? (tok ,timeout-name 0)))]))))
	      (run-simulator-alpha
	       (cleanup-token-machine newprog)
	       'timeout 5000)
	      )))))
      unspecified]


     ;; ["Gradients (ETX):  " 
     

     ["Gradients: execute a return from 1-hop neighbors. Manual timeout.  (NONDETERMINISTIC)"
      , (tm-to-list
		'(tokens
		  (SOC-start () ;(printf "SOCSTART ") 
			     (gemit tok1)
			     ;; Manual timeout:
			     ;(timed-call 1000 tok1)
			     )
		  (catcher (v) (printf " ~a " v))
		  (tok1 () (printf "_ ")
			(if (= (ghopcount) 0) (grelay))
			(greturn (my-id) (to catcher)))
		  )
		'[simalpha-channel-model 'lossless]
		'[simalpha-placement-type 'connected]
		'[simalpha-failure-model 'none])
	;; Should get a list of underscores and nodeids. 
      ,(lambda (lst)
	 (let ((lst (filter number? lst)))
	   (and
	    (eq? BASE_ID (car lst))         ;; First should come BaseID:
	    (not (memq BASE_ID (cdr lst)))  ;; And that should be the only BaseID
	    (> (length lst) 1)              ;; Should have some neighbors, its connected.
	    (eq? (length lst) (length (list->set lst))))))]

     ;; [2005.10.06] After doing my refactoring to make multiple return-handler tokens
     ;; this isn't working.  We're only getting a return from one of our neighbors.
     ["Gradients: execute a repeated return from 1-hop neighbors. (NONDETERMINISTIC)"
      , (tm-to-list
		'(tokens
		  (SOC-start () (printf "(")
			     (call tok1 '10)
			     (timed-call 8000 close))
		  (close () (printf ")"))
		  (catcher (v) (printf "~a " v))
		  (tok1 (reps) 
			(printf ") (")
			(gemit tok2)
			(if (> reps 0)
			    (timed-call 1000 tok1 (- reps 1))))
		  (tok2 () ;(printf "_ ") 
			(greturn (my-id) 
				 (to catcher))) )
		'[sim-timeout #f]
		'[simalpha-channel-model 'lossless]
		'[simalpha-failure-model 'none])
      ,(lambda (x)
	 ;; ASSUMES LOSSLESS CHANNELS AND DETERMINISTIC TIMING:
	 ;; Makes sure we hear from the same neighbors every time:
	 ;(printf "Checking : ~a\n" x)
	 ;(set! x (map (lambda (l) (filter number? l)) x))
	 (and (eq? (car x) ())
	      ;(equal? (cadr x) (list BASE_ID)) ;; No longer true.  Not staggering epochs for non-aggregated greturn.
	      (all-equal? (map (lambda (l) (sort < (filter number? l))) 
			       (rdc (cddr x))))))]


     ;; [2005.11.03] Ok having problems with this one when using closure-convert
     ;; [2005.11.03] Hmm.. it passed on fort, but only after one retry.  What's wrong?
     ["Gradients: execute a repeated return from whole network. (NONDETERMINISTIC)"
      retry
      , (tm-to-list
		'(tokens
		  (SOC-start () (printf "(")
			     (call tok1 '5)
			     (timed-call 20000 close))
		  (close () (printf ")"))
		  (catcher (v) (printf " ~a " v))
		  (tok1 (reps) 
			(printf ") (")
			(gemit tok2)
			(if (> reps 0)
			    (timed-call 1000 tok1 (- reps 1))))
		  (tok2 () (grelay) (greturn (my-id) (to catcher)))
		  )
		'[sim-timeout 30000]
		'[simalpha-consec-ids #t]
		'[simalpha-placement-type 'connected]
		'[simalpha-channel-model 'lossless]
		'[simalpha-failure-model 'none])
      ;; You will see a staggered reception of results.
      ;; First from the one-hop neighbors, then also the two-hops, and so on.
      ,(lambda (x) 
	 (and
	 ;; Let's make sure the list increases in distance at first:
; 	 ;; Check the first three.
; 	  (let ([one   (length (car x))]
; 	       [two   (length (cadr x))]
; 	       [three (length (caddr x))])
; 	   (< one two three))
	 ;; Also check to make sure whe heard from everyone.
	 (= (sim-num-nodes)
	    (length (list->set (apply append x))))
	 ))]

     ;; [2005.11.16] This just FAILED when I had realtime mode turned on.  But succeeded when I tried again.
     ["Gradients: execute a repeated return from 2-hop neighbors. Tame network.  (NONDETERMINISTIC)"
      ;; This is nondeterministic because it tries to verify that there are 
      ;; more 2-hop neighbors than there are 1-hop neighbors.  Usually true but not guaranteed.
      retry
      (let ((lst , (tm-to-list
		'(tokens
		  (SOC-start () (printf "(")
			     (call tok1 '5)
			     (timed-call 20000 close))
		  (close () (printf ")"))
		  (catcher (v) (printf "~a " v))
		  (tok1 (reps)  
			(printf ") (")
			(gemit tok2)
			(if (> reps 1)
			    (timed-call 1500 tok1 (- reps 1))))
		  (tok2 () (grelay) 
			(if (and (> (gdist) 0) (<= (gdist) 10))
			    (greturn (gversion) (to catcher))
			(if (and (> (gdist) 10) (<= (gdist) 20))
			    (greturn (+ 100 (gversion)) (to catcher)))
			))
		  )
		'[sim-timeout 30000]
		'[simalpha-consec-ids #t]
		'[simalpha-placement-type 'gridlike]
		'[simalpha-channel-model 'lossless]
		'[simalpha-failure-model 'none]
		)))
	    (list 
	     lst
	    (map (lambda (ls)
		   (let ((small (filter (lambda (n) (< n 100)) ls))
			 (big   (filter (lambda (n) (> n 100)) ls)))
		     (list (length big) (length small))))

		 ;; Don't count first or last batch:
		 (cddr (rdc lst)))))
      ,(lambda (result) 
	 (and 
	  ;; This asserts that there are more two-hop than one-hop neighbors:
	  (andmap (lambda (x) (apply > x))
		  (cadr result))
	 ))]

     ["#1 Now before doing aggregated greturn, let's manually do some subcalls and return up values."
      , (tm-to-list
	 `(tokens
	      (SOC-start () (bcast tok1) (timed-call 50 retransmit))
	    (retransmit () (bcast tok1))
	    (catcher (v) (if (= (my-id) ,BASE_ID)
			    (printf "(~a ~a)\n" (my-clock) v)))
	    (tok1 () (call handler (subcall f (my-id) 3)))
	    (handler (v) (stored (acc '()))
		     (set! acc (cons v acc))
		     (if (not (token-scheduled? timeout))
			 (timed-call 1000 timeout)))
	    (timeout () 
		     (bcast catcher (ext-ref handler acc)))
	    (f (x y) (return (+ x y)))
	    ))
	,(lambda (ls)
	   (match ls
	     [((,t (,id1 ,id2)) ...) (equal? id1 id2)]
	     [,_ #f]))]

     ["#2 Let's manually do some more tricky continuations and stress closure-convert."
      (parameterize ([simalpha-zeropad-args 'warning];; This has to be allowed  because we're using closure-convert.
		     )
	
      (let ((prt (open-output-string)))
	(display "(" prt)
	(run-simulator-alpha
	 (cleanup-token-machine
	  (closure-convert
	   (cleanup-token-machine
	    '(tokens
		 (SOC-start subtok_ind () (stored)
			    (let ([k_88 (lambda (HOLE_89) (printf "k2 ~a\n" HOLE_89))])
			      (if '#t
				  (call (tok sum 0)
					(lambda (HOLE_90) (printf "k1 ") (kcall k_88 HOLE_90))
					99 
					1)
				  (kcall k_88 #f))))
	       (sum (k x y) (kcall k (+ x y)))))))
	 'outport prt)
	(display ")" prt)
	(read (open-input-string (get-output-string prt)))))
      (k1 k2 100)]

     ;; This was how I realized what eggregious read-write atomicity
     ;; problems I was having in my gradient/subcall generated code:
     ;; [2005.10.31] Changed to use call-fast!!!
     ["#3 Let's manually do some more tricky continuations and stress closure-convert."
      (parameterize (
		     [simulation-logger-fasl-batched #f] ;; Can't write continuations to log file in fasl mode.
		     )
      (let ((prt (open-output-string)))
	(display "(" prt)
	(run-simulator-alpha
	 (cleanup-token-machine
	  (id;closure-convert
	   (id;sever-cont-state
	   (id;cleanup-token-machine
	    '(tokens
	       [sum subtok_ind
		    (k_98 x y)
		    (stored)
		    (printf "(sum ~a ~a) \n" x y)
		    (kcall k_98 (+ x y))]
	       [SOC-start () 
			  (call TEST 0 'rhlocal 1000 0 0)
			  (call TEST 0 'rhlocal 13 0 0)
			  ;(timed-call 10 TEST 0 'rhlocal 13 0 0)
			  ]
	       [timeout () 
			(printf " (timeout ~a) \n" (ext-ref TEST acc_92))]
	       [TEST (destid flag val toind viaind)
		     (stored (acc_92 '0))
		(let ([k_99 (lambda (HOLE_100) (printf "outif2 ")
				    HOLE_100)])
		  (if (eq? flag 'rhlocal)
		      (call-fast (tok sum 0)
			    (lambda (HOLE_101)
			      (kcall k_99
				     (begin
				       (printf "(acc ~s to ~s) \n" acc_92 HOLE_101)
				       (set! acc_92 HOLE_101)
				       (printf "local-and-fire ")
				       (if (not (token-scheduled? timeout))
					   (timed-call 1000 timeout)))))

			    val
			    acc_92)
		      ))])))))
	 'outport prt
	 )
	(display ")" prt)
	(read (open-input-string (get-output-string prt)))      
	))
      ((sum 1000 0) (acc 0 to 1000) local-and-fire outif2
       (sum 13 1000) (acc 1000 to 1013) local-and-fire outif2
       (timeout 1013))]

;      (let ((prt (open-output-string)))
;        (display "(" prt)
;        (run-simulator-alpha
; 	(cleanup-token-machine
; 	 (id;closure-convert
; 	  (id;sever-cont-state
; 	   (id;cleanup-token-machine
; 	    '(tokens
; 	       [SOC-start ()
; 			  (call TEST 0 'rhlocal 1000 0 0)
; 			  (call TEST 0 'rhlocal 3 0 0)
; 			  (timed-call 1000 timeout)
; 			  ]
; 	       [timeout () (printf "~a\n" (ext-ref TEST acc))]
; 	       [TEST (destid flag val toind viaind)
; 		     (stored (acc '0))
; 		     (set! acc (+ acc val))])))))
; 	)
;        )
    

     ["Gradients: Now try aggregated greturn.  Basically counts one-hop neighbors."
      (filter (lambda (x) (not (zero? x)))
	      , (tm-to-list
		'(tokens 
		  (SOC-start () (gemit tok1))
		  (catcher (x) (printf "~a " x))
		  (tok1 () 
			(if (= (ghopcount) 0) (grelay))
			(greturn (gdist)
				 (to catcher)
				 (seed 0)
				 (aggr sum)))
		  (sum (x y) (+ x y)))
		'[regiment-verbose #f]
		'[simalpha-placement-type 'connected]
		'[simalpha-channel-model 'lossless]
		'[simalpha-failure-model 'none]		
		))
      ,(lambda (ls)
	 (and (= (length ls) 1)
	      (> (car ls) 0)))]


     ["Gradients: Now try cons-aggregated greturn from one-hops (Definitely SIM ONLY, does alloc)"
      ;No it's not, require connected: ;retry  ;; It's possibly we have no neighbors at all!
      (map list->set
	   (filter (lambda (x) (not (null? x)))
		   , (tm-to-list
		     '(tokens 
		       (SOC-start () (call tok1 '5))
		       (tok1 (reps)
			     (gemit tok2)
			     (if (> reps 1)
				 (timed-call 1000 tok1 (- reps 1))))
		       (catcher (x) (printf "~a " x))
		       (tok2 () 
			     (if (= (ghopcount) 0) (grelay))
			     (greturn (list (gdist))
				      (to catcher)
				      (seed ())
				      (aggr f)))
		       (f (x y) (append x y)))
		     '[sim-timeout 10.0]
		     '[simalpha-placement-type 'connected]
		     '[simalpha-channel-model 'lossless]
		     '[simalpha-failure-model 'none]		
		     )))
      ;; Epoch staggered aggregation
      ((0) (0 10) (0 10) (0 10) (0 10) (10))
      ]

     ;; [2005.11.03] This totally fails with closure-convert.
     ["Gradients: Same thing but to whole network."
      retry ;; Really messed up topology could cause this to not work.
      , (tm-to-list
	'(tokens 
	  (SOC-start () (call tok1 '10))
	  (tok1 (reps)
		(gemit tok2)
		(if (> reps 1)
		    (timed-call 1000 tok1 (- reps 1))))
	  (catcher (x) (printf "~a " x))
	  (tok2 () 
		;(printf "~s " (my-id)) ;(printf "~s.~s tok2..\n" (my-clock) (my-id))
		(grelay)
		(greturn (list (gdist))
			 (to catcher)
			 (seed ())
			 (aggr f)))
	  (f (x y) (append x y)))
	'[regiment-verbose #f]
	'[sim-timeout 10.0]
	'[simalpha-placement-type 'connected]
	'[simalpha-consec-ids #t]
	'[simalpha-failure-model 'none]
	'[simalpha-channel-model 'lossless]
	'[sim-num-nodes 10]
	)
      ;; Epoch staggered aggregation
	,(lambda (x) 
	;; Let's make sure the list increases in distance at first:
	;; Check the first three.

	(let ([one   (/ (apply + (car x)) (length (car x)))]
	      [two   (/ (apply + (cadr x)) (length (cadr x)))]
	      [three (/ (apply + (caddr x)) (length (caddr x)))])
	  (< one two three)))]

     ["Gradients: Now look at clock-skew."
      (map (lambda (ls) (and (not (null? ls))
			     (- (apply max ls) (apply min ls))))
      , (tm-to-list
	'(tokens 
	  (SOC-start () (call tok1 '10))
	  (tok1 (reps)
		(gemit tok2)
		(if (> reps 1)
		    (timed-call 1000 tok1 (- reps 1))))
	  (catcher (x) (printf "~a " x))
	  (tok2 () 
		(grelay)
		(greturn (list (my-clock)) ; (my-id)))
			 (to catcher)
			 (seed ())
			 (aggr f)))
	  (f (x y) (append x y)))
	'[regiment-verbose #f]
	'[sim-timeout 5000]
	))
      ;; Epoch staggered aggregation
      ,(lambda (x) 
	;; Let's make sure the list increases in distance at first:
	;; Check the first three.
	 (apply <= (list-head (filter number? x) 5)))]

     ["Gradients: now launch three nested gradients. (NONDETERMINISTIC)"
      retry ;; Must retry, network might not be connected
      , (tm-to-list
		'(tokens
		  (SOC-start () 
			     (printf "(~a ~a \"A launching\")\n" (my-clock) (my-id))
			     (gemit a)
			     (timed-call 1000 print-dists))
		  (print-dists ()
			     (printf "(\"After a period of time, dists at base are:\" ~a ~a ~a)"
				     (gdist a) (gdist b) (gdist c)))
		  (a () (grelay)
		        (if (= (my-id) 15)
			    (begin 
			      (printf "(~a ~a ~a \"B launching\")\n" (my-clock) (my-id) (gdist a))
			      (gemit b))))
		  (b () (grelay)
		        (if (= (my-id) 20)
			    (begin
			      (printf "(~a ~a ~a ~a \"C launching\")\n" (my-clock) (my-id) (gdist a) (gdist b))
			      (gemit c))))
		  (c () (grelay)
		        (if (= (my-id) BASE_ID)
			    (begin 
			      (printf "(~a ~a ~a ~a ~a \"C hit home!\")\n" (my-clock) (my-id) (gdist a) (gdist b) (gdist c))
			      ;(printf "home")
			      )))
		  )
		'[simalpha-failure-model 'none]
		'[simalpha-channel-model 'lossless]
		'[simalpha-consec-ids #t]
		'[sim-num-nodes 30])
      ,(lambda (ls)
	 ;; Received all messages:
	 (= (length ls) 5))]

     ["Gradients: return value through three nested gradients. (NONDETERMINISTIC)"
      ;retry ;; Must retry, network might not be connected
      (rac , (tm-to-list
		'(tokens
		  (SOC-start () 
			     (printf "(A-launch ~a ~a)\n" (my-clock) (my-id))
			     (gemit a)
			     (timed-call 1000 return-up))
		  (return-up ()
			     (greturn 8967 (to catcher1) (via c)))
		  (catcher1 (v) 
			    (printf "(Returned-via-C ~a)\n" v)
			    (greturn v (to catcher2) (via b)))
		  (catcher2 (v) 
			    (printf "(Returned-via-B ~a)\n" v)
			    (greturn v (to catcher3) (via a)))
		  (catcher3 (v) (printf "(catch3 ~a ~a ~a ~a)" (my-id) 
					(> (my-clock) 1000)
					(< (my-clock) 2000)
					v))

		  (a () (grelay)
		     (if (= (my-id) 15)
			 (begin
			   (printf "(B-launch ~a ~a ~a)\n" (my-clock) (my-id) (gdist a))
			   (gemit b))))
		  (b () (grelay)
		        (if (= (my-id) 20)
			    (begin
			      (printf "(C-launch ~a ~a ~a ~a)\n" (my-clock) (my-id) (gdist a) (gdist b))
			      (gemit c))))
		  (c () (grelay)
		     (if (= (my-id) BASE_ID)
			 (printf "(C-hit-home ~a ~a ~a ~a ~a)\n" (my-clock) (my-id) (gdist a) (gdist b) (gdist c))))
		  )
		'[sim-timeout 1500]
		'[simalpha-dbg-on #f]
		'[simalpha-placement-type 'connected]
		'[simalpha-failure-model 'none]
		'[simalpha-channel-model 'lossless]
		'[simalpha-consec-ids #t]
		'[sim-num-nodes 30]))

      ;((A-launch 1 0) (B-launch 64 15 2) (C-launch 158 20 1 3) (C-hit-home 190 0 0 2 1) 
      ; (Returned-via-C 8967) (Returned-via-B 8967) 
      (catch3 ,BASE_ID #t #t 8967)]


     ;; FIXME: This needs more work:
     ["Gradients: let a thousand flowers bloom (gradient from everywhere)."
      retry ;; Must retry, network might not be connected
     , (tm-to-list
		'(tokens
		  (SOC-start () 
			     (gemit spark)
			     ;(timed-call 1000 return-up)
			     )
		  (spark () (grelay)
			    (printf "(Spark ~a)\n" (my-id))
			    (gemit secondary))
		  (secondary () (grelay)
			     (printf "(~a ~a ~a ~a)\n" (gorigin) (gparent) (my-id) (gdist))))
		'[simalpha-failure-model 'none]
		'[simalpha-channel-model 'lossless]
		'[simalpha-consec-ids #t]
		'[sim-num-nodes 30])
      ;; FIXME : Finish
      unspecified]

     ;; [2006.02.12] Strange this just failed returning '().  Shouldn't be a comm failure...
     ;; [2006.03.24] Note, this doesn't really work, can't currently
     ;; run the simulator if there are still gemit's in the program.
     #;
     ["Test soc-return (#1).  Try it w/out desugar-soc-return."
      (parameterize ([unique-name-counter 0] 
		     [simalpha-dbg-on #f])
      (parameterize ([pass-list (rdc (list-remove-after desugar-macros (pass-list)))])
	(let ([prog (run-compiler 399)])
	  (run-simulator-alpha prog))))
      (399)]

     ["Test soc-return (#2).  Try it WITH desugar-soc-return, but still on base station."
      (parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
      ;; Go all the way through desugar-gradients and the subsequent cleanup-token-machine
      (parameterize ([pass-list (rdc (list-remove-after cps-tokmac (pass-list)))])
	(let ([prog (run-compiler 399)])
	  (run-simulator-alpha prog))))
      (399)]

    ["Test soc-return (#3). soc-returns from one hop neighbors, without desugar gradients."
      (parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
	(let ([prog (cleanup-token-machine
		     '(tokens
		       (SOC-start () (soc-return (my-id)) (bcast tok1))
		       (tok1 () 
			     (printf " recvd ")
			     (soc-return (my-id))
			     )
		       ))])
	  (run-simulator-alpha prog)))
      ;; Result should be base_id followed by some number of non-base-ids.
      ,(lambda (ls)
	 (and (> (length ls) 1)
	      (eq? (car ls) BASE_ID)
	      (andmap (lambda (n) (not (eq? n BASE_ID))) (cdr ls))))
      ]

    ;; This is broken:  It's an incorrect algorithm:
    ;; [2005.11.08]  Failed on this under PLT (but then passes when I try again... HUH?)
    ["Elect leader. #1"
     , (tm-to-list
	'(tokens
	   [node-start () 
	       (gemit (tok lead (my-id)))
	       (printf "\nLaunch: ~a " (my-id))
	       ]
	   [lead id () 		 
		 (let-stored ([cur-leader (my-id)])
		   (if (> id cur-leader)
		       (begin 
			 (printf "~a " id) (flush-output-port)
			 (set! cur-leader id)
			 (grelay))
		       (begin 
			 (printf "_ ") (flush-output-port)
			 (gemit (tok lead cur-leader)))))]
	   )
	'[sim-num-nodes 3]
	'[simalpha-consec-ids #f]
	'[sim-timeout 10000])
       unspecified]


    ;; This is better.
    ["Elect leader. #2" 
     , (tm-to-list
	'(tokens
	   [SOC-start () (timed-call 1000 open)
		         (timed-call 6000 close)]
	   [open () (printf "\n(final ")]
	   [close () (printf ")")]
	   [node-start () (stored [cur-leader #f])
	       (set! cur-leader (my-id))
	       (gemit (tok lead (my-id)))
	       (printf "(launch ~a) \n" (my-id))
	       (timed-call 5000 final-report)
	       ]
	   [lead id () 		 
		 (if (< id (ext-ref node-start cur-leader))
		     (begin 
		       (printf "(~a ~a) " id (ext-ref node-start cur-leader)) (flush-output-port)
		       (ext-set! node-start cur-leader id)
		       (grelay))
		     (begin 
		       (printf "~a " ;(my-id) 
			       (ext-ref node-start cur-leader)) 
		       (flush-output-port)
		       ;(gemit (tok lead (ext-ref node-start cur-leader)))
		       ))]
	   [final-report () (printf "~n   ~a " (ext-ref node-start cur-leader))]
	   )
	'[sim-num-nodes 10]
	'[simalpha-consec-ids #f]
	'[simalpha-placement-type 'connected]
	'[simalpha-channel-model 'linear-disc]
	'[sim-timeout 10000])
       unspecified]

    ;; [2005.11.01] Whoa!  I got two winners from this even with these network conditions:
    ;; [2005.11.03] FIXME WEIRD: when I first load the compiler this returns nothing.  Then on subsequent runs it does!
    ;; [2005.11.07] Got two winners again.  Got to save the seed and figure out what's going on.
    ;; [2005.11.09] Screws up only occassionally, can run 20 times in a row with no problems.  Always two leaders.
    ;; [2005.11.09] OH DUH.  That was because of random ids that could be the same!!  Makes sense.
   ["Now test elect-leader macro."
;     retry ;; TEMP: FIXME: SHOULDNT NEED RETRY
     , (tm-to-socvals
	'(tokens
	   [SOC-start () (leds on green) (gemit tree)] 
	   [tree () (grelay)]
	   [node-start () (elect-leader tok1)]
	   [tok1 (ldr _)
		 (if (= ldr (my-id))
		     (call YEP_LEADER)
		     (leds on blue))]
	   [YEP_LEADER () (leds on red)
		       ;;(fprintf (console-error-port) "\n\n WINNER: ~s at nod ~s\n" (subcall f))
		       (greturn (my-id) 
				(to SOC-return-handler) 
				(via tree))
		       ])
	'[simalpha-channel-model  'lossless]
	'[simalpha-placement-type 'gridlike] ;'connected]
	'[simalpha-failure-model  'none]
	'[sim-num-nodes 30]
	'[simalpha-consec-ids #t]
	'[simalpha-graphics-on #t]
	)
       ;; This requires that you get actual minimum:
       ,(lambda (ls)
	  (and (= 1 (length ls))
	       (= (car ls) 
		  (apply min 
			 (map node-id 
			   (map simobject-node 
			     (simworld-all-objs
			      (simalpha-current-simworld))))
			 ))))]

   ;; This senses which node is farthest from the (0,0) origin (upper left corner).  
   ;; (Not farthest from the base station!)
   ["Now test elect-leader macro with a supplied criterion function.  Elect most distant from origin."
    (length , (tm-to-socvals
	'(tokens
	   [SOC-start () (leds on green) (gemit tree)] 
	   [tree () (grelay)]
	   [node-start () (elect-leader tok1 f)]
	   [f () (sync-sense)]
	   [tok1 (ldr _)
		 (setlabel "<~a>" ldr)
		 (if (= ldr (my-id))
		     (begin (leds on red)
			    (greturn (my-id) 
				     (to SOC-return-handler) 
				     (via tree)))
		     (leds on blue))])
	'[simalpha-sense-function-constructor sense-dist-from-origin]
	'[simalpha-zeropad-args #t]
	'[simalpha-channel-model  'lossless]
	'[simalpha-placement-type 'gridlike] ;'connected]
	'[simalpha-failure-model  'none]
	'[sim-num-nodes 30]
	'[simalpha-consec-ids #t]
	'[simalpha-graphics-on #t]
	))
    1] ;; Get one leader only

   ["Elect leader using custom comparator: sum of dist origin and dist lower left corner."
    (length , (tm-to-socvals
	'(tokens
	   [SOC-start () (leds on green) (gemit tree)]
	   [tree () (grelay)]
	   [node-start () (elect-leader tok1 crit comp)]
	   [crit () (vector (float->int (locdiff (cons '0 (cons '30 '())) (loc)))
			    (float->int (locdiff (cons '60 (cons '30 '())) (loc))))]
	   [comp (a b)
#;		   (if (and (> (vector-ref a 0) (vector-ref b 0))
			    (> (vector-ref a 1) (vector-ref b 1)))
		       1
		       (if (and (> (vector-ref b 0) (vector-ref a 0))
				(> (vector-ref b 1) (vector-ref a 1)))
			   -1
			   0))
		 (let ((v1 (* (vector-ref a 0) (vector-ref a 1)))
		       (v2 (* (vector-ref b 0) (vector-ref b 1))))
		   (if (> v1 v2) 1
		   (if (= v1 v2) 0
		       -1))
		   )]
	   [tok1 (ldr val)
		 (setlabel "<~a,~a>" ldr val)
		 (if (= ldr (my-id))
		     (begin (leds on red)
			    (greturn (my-id) 
				     (to SOC-return-handler) 
				     (via tree)))
		     (leds on blue))])
	'[simalpha-sense-function-constructor sense-dist-from-origin]
	'[simalpha-zeropad-args #t]
	'[simalpha-channel-model  'lossless]
	'[simalpha-placement-type 'gridlike] ;'connected]
	'[simalpha-failure-model  'none]
	'[sim-num-nodes 30]
	'[simalpha-consec-ids #t]
	'[simalpha-graphics-on #t]
	))
    1]

   ["Elect leader based on number of hops from origin."
    (length , (tm-to-socvals
	'(tokens
	   [SOC-start () (leds on green) (gemit tree)] 
	   [tree () (grelay)]
	   ;; Wait till the tree has spread.
	   [node-start () (timed-call 1000 elect)]
	   [elect () (elect-leader tok1 f)]
	   ;; Elect based on hopcount from root:
	   [f () (ghopcount tree)]
	   [tok1 (ldr val)
		 (setlabel "<~a,~a>" ldr val)
		 (if (= ldr (my-id))
		     (begin (leds on red)
			    (greturn (my-id) 
				     (to SOC-return-handler) 
				     (via tree)))
		     (if (odd? (ghopcount tree))
			 (leds on blue)))])
	'[sim-timeout 10.0]
	'[simalpha-sense-function-constructor sense-dist-from-origin]
	'[simalpha-zeropad-args #t]
	'[simalpha-channel-model  'lossless]
	'[simalpha-placement-type 'gridlike] ;'connected]
	'[simalpha-failure-model  'none]
	'[sim-num-nodes 30]
	'[simalpha-consec-ids #t]
	'[simalpha-graphics-on #t]
	))
    1]        ;; We should only get one leader:

   ;; [2005.11.15] This can currently elect two leaders!!
   ;; I think the competition-flood is petering out too soon.
   ["Elect a leader more asynchronously, first flood the consideration token."
     , (tm-to-socvals
	'(tokens
	   [SOC-start () (leds on green) (flood tree)]
	   [tree () (leds on blue)
		 (elect-leader memb)]
	   [memb (ldr _)
		 (setlabel "(~s)" ldr)
		 (if (= ldr (my-id))
		     (begin (leds on red)
			    (greturn (my-id) 
				     (to SOC-return-handler) 
				     (via tree))))])
	'[simalpha-zeropad-args #t]
	'[simalpha-channel-model  'lossless]
	'[simalpha-placement-type 'gridlike] ;'connected]
	'[simalpha-failure-model  'none]
	'[sim-num-nodes 30]
	'[simalpha-consec-ids #t]
	'[simalpha-graphics-on #t]
	)
       ;; This requires that you get actual minimum:
       unspecified]

   ["Elect a leader within a constrained area."
     , (tm-to-socvals
	`(tokens
	   [SOC-start () (gemit tree)]
	   [tree () 
		 (if (< (ghopcount) 2) (grelay tree))
		 (leds on blue)
		 (elect-leader memb f)]
	   [f () 
;	      (if (token-present? tree)
;		  (begin 
;		    (printf " PRESENT: ~a HOPCOUNT: ~a  DIST:~a\n" (token-present? (tok tree 0)) 
;			    (ghopcount (tok tree 0)) (gdist (tok tree 0)))
;		    (printf " EXT_REF:~a\n" (ext-ref (tok tree 0) stored_g_hopcount))
;		    (if (ghopcount (tok tree 0)) (ghopcount (tok tree 0)) 'FOOBAR??))
;		  -1000)
	      (if (= (my-id) ,BASE_ID)
		  -1000
		  (my-id))]
	   [memb (ldr val)
		 (setlabel "(~s)" ldr)
		 (if (= ldr (my-id))
		     (begin (leds on red)
			    (greturn (my-id) 
				     (to SOC-return-handler) 
				     (via tree))))])
	'[simalpha-zeropad-args #t]
	'[simalpha-channel-model  'lossless]
	'[simalpha-placement-type 'gridlike] ;'connected]
	'[simalpha-failure-model  'none]
	'[sim-num-nodes 30]
	'[simalpha-consec-ids #t]
	'[simalpha-graphics-on #t]
	)
       ;; This requires that you get actual minimum:
       unspecified]

   ;; [2005.11.15] Just failed once when testing on Windows. (Worked on retry.)
   ["Elect leader -- test code generated by anchor-at on [2005.11.15]"
    retry
    ;; AHH, the problem with this was that it was starting the flood from everywhere.
    ;; Not just from SOC-start.
    , (tm-to-socvals
       '(tokens 
;	  (node-start () (call leaf-pulsar_result))
;	  (node-start () (call f_token_result))
	  (SOC-start () (call f_token_result) (call spread-global))
	  (f_token_result () 
			  (printf "\nStarting flood:\n")
			  (flood constok))
	  (constok () (elect-leader m_token_result))
;	  (f_token_result
;		 ()
;		 (draw-mark (list '30 '40))
;		 (leds on blue))
	  (m_token_result (ldr val) (leds on red))
	  (m_token_result (ldr val) 
			  (printf "\n~a Finished: (~a, ~a)" (my-id) ldr val)
			  (if (= ldr (my-id)) 
			      ;; Just wait for the global tree to get there.
			      (timed-call 1000 return_home)))
	  (return_home () 
		       (printf "\n~a: Returning home on tree ~a" (my-id) (token-present? global-tree))
		       (greturn (list 'ANCH (my-id))
				(to SOC-return-handler)
				(via global-tree)))
;	  (leaf-pulsar_result
;		 ()
;		 (call f_token_result)
;		 (timed-call 1000 leaf-pulsar_result))
	  (spread-global
		 ()
		 (gemit global-tree)
		 ;(timed-call 1000 spread-global)
		 )
	  (global-tree () (grelay)) (catcher (v) (soc-return v)))
       '[sim-timeout 5000]
       '[simalpha-zeropad-args #t]
       '[simalpha-channel-model  'lossless]
       '[simalpha-placement-type 'gridlike] ;'connected]
       '[simalpha-failure-model  'none]
       '[sim-num-nodes 30]
       '[simalpha-consec-ids #t]
       '[simalpha-graphics-on #t])
      ((ANCH ,(min 1 BASE_ID)))]


#; ;; FIXME: FINISH
    ["Again elect-leader macro, but with a function to maximize."
     , (tm-to-socvals
	'(tokens
	   [SOC-start () (gemit tree)] 
	   [tree () (grelay)]
	   [node-start () (timed-call 500 start-election)]
	   [start-election () (elect-leader lead fun)]
	   [lead () (greturn (my-id) (to SOC-return-handler) (via tree))]
	   [fun () (my-id)])
	'[simalpha-channel-model  'lossless]
	'[simalpha-placement-type 'connected]
	'[simalpha-failure-model  'none]
	'[sim-num-nodes 10]
	'[simalpha-consec-ids #f])
       ;; This requires that you get actual minimum:
       ,(lambda (ls)
	  (and (= 1 (length ls))
	       (= (car ls) 
		  (apply min 
			 (map node-id 
			   (map simobject-node 
			     (simworld-all-objs
			      (simalpha-current-simworld))))
			 ))))]

	
    ["Test flood macro."
     , (tm-to-list
	'(tokens
	   [SOC-start () (flood tok1)]
	   [tok1 () (printf "~a " (my-id))])
	'[simalpha-channel-model 'lossless]
	'[simalpha-placement-type 'connected]
	'[sim-num-nodes 10]
	)
       ,(lambda (ls)
	  (equal? (sort < ls)
		  (sort < (map node-id 
			       (map simobject-node 
				    (simworld-all-objs
				     (simalpha-current-simworld)))))))]

    ["Run and simulate complete regiment program." 
     retry
     (parameterize ([simalpha-channel-model 'lossless]
		    [simalpha-failure-model  'none]
		    [deglobalize-markup-returns #f]
		    [default-slow-pulse '1000]
		    [default-fast-pulse '100]
		    [sim-timeout 400])
       (run-simulator-alpha (run-compiler '(rmap nodeid world))))
     ,(lambda (ls)
	;; Can't make very strong statements about timing, but we
	;; shoud have heard from the first *two* generations by this
	;; time:
	;; UNLESS, RADIO_DELAY is set really large:
	(and (> (length ls) (* 2 (sim-num-nodes)))
	     (equal?
	      (sort < (cons BASE_ID (cdr (iota (sim-num-nodes)))))
	      (sort < (list->set ls)))))]

    ;; [2005.11.14] Huh, just started getting some errors on this when running from command line.
    ["Run a simple fold in regiment." 
     (parameterize ([simalpha-channel-model 'lossless]
		    [simalpha-failure-model  'none]
		    [sim-num-nodes 30]
		    [default-slow-pulse '1000]
		    [default-fast-pulse '100]
		    [simalpha-zeropad-args 'warning] ;; Sync-sensing necessitates continuations.
		    )
       (run-simulator-alpha 
	(run-compiler 
	 '(letrec ([readings (rmap (lambda (n) (sense "temp" n)) world)]
		   [sum (rfold + 0 readings)])
	    sum)
	 )
	'timeout 1500))
     unspecified]

    ["Count nodes in network under message loss conditions."  
     retry
     (parameterize ([simalpha-channel-model 'linear-disc]
		    [simalpha-failure-model  'none]
		    [simalpha-placement-type 'connected]
		    [default-slow-pulse '1000]
		    [default-fast-pulse '100]
		    [simalpha-inner-radius 10]
		    [simalpha-outer-radius 20]
		    [simalpha-world-xbound 60]
		    [simalpha-world-ybound 60]
		    [sim-num-nodes 10])
       (run-simulator-alpha 
	(run-compiler 
	 '(letrec ([readings (rmap (lambda (_) 1) world)]
		   [sum (rfold + 0 readings)])
	    sum)
	 )
	'timeout 1000))
     ;; Should have missed a message in one out of the last 3 aggregations:
     ,(lambda (ls) (< (apply + (list-head (reverse ls) 3)) 30))]

    ["Run an average 'temperature' calculation in regiment." 
     (parameterize ([simalpha-channel-model 'lossless]
		    [simalpha-failure-model  'none]
		    [simalpha-zeropad-args 'warning] ;; Sync-sensing necessitates continuations.
		    [simalpha-sense-function-constructor sense-sine-wave])
       (run-simulator-alpha 
	(run-compiler 
	 '(letrec ([readings (rmap (lambda (n) (cons (sense "temp" n) (cons 1 ())))
				   world)]
		   [aggr (lambda (x y)
			   (cons (+ (car x)
				    (car y))
				 (cons (+ (car (cdr x))
					  (car (cdr y)))
				       ())))]
		   [div (lambda (v) 
			  (if (= (car (cdr v)) 0) 0 ;; Just return zero if there are no samples to avg.
			      (/ (car v) (car (cdr v)))))]
		   [sums (rfold aggr (cons 0 (cons 0 ())) readings)]
		   [result (smap div sums)])
	    result)
	 ;'verbose
	 ;'barely-tokens
	 )
	'timeout 3000))
     ;;; FIXME: constrain further:
     unspecified]

    ["Regiment: Run a filter and then aggregate" 
     (parameterize ([simalpha-channel-model 'lossless]
		    [simalpha-failure-model  'none]
		    [simalpha-zeropad-args 'warning] ;; TEMP: TODO: THINK ABOUT THIS.
		    [simalpha-sense-function-constructor sense-sine-wave])
       (run-simulator-alpha 
	(run-compiler 
	 '(rfold append	'()
		 (rmap (lambda (n) (cons (nodeid n) '()))
		       (rfilter (lambda (n) (even? (nodeid n))) world)))
	 ;'verbose
	 ;'barely-tokens
	 )
	'timeout 5000))
     ,(lambda (ls) (andmap even? (apply append ls)))]

#;
    ["Run an average 'temperature' calculation in regiment." 
     (parameterize ([simalpha-channel-model 'lossless]
		    [simalpha-failure-model  'none])
       (run-simulator-alpha 
	(run-compiler 
	 '(letrec ([readings (rmap (lambda (n) (cons (sense "temp" n) 1))
				   world)]
		   [aggr (lambda (x y)
			   (cons (+ (car x)
				    (car y))
				 (+ (cdr x)
				    (cdr y))))]
		   [sum (rfold aggr (cons 0 0) readings)])
	    (smap (lambda (v) (/ (car v) (cdr v)))
		  sum))
	 )
	'timeout 2000))
     unspecified]

#;
    ;; Finish
    ["Run an average 'temperature' calculation in regiment." 
     (parameterize ([simalpha-channel-model 'lossless]
		    [simalpha-failure-model  'none])
       (run-simulator-alpha 
	(run-compiler 
	 '(letrec ([readings (rmap (lambda (n) (vector (sense "temp" n) 1))
				   world)]
		   [aggr (lambda (x y)
			   (vector (+ (vector-ref x 0)
				    (vector-ref y 0))
				 (+ (vector-ref x 1)
				    (vector-ref y 1))))]
		   [sum (rfold aggr (vector 0 0) readings)])
	    (smap (lambda (v) (/ (vector-ref 0) (vector-ref 1)))
		  sum))
	 )
	'timeout 2000))
     unspecified]


    ["Gradients:  Return an average sensor reading over the network."
     , (tm-to-socvals
	'(tokens 
	  (SOC-start () (call spread))
	  (spread () (gemit tree) (timed-call 600 spread))
	  (catcher (v) 
		   (if (> (cadr v) 0)
		       (begin 
;			 (printf "Got soc-val at time ~a: (~a ~a) avg: ~a\n"
;				 (my-clock) (car v) (cadr v) (/. (car v) (int->float (cadr v))))
			 (soc-return (list (my-clock) 
					   (sync-sense)
					   (float->int (/. (car v) (int->float (cadr v)))))))
		       (printf "Soc-val at time ~a: but it's empty!!\n" (my-clock))))

	  (tree () 
;		(printf "~a tree..\n" (my-clock))
		(activate upfeed) (grelay))
	  (upfeed ()
;		  (printf "~a ~a upfeed...\n" (my-id) (my-clock))
		  (greturn (list (int->float (sync-sense)) 1)
			   (to catcher)
			   (via tree)
			   (seed (list 0. 0))
			   (aggr avg_aggr))
		  (timed-call 130 upfeed))

	  (avg_aggr (x y)
		    ;(printf "Average acc: ~a ~a\n"  x y)
		    (return (list (+. (car x) (car y))
				  (+ (cadr x) (cadr y))))))
	  '[regiment-verbose #f]
	  '[sim-timeout 4000]
	  '[simalpha-zeropad-args 'warning] ;; Must be on for sensing
;	  '[simalpha-stream-result #t]
	  )
       unspecified]

    ;; [2005.11.07] Seems to throw an error sometimes!??
     ["Run complex buffered-gradient TM from file"
      , (tm-to-list (car (file->slist (++ (REGIMENTD) "/src/demos/token_machs/buffered_gradients.tm")))
		    '[sim-timeout 5000])
	unspecified]
     
     ["Regiment: aggregate all node ids (using a list accumulator)."
      (map (lambda (ls) (sort < ls))
      (parameterize ([simalpha-channel-model 'lossless]
		     [simalpha-placement-type 'connected]
		     [simalpha-failure-model  'none]
		     [default-slow-pulse '1000]
		     [default-fast-pulse '100]
		     [sim-num-nodes 10]
		     [sim-timeout 2000])
       (run-simulator-alpha 
	(run-compiler 
	 '(rfold append () 
		 (rmap (lambda (n) (cons (nodeid n) '())) world))))))
      ;; Make sure we heard from everyone by the end there:
      , (lambda (ls)
	  (let ((ls (list-head (reverse ls) 2)))
	    (and (apply equal? ls)
		 (equal? (car ls) 
			 (sort < (cons BASE_ID (cdr (iota 10)))))
		 )))] 

     ["Regiment: Fire a simple event when a threshold is crossed."
      (parameterize ([simalpha-channel-model 'lossless]
		     [simalpha-placement-type 'connected]
		     [simalpha-failure-model  'none]
		     ;[simalpha-sense-function-constructor sense-noisy-rising] ;; TODO: FIXME: FINISH this sensing model.
		     [simalpha-zeropad-args 'warning] ;; Must be on for sensing.
		     [simalpha-sense-function-constructor sense-random-1to100]
		     [default-slow-pulse '1000]
		     [default-fast-pulse '100]
		     [sim-timeout 2000])
	(run-simulator-alpha 
	 (run-compiler 
	  '(rwhen-any (lambda (pr) (> (car (cdr pr)) 99))
		      (rmap (lambda (n) (cons (nodeid n) (cons (sense "temp" n) '())))
			    world)))
	 ))
      ,(lambda (ls) (not (null? ls)))]


;; FIXME: FINISH
     ["Regiment: Test an anchor-maximizing anchor election."
      (parameterize ([simalpha-channel-model 'lossless]
		     [simalpha-placement-type 'connected]
		     [simalpha-failure-model  'none]
		     [simalpha-sense-function-constructor sense-dist-from-origin]
		     [simalpha-graphics-on #t]
		     [sim-timeout 2000])
	(run-simulator-alpha 
	 (run-compiler 
	  '(anchor-maximizing (lambda (a_node) (sense "temp" a_node)) world)
	  ;'(anchor-at 30 40)
	  ;'verbose
	  )))

      unspecified
      ]

;; [2006.02.18] Now we test using some of the demo programs.  This is
;; to better our regression testing -- we don't want the demo programs
;; to stop working without us knowing!!

["Demos: smap2_two_anchors.rs"
 (parameterize ([deglobalize-markup-returns #t])
   (load-regiment (++ (REGIMENTD) "/demos/regiment/smap2_two_anchors.rs")))
 ,(lambda (x) 
    (match (map cadr x)
      ;; Receive one or the other first:
      [(#(#f ,b1) #(,a2 ,b2) . ,rest) (guard (equal? b1 b2)) #t]
      [(#(,a1 #f) #(,a2 ,b2) . ,rest) (guard (equal? a1 a2)) #t]
      [,else #f]))]

["Demos: simple/events.rs" retry
 (parameterize ([deglobalize-markup-returns #t])
   (load-regiment (++ (REGIMENTD) "/demos/regiment/simple/events.rs")))
 ,(lambda (ls)
    (andmap (lambda (x) (> (vector-ref x 1) 90)) (map cadr ls)))]

["Demos: nested_regions.rs"
 ;; Expects the nested_regions.rs program to return all node-ids of
 ;; nodes that are neighbors of nodes 6 and 14.
 (parameterize ([deglobalize-markup-returns #f]
		[sim-num-nodes 30]
		[simalpha-realtime-mode #f]
		[simalpha-placement-type 'connected]
		[simalpha-outer-radius 15]
		[simalpha-inner-radius 10]
		;; With no failure, this should return all neighbors:
		[simalpha-failure-model 'none]
		[desugar-gradients-mode 'etx]
		[simalpha-channel-model 'lossless])
   (sort < (list->set 
	    (load-regiment (++ (REGIMENTD) "/demos/regiment/nested_regions.rs")))
	 ))
 ,(lambda (set1)
    (let ([sim (simalpha-current-simworld)])
      ;; Should return the neighbors of 6 and 14 under perfect message conditions.
      ;; If there are no neighbors this test was written wrong.
      (define nbrs1 (nodeid->neighbors 6))
      (define nbrs2 (nodeid->neighbors 14))
      (define set2 (sort < (union '(6 14) nbrs1 nbrs2)))
      ;(printf "\n  Actual neighbors: ~s \n" set2)
      (ASSERT (not (null? set1)))
      (ASSERT (set? nbrs1))
      (ASSERT (set? nbrs2))
      (equal? set1 set2)))]


["Demos: nested_regions_folded.rs -- now with folds"
 ;; Expects the nested_regions.rs program to return all node-ids of
 ;; nodes that are neighbors of nodes 6 and 14.
 (parameterize ([deglobalize-markup-returns #f]
		[sim-num-nodes 30]
		[simalpha-dbg-on #f]
		[simalpha-realtime-mode #f]
		[simalpha-placement-type 'connected]
		[simalpha-outer-radius 15]
		[simalpha-inner-radius 10]
		;; With no failure, this should return all neighbors:
		[simalpha-failure-model 'none]
		[desugar-gradients-mode 'etx]
		[simalpha-channel-model 'lossless])
   (load-regiment (++ (REGIMENTD) "/demos/regiment/nested_regions_folded.rs"))
#;
   (sort < (list->set 
	    (load-regiment (++ (REGIMENTD) "/demos/regiment/nested_regions_folded.rs")))
	 ))

 unspecified]


#;
["Demos: "
 (load-regiment (++ (REGIMENTD) "/demos/regiment/"))
 3]

;; TODO: simple/events
;; TODO: simple/union
;; TODO: simple/fold
;; TODO: Demo average_temperature2
;; TODO: nested_regions
;; TODO: static_elab
;; TODO: tracking??
;; TODO: khood_anchor_at??


#;   ;; FIXME : TODO : REACTIVATE      
     ["Regiment: nested regions, sum number of neighbors."
      (parameterize ([simalpha-channel-model 'lossless]
		     [simalpha-failure-model  'none])
	(run-simulator-alpha 
	 (run-compiler 
	  '(letrec ([nbrs (rmap (lambda (n) (khood (node->anchor n) 1))
				world)]
		    ;; Compute sizes:
		    [sizes (rmap (lambda (a) (rfold + 0 (rmap (lambda (n) 1) a)))
				 nbrs)])
		   ;; Return a list of sizes:
	     (rfold append () (rmap (lambda (x) (cons x '())) sizes)))) 
	'timeout 2000))
      unspecified]
     
     



     ;; TODO FIXME: This causes a system freeze when you attempt to simulate.
#; 
    ["Finish assembly of a simple rfold over a rmap"
      (parameterize ([unique-name-counter 0] [simalpha-dbg-on #t])
      (parameterize ([pass-list (list-remove-before deglobalize (pass-list))])
	(let ((prog
	       (run-compiler
		'(add-places-language
		  '(program
		    (props (result_4 local) (a_1 local)
			   (tmpworld_7 leaf region area distributed)
			   (tmpfunc_8 function local) (result_5 local) (b_2 local)
			   (a_3 local) (tmprmap_9 area distributed)
			   (tmpfunc_10 function local)
			   (result_6 signal distributed final))
		    (control-flow (SOC tmpworld_7 tmprmap_9 result_6))
		    (lazy-letrec
		     ((result_6 100 (X_12) SOC (rfold tmpfunc_10 '0 tmprmap_9))
		      (tmpfunc_10
		       #f
		       _
		       _
		       (lambda (a_3 b_2)
			 (lazy-letrec ((result_5 #f _ _ (+ a_3 b_2))) result_5)))
		      (tmprmap_9 100 (X_11) (X_11) (rmap tmpfunc_8 tmpworld_7))
		      (tmpfunc_8
		       #f
		       _
		       _
		       (lambda (a_1)
			 (lazy-letrec ((result_4 #f _ _ (nodeid a_1))) result_4)))
		      (tmpworld_7 1000 _ _ world))
		     result_6))))))
	      (let ((lst 
		     (let ([prt (open-output-string)])
		       (display "(" prt)
		       (run-simulator-alpha prog 'outport prt)
		       (display ")" prt)
		       (read (open-input-string (get-output-string prt))))))
		lst))))
      foo]	


    ;; If graphics are enabled, we run some very simple tests to draw different world topologies.
    ;; [2005.11.15] I put these at the end so they don't mess up the other numbering.
    ,@(IF_GRAPHICS
       `(["Graphics: draw a gridish world."
	  (begin , (tm-to-socvals '(tokens)
				  '[simalpha-placement-type 'gridlike])
		   (simalpha-draw-world (simalpha-current-simworld)))
	  unspecified]
	 ["Graphics: draw a random world."
	  (begin , (tm-to-socvals '(tokens)
				  '[simalpha-placement-type 'random])
		   (simalpha-draw-world (simalpha-current-simworld)))
	  unspecified]
	 ["Graphics: draw a connected world."
	  (begin , (tm-to-socvals '(tokens)
				  '[simalpha-placement-type 'connected])
		   (simalpha-draw-world (simalpha-current-simworld)))
	  unspecified]
	 )
       ())

)
