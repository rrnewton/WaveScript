
  `( 
    ;; Urg, this is wrong:
    ;    [(deep-assq 'startup (run-compiler '(circle-at '(30 40) 50))) (startup)]
    
    ["Verify that the trivial program produces no token bindings but the defaults"
     (filter (lambda (tokbind)
	       (not (memq (car tokbind) '(spread-global global-tree catcher))))
	     (cdr (deep-assq 'tokens (compile-to-tokens '3))))
     ()]

     
     ["Testing simple combinations of passes: generate a continuation." 
      (let ((toks (cdr (deep-assq 'tokens 
		 (closure-convert (cleanup-token-machine '(tokens (tok1 () (subcall tok2)))))))))
	(let ((x (remq 'SOC-start (remq 'node-start (remq 'actual-node-start (map car toks))))))
	  ;; This is the continuation that was added:
	  (length x)))
      1]
         

    ["Simalpha: Now we test running the Simulator Alpha on a very simple token machine."
     (parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
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
	     [SOC-start () (gemit lightup)]
	   [lightup () (leds toggle red) (grelay)]))
       unspecified]

     ["Respect call order."
      , (tm-to-list
	 '(tokens 
	      (SOC-start () 
			 (call a)
			 (call b)
			 (call c))
	    (a (x) (printf "a "))
	    (b (x) (printf "b "))
	    (c (x) (printf "c "))
	    ))
      (a b c)]

     ["Check call scheduling order."
      , (tm-to-list
       '(tokens 
	    (SOC-start () (call a)
		       (call b))
	  (a (x) (printf "a ") (call a2))
	  (b (x) (printf "b ") (call b2))
	  (a2 (x) (printf "a2 "))
	  (b2 (x) (printf "b2 "))
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

     ["Timed tokens: test the simulator with timed tokens."
      , (tm-to-list '(tokens 
			 (SOC-start () 
				    (timed-call 200 tok1)
				    (timed-call 100 tok2))
		       (tok1 (x) (printf "tok1 "))
		       (tok2 (x) (printf "tok2 ")
			     (timed-call 50 tok3))
		       (tok3 (x) (printf "tok3 "))))
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
	    (a (x) (printf "a "))
	    (b (x) (printf "b "))
	    (c (x) (printf "c "))
	    ))
	(c b a)]

     ["Timed tokens 5"
      , (tm-to-list 
	 '(tokens 
	      (SOC-start () 
			 (timed-call 200 a)
			 (call b)
			      (timed-call  50 c))
	    (a (x) (printf "a "))
	    (b (x) (printf "b "))
	    (c (x) (printf "c "))
	    ))
	(b c a)]

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
	      (and (= (- y x) SCHEDULE_DELAY)
		   (> z 100))]))
	]
			 
    ["Test reading sensor values in simulation."
     , (tm-to-socvals
	'(tokens
	   (SOC-start () (call loop 100))
	   (loop (reps) (if (> reps 0)
			    (begin (soc-return (list (my-clock) (local-sense)))
				   (timed-call 100 loop (- reps 1))))))
	'[simalpha-sense-function sense-sine-wave])
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
      , (tm-to-list
		 '(tokens 		   
		   (SOC-start ()
			      (printf "a ")
			      (subcall tok1)
			      (printf "c ")
			      (call tok2)
			      (printf "d "))
		   (tok1 () (printf "b "))
		   (tok2 () (printf "e "))
		 ))
	(a b c d e)]

     ["Subcall: Make sure simulator can handle subcalls directly if need be #2."
      , (tm-to-list
		 '(tokens 		   
		   (SOC-start () (printf "~a " (subcall tok1)))
		   (tok1 () (return 349))
		 ))
	(349)]

     ;; FIXME: Add better oracle
     ["Testing sim: 'manually' propogate a flood"
      , (tm-to-list (car (file->slist "demos/manual_tree.tm"))
		    '[simalpha-timeout 5000])
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
	      '(parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
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
	    (fluid-let ((pass-names '(cleanup-token-machine cps-tokmac )))
	      ,commontest)
	    (result 2007)]
	   ["Same test but now with closure-convert"
	    (fluid-let ((pass-names '(cleanup-token-machine cps-tokmac closure-convert cleanup-token-machine)))
	      ,commontest)
	    (result 2007)]))

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

     ["Now keep stored vars on all 1-hop neighbors, make sure they don't change."  
      ,(tm-to-list
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
	    (fluid-let ((pass-names '(cleanup-token-machine 
				      desugar-let-stored  rename-stored
				      cps-tokmac      )))
	      ,common)	   
	    (c ab c b)]
	   ["Same test but with closure-convert."
	    (fluid-let ((pass-names '(cleanup-token-machine 
				      desugar-let-stored  rename-stored
				      cps-tokmac closure-convert   )))
	      ,common)
	    (c ab c b)]))


     ,@(let ((common 
	      '(parameterize ((unique-name-counter 0)
			      (simalpha-dbg-on #f))
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
	   (fluid-let ((pass-names '(cleanup-token-machine 
				     desugar-let-stored  rename-stored
				     cps-tokmac )))
	     ,common)
	   (ab b c c)]
	   ["And one last time using subcall and closure convert."
	    (fluid-let ((pass-names '(cleanup-token-machine 
				      desugar-let-stored  rename-stored
				      cps-tokmac closure-convert cleanup-token-machine)))
	      ,common)
	    (ab b c c)
	    ;(c ab c b)
	    ]))

     ;; FIXME BUG: I got an error on this test when running all units.
     ;; Unfortunately, it was not repeatable, and I don't know what the error was.
     ;; I should modify the unit tester to save output for erroneous tests. [2005.10.17]
     ["Test gradient ghopcount, gversion, gparent, gorigin."
      (let ((lst ,(tm-to-list
		   '(tokens
		      (SOC-start () (gemit tok1))
		      (tok1 () (printf "(~a : ~a ~a ~a ~a : ~a)" (my-id) (gparent) (gorigin) (ghopcount) (gversion) (my-clock)))
		      ))))
	(let ((base (cdr (assq BASE_ID lst)))
	      (others (map cdr (alist-remove BASE_ID lst))))
	  (if (all-equal? others)
	      ;; Return something that won't vary based on sim parameters:
	      (list (assq BASE_ID lst) (car others))
	      `(ERROR: ,others))))
      ;; This timing stuff is a bit fragile
      ((10000 : atroot 10000 0 1 : 2) (: 10000 10000 1 1 : ,(+ RADIO_DELAY SCHEDULE_DELAY 1)))]


     ;; TODO: need to explicitely control tho network parameters for this one:
     ["Gradients: just a gemit and unconditional grelay. (NONDETERMINISTIC)" 
      retry ;; Retry this test if it fails... it's nondeterministic.
      (let ((lst , (tm-to-list
		    '(tokens
			 (SOC-start () (gemit tok1))
		       (tok1 (x) (printf "~a " (gdist)) (grelay))))))
	(list (length lst) 
	      (car lst)
	      (cadr lst)
	      (> (car (reverse lst)) 1)
	      (equal? lst (sort < lst)))
	) ;; Only true with VERY restricted simulation model.
      (30 0 1 #t #t)]


     ["Gradients: make a two hop neighborhood. (NONDETERMINISTIC)"
      (let ((lst , (tm-to-list
		'(tokens
		  (SOC-start () (gemit tok1))
		  (tok1 (x) (printf "~a " (gdist)) (if (< (gdist) 2) (grelay)))))))
	(list (< (length lst) 30)
	      (sort < (list->set lst))
	      (equal? lst (sort < lst)))) ;; Only true with VERY restricted simulation model.
      (#t (0 1 2) #t)]

;; This case was too fragile and dependent on the ordering.  I could make it better and bring it back.
;; Problem is that it depends on the aggregator being turned on, because without aggregation we no longer use the timer.
     ["Gradients: Make sure the timer gets set right. "
      (parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
      (fluid-let ([pass-names
		   '(cleanup-token-machine  desugar-gradients
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
			(greturn (my-id) (to catcher)))
		  ))
      ,(lambda (lst)
	 (let ((lst (filter number? lst)))
	   (and
	    (eq? BASE_ID (car lst))
	    (not (memq BASE_ID (cdr lst)))
	    (> (length lst) 1)
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
		'[simalpha-timeout #f]
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
	 (= (simalpha-num-nodes)
	    (length (list->set (apply append x))))
	 ))]

     ["Gradients: execute a repeated return from 2-hop neighbors. (NONDETERMINISTIC)"
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
			(if (= (gdist) 1)
			    (greturn (gversion) (to catcher))
			(if (= (gdist) 2)
			    (greturn (+ 100 (gversion)) (to catcher)))
			))
		  ))))
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

     ["Gradients: Now try aggregated greturn."
      (filter (lambda (x) (not (zero? x)))
	      ,(tm-to-list
		'(tokens 
		  (SOC-start () (gemit tok1))
		  (catcher (x) (printf "~a " x))
		  (tok1 () 
			(greturn (gdist)
				 (to catcher)
				 (seed 0)
				 (aggr sum)))
		  (sum (x y) (+ x y)))
		'[regiment-verbose #f]))
      ,(lambda (ls)
	 (and (= (length ls) 1)
	      (> (car ls) 0)))]

     ["Gradients: Now try cons-aggregated greturn from one-hops"
      retry  ;; It's possibly we have no neighbors at all!
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
			     (greturn (list (gdist))
				      (to catcher)
				      (seed ())
				      (aggr f)))
		       (f (x y) (append x y))))))
      ;; Epoch staggered aggregation
      ((0) (0 1) (0 1) (0 1) (0 1) (1))
      ]

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
		(grelay)
		(greturn (list (gdist))
			 (to catcher)
			 (seed ())
			 (aggr f)))
	  (f (x y) (append x y)))
	'[regiment-verbose #f]
	'[simalpha-placement-type 'gridlike]
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
		'[simalpha-num-nodes 30])
      ,(lambda (ls)
	 ;; Received all messages:
	 (= (length ls) 5))]

     ["Gradients: return value through three nested gradients. (NONDETERMINISTIC)"
      retry ;; Must retry, network might not be connected
     , (tm-to-list
		'(tokens
		  (SOC-start () 
			     (gemit a)
			     (timed-call 1000 return-up))
		  (return-up ()
			     (greturn 8967
				      (to catcher1)
				      (via c)))
		  (catcher1 (v) (greturn v 
					 (to catcher2)
					 (via b)))
		  (catcher2 (v) (greturn v
					 (to catcher3)
					 (via a)))
		  (catcher3 (v) (printf "~a ~a ~a ~a" (my-id) 
					(> (my-clock) 1000)
					(< (my-clock) 2000)
					v))

		  (a () (grelay)
		        (if (= (my-id) 15)
			      (gemit b)))
		  (b () (grelay)
		        (if (= (my-id) 20)
			      (gemit c)))
		  (c () (grelay))
		  )
		'[simalpha-failure-model 'none]
		'[simalpha-channel-model 'lossless]
		'[simalpha-consec-ids #t]
		'[simalpha-num-nodes 30])
      (,BASE_ID #t #t 8967)]


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
		'[simalpha-num-nodes 30])
      ;; FIXME : Finish
      unspecified]

     ["Test soc-return (#1).  Try it w/out desugar-soc-return."
      (parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
      (fluid-let ([pass-names (rdc (list-remove-after 'desugar-soc-return pass-names))])
	(let ([prog (run-compiler 399)])
	  (run-simulator-alpha prog))))
      (399)]

     ["Test soc-return (#2).  Try it WITH desugar-soc-return, but still on base station."
      (parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
      ;; Go all the way through desugar-gradients and the subsequent cleanup-token-machine
      (fluid-let ([pass-names (rdc (list-remove-after 'cps-tokmac pass-names))])
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
	'[simalpha-num-nodes 3]
	'[simalpha-consec-ids #f]
	'[simalpha-timeout 10000])
       unspecified]


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
	'[simalpha-num-nodes 10]
	'[simalpha-consec-ids #f]
	'[simalpha-placement-type 'connected]
	'[simalpha-channel-model 'linear-disc]
	'[simalpha-timeout 10000])
       unspecified]
	
    ["Now test elect-leader macro."
     , (tm-to-list
	'(tokens
	   [SOC-start () (elect-leader tok1)]
	   [tok1 () (printf "~a " (my-id))])
	'[simalpha-channel-model 'lossless]
	'[simalpha-num-nodes 10])
       unspecified]
	
    ["Test flood macro."
     , (tm-to-list
	'(tokens
	   [SOC-start () (flood tok1)]
	   [tok1 () (printf "~a " (my-id))])
	'[simalpha-channel-model 'lossless]
	'[simalpha-placement-type 'connected]
	'[simalpha-num-nodes 10]
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
		    [simalpha-timeout 400])
       (run-simulator-alpha (run-compiler '(rmap nodeid world))))
     ,(lambda (ls)
	;; Can't make very strong statements about timing, but we
	;; shoud have heard from the first *two* generations by this
	;; time:
	;; UNLESS, RADIO_DELAY is set really large:
	(and (> (length ls) (* 2 (simalpha-num-nodes)))
	     (equal?
	      (sort < (cons BASE_ID (cdr (iota (simalpha-num-nodes)))))
	      (sort < (list->set ls)))))]

    ["Run a simple fold in regiment." 
     (parameterize ([simalpha-channel-model 'lossless]
		    [simalpha-failure-model  'none]
		    [simalpha-num-nodes 30])
       (run-simulator-alpha 
	(run-compiler 
	 '(letrec ([readings (rmap sense world)]
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
		    [simalpha-inner-radius 10]
		    [simalpha-outer-radius 20]
		    [simalpha-world-xbound 60]
		    [simalpha-world-ybound 60]
		    [simalpha-num-nodes 10])
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
		    [simalpha-sense-function sense-sine-wave])
       (run-simulator-alpha 
	(run-compiler 
	 '(letrec ([readings (rmap (lambda (n) (cons (sense n) 1))
				   world)]
		   [aggr (lambda (x y)
			   (cons (+ (car x)
				    (car y))
				 (+ (cdr x)
				    (cdr y))))]
		   [div (lambda (v) 
			  (if (= (cdr v) 0) 0 ;; Just return zero if there are no samples to avg.
			      (/ (car v) (cdr v))))]
		   [sums (rfold aggr (cons 0 0) readings)]
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
		    [simalpha-sense-function sense-sine-wave])
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
	 '(letrec ([readings (rmap (lambda (n) (cons (sense n) 1))
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
	 '(letrec ([readings (rmap (lambda (n) (vector (sense n) 1))
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
	  (spread () (gemit tree) (timed-call 600 SOC-start))
	  (catcher (v) 
		   (if (> (cadr v) 0)
		       (begin 
;			 (printf "Got soc-val at time ~a: (~a ~a) avg: ~a\n"
;				 (my-clock) (car v) (cadr v) (/. (car v) (int->float (cadr v))))
			 (soc-return (list (my-clock) 
					   (local-sense)
					   (float->int (/. (car v) (int->float (cadr v)))))))
		       (printf "Soc-val at time ~a: but it's empty!!\n" (my-clock))))

	  (tree () 
;		(printf "~a tree..\n" (my-clock))
		(activate upfeed) (grelay))
	  (upfeed ()
;		  (printf "~a ~a upfeed...\n" (my-id) (my-clock))
		  (greturn (list (int->float (local-sense)) 1)
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
	  '[simalpha-timeout 4000]
;	  '[simalpha-stream-result #t]
	  )
       unspecified]

     ["Run complex buffered-gradient TM from file"
      , (tm-to-list (car (file->slist "demos/buffered_gradients.tm")) 
		    '[simalpha-timeout 5000])
      unspecified]


     ;; TODO FIXME: This causes a system freeze when you attempt to simulate.
#; 
    ["Finish assembly of a simple rfold over a rmap"
      (parameterize ([unique-name-counter 0] [simalpha-dbg-on #t])
      (fluid-let ([pass-names (list-remove-before 'deglobalize pass-names)])
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

)