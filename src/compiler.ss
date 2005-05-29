;; This is the core.  Loaded by both the Chez and PLT versions.

;;======================================  
;(display "Loading main compiler module.  RegionStreams Demo.")
;(newline)

(define-regiment-parameter regiment-version 0.58)

;; This is a global variable mutated by the node-programs in the
;; simulator_nought...  Counts total communications received.
;;(define total-messages 0)
;; Moved this *again*.  Now the simulator just defines this (via
;; eval), when it starts running.

(define pass-names
  '(verify-regiment
    eta-primitives
    rename-var
    remove-unquoted-constant                        ;;  5
    
    static-elaborate
    
    reduce-primitives    
    remove-complex-constant                         ;;  7
    uncover-free                                    ;; 14
    ;    convert-closure                                 ;; 15
    lift-letrec                                     ;; 16
    lift-letrec-body                                ;; 22
    remove-complex-opera*
    verify-core
    classify-names
    add-heartbeats
    add-control-flow
    add-places
    ;    add-routing
    analyze-places
    deglobalize
    
    cleanup-token-machine    

    desugar-gradients
    cleanup-token-machine   ;; Rerun to expand out some stuff.

    desugar-let-stored
    rename-stored
    
    ;    analyze-tokmac-recursion
    ;    inline-tokmac
    cps-tokmac
    closure-convert
    ;    verify-token-machine
    ;    haskellize-tokmac 
    ))



;; ==================================================================
;; Functions for input/output to filesystem and for invoking compiler.

(define (dump-tokenmachine-to-file prog fn)
  (match prog
    [(haskellize-tokmac-language ,str)
     (with-output-to-file fn
       (lambda () (display str) (newline))
       'replace)]
    ;; If it's an earlier file, pretty print it:
    [(,lang ,prog)
     (with-output-to-file fn
       (lambda () (pretty-print `(,lang ,prog)))
       'replace)]
    [,other (error 'dump-tokenmachine-to-file "invalid input: ~S" prog)]))

;; This dumps to file only when provided the optional string filename argument:
;; The symbolic options are:  'barely-tokens 'almost-tokens 'almost-haskell 'haskell-tokens
(define (run-compiler p . args )
  (let ([filename #f]
	[passes pass-names])    
    (for-each (lambda (arg)
		(cond
                  [(string? arg) ;; It's an output filename.
                   (set! filename arg)]
                  [(eq? arg 'barely-tokens)
                   (set! passes (list-remove-after 'deglobalize pass-names))]
                  [(eq? arg 'almost-tokens)
                   (set! passes (list-remove-first 'deglobalize
                                                   (list-remove-after 'deglobalize pass-names)))]
                  [(eq? arg 'almost-haskell)
                   (set! passes (remq 'haskellize-tokmac pass-names))]
                  [(eq? arg 'haskell-tokens) (void)]))
	      args)
    (let ((funs (map eval passes)))
      (let loop ([p p] [funs funs])
        (if (null? funs) 
            (begin (if filename (dump-tokenmachine-to-file p filename)
                       p))
(loop ((car funs) p) (cdr funs)))))))

;; This one just stops after deglobalize:
(define (compile-to-tokens p . args)
  (apply run-compiler p 'barely-tokens args))
(define (compile-almost-to-tokens p . args)
  (apply run-compiler p 'almost-tokens args))

(define rc run-compiler) ;; shorthand
(define ct compile-to-tokens) ;; shorthand

;; This finishes off the compilation of scheme-format token machine.
(define (assemble-tokmac tm . args)
  (let ([starting-place 
	 (match tm 
		[(,lang ,prog)
		 (case lang
		   [(deglobalize-lang) 'deglobalize]
		   [(cleanup-token-machine-lang) 'cleanup-token-machine]
		   [(cps-tokmac-lang) 'cps-tokmac]

		   ;[(haskellize-tokmac-lang) (error...
		   [else 'deglobalize])])
	 ])
  (let ((passes (cdr (list-remove-before starting-place pass-names))))
;    (lambda (tm)
      (fluid-let ([pass-names passes])
	(apply run-compiler tm args)))))

(define at assemble-tokmac) ;; shorthand
(define assemble at) ;; shorthand

(define test
  (lambda (set)
    (fluid-let ([tests 
		 (map (lambda (p) 
			`(base-language '(program ,p)))
		      set)])
      (test-all))))

(define r  ;; shorthand
  (letrec ((loop
	    (case-lambda 
	     [(pass x)
	      (let ((prog  x))
		(fluid-let ((pass-names (list-remove-after pass pass-names)))
		  (parameterize ((tracer #t))
				(test-one prog))))]
	     [(x) (loop (rac pass-names) x)])))
    loop))

;; Token and later compiler:
(define (tr x)  ;; shorthand
  (let ((prog  x))
    (parameterize ((tracer #t)
		   (game-eval (lambda args 'unspecified))
		   (host-eval (lambda args 'unspecified)))
      (fluid-let ((pass-names (cdr (list-remove-before 'deglobalize pass-names))))
	(test-one prog)))))

;; Temp =============================================================

(newline)

(define (rr) (r '(circle 50 (anchor-at '(30 40))))) ;; shorthand

(define (doit x) ;; shorthand
  (cleanse-world)
  (run-simulation (build-simulation (compile-simulate-nought x))		  
		  20.0))

;; This is my big target program!!
'(define theprog
  '(let* ((R (circle-at 50 '(30 40)))
	 (f (lambda (tot next)
	      (cons (+ (car tot) (sense next))
		    (+ (cdr tot) 1))))
	 (g (lambda (tot) (/ (car tot) (cdr tot))))
	 (avg (smap g (rfold f (cons 0 0) R))))
    (until (pred (lambda (x) (> x 15.3)) avg)
	   R
	   (circle-at 100 '(0 0)))))

(define prog
  '(program
     (bindings (tmp_3 (cons '40 '())) (tmp_1 (cons '30 tmp_3)))
     (socpgm (bindings) (call f_token_result_2))
     (nodepgm
       (tokens
         (f_token_result_2 () (flood token_4))
         (token_4
           ()           
           (if (< (locdiff (loc) tmp_1) 10.0)
               (begin
                 (disp "PROG: blah blah calling elect leader")
                 (elect-leader m_token_result_2))
               '#f))
         (m_token_result_2 ()
                           (disp "PROG: Bang, election finished, got result..")
                           (soc-return (list 'anch this))))
       (startup))))


;; These are some temporary diagnostic functions:
(define (all-incoming) ;; shorthand
  (filter (lambda (ls) (not (null? ls)))
          (map simobject-incoming all-objs)))
(define (all-homepage) ;; shorthand
  (filter (lambda (ls) (not (null? ls)))
          (map simobject-homepage all-objs)))


(define (sim) (build-simulation  ;; shorthand
	     (compile-simulate-nought 
	      (cadadr (run-compiler '(anchor-at '(30 40)))))))

;; HOW TO RUN:
;; ----------------------------------------
;; Here I'll give some examples of how to run the system.  
;; This is for me -- because I forget how to invoke things over time.
;; (Though the unit tests give me something...)

;; simulator_nought.examples.ss -- has some example token machines.


;(define (t1) (init-world) (run-simulation        sim 2.0))
;(define (t2) (init-world) (run-simulation-stream sim 2.0))

;; Sigh, first class tokens:
;(r '(rmap (lambda (x) (rmap (lambda (y) y) world)) world)) 
  

(define theprog '(rfold + 0 (rmap sense world)))

;; I'm binding all these little random letter combinations!  Bah!
(define mp;;myprog ;; shorthand
;  '(rfold + 0 (rmap sense (circle-at '(30 40) 10))))
  '(rfold + 0 (rmap sense (khood-at '(30 40) 10))))


;; This requires pass21_cleanup-token-machine.ss as well as helpers.ss
;; This handles writing the generated code to a file and loading it back.
;; FLAGS:
;; 'numnodes int -- Set the number of nodes in the world to int.
;; 'outport prt  -- Set the printed output of the simulation to port prt.
;; 'srand int    -- Seed the random number generator with int.
(define run-simulator-alpha
  (letrec ([run-alpha-loop
	    (lambda args
	      (match args
		     ;; This is a weak requirement... 
		     ;; We consider the first arg to represent a token machine if it is a *list*.
		     [(,tm . ,rest) (guard (list? tm))
		      (match tm
			;; Already compiled
			[(define (node-code this) ,e)
			 (let ((out (open-output-file "_genned_node_code.ss" 'replace)))			      
			   (parameterize ([print-level #f]
					  [pretty-maximum-lines #f]
					  [print-graph #t])
   		              (write tm out);(pretty-print tm out)
			      (newline out)
			      (close-output-port out)))
			 (read-params rest)]
			;; Otherwise compile it
			[,tm			     
			 (let ((cleaned tm )) ;;;(cleanup-token-machine tm)))
			   (let ([comped (compile-simulate-alpha cleaned)])
			     (let ((out (open-output-file "_genned_node_code.ss" 'replace)))
;			    (printf "Ouputting token machine to file: _genned_node_code.ss~n")
			    (parameterize ([print-level #f]
					   [pretty-maximum-lines #f]
					   [print-graph #t])				    					  
			    (pretty-print comped out)
			    (newline out)
			    (newline out)
			    (display ";; Above is compiled program for this token machine: " out)
			    (newline out)
			    (display "'" out)
			    (pretty-print tm out)
			    (newline out))
			    (close-output-port out))
			  (read-params rest)
			  ))])]
		     [(,rest ...) (read-params rest)]
		     ))]
	    [read-params
	     (lambda (params)	       
	       (match params
;		      [,x (guard (disp "read params" params) #f) 3]
		      [() 
		       (load "_genned_node_code.ss")
                       ;; We have to do this because of the module system:
                       (let ((node-code (eval 'node-code)))
                         ;(disp "NODE CODE:" node-code) ;" eq: " (eq? node-code (eval 'node-code)))
                         ;(printf "Node code loaded from file.~n")
                         ;(if (not node-code)  (error 'run-simulator-alpha "node-code not defined!"))
                         (start-alpha-sim node-code 10.0 'simple))]
		      [(numnodes ,n . ,rest)
		       (if (not (integer? n))
			   (error 'run-simulator-alpha
				  "'numnodes switch should be followed by an integer, not: ~a" n))
		       (parameterize ([simalpha-num-nodes n])
				     (read-params rest))]
		      [(outport ,p . ,rest)
		       (if (not (output-port? p))
			   (error 'run-simulator-alpha
				  "'outport switch should be followed by a port object, not: ~a" n))
		       (parameterize ([simalpha-output-port p])
				     (read-params rest))]
		      [(srand ,n . ,rest)
;		       (if (not (integer? n))
;			   (error 'run-simulator-alpha
;				  "'srand switch should be followed by an integer, not: ~a" n))
		       (let ([stored-state #f])
			 (dynamic-wind
			     (lambda () (set! stored-state (reg:get-random-state)))
			     (lambda () (read-params rest))
			     (lambda () (reg:set-random-state! stored-state))))
		       ]))])
	   run-alpha-loop))
	    
(define ra run-simulator-alpha) ;; shorthand

;;======================================================================

;; These are some of our system tests.  They test the compiler and the simulator as a whole.
;; The rest of the system tests are in the files named tests_*.ss
;; But some of the below tests may also be miscellaneous unit tests that require more than one module.
(define these-tests 
  `( 
    ;; Urg, this is wrong:
    ;    [(deep-assq 'startup (run-compiler '(circle-at '(30 40) 50))) (startup)]
    
    ["Verify that the trivial program produces no token bindings but the defaults"
     (filter (lambda (tokbind)
	       (not (memq (car tokbind) '(spread-global global-tree))))
	     (cdr (deep-assq 'tokens (compile-to-tokens '3))))
     ()]

    ["Now we test running the Simulator Alpha on a very simple token machine."
     (let ((prt (open-output-string)))
       (display "(" prt)
       (run-simulator-alpha 
	(cleanup-token-machine '(tokens (node-start () (display " ") (display (my-id)))))
	'outport prt)
       (display ")" prt)
       (read (open-input-string (get-output-string prt))))
     ,(lambda (ls) 	
	(set-equal? (list->set ls)
		    (list->set (cons BASE_ID (cdr (iota 30))))))]


    ;; [2005.05.29] Note tok1 should be statically called and is currently called dynamically!
    ;; Oh duh, that's because all calls go through the dyndispatch table.
     ["Run simulator on simple subcall program." 
      (fluid-let ((pass-names '(cleanup-token-machine cps-tokmac closure-convert)))
	 (let ((prog 
		(cleanup-token-machine (run-compiler
	       '(tokens 
		 (SOC-start () (printf "result ~a" (subcall tok1 3)))
		 (tok1 (x) (return (+ x 300)))
		 )))))
	   
	   (disp "CLEANED")(pretty-print prog)

	   (let ((prt (open-output-string)))
	     (display "(" prt)       
	     (run-simulator-alpha prog 'outport prt)
	     (display ")" prt)
	     (read (open-input-string (get-output-string prt))))))
     (result 303)]
     
     ,@(let ([commontest 
	     '(let ((prog 
		     (run-compiler
		      '(tokens 
			(SOC-start () (printf "result ~a" (+ (subcall tok1 4) (subcall tok1 3))))
			(tok1 (x) (return (+ x 1000)))
			))))
		(let ((prt (open-output-string)))
		  (display "(" prt)
		  (run-simulator-alpha prog 'outport prt)
		  (display ")" prt)
		  (read (open-input-string (get-output-string prt)))))])	 
	 `(["Add two subcalls (only through cps-tokmac)"
	    (fluid-let ((pass-names '(cleanup-token-machine cps-tokmac )))
	      ,commontest)
	    (result 2007)]
	   ["Same test but now with closure-convert"
	    (fluid-let ((pass-names '(cleanup-token-machine cps-tokmac closure-convert)))
	      ,commontest)
	    (result 2007)]))
     
     ["Testing simple combinations of passes: generate a continuation." 
      (let ((toks (cdr (deep-assq 'tokens 
		 (closure-convert (cleanup-token-machine '(tokens (tok1 () (subcall tok2)))))))))	
	(let ((x (remq 'SOC-start (remq 'node-start (map car toks)))))
	  ;; This is the continuation that was added:
	  (length x)))
      1]



    		
     ["Write a troublesome simulator program to disk and try to execute it."
      (let ((prt (open-output-string)))
	(display "(" prt)
	(run-simulator-alpha
	 '(define (node-code this) (let ((local-sense (lambda () ((current-sense-function) (node-pos (simobject-node this)))))) (let* () (letrec ((SOC-return-handler (lambda #0=(current-vtime subtokind . vals) (let ((x #1=(quote sim-alpha-uninitialized))) (let #2=((numvals (length vals))) (if (< numvals 1) (warning #3=(quote simulator-alpha) #4="executing ~a padding args ~a with zero." (quote SOC-return-handler) (list-tail (quote #5=(x)) . #6=(numvals)))) (if (> numvals 1) (error #7=(quote simulator-alpha) #8="executing ~a, got excess vals ~a for args ~a" (quote SOC-return-handler) vals (quote #5#))) (if #9=(null? vals) (set! x . #10=(0)) (begin (set! x . #11=((car vals))) . #12=((set! vals (cdr vals))))) #13="Done initializing arguments." (let* (#14=(the-store (simobject-token-store this)) (simtok-obj (make-simtok (quote SOC-return-handler) . #15=(subtokind))) . #16=((old-outgoing (simobject-outgoing-msg-buf this)) (old-local (simobject-local-msg-buf this)))) #17=(DEBUGMODE (check-store the-store)) #18="Is there already an allocated token object?:" (let #19=((tokobj (hashtab-get the-store simtok-obj))) (if #20=(not tokobj) (begin #21="If not, then we allocate that token object..." #22=" setting the invoke counter to zero." (set! tokobj (vector 0)) . #23=((hashtab-set! the-store simtok-obj tokobj)))) #24=(set-simobject-outgoing-msg-buf! this (quote ())) #25=(set-simobject-local-msg-buf! this (quote ())) (if (= (quote 10000) #26=(node-id (simobject-node this))) (simulator-soc-return x) (error (quote SOC-return-handler) "ran on non-base node! id: ~a" #26#)) . #27=((set-simobject-outgoing-msg-buf! this (append (reverse (simobject-outgoing-msg-buf this)) old-outgoing)) (set-simobject-local-msg-buf! this (append (reverse (simobject-local-msg-buf this)) old-local)) (void)))))))) (node-start (lambda #0# (let () (let #2# (if (< numvals 0) (warning #3# #4# (quote node-start) (list-tail (quote ()) . #6#))) (if (> numvals 0) (error #7# #8# (quote node-start) vals (quote ()))) #13# (let* (#14# (simtok-obj (make-simtok (quote node-start) . #15#)) . #16#) #17# #18# (let #19# (if #20# (begin #21# #22# (set! tokobj (vector 0)) . #23#)) #24# #25# (void) . #27#)))))) (SOC-start (lambda #0# (let () (let #2# (if (< numvals 0) (warning #3# #4# (quote SOC-start) (list-tail (quote ()) . #6#))) (if (> numvals 0) (error #7# #8# (quote SOC-start) vals (quote ()))) #13# (let* (#14# (simtok-obj (make-simtok (quote SOC-start) . #15#)) . #16#) #17# #18# (let #19# (if #20# (begin #21# #22# (set! tokobj (vector 0)) . #23#)) #24# #25# (set-simobject-local-msg-buf! this (cons (make-simevt #f (bare-msg-object (make-simtok (quote tok1) 0) (list (begin #28="This whole block represents the allocation of a continuation closure:" (let ((kind_4 (if (hashtab-get the-store (make-simtok (quote K_3) 0)) (let ((new (+ (quote 1) (let ((exttokobj (hashtab-get the-store (make-simtok (quote K_3) 0)))) (if exttokobj (vector-ref exttokobj 1) . #29=(#f)))))) (begin (let ((exttokobj (hashtab-get the-store (make-simtok (quote K_3) 0)))) (if exttokobj (vector-set! exttokobj 1 new) (warning #30=(quote ext-set!) #31="token not present: ~a" (quasiquote (K_3 . subtok))))) new)) (begin #32="Allocate this zeroeth token object just to hold a counter MEMORY WASTEFUL!:" (set-simobject-local-msg-buf! this (cons (make-simevt #f (bare-msg-object (make-simtok (quote K_3) 0) (list (quote 11) (void)) . #33=(current-vtime))) . #34=((simobject-local-msg-buf this)))) (quote 1))))) (begin #35="Do the actual token object (closure) allocation.  Capture freevars:" (set-simobject-local-msg-buf! this (cons (make-simevt #f (bare-msg-object (make-simtok (quote K_3) kind_4) (list (quote 11)) . #33#)) . #34#)) #36="Return the name of this continuation object:" (make-simtok (quote K_3) kind_4)))) (quote 4)) . #33#)) . #34#)) . #27#)))))) (K_3 (lambda #0# (let ((flag #1#) (fv0 #1#)) (let #2# (if (< numvals 2) (warning #3# #4# (quote K_3) (list-tail (quote #37=(flag fv0)) . #6#))) (if (> numvals 2) (error #7# #8# (quote K_3) vals (quote #37#))) (if #9# (set! flag . #10#) (begin (set! flag . #11#) . #12#)) (if #9# (set! fv0 . #10#) (begin (set! fv0 . #11#) . #12#)) #13# (let* (#14# (simtok-obj (make-simtok (quote K_3) . #15#)) . #16#) #17# #18# (let #19# (if #20# (begin #21# #22# (set! tokobj (vector 0 0)) . #23#)) #24# #25# (if (= flag (quote 11)) (if (= subtokind (quote 0)) (void) (void)) (begin (set-simobject-local-msg-buf! this (cons (make-simevt #f (bare-msg-object (make-simtok (quote tok1) 0) (list (begin #28# (let ((kind_2 (if (hashtab-get the-store (make-simtok (quote K_1) 0)) (let ((new (+ (quote 1) (let ((exttokobj (hashtab-get the-store (make-simtok (quote K_1) 0)))) (if exttokobj (vector-ref exttokobj 1) . #29#))))) (begin (let ((exttokobj (hashtab-get the-store (make-simtok (quote K_1) 0)))) (if exttokobj (vector-set! exttokobj 1 new) (warning #30# #31# (quasiquote (K_1 . subtok))))) new)) (begin #32# (set-simobject-local-msg-buf! this (cons (make-simevt #f (bare-msg-object (make-simtok (quote K_1) 0) (list (quote 11) (void)) . #33#)) . #34#)) (quote 1))))) (begin #35# (set-simobject-local-msg-buf! this (cons (make-simevt #f (bare-msg-object (make-simtok (quote K_1) kind_2) (list (quote 11) fv0) . #33#)) . #34#)) #36# (make-simtok (quote K_1) kind_2)))) (quote 3)) . #33#)) . #34#)) (hashtab-remove! the-store (make-simtok (quote K_3) subtokind)))) . #27#)))))) (K_1 (lambda #0# (let ((flag #1#) (fv0 #1#)) (let #2# (if (< numvals 2) (warning #3# #4# (quote K_1) (list-tail (quote #38=(flag fv0)) . #6#))) (if (> numvals 2) (error #7# #8# (quote K_1) vals (quote #38#))) (if #9# (set! flag . #10#) (begin (set! flag . #11#) . #12#)) (if #9# (set! fv0 . #10#) (begin (set! fv0 . #11#) . #12#)) #13# (let* (#14# (simtok-obj (make-simtok (quote K_1) . #15#)) . #16#) #17# #18# (let #19# (if #20# (begin #21# #22# (set! tokobj (vector 0 0 (quote 0))) . #23#)) #24# #25# (if (= flag (quote 11)) (if (= subtokind (quote 0)) (void) (begin (vector-set! tokobj 2 fv0))) (begin (printf (quote "result ~a") (+ (vector-ref tokobj 2) fv0)) (hashtab-remove! the-store (make-simtok (quote K_1) subtokind)))) . #27#)))))) (tok1 (lambda #0# (let ((k_58 #1#) (x #1#)) (let #2# (if (< numvals 2) (warning #3# #4# (quote tok1) (list-tail (quote #39=(k_58 x)) . #6#))) (if (> numvals 2) (error #7# #8# (quote tok1) vals (quote #39#))) (if #9# (set! k_58 . #10#) (begin (set! k_58 . #11#) . #12#)) (if #9# (set! x . #10#) (begin (set! x . #11#) . #12#)) #13# (let* (#14# (simtok-obj (make-simtok (quote tok1) . #15#)) . #16#) #17# #18# (let #19# (if #20# (begin #21# #22# (set! tokobj (vector 0)) . #23#)) #24# #25# (set-simobject-local-msg-buf! this (cons (make-simevt #f (bare-msg-object k_58 (list (quote 99) (+ x (quote 1000))) . #33#)) . #34#)) . #27#))))))) (let ((dyndispatch_table (make-default-hash-table))) (begin (void) (hashtab-set! dyndispatch_table (quote SOC-return-handler) SOC-return-handler) (hashtab-set! dyndispatch_table (quote node-start) node-start) (hashtab-set! dyndispatch_table (quote SOC-start) SOC-start) (hashtab-set! dyndispatch_table (quote K_3) K_3) (hashtab-set! dyndispatch_table (quote K_1) K_1) (hashtab-set! dyndispatch_table (quote tok1) tok1)) (lambda (msgob current-vtime) (mvlet (((name subtok) (let ((tok (msg-object-token msgob))) (values (simtok-name tok) (simtok-subid tok))))) (let ((handler (hashtab-get dyndispatch_table name))) (if (not handler) (error (quote node-code) "dyndispatch: no handler for token name: ~a in table: ~n~a" name dyndispatch_table)) (apply handler current-vtime subtok (msg-object-args msgob))))))))))
	 'outport prt)
	(display ")" prt)
	(read (open-input-string (get-output-string prt))))
      (result 2007)]




))


(define test-this (default-unit-tester "Main compiler unit." these-tests))
(define maintests these-tests)
(define maintest test-this)


'
     (fluid-let ((pass-names
		  (list-remove-after desugar-gradients ;'cps-tokmac
				     (list-remove-before 'cleanup-token-machine pass-names))))
       (disp "PASS NAMES" pass-names)
       (game-eval (lambda args (void)))
       (let ((prog 
	      (r
	       '(tokens 
		 (SOC-start () (emit gradient))
		 (gradient () 
			   (greturn x (to handler))
			   (relay))
		 (handler (x) (display " ") (display x))
		 ))))
	 (disp "PROG")
	 (pp prog)
;	 (run-simulator-alpha prog)
	 ))



#;
    (fluid-let ((pass-names '(cleanup-token-machine cps-tokmac closure-convert)))
	 (let ((prog 
		(run-compiler
	       '(tokens 
		 (SOC-start () (printf "result ~a" (subcall tok1 3)))
		 (tok1 (x) (return (* x (+ (subcall tok2) (subcall tok3)))))
		 (tok2 () (return 55))
		 (tok3 () (return 45))
		 ))))
	   (let ((prt (open-output-string)))
	     (display "(" prt)       
	     (run-simulator-alpha prog 'outport prt)
	     (display ")" prt)
	     (read (open-input-string (get-output-string prt))))))



	       '(tokens 
		 (SOC-start () (printf "result ~a" (subcall tok1 3)))
		 (tok1 (x) (return (* x (+ (subcall tok2) (subcall tok3)))))
		 (tok2 () (return 55))
		 (tok3 () (return 45))
		 )


#;    [
     '(fluid-let ((pass-names
		  (list-remove-after desugar-gradients ;'cps-tokmac
				     (list-remove-before 'cleanup-token-machine pass-names))))
       (disp "PASS NAMES" pass-names)
       (let ((prog 
	      (run-compiler
	       '(tokens 
		 (SOC-start () (emit gradient))
		 (gradient () 
			   (greturn x (to handler))
			   (relay))
		 (handler (x) (display " ") (display x))
		 ))))
	 (disp "PROG")
	 (pp prog)
;	 (run-simulator-alpha prog)
	 ))
     
     ,(lambda a #t)
     ]
