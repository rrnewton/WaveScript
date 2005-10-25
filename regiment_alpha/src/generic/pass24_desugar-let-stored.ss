
;; QUESTION: SHOULD let-stored bound variables be able to be referred
;; to externally with "ext-refs" or should they be local.  They
;; probably should be local-only, but I haven't implemented it that
;; way now.
;; ANSWER: [2005.09.24]  I'm deciding NO, for now.  Uniquely renaming let-stored vars.

;; Output grammar:

;; basic_tml_grammar, as verified below:

(define desugar-let-stored
  (build-compiler-pass
   'desugar-gradientsa
   `(input)
   `(output (grammar ,basic_tml_grammar PassInput))
  (let ()
    ;; Get the free vars from an expression
    (define (free-vars e)
      (tml-generic-traverse
       ;; driver, fuser, expression
       (lambda  (x loop) 
	 (match x
		[,v (guard (symbol? v)) (list v)]
		[(let ([,lhs ,[rhs]]) ,[bod])
		 (append rhs (remq lhs bod))
		 ]

;; TODO: VERIFY: does this make sense:
		[(let-stored ([,lhs ,[rhs]] ...) ,[bod])
		 (append rhs (difference bod lhs))]

		[,x (loop x)]))
       (lambda (ls _) (apply append ls))
       e))

;; Returns a list of stored var bindings (var, value pairs) and a transformed expression
;; [2005.10.03]  This substitution part would really be a lot simpler if I used tml-generic-traverse.
(define process-expr 
  (lambda (subst expr) 
    ;; subst contains both normal local vars and "stored" vars,
    ;; It's an association list between bound-var [old] name and its new name.
;  (trace-lambda PE (subst expr)
  (match expr
;    [,x (guard (begin (disp "PEXP" x) #f)) 3]
    [(quote ,const)                    (values () `(quote ,const))]
    [,num (guard (number? num))        (values () num)]
    [(tok ,t ,n) (guard (number? n))   (values () `(tok ,t ,n))]
    [(tok ,t ,[st e])                  (values st `(tok ,t ,e))]
    ;; No renaming or anything here:
    [(ext-ref ,tok ,var)               (values () `(ext-ref ,tok ,var))]
    [(ext-set! ,tok ,var ,[st e])      (values st `(ext-set! ,tok ,var ,e))]

    [,var (guard (symbol? var))        
	  (let ((entry (assq var subst)))
	    (if entry 
		(values () (cadr entry))
		(values () var)))]
    [(set! ,v ,[rst rhs])
	  (let ((entry (assq v subst)))
	    (if entry 
		(values rst `(set! ,(cadr entry) ,rhs))
		(values rst `(set! ,v ,rhs))))]

    ;; For now we just don't touch the insides of a dbg statement:
    ;; [2005.10.03] ACK, I had decided so as not to mess with complex constants used.
    ;; But this gives me problems, it doesn't receive var renames if it's opaque!! 
    ;; So openning up that black box again.
    ;[(dbg ,rand ...) (values () `(dbg ,rand ...))]

    [(begin ,[st* xs] ...)
     (values (apply append st*) (make-begin xs))]
    [(if ,[tst test] ,[cst conseq] ,[ast altern])
     (values (append tst cst ast)
             `(if ,test ,conseq ,altern))]
    [(let ([,lhs ,[rst rhs]]) ,body)
     (mvlet ([(bst newbod) (process-expr (cons `(,lhs ,lhs) subst) body)])
	    (values (append bst rst)
		    `(let ([,lhs ,rhs]) ,newbod)))]

    ;; Being lenient, and accepting this even though it should be out of the language by now.
    [(lambda (,args ...) ,body)
     (mvlet ([(bst newbod) (process-expr (append (map list args args) subst) body)])
	    (values bst
		    `(lambda ,args ,newbod)))]

;; These are just primitives, they do not need their own form.
;    [(,call-style ,[st* args*] ...)
;     (guard (memq call-style '(call timed-call bcast )))
;     (values (apply append st*)
;             `(,call-style ,args* ...))]
;    [(return ,[xst x]) (values xst `(return ,x))]

    ;; The semantics of let-stored are that the first time the
    ;; expression is executed (and only the first), the RHS is
    ;; evaluated and stored.
    [(let-stored () ,[st body]) (values st body)]
    ;; Multiple bindings just expand out in a let* style:
    [(let-stored ([,lhs1 ,rhs1] [,lhs2 ,rhs2] [,lhs* ,rhs*] ...) ,body)
     (process-expr subst `(let-stored ([,lhs1 ,rhs1]) 
			  (let-stored ([,lhs2 ,rhs2])
			     (let-stored ([,lhs* ,rhs*] ...) ,body))))]

    [(let-stored ([,lhs ,[rst rhs]]) ,body)
     (let ([newvar (unique-name 'stored-liftoption)] ;; This is a "presence bit" for the let-stored var
	   [newlhs (unique-name lhs)]) ;; Deciding to rename these vars to avoid capture, no ext-refs!
       (mvlet ([(bst newbod) (process-expr (cons `(,lhs ,newlhs) subst) body)])
	      ;; The stored var is initially "null" (uninitialized)
	      ;; The new "presence bit" is initially false:
               (values (append `([,newlhs 'let-stored-uninitialized] [,newvar '#f]) rst bst)
                       (make-begin 
                        (list `(if ,newvar ;; If first time, initialize
				   (void)
                                   (begin ;; This is where the rhs is finally evaluated:
                                     (set! ,newvar '#t)
                                     (set! ,newlhs ,rhs)))
                               newbod)))))]

    [(leds ,what ,which) (values () `(leds ,what ,which))]
    [(,prim ,[rst* rands] ...)
     (guard (or (token-machine-primitive? prim)
                (basic-primitive? prim)))
     (values (apply append rst*)
             `(,prim ,rands ...))]
    [(app ,[rst1 rator] ,[rst* rands] ...)
     (warning 'desugar-let-stored
              "arbitrary application of rator: ~s~n" rator)
     (values (apply append rst1 rst*)
             `(app ,rator ,rands ...))]

    [,otherwise
	 (error 'desugar-let-stored:process-expr 
		"bad expression: ~s" otherwise)]
	)))
      

(define (process-tokbind tb)
  (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
         (mvlet ([(newstored newbod) (process-expr (map car constbinds) body)])
                `[,tok ,id ,args (stored ,@stored ,@newstored) ,newbod])))

;; Main body of this pass:
(lambda (prog)
  (match prog
    [(,lang '(program (bindings ,constbinds ...)
		      (nodepgm (tokens ,[process-tokbind -> toks] ...))))
     `(desugar-let-stored-lang
       '(program (bindings ,constbinds ...)
		 (nodepgm (tokens ,toks ...))))]))
)))

;; Here I've decided to start breaking my rules about unit tests requiring
;; only the content of that file.
;; This requires a number of other passes as well as the simulator.
(define these-tests
  (let () 

    (define sim-to-string
      (lambda (prog)
	(import simulator_alpha_datatypes)
	(parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
		      (let ((prt (open-output-string)))
			(run-simulator-alpha prog 'outport prt)
			(get-output-string prt)))))
  
    (define simulate-and-compare
      (lambda (pass origprog)
	(import simulator_alpha_datatypes)
	(let ([result1 (sim-to-string origprog)]
	      [result2 (sim-to-string (pass origprog))])
	  (list result1 result2))))
  `(  
    ["Run simulator on empty TMs" 
     (,simulate-and-compare desugar-let-stored (cleanup-token-machine '()))
     ("" "")]

    ["Now start to test let-stored in a basic way."
     (,sim-to-string
      (desugar-let-stored 
       (cleanup-token-machine 
	'(tokens
	  (SOC-start () (call foo) (call foo))
	  (foo () (let-stored ((x1 #t))
			      (if x1 (begin (set! x1 #f)
					    (printf "1")))))))))
     "1"]

    ["Now seeing if it only initializes at the right time."
     (,sim-to-string
      (desugar-let-stored 
       (cleanup-token-machine 
	'(tokens
	  (SOC-start () (call foo 1) (call foo 2) (call foo 2) (call foo 1))
	  (foo (v) (if (= v 2)
		       (let-stored ((x1 (begin (printf "2") #t)))				  
				   (if x1 (begin (set! x1 #f)
						 (printf "3"))
				       (printf "4")))
		       (printf "1")))))))
     "12341"]

    
    ;; In the process of debugging this right now. [2005.09.22]
    ;; Seems like it might be a problem with the execution, not this pass.
    ["Now test scoping."
     (,sim-to-string
      ;; Got to include the renaming to make absolutely sure the x's don't collide:
      ;(rename-stored ;; No longer true.. fixed it so it renames let-stored vars [2005.09.24]
       (desugar-let-stored 
	(cleanup-token-machine 
	 '(tokens
	   (SOC-start () (call bar 1))
	   (bar (x)
		(display x)
		(let-stored ((x 2))
			    (display x)))))))
      "12"]

)))

(define test-this (default-unit-tester
		    "24: Desugar-Let-Stored: convert let-stored to plain stored variables."
		    these-tests))

(define test24 test-this)
(define tests24 these-tests)
(define test-desugar-let-stored  test-this)
(define tests-desugar-let-stored these-tests)

