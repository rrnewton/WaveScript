
;; QUESTION: SHOULD let-stored bound variables be able to be referred
;; to externally with "ext-refs" or should they be local.  They
;; probably should be local-only, but I haven't implemented it that
;; way now.
;; ANSWER: [2005.09.24]  I'm deciding NO, for now.  Uniquely renaming let-stored vars.

;; Output grammar:

;; basic_tml_grammar, as verified below:


(module desugar-let-stored mzscheme
  (require "../../../plt/common.ss"
           (all-except "cleanup-token-machine.ss" test-this these-tests)
	   (all-except "../../compiler_components/tml_generic_traverse.ss" test-this these-tests)
	   (all-except "../../sim/simulator_alpha.ss" test-this these-tests))
  (provide desugar-let-stored
	   test-desugar-let-stored test-this)
  (chezimports )


(define desugar-let-stored
  (build-compiler-pass
   'desugar-gradientsa
   `(input)
   `(output );(grammar ,basic_tml_grammar PassInput)) ;; TODO FIXME REENABLE
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
    (tml-generic-traverse
     (lambda (x autoloop)
       (match  x
					;    [,x (guard (begin (disp "PEXP" x) #f)) 3]
	 [,var (guard (symbol? var))
	       (let ((entry (assq var subst)))
		 (if entry 
		     (vector () (cadr entry))
		     (vector () var)))]
	 [(set! ,v ,[x])
	  (let-match ([#(,rst ,rhs) x])
	    (let ((entry (assq v subst)))
	      (if entry 
		  (vector rst `(set! ,(cadr entry) ,rhs))
		  (vector rst `(set! ,v ,rhs)))))]
	 [(let ([,lhs ,[x]]) ,body)
	  (let-match ([#(,rst ,rhs) x]
		      ;; This is a cheesy way of shadowing stored variable refs:
		      [#(,bst ,newbod) (process-expr (cons `(,lhs ,lhs) subst) body)])
	    (vector (append bst rst)
		    `(let ([,lhs ,rhs]) ,newbod)))]
	 ;; Being lenient, and accepting this even though it should be out of the language by now.
	 [(lambda (,args ...) ,body)
	  (let-match ([#(,bst ,newbod) (process-expr (append (map list args args) subst) body)])
	    (vector bst
		    `(lambda ,args ,newbod)))]
	 ;; The semantics of let-stored are that the first time the
	 ;; expression is executed (and only the first), the RHS is
	 ;; evaluated and stored.
	 [(let-stored () ,[x])
	  (let-match ((#(,st ,body) x))
	    (vector st body))]
	 ;; Multiple bindings just expand out in a let* style:
	 [(let-stored ([,lhs1 ,rhs1] [,lhs2 ,rhs2] [,lhs* ,rhs*] ...) ,body)
	  (process-expr subst `(let-stored ([,lhs1 ,rhs1]) 
				 (let-stored ([,lhs2 ,rhs2])
				   (let-stored ([,lhs* ,rhs*] ...) ,body))))]
	 ;; A single binding desugars here:
	 [(let-stored ([,lhs ,[x]]) ,body)
	  (let-match ((#(,rst ,rhs) x))
	    (let ([newvar (unique-name 'stored-liftoption)] ;; This is a "presence bit" for the let-stored var
		  [newlhs (unique-name lhs)]) ;; Deciding to rename these vars to avoid capture, no ext-refs!
	      (let-match ([#(,bst ,newbod) (process-expr (cons `(,lhs ,newlhs) subst) body)])
		;; The stored var is initially "null" (uninitialized)
		;; The new "presence bit" is initially false:
		(vector (append `([,newlhs 'let-stored-uninitialized] [,newvar '#f]) rst bst)
			(make-begin 
			 (list `(if ,newvar ;; If first time, initialize
				    (void)
				    (begin ;; This is where the rhs is finally evaluated:
				      (set! ,newvar '#t)
				      (set! ,newlhs ,rhs)))
                               newbod))))))]
	 [,other (autoloop other)]))
     (lambda (results recombine)
;       (let ([stored (map (lambda (x) (vector-ref x 0)) results)]
;	     [exps   (map (lambda (x) (vector-ref x 1)) results)])
       ;; This pattern doesn't work in the PLT-port of match: 
       (let-match (((#(,stored ,exps) ...) results))
	 (vector (apply append stored)
		 (apply recombine exps))))
     expr)))

(define (process-tokbind tb)
  (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
         (let-match ([#(,newstored ,newbod) (process-expr (map car constbinds) body)])
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
(define-testing these-tests
  (let () 

    (define sim-to-string
      (lambda (prog)
	;(import simulator_alpha_datatypes)
	(parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
		      (let ((prt (open-output-string)))
			(run-simulator-alpha prog 'outport prt) 
			(get-output-string prt)))))
  
    (define simulate-and-compare
      (lambda (pass origprog)
	;(import simulator_alpha_datatypes)
	(let ([result1 (sim-to-string origprog)]
	      [result2 (sim-to-string (pass origprog))])
	  (list result1 result2))))
  `(  
    ["Run simulator on empty TMs" 
     (,simulate-and-compare desugar-let-stored (,cleanup-token-machine '()))
     ("" "")]

    ["Now start to test let-stored in a basic way."
     (,sim-to-string
      (desugar-let-stored 
       (,cleanup-token-machine 
	'(tokens
	  (SOC-start () (call foo) (call foo))
	  (foo () (let-stored ((x1 #t))
			      (if x1 (begin (set! x1 #f)
					    (printf "1")))))))))
     "1"]

    ["Now seeing if it only initializes at the right time."
     (,sim-to-string
      (desugar-let-stored 
       (,cleanup-token-machine 
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
	(,cleanup-token-machine 
	 '(tokens
	   (SOC-start () (call bar 1))
	   (bar (x)
		(display x)
		(let-stored ((x 2))
			    (display x)))))))
      "12"]

)))

(define-testing test-this (default-unit-tester
		    "24: Desugar-Let-Stored: convert let-stored to plain stored variables."
		    these-tests))

(define test24 test-this)
(define tests24 these-tests)
(define test-desugar-let-stored  test-this)
(define tests-desugar-let-stored these-tests)

) ; End module

;(require desugar-let-stored) (test-desugar-let-stored)
