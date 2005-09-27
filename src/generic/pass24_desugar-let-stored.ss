
;; QUESTION: SHOULD let-stored bound variables be able to be referred
;; to externally with "ext-refs" or should they be local.  They
;; probably should be local-only, but I haven't implemented it that
;; way now.
;; ANSWER: [2005.09.24]  I'm deciding NO, for now.  Uniquely renaming let-stored vars.

;; Input Grammar:

;;;  <Pgm> ::= (program (bindings <Cbind>*) <NodePgm>)
;;;  <NodePgm> ::= (nodepgm (tokens <TokBinding>*))
;       NOTE: tokens will inclide a binding for SOC-start and node-start
;;;  <Cbind> ::= (<var> <Exp>)
;       NOTE: This expressions will be statically calculable -- constants.
;;;  <TokBinding> ::= (<TokName> <SubtokId> (<Var> ...) (bindings <Cbind>*) (stored <Stored>) <Expr>)
;;;  <TokName>   ::= <Symbol> 
;;;  <SubtokId>  ::= <Number>
;;;  <Token>     ::= (tok <TokName>) | (tok <Tokname> <Int>)
;;;  <DynToken>  ::= (tok <Tokname> <Expr>) | <Token>
;;;     NOTE: Either the whole token reference or just the sub-index can be dynamic.
;;;  <Expr>      ::= (quote <Constant>)
;;;                | <Var>
;;;                | <DynToken>
;;;                | (set! <Var> <Expr>)
;;;                | (ext-ref <Token> <Var>)
;;;                | (ext-set! <Token> <Var> <Expr>)
;       NOTE: These are static token refs for now.
;;;                | (begin <Expr> ...)
;;;                | (let ((<Symbol> <Expr>)) <Expr>)
;;;                | (if <Expr> <Expr> <Expr>)
;;;                | (subcall <DynToken> <Expr>...)
;;;                | (return <Expr>)
;;;                | (<Prim> <Expr> ...)
;;;                | (<Expr> ...)
;;;                | <Sugar> 
;;;                | (leds <Red|Yellow|Green> <On|Off|Toggle>)
;;;  <Prim> ::= <BasicPrim> 
;;;           | call | subcall | timed-call | bcast
;;;           | is_scheduled | deschedule | is_present | evict
;;;  <Sugar>  ::= (flood <Expr>)
;;;           | (elect-leader <Token> [<Token>])
              ;; <TODO> optional second argument.. decider

(define desugar-let-stored
  (let ()


    ;; This is confusing, but there are so many small traversals of
    ;; the program tree in this pass that it is much easier to reuse this tree walk:
    ;; It's complex, but saves a lot of boilerplate. (See peyton jones "boilerplate" papers.)
    ;;
    ;; NOTE: Duplicated code.  This function also appears in other passes, where it 
    ;; works on a slightly different grammar.
    ;;
    ;; NOTE: A common failure mode when using this is invoking the
    ;; wrong loop when making a recursive pattern match.  Be careful.
    (define (generic-traverse driver fuse e)
      ;; The "driver" takes the first shot at an expression, transforms the
      ;; subcases that it wants to, and then hands the rest on to its
      ;; continuation to do the automated traversal. The automated
      ;; traversal, in turn, uses the "fuse" function to glue back together
      ;; the parts of the tree.  The fuse function is passed a list of child
      ;; exprss and another continuation representing the "default fuser" which
      ;; just puts the expression back together like it was before (given the child terms).
      ;; Types:
      ;;   driver : expr, (expr -> 'intermediate) -> 'result)
      ;;   fuse : 'intermediate list, (expr list -> expr) -> 'intermediate)
      ;;   e : expr 
      ;; Return value: 'result 
      (let loop ((e e))
	(driver e 
	   (lambda (x)
	     (match x
;		    [,x (guard (begin (printf "~nGenTrav looping: ") (display-constrained (list x 50)) (newline) #f)) 3]
		    [,const (guard (constant? const)) (fuse () const)]
		    [(quote ,const)                (fuse ()   `(quote ,const))]
		    [,var (guard (symbol? var))    (fuse ()    var)]
		    [(tok ,tok)                    (fuse ()   `(tok ,tok))]
		    [(tok ,tok ,n) (guard (integer? n)) (fuse () `(tok ,tok ,n))]
		    [(tok ,tok ,[loop -> expr])    (fuse (list expr) `(tok ,tok ,expr))]
		    [(ext-ref ,tok ,var)           (fuse ()   `(ext-ref ,tok ,var))]
		    [(ext-set! ,tok ,var ,[loop -> expr])  
		                                   (fuse (list expr) `(ext-set! ,tok ,var ,expr))]
		    [(set! ,v ,[loop -> e])        (fuse (list e)    `(set! ,v ,e))]
		    [(leds ,what ,which)           (fuse () `(leds ,what ,which))]
		    [(begin ,[loop -> x] ...)      (fuse x           `(begin ,x ...))]
		    [(if ,[loop -> a] ,[loop -> b] ,[loop -> c])
		                                   (fuse (list a b c) `(if ,a ,b ,c))]
		    [(let ([,lhs ,[loop -> rhs]]) ,[loop -> bod])
		                                   (fuse (list rhs bod) `(let ([,lhs ,rhs]) ,bod))]
		    ;; "activate" and the gradient calls have already been desugared:
		    [(,call ,[loop -> rator] ,[loop -> rands] ...)
		     (guard (memq call '(bcast subcall call)))
		     (fuse (cons rator rands) `(,call ,rator ,rands ...))]
		    [(timed-call ,time ,[loop -> rator] ,[loop -> rands] ...)
		     (guard (number? time))		     
		     (fuse (cons rator rands) `(timed-call ,time ,rator ,rands ...))]
		    [(return ,[loop -> x])         (fuse (list x) `(return ,x))]
		    [(,prim ,[loop -> rands] ...)
		     (guard (or (token-machine-primitive? prim)
				(basic-primitive? prim)))
		     (fuse rands `(,prim ,rands ...))]
		    [(app ,[loop -> rator] ,[loop -> rands] ...)
		     (fuse (cons rator rands) `(app ,rator ,rands ...))]
		    [,otherwise
		     (error 'generic-traverse
			    "bad expression: ~s" otherwise)])))))


    ;; Get the free vars from an expression
    (define (free-vars e)
      (generic-traverse
       ;; driver, fuser, expression
       (lambda  (x loop) 
	 (match x
		[,v (guard (symbol? v)) (list v)]
		[(let ([,lhs ,[rhs]]) ,[bod])
		 (append rhs (remq lhs bod))
		 ]

;; FIXME TODO:
;		[(let-stored ...


		[,x (loop x)]))
       (lambda (ls _) (apply append ls))
       e))

;; Returns a list of stored var bindings (var, value pairs) and a transformed expression
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

    [(begin ,[st* xs] ...)
     (values (apply append st*) (make-begin xs))]
    [(if ,[tst test] ,[cst conseq] ,[ast altern])
     (values (append tst cst ast)
             `(if ,test ,conseq ,altern))]
    [(let ([,lhs ,[rst rhs]]) ,body)
     (mvlet ([(bst newbod) (process-expr (cons `(,lhs ,lhs) subst) body)])
	    (values (append bst rst)
		    `(let ([,lhs ,rhs]) ,newbod)))]

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

(lambda (prog)
  (match prog
    [(,lang '(program (bindings ,constbinds ...)
		      (nodepgm (tokens ,[process-tokbind -> toks] ...))))
     `(desugar-let-stored-lang
       '(program (bindings ,constbinds ...)
		 (nodepgm (tokens ,toks ...))))]))
))

;; Here I've decided to start breaking my rules about unit tests requiring
;; only the content of that file.
;; This requires a number of other passes as well as the simulator.
(define these-tests
  (let () 

    (define sim-to-string
      (lambda (prog)
	(parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
		      (fluid-let ((pass-names '(cleanup-token-machine cps-tokmac closure-convert)))
			(let ((prt (open-output-string)))
			  (run-simulator-alpha prog 'outport prt)
			  (get-output-string prt))))))

    (define simulate-and-compare
      (lambda (pass origprog)
	(let ([result1 (sim-to-string origprog)]
	      [result2 (sim-to-string (pass origprog))])
	  (list result1 result2))))
  `(  
    ["Run simulator on empty TMs" 
     (simulate-and-compare desugar-let-stored (cleanup-token-machine '()))
     ("" "")]

    ["Now start to test let-stored in a basic way."
     (sim-to-string
      (desugar-let-stored 
       (cleanup-token-machine 
	'(tokens
	  (SOC-start () (call foo) (call foo))
	  (foo () (let-stored ((x1 #t))
			      (if x1 (begin (set! x1 #f)
					    (printf "1")))))))))
     "1"]

    ["Now seeing if it only initializes at the right time."
     (sim-to-string
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
     (sim-to-string
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

