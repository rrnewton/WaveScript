;; [2005.02.10]

;; cps-tokmac
;; This pass does the CPS transformation for the token machine.

;; INIT message = 0
;; CALL message = 1


;;; Input Grammar:
;;; This is the standard "Post-Cleanup" grammar for token machines.
;;; (except with gradients already desugared.)

;;; NOTE: For now, subtokens are required to call static token
;;; destinations such that we know who to modify for CPS!

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
;;;                | (<Prim> <Expr> ...)
;;;                | (<Expr> ...)
;;;                | (leds <Red|Yellow|Green> <On|Off|Toggle>)
;;;  <Prim> ::= <BasicPrim> 
;;;           | call | subcall | timed-call
;;;           | is_scheduled | deschedule | is_present | evict

;;; The call forms are all just plain prims.  This reflects the fact
;;; that they are permitted to have dynamically computed token
;;; arguments, and also dynamically computed times for timed-call.


;;; Output Grammar:

;;; Subcall's and "return" statements are gone.


;; I accumulate tests through mutation throughout this file.
;; This method allows me to test internal functions whose definitions
;; are not exposed at the top level.
(define these-tests '())

    (define INIT 0)
    (define CALL 0)
    (define FREEVAR0 'fv0)

    ;; Name of the token which represents the global variable storing the continuation
    ;; token count.
    (define KCOUNTER 'kcounter)
    ;; And the root name of continuation tokens.
    (define KNAME 'K)

    (define (id x) x)

    ;; This is confusing, but there are so many small traversals of
    ;; the program tree in this pass that it is much easier to reuse this tree walk:
    ;;
    ;; NOTE: A common failure mode when using this is invoking the
    ;; wrong loop when making a recursive pattern match.  Be careful.
    (define (generic-traverse driver fuse e)
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
		[,x (loop x)]))
       (lambda (ls _) (apply append ls))
       e))

    ;; Returns a list of tokens referenced in (tok _ _) expressions.
    ;; The consumer probably wants to convert the output to a set.
    (define (toks-referenced e)
      (generic-traverse
       ;; driver, fuser, expression
       (lambda  (x loop) 
	 (match x
		[(tok ,t ,n) (guard (integer? n)) (list t)]
		[(tok ,t ,[e]) (cons t e)]
		[,x (loop x)]))
       (lambda (ls _) (apply append ls))
       e))

    ;; Returns those tokens referenced in (tok _ _) expressions which
    ;; might escape.  That is, token "values" that are not immediately
    ;; consumed by primitives.
    ;; This is a conservative estimate.  It could get much better with some dataflow analysis.
    (define (toks-escaped e)
      (let ([allowed1
	     (lambda (loop)
	       (lambda (x)
		 (disp "allowed: " x)
		 (match x 
			[(tok ,t ,n) (guard (integer? x)) '()]
			[(tok ,t ,[e]) e]
			[,x (loop x)])))])
	(generic-traverse
	 ;; driver, fuser, expression
	 (lambda  (x loop)
	   (define allowed (allowed1 loop))
	   (match x
;		  [,x (guard (begin (printf "~ntoks-escaped looping: ") (display-constrained (list x 50)) (newline) #f)) 3]
		  [(ext-ref ,[allowed -> t] ,v) t]
		  [(ext-set! ,[allowed -> t] ,v ,[e]) (append t e)]
		  [(,call ,[allowed -> rator] ,[rands] ...)
		   (guard (memq call '(bcast call)))
		   (apply append rator rands)]
		  [(timed-call ,t ,[allowed -> rator] ,[rands] ...)a
		   (guard (number? t))
		   (apply append rator rands)]
		  [(,prim ,[allowed -> args])
		   (guard (or (token-machine-primitive? prim)
			      (basic-primitive? prim)))
		   (apply append args)]
		  [(tok ,t ,n) (guard (integer? n)) (list t)]
		  [(tok ,t ,[e]) (cons t e)]
		  [,x (loop x)]))
       (lambda (ls _) (apply append ls))
       e)))

    ;; Replace the free vars in an expression with "fv0" "fv1" etc.
    (define (number-freevars e)
      (define (build-fv x fvs) (string->symbol (format "fv~a" (list-find-position x fvs))))
      (let outer ((e e) (fvs (free-vars e)))
	(generic-traverse
	 (lambda  (x loop)
	   (match x
		  [(let ((,lhs ,[rhs])) ,bod)
		   `(let ((,lhs ,rhs))
		      ,(outer bod (remq lhs fvs)))]
		  [,x (guard (memq x fvs))
 		      (build-fv x fvs)]
		  [(set! ,x ,[e]) (guard (memq x fvs))
		   `(set! ,(build-fv x fvs) ,e)]
		  [,other (loop other)]))
	 (lambda (ls def) def)
	 e)))

  ;; Traverse the expression looking for tokens tainted by "subcalls"
  (define (get-tainted expr)
    (generic-traverse
     (lambda (x loop)
       (match x
	      [(subcall (tok ,t ,[e]) ,[args] ...)
	       (apply append (list t) e args)]
	      [,e (loop e)]))
     (lambda (ls _) (apply append ls))
     expr))

  ;; Traverse the expression checking to see if it has a (return _) statement.
  (define (has-return? expr)
    (generic-traverse
       (lambda (x loop)
	 (match x 
		[(return ,e) #t]
		[,e (loop e)]))
       (lambda (ls _) (ormap id ls))
       expr))

  ;; Verify that every program path ends in a (return _) statement.
  (define (valid-returns? expr)
    (let ((nonret (lambda (expr)
		    (generic-traverse
		     (lambda (exp loop)
		       (match exp
			      [(return ,_) #f]
			      [,other (loop other)]))
		     (lambda (ls _) (andmap id ls))
		     expr))))
      ;; Now, this loop climbs down the spine of the expression,
      ;; making sure all paths end in return:
      (match expr
	   [(return ,[nonret -> e]) e]
	   [(begin ,[nonret -> e*] ... ,[e])
	    (and e (andmap id e*))]
	   [(let ((,lhs ,[nonret -> rhs])) ,[bod])
	    (and rhs bod)]
	   [(if ,[nonret -> a] ,[b] ,[c])
	    (and a b c)]
	   [,_ #f])))

     ;; Takes a tainted tokbind; we add a continuation argument and desugar (return _) statements.
     (define kify-tokbind
	(lambda (tb)
	  (mvlet ([(tok subid args stored constbinds body) (destructure-tokbind tbs)])
	    (let* ([k (unique-name 'k)]	    
		   [desugar-return 
		    (lambda (expr)
		      (generic-traverse
		       (lambda (expr loop) ;; driver
			 (match expr
				[(return ,[x]) `(call ,k ,x)]
				[(set! ,v ,[e])
				 `(begin `(set! ,v ,e) (call ,k (void)))]
				[,x (loop x)]))
		       (lambda (_ exp) exp) ;; fuser
		       expr))])
	      `[,tok ,id (,k ,@args) (stored ,@stored)
		     ,(desugar-return body)]))))
		           
    ;; process-subcalls expands out all subcalls 
    ;; Best way to represent a thing with a hole in it is a continuation:
    ;; Thus this pass itself carries a continuation representing the context of the current expression.
    (define process-subcalls
      (lambda (expr)
      ;; Continuation code must reside in new handlers:
      (let ([new-handlers '()]) ;; MUTABLE
	;; These simply accumulate.  There is no reason to complicate
	;; the continuation passing algorithm below by adding extra
	;; arguments to the continuation.

	(define loop-list
	  (lambda (ls  pvk)
	     (let inner ([ls ls] [thisk pvk]) ;(lambda (ls) (pvk (reverse ls)))])
	       (if (null? ls) 
		   (thisk '())
		   (loop (car ls)
			 (lambda (x)
			   (inner (cdr ls) 
				  (lambda (ls) (thisk (cons x ls))))))))))
	;; Pvk is the continuation representing the enclosing expression to the current being processed.
	(define loop
	  (lambda (expr pvk)
	    (match expr
;	     [,x (guard (begin (display-constrained "\n  Loop: " (list x 50) "\n")
;;			       (display-constrained "  Cont: " (list (pvk (unique-name 'HOLE)) 50) "\n")
;			       #f)) 3]		   
	     [,const (guard (constant? const)) 
		     (pvk `(quote ,const))]
	     [(quote ,const) (pvk `(quote ,const))]
	     [,var (guard (symbol? var)) (pvk var)]
	     [(set! ,v ,expr)
	      (loop expr (lambda (e) (pvk `(set! ,v ,e))))]
	     [(tok ,tok) (pvk `(tok ,tok))]
	     [(tok ,tok ,n) (guard (number? n))
	      (pvk '(tok ,tok ,n))]
	     [(tok ,tok ,expr) 
	      (loop expr
		    (lambda (e) (pvk `(tok ,tok ,e))))]
	     [(ext-ref ,tok ,var) (pvk `(ext-ref ,tok ,var))]
	     [(ext-set! ,tok ,var ,expr)
	      (loop expr
		    (lambda (e) (pvk `(ext-set! ,tok ,var ,e))))]
             [(begin) (pvk '(void))]
	     [(begin ,x) (loop x pvk)]
	     [(begin ,x ,y ...)
	      (loop x 
		    (lambda (e) 
		      (make-begin 
		       `(begin ,e
			       ,(loop `(begin ,@y)
				      (lambda (e2) (pvk (make-begin `(begin ,e2)))))))))]

              [(if ,test ,conseq ,altern)
	      (loop test 
		    (lambda (t)
		      (loop conseq
			(lambda (c)
			  (loop altern
			    (lambda (a)
			      (pvk `(if ,t ,c ,a))))))))]
	     [(let ((,lhs ,rhs)) ,body)
	      (loop rhs
		 (lambda (r)
		   (loop body
			 (lambda (b)
			   (pvk `(let ([,lhs ,r]) ,b))))))]
	     
	     [(subcall ,tok ,args* ...)
	      (let* (;; This expression represents the continuation of the subcall:
		     [broken-off-code (pvk FREEVAR0)] ;(pvk RETVAL_VARNAME)]
		     [_ (disp "GOT BROKEN OFF: " broken-off-code)]
		     [fvs (remq FREEVAR0 (free-vars broken-off-code))]
;		     [__ (disp "GOT fvs: " fvs)]
		     [args (map (lambda (n)
				  (string->symbol (format "arg~a" n)))
				(iota (length fvs)))]
		     [fvns (map (lambda (n)
				   (string->symbol (format "fv~a" n)))
				(iota (length fvs)))]
		     ;; Get a fresh name & subtokindex name for our continuation:
		     [kind (unique-name 'kind)]
		     [kname (unique-name 'K)]
		     )
		(set! new-handlers
		      (cons
		       `(,kname subtokind (flag ,@args)
				 (stored [,KCOUNTER 0] ,@fvns)
					;(map list fvns args))
					;(make-vector ,(length fvs)))
				 (if (eq? flag ',INIT)
				     (if (= subtokind '0)
					 ;; No freevars if we're just initializing the counter-object.
					 (void)
					 (begin
					   ,@(map (lambda (fv a)
						    `(set! ,fv ,a))
						  fvns args)))
				     ;; Otherwise, assume the flag is CALL
				     (begin
				       ,(number-freevars broken-off-code)
				       ;; Since these are one-shot continuations, 
				       ;; we deallocate ourselves on the way out:
				       (evict ,kname)
				       )))
		       new-handlers))		
		(loop-list args* 
			   (lambda (ls)
			       `(let ([,kind 
				      (if (token-present? (tok ,kname 0))
					  (let ([new (+ '1 (ext-ref (tok ,kname 0) ,KCOUNTER))])
					    (begin (ext-set! (tok ,kname 0) ,KCOUNTER new)
						   new))
					  (begin (call (tok ,kname 0) ',INIT)
						 '1))])
				  (begin 
				    ;; Initialize continuation:
				    (call (tok ,kname ,kind) ',INIT ,@fvs)
				    ;; Call function with continuation token:
				    (call ,tok (tok ,kname ,kind) ,@ls)))))
		)]

	     [(,call ,tok ,args* ...)
	      (guard (memq call '(call bcast)))
	      (loop-list args* 
			 (lambda (ls) (pvk `(,call ,tok ,@ls))))]
	     [(timed-call ,time ,tok ,args* ...)
	      (loop-list args* 
			 (lambda (ls) (pvk `(timed-call ,time ,tok ,@ls))))]
	     [(leds ,what ,which) (pvk `(leds ,what ,which))]
	     [(,prim ,rands ...)
	      (guard (or (token-machine-primitive? prim)
			 (basic-primitive? prim)))
	      (loop-list rands 
			 (lambda (ls)
			   (pvk
			    `(,prim ,@ls))))]
	     [(app ,opera* ...)
	      (warning 'cps-tokmac 
		       "arbitrary application of rator: ~s" opera*);(car opera*))
	      (loop-list opera*  (lambda (ls) (pvk (cons 'app ls))))]

	     [,otherwise
	      (error 'cps-tokmac:process-subcalls
		     "bad expression: ~s" otherwise)]
	     )))
		
	;; process-subcalls returns an expression and new handlers
	  (let ((newexpr (loop expr (lambda (x) x))))
	    (values newexpr 
		    new-handlers)))))
      

    ;; Tainted tokens must take continuations and return values to them:
   (define (find-tainted tbs)      
     (lambda (tbs)
       (define tainted-toks '())
       33
       ))

    ;; Returns a set of tainted token-names and a list of new tokbinds.
      (define process-tokbinds
	(lambda (tokenbinds)
	  (let loop ([tbs tokenbinds] [tainted '()] [tbacc '()])
	    (if (null? tbs)
		(values (list->set tainted) tbacc)
		(mvlet ([(tok subid args stored constbinds body) (destructure-tokbind (car tbs))])
		  (if (not (null? constbinds)) (error 'cps-tokmac "Not expecting local constbinds!"))
		  (mvlet ([(newexpr taints newtokbinds) (process-subcalls body)])
			 (loop (cdr tbs)
			       (append taints tainted)
			       (cons 
				`(,tok ,subid ,args (stored ,@stored) ,newexpr)
				(append newtokbinds tbacc)))))))))


;		 (if (not (null? constbinds)) (error 'cps-tokmac "Not expecting local constbinds!"))






(call-with-values (lambda () (process-subcalls '(begin '1 (subcall t '2) '3))) list)







    ;; Now add unit tests of the above internal helper functions:
    (set! these-tests
          (append 
	   `(
	     ["Testing free-vars" 
	      (,free-vars '(dbg '"w%d%d\n" fv0 x))
	       ,(lambda (ls) (set-equal? ls '(fv0 x)))]
	     [(,free-vars '(begin x y (let ((z 3)) z)))
	      ,(lambda (ls) (set-equal? ls '(x y)))]
	     [(,free-vars '(begin a (if b c (let ((d e)) d)) f))
	      ,(lambda (ls) (set-equal? ls '(a b c e f)))]


	     ["Next testing number-freevars"
	      (,number-freevars '(begin x y (let ((z '3)) z)))
	      ,(lambda (exp)
		 (set-equal? (free-vars exp)
			     '(fv0 fv1)))]
	     [(,number-freevars '(begin x y (let ((z 3)) (+ ht z))))
	      ,(lambda (exp)
		 (set-equal? (free-vars exp)
			     '(fv0 fv1 fv2)))]	     


	     ["Testing toks escaped"
	      (toks-escaped '(begin '1
				    (let ([kind_4 '22])
				      (begin '11
					     (call t (tok K_5 kind_4) '2)))))
	      (K_5)]
	     [(toks-escaped '(begin '1
				    (let ([kind_4 '22])
				      (begin '11
					     (call (tok K_5 kind_4) '2)))))
	      ()]

        
	     ["Now testing valid-returns?"
	      (,valid-returns? ''3) #f]
	     [(,valid-returns? '(return '3)) #t]
	     [(,valid-returns? '(return (return '3))) #f]
	     [(,valid-returns? '(begin '1 '2 (return '3))) #t]
	     [(,valid-returns? '(return (begin '1 '2 '3))) #t]    
	     [(,valid-returns? '(begin '1 '2 '3)) #f]
	     [(,valid-returns? '(if '1 '2 '3)) #f]
	     [(,valid-returns? '(if (return '1) '2 '3)) #f]
	     [(,valid-returns? '(if '1 (return '2) '3)) #f]
	     [(,valid-returns? '(if '1 '2 (return '3))) #f]
	     [(,valid-returns? '(if '1 (return '2) (return '3))) #t]
	     [(,valid-returns? '(begin '1 (if '2 (return '3) (let ((x '4)) (return x))))) #t] 
	     [(,valid-returns? '(begin '1 (if '2 (return '3) (return (let ((x '4)) x))))) #t] 
	     [(,valid-returns? '(begin '1 (return (if '2 '3 (let ((x '4)) x))))) #t] 
	     [(,valid-returns? '(return (begin '1 (if '2 '3 (let ((x '4)) x))))) #t]
	     [(,valid-returns? '(return (begin '1 (if '2 '3 (let ((x (return '4))) x))))) #f]
	     [(,valid-returns? '(begin '1 (return (if '2 '3 (let ((x '4)) (return x)))))) #f]
	     
	     ) these-tests))



(define cps-tokmac
  (let ()


    ;; CPS-tokmac main body:
    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...)
			  (nodepgm (tokens ,nodetoks ...))))
	 ;; This does the main transformation, eliminates 'subcall':
	 (mvlet ([(tainted newtoks1) (process-tokbinds nodetoks)])
	   (disp "GOT tainted: " tainted)
	   (let ([newtoks2
		  ;; This just adds extra arguments ot the appropriate handlers.
		  (append 
		   (filter (lambda (x) (not (memq (car x) tainted))) newtoks1)
		   (amend-tainted (filter (lambda (x) (memq (car x) tainted)) newtoks1)))])
	     `(cps-tokmac-lang '(program (bindings ,constbinds ...)
					 (nodepgm (tokens ,@newtoks2))))))]))))


;; Finally we add some tests for the whole module -- for the externally visible parts of the module.
(set! these-tests
  (append 
   `(
    
    ["Put an empty test through." 
     (cps-tokmac
      '(cps-tokmac-lang
	'(program
	  (bindings )
	  (nodepgm (tokens)))))
     ,(lambda (_) #t)]
    )
   these-tests))


(define test-this (default-unit-tester
		    "Pass cps-tokmac: use CPS on blocking calls."
		    these-tests))


(define test27 test-this)
(define tests27 these-tests)
(define test-cps-tokmac test-this)
(define tests-cps-tokmac these-tests)


(define (temptest)
  (cps-tokmac
   '(rename-stored-lang
  '(program
     (bindings)
     (nodepgm
       (tokens
         (SOC-start
           subtok_ind
           ()
           (stored)
           (begin (void)
                  (call (tok spread-global 0))
                  'multiple-bindings-for-token))
         (node-start
           subtok_ind
           ()
           (stored)
           (begin (call (tok leaf-pulsar_tmpworld_7 0))
                  (call (tok spark-world 0))))
         (f_token_tmpworld_7
           subtok_ind
           ()
           (stored)
           (call (tok m_token_tmpworld_7 0)))
         (spark-world
           subtok_ind
           ()
           (stored)
           (call (tok m_token_tmpworld_7 0)))
         (tmpfunc_8
           subtok_ind
           (a_1)
           (stored)
           (let ([result_4 (local-sense)]) result_4))
         (m_token_tmpworld_7
           subtok_ind
           ()
           (stored)
           (if (not (token-scheduled? (tok f_token_tmprmap_9 0)))
               (call (tok f_token_tmprmap_9 0))
               (void)))
         (f_token_tmprmap_9
           subtok_ind
           ()
           (stored)
           (begin (call (tok m_token_tmprmap_9 0)
                        (call (tok tmpfunc_8 0) this))
                  (timed-call 100 (tok f_token_tmprmap_9 0))))
         (tmpfunc_10
           subtok_ind
           (a_3 b_2)
           (stored)
           (let ([result_5 (+ a_3 b_2)]) result_5))
         (m_token_tmprmap_9
           subtok_ind
           (v)
           (stored)
           (call (tok f_token_result_6 0) v))
         (f_token_result_6
           subtok_ind
           (v)
           (stored)
           (let ([aggrID_20 (+ (* '1000 '0) '0)])
             (call (tok returnhandler_23 aggrID_20)
                   (my-id)
                   '222
                   v
                   '0
                   '0)))
         (m_token_result_6
           subtok_ind
           (v)
           (stored)
           (let ([socretval_13 v])
             (if (= (my-id) '10000)
                 (call (tok SOC-return-handler 0) socretval_13)
                 (let ([aggrID_15 (+ (* '1000 '0) '0)])
                   (call (tok returnhandler_18 aggrID_15)
                         (my-id)
                         '222
                         socretval_13
                         '0
                         '0)))))
         (leaf-pulsar_tmpworld_7
           subtok_ind
           ()
           (stored)
           (begin (call (tok f_token_tmpworld_7 0))
                  (timed-call 1000 (tok leaf-pulsar_tmpworld_7 0))))
         (spread-global
           subtok_ind
           ()
           (stored (ver_33 (void)) (storedliftoption_32 '#f))
           (begin (if storedliftoption_32
                      (void)
                      (begin (set! storedliftoption_32 '#t)
                             (set! ver_33 '0)))
                  (ext-set! #0=(tok global-tree 0) storedgparent_31 '0)
                  (set! ver_33 (+ '1 ver_33))
                  (dbg '"Emitting %d from %d\n" '#0# (my-id))
                  (bcast (tok global-tree 0) (my-id) '1 ver_33)
                  (timed-call 1000 (tok spread-global 0))))
         (global-tree
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (storedgparent_31 '#f)
             (storedgorigin_30 '#f)
             (storedghopcount_29 '#f)
             (storedgversion_28 '#f))
           (if (if (not storedghopcount_29)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version storedgversion_28)
                           (if (= g_version storedgversion_28)
                               (< g_hopcount storedghopcount_29)
                               '#f)
                           '#f)
                       '#f))
               (begin (bcast
                        (tok global-tree subtok_ind)
                        (my-id)
                        g_origin
                        (+ '1 g_hopcount)
                        g_version)
                      (if (not (= g_hopcount '0))
                          (begin (set! storedgparent_31 g_parent)
                                 (set! storedgorigin_30 g_origin)
                                 (set! storedghopcount_29 g_hopcount)
                                 (set! storedgversion_28 g_version))
                          (void)))
               (void)))
         (returnhandler_23
           retid
           #1=(destid flag val toind viaind)
           (stored (acc_27 '0))
           (if (= flag '222)
               (let ([oldacc_22 acc_27])
                 (begin (set! acc_27 '0)
                        (if (not (token-present? (tok global-tree viaind)))
                            (void)
                            (let ([parentpointer_24
                                   (ext-ref
                                     (tok global-tree . #2=(viaind))
                                     storedgparent_31)])
                              (if (not parentpointer_24)
                                  (if (= '0 parentpointer_24)
                                      (call (tok m_token_result_6 toind)
                                            (subcall
                                              (tok tmpfunc_10 0)
                                              val
                                              oldacc_22))
                                      (bcast
                                        (tok returnhandler_23 retid)
                                        parentpointer_24
                                        '333
                                        (subcall
                                          (tok tmpfunc_10 0)
                                          val
                                          oldacc_22)
                                        '0
                                        '0))
                                  (void))))))
               (if (not (if (= destid '0) '#t (= destid (my-id))))
                   (void)
                   (set! acc_27 (subcall (tok tmpfunc_10 0) val acc_27)))))
         (returnhandler_18
           retid
           #1#
           (stored (acc_26 ()))
           (if (= flag '222)
               (let ([oldacc_17 acc_26])
                 (begin (set! acc_26 '())
                        (if (not (token-present? (tok global-tree viaind)))
                            (void)
                            (let ([parentpointer_19
                                   (ext-ref
                                     (tok global-tree . #2#)
                                     storedgparent_31)])
                              (if (not parentpointer_19)
                                  (if (= '0 parentpointer_19)
                                      (call (tok SOC-return-handler toind)
                                            (cons val oldacc_17))
                                      (bcast
                                        (tok returnhandler_18 retid)
                                        parentpointer_19
                                        '333
                                        (cons val oldacc_17)
                                        '0
                                        '0))
                                  (void))))))
               (if (not (if (= destid '0) '#t (= destid (my-id))))
                   (void)
                   (set! acc_26 (cons val acc_26)))))))))))


(define (testcps e)
  (cps-tokmac `(foolang '(program (bindings) (nodepgm (tokens (toknought () ,e)))))))

;(cps-tokmac '(toheu '(program (bindings) (nodepgm (tokens (tok1 () (begin '1 (subcall tok2 '9) '3)) (tok2 (x) x))))))

'(cps-tokmac '(toheu 
	      '(program (bindings) 
			(nodepgm (tokens 
				  (tok1 () (begin '1 (subcall tok2 '9) '3)) 
				  (tok2 (x) x)
				  (tok3 () (dbg "woot %d\n" (subcall tok1)))
				  )))))
