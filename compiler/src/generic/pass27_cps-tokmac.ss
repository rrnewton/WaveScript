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

;;; Subcall's are gone.


(define cps-tokmac
  (let ()

    (define INIT 0)
    (define CALL 0)

    ;; Name of the token which represents the global variable storing the continuation
    ;; token count.
    (define KCOUNTER 'kcounter)
    ;; And the root name of continuation tokens.
    (define KNAME 'K)

    (define (free-vars e)
	(match e
	  [,const (guard (constant? const)) '()]
	  [(quote ,const) '()]	  
	  [,var (guard (symbol? var)) (list var)]
	  [(tok ,tok) '()]
	  [(tok ,tok ,[expr]) expr]
	  [(ext-ref ,tok ,var) '()]
	  [(ext-set! ,tok ,var ,[expr]) expr]
	  [(set! ,v ,[e]) (cons v e)]
	  [(begin ,[xs] ...) (apply append xs)]
	  [(if ,[test] ,[conseq] ,[altern]) 
	   (append test conseq altern)]
	  [(let ( (,lhs ,[rhs])) ,[body])
	   (append rhs (remq lhs body))]
	  [(leds ,what ,which) '()]
	  [(,prim ,[rands] ...) 
	   (guard (or (token-machine-primitive? prim)
		      (basic-primitive? prim)))
	   rands]
	  [(app ,[rator] ,[rands] ...) `(apply append rator rands)]
	  [,otherwise
	   (error 'cps-tokmac:freevars 
		  "bad expression: ~s" otherwise)]))

    (define (number-freevars e)
      (let loop ([expr e] [fvs (free-vars e)])	
	(match expr
	  [,x (guard (memq x fvs)) 
	      ;`(vector-ref args (quote ,(list-find-position x fvs)))
	      (string->symbol (format "fv~a" (list-find-position x fvs)))]
	  [(set! ,x ,[e]) (guard (memq x fvs))
	   `(set! ,(string->symbol (format "fv~a" (list-find-position x fvs)))
		  ,e)]
	  
	  [,const (guard (constant? const)) `(quote ,const)]
	  [(quote ,const) `(quote ,const)]
	  [,var (guard (symbol? var)) var]
	  [(tok ,tok) `(tok ,tok)]
	  [(tok ,tok ,[expr]) `(tok ,tok ,expr)]
	  [(ext-ref ,tok ,var) `(ext-ref ,tok ,var)]
	  [(ext-set! ,tok ,var ,[expr]) `(ext-set! ,tok ,var ,expr)]
	  [(set! ,v ,[e]) `(set! ,v ,e)]

	  [(begin ,[xs] ...) `(begin ,@xs)]
	  [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
	  [(let ( (,lhs ,[rhs]) ...) ,body)	 
	   `(let  ([,lhs ,rhs])
	      ,(loop body (remq lhs fvs)))]
;	  [(,call-style ,tok ,[args*] ...)
;	   (guard (memq call-style '(call subcall)))
;	   `(,call-style ,tok ,args* ...)]
;	  [(timed-call ,time ,tok ,[args*] ...)
;	   `(timed-call ,time ,tok ,args* ...)]
	  [(leds ,what ,which) `(leds ,what ,which)]
	  [(,prim ,[rands] ...)
	   (guard (or (token-machine-primitive? prim)
		      (basic-primitive? prim)))
	   `(,prim ,rands ...)]
	  [(,[rator] ,[rands] ...) `(,rator ,rands ...)]
	  [,otherwise
	   (error 'cps-tokmac:number-freevars 
		  "bad expression: ~s" otherwise)])))

      (define amend-tainted
	(let ()
	  (define (kify e) `(call k ,e))
	  (define (put-k expr)
	    (match expr
		   [,const (guard (constant? const)) (kify `(quote ,const))]
		   [(quote ,const) (kify `(quote ,const))]
		   [,var (guard (symbol? var)) (kify var)]
		   [(tok ,tok) (kify `(tok ,tok))]
		   [(tok ,tok ,[expr]) (kify `(tok ,tok ,expr))]
		   [(ext-ref ,tok ,var) (kify `(ext-ref ,tok ,var))]
		   [(ext-set! ,tok ,var ,[expr]) (kify `(ext-set! ,tok ,var ,expr))]
		   [(set! ,v ,[e]) `(begin `(set! ,v ,e) ,(kify '(void)))]
		   ;; This case shouldn't be used:
		   [(begin) '(begin)]
		   [(begin ,xs ...) 
		    `(begin ,@(rdc xs) ,(kify (rac xs)))]
		   [(if ,test ,[conseq] ,[altern])
		    `(if ,test ,conseq ,altern)]
		   [(let ( (,lhs ,[rhs]) ...) ,body)	 
		    `(let  ([,lhs ,rhs])
		       ,(kify body))]
		   [(leds ,what ,which) (kify `(leds ,what ,which))]
		   [(,prim ,rands ...)
		    (guard (or (token-machine-primitive? prim)
			       (basic-primitive? prim)))
		    (kify `(,prim ,rands ...))]
		   [(,[rator] ,[rands] ...) 
		    (kify `(,rator ,rands ...))]
		   [,otherwise
		    (error 'cps-tokmac:number-freevars 
			   "bad expression: ~s" otherwise)]))

	  (define do-amend
	    (lambda (tokbind)
	      (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tokbind)])
		     (if (not (null? constbinds)) (error 'cps-tokmac "Not expecting local constbinds!"))
		     `(,tok ,id (k ,@args) (stored ,@stored)
			     ,(amend-tainted body)))))
	  
	  (lambda (tokbinds)
	    (map do-amend tokbinds))))

	  
    ;; Best way to represent a thing with a hole in it is a continuation:
    (define process-expr
      (lambda (expr)
	;; These simply accumulate.  There is no reason to complicate
	;; the continuation passing algorithm below by adding extra
	;; arguments to the continuation.

	;; Tainted tokens, must take continuations and return values to them:
	(define tainted-toks '())
	;; Continuation code must reside in new handlers:
	(define new-handlers '())

	(define loop-list
	  (lambda (ls  pvk)
	    (pvk
	     (let inner ([ls ls])
	       (if (null? ls) '()
		   (loop (car ls)  
			 (lambda (x)
			   (cons x (inner (cdr ls))))))))))

	(define loop
	  (lambda (expr  pvk)
	    (match expr
	     [,x (guard (begin (display-constrained "\n  PExp: " (list x 50) "\n")
;			       (display-constrained "  Cont: " (list (pvk (unique-name 'HOLE)) 50) "\n")
			       #f)) 3]
		   
	     [,const (guard (constant? const)) 
		     (pvk `(quote ,const))]
	     [(quote ,const) (pvk `(quote ,const))]
	     [,var (guard (symbol? var)) (pvk var)]
	     [(set! ,v ,expr)
	      (loop expr (lambda (e) (pvk `(set! ,v ,e))))]
	     [(tok ,tok) (pvk `(tok ,tok))]
	     [(tok ,tok ,expr) 
	      (loop expr
		    (lambda (e) (pvk `(tok ,tok ,e))))]
	     [(ext-ref ,tok ,var) (pvk `(ext-ref ,tok ,var))]
	     [(ext-set! ,tok ,var ,expr)
	      (loop expr
		    (lambda (e) (pvk `(ext-set! ,tok ,var ,e))))]
             [(begin) (pvk '(void))]
	     [(begin ,x ,y ...)
	      (loop x 
		    (lambda (e) 
                      (loop `(begin ,@y)
                            (lambda (e2)
                              (pvk ;`(begin ,e ,e2)
			       (make-begin `(begin ,e ,e2))
			       )))))]
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
		     [broken-off-code (pvk 'fv0)] ;(pvk RETVAL_VARNAME)]
		     [_ (disp "GOT BROKEN OFF: " broken-off-code)]
		     [fvs (free-vars broken-off-code)]
		     [args (map (lambda (n)
				  (string->symbol (format "arg~a" n)))
				(iota (length fvs)))]
		     [fvns (map (lambda (n)
				   (string->symbol (format "fv~a" n)))
				(iota (length fvs)))]
		     )
		;; Now we mutate our accumulators:
		(set! tainted-toks (cons tok tainted-toks))
		(set! new-handlers
		      (cons
		       `(,KNAME id (flag ,@args)
				 (stored ,@fvns)
					;(map list fvns args))
					;(make-vector ,(length fvs)))
				 (if (eq? flag ,INIT)
				     `(begin
				       ,@(map (lambda (fv a)
					       `(set! ,fv ,a))
					      fvns args))
				     ;; Otherwise, assume the flag is CALL
				     (begin
				       ,(number-freevars broken-off-code)
				       ;; Since these are one-shot continuations, 
				       ;; we deallocate ourselves on the way out:
				       (evict ,KNAME)
				       )))
		       new-handlers))
		
		(loop-list args* 
			   (lambda (ls)
			     ;; Get a fresh subtokname for our continuation:
			     `(let ([ktok (make-tokname ',KNAME 
							(ext-ref ',KCOUNTER counter))])
				(call ktok ,INIT ,@fvs)
				(call ,tok ktok ,@ls))))
		)]

	     [(call ,tok ,args* ...)
	      (loop-list args* 
			 (lambda (ls)
			   (pvk
			    `(call ,tok ,@ls))))]
	     [(timed-call ,time ,tok ,args* ...)
	      (loop-list args* 
			 (lambda (ls)
			   (pvk
			    `(timed-call ,time ,tok ,@ls))))]
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
	      (error 'cps-tokmac:process-expr 
		     "bad expression: ~s" otherwise)]
	     )))
	
	;; process-expr returns an expression, tainted-tokens, and new handlers
	(let ((newexpr (loop expr (lambda (x) x))))
	  (values newexpr 
		  tainted-toks
		  new-handlers))))

    ;; Returns a set of tainted token-names and a list of new tokbinds.
      (define process-tokbinds
	(lambda (tokenbinds)
	  (let loop ([tbs tokenbinds] [tainted '()] [tbacc '()])
	    (if (null? tbs)
		(values (list->set tainted) tbacc)
		(mvlet ([(tok subid args stored constbinds body) (destructure-tokbind (car tbs))])
		  (if (not (null? constbinds)) (error 'cps-tokmac "Not expecting local constbinds!"))
		  (mvlet ([(newexpr taints newtokbinds) (process-expr body)])
			 (loop (cdr tbs)
			       (append taints tainted)
			       (cons 
				`(,tok ,subid ,args (stored ,@stored) ,newexpr)
				(append newtokbinds tbacc)))))))))

    ;; CPS-tokmac main body:
    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...)
			  (nodepgm (tokens ,nodetoks ...))))
	 (mvlet ([(tainted newtoks1) (process-tokbinds nodetoks)])
	   (let ([newtoks2
		  (map (lambda (tb)
			 (if (memq (car tb) tainted)
			     (amend-tainted tb)
			     tb))
		       newtoks1)])
	     `(cps-tokmac-lang '(program (bindings ,constbinds ...)
					 (nodepgm (tokens ,@newtoks2))))))]))))



(define these-tests
  `(
    
    ["Put an empty test through." 
     (cps-tokmac
      '(cps-tokmac-lang
	'(program
	  (bindings )
	  (nodepgm (tokens)))))
     ,(lambda (_) #t)]


))


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
