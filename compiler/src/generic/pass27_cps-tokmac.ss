;; [2005.02.10]

;; cps-tokmac
;; This pass does the CPS transformation for the token machine.

;; INIT message = 0
;; CALL message = 1


;;; Input Grammar:
;;; This is the standard "Post-Cleanup" grammar for token machines.
;;; (Except with gradients already desugared.)

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
;;;           | call | subcall | timed_call
;;;           | is_scheduled | deschedule | is_present | evict

;;; The call forms are all just plain prims.  This reflects the fact
;;; that they are permitted to have dynamically computed token
;;; arguments, and also dynamically computed times for timed_call.


;;; Output Grammar:

;;; Subcall's are gone.



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
	  [(begin ,[xs] ...) (apply append xs)]
	  [(if ,[test] ,[conseq] ,[altern]) 
	   (append test conseq altern)]
	  [(let ( (,lhs ,[rhs]) ...) ,[body])
	   (append rhs (remq lhs body))]
	  [(leds ,what ,which) '()]
	  [(,prim ,[rands] ...) rands]
	  [(,[rator] ,[rands] ...) `(apply append rator rands)]
	  [,otherwise
	   (error 'cps-tokmac:freevars 
		  "bad expression: ~s" otherwise)]))

    (define (number-freevars e)
      (let loop ([expr e] [fvs (free-vars e)])	
	(match expr
	  [,x (guard (memq x fvs)) 
	      ;`(vector-ref args (quote ,(list-find-position x fvs)))
	      (string->symbol (format "fv~a" (list-find-position x fvs)))]
	  
	  [,const (guard (constant? const)) `(quote ,const)]
	  [(quote ,const) `(quote ,const)]
	  [,var (guard (symbol? var)) var]
	  [(tok ,tok) `(tok ,tok)]
	  [(tok ,tok ,[expr]) `(tok ,tok ,expr)]
	  [(ext-ref ,tok ,var) `(ext-ref ,tok ,var)]
	  [(ext-set! ,tok ,var ,[expr]) `(ext-set! ,tok ,var ,expr)]
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
	      (match tokbind
		     [(,tok ,subid (,args ...) (bindings ,b ...) (stored ,s ...) ,e)
		      `(,tok ,subid (k ,args ...) (bindings ,b ...) (stored ,s ...) 
			     ,(amend-tainted e))]
		     )))
	  
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
	     [,const (guard (constant? const)) 
		     (pvk `(quote ,const))]
	     [(quote ,const) (pvk `(quote ,const))]
	     [,var (guard (symbol? var)) (pvk var)]
	     [(tok ,tok) (pvk `(tok ,tok))]
	     [(tok ,tok ,[expr]) 
	      (loop expr
		    (lambda (e) (pvk `(tok ,tok ,e))))]
	     [(ext-ref ,tok ,var) (pvk `(ext-ref ,tok ,var))]
	     [(ext-set! ,tok ,var ,[expr]) 
	      (loop expr
		    (lambda (e) (pvk `(ext-set! ,tok ,var ,e))))]
	     [(begin ,x ,y ...)
	      (loop x 
		    (lambda (e) 
		      (pvk 
		       (make-begin 
			'(begin ,e			 
				,(loop `(begin ,y ...)))))))]
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
			   `(let*  ([,lhs ,r]) ,b)))))]
	     
	     [(subcall ,tok ,args* ...)
	      (let* (;; This expression represents the continuation of the subcall:
		     [broken-off-code (pvk 'fv0)] ;(pvk RETVAL_VARNAME)]
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

	     [(call ,tok ,[args*] ...)
	      (loop-list args* 
			 (lambda (ls)
			   (pvk
			    `(call ,tok ,@ls))))]
	     [(timed-call ,time ,tok ,[args*] ...)
	      (loop-list args* 
			 (lambda (ls)
			   (pvk
			    `(timed-call ,time ,tok ,@ls))))]
	     [(leds ,what ,which) (pvk `(leds ,what ,which))]

	     [(,prim ,[rands] ...)
	      (guard (or (token-machine-primitive? prim)
			 (basic-primitive? prim)))
	      (loop-list rands 
			 (lambda (ls)
			   (pvk
			    `(,prim ,@ls))))]
	     [(,[opera*] ...)
	      (loop-list opera*  pvk)]

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
		(match (car tbs)
		    [(,tok ,subid ,args (bindings ,b ...) (stored ,s ...) ,expr)
		     (mvlet ([(newexpr taints newtokbinds)			     
                              (process-expr expr)])
			    (loop (cdr tbs)
				  (append taints tainted)
				  (cons 
				   `(,tok ,subid ,args (bindings ,b ...) (stored ,s ...) ,newexpr)
                                   (append newtokbinds
                                           tbacc))))]
		    )))))

(define cps-tokmac
  (let ()

      	 
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
	     `(,lang '(program (bindings ,constbinds ...)
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

