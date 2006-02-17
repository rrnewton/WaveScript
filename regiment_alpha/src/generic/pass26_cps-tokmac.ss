
;;;; .title: CPS tokmac

;;;; [2005.02.10] <br> 
;;;; This pass does the CPS transformation for the token machine.  <br><br>

;;;; Introduces lambda's and kcall.                                <br><br>


; INIT message = 0
; CALL message = 1

; --------------------------------------------------------------------------------

;;;;   Input Grammar:                                                   <br>
;;;;   This is the standard "Post-Cleanup" grammar for token machines.  <br>
;;;;   (except with gradients already desugared.)                       <br><br>

;   NOTE: For now, subtokens are required to call static token
;   destinations such that we know who to modify for CPS!

;    <Pgm> ::= (program (bindings <Cbind>*) <NodePgm>)
;    <NodePgm> ::= (nodepgm (tokens <TokBinding>*))
;       NOTE: tokens will inclide a binding for SOC-start and node-start
;    <Cbind> ::= (<var> <Exp>)
;       NOTE: This expressions will be statically calculable -- constants.
;    <TokBinding> ::= (<TokName> <SubtokId> (<Var> ...) (bindings <Cbind>*) (stored <Stored>) <Expr>)
;    <TokName>   ::= <Symbol> 
;    <SubtokId>  ::= <Number>
;    <Token>     ::= (tok <TokName>) | (tok <Tokname> <Int>)
;    <DynToken>  ::= (tok <Tokname> <Expr>) | <Token>
;       NOTE: Either the whole token reference or just the sub-index can be dynamic.
;    <Expr>      ::= (quote <Constant>)
;                  | <Var>
;                  | <DynToken>
;                  | (set! <Var> <Expr>)
;                  | (ext-ref <Token> <Var>)
;                  | (ext-set! <Token> <Var> <Expr>)
;       NOTE: These are static token refs for now.
;                  | (begin <Expr> ...)
;                  | (let ((<Symbol> <Expr>)) <Expr>)
;                  | (if <Expr> <Expr> <Expr>)
;                  | (subcall <DynToken> <Expr>...)
;                  | (<Prim> <Expr> ...)
;                  | (<Expr> ...)
;                  | (leds <Red|Yellow|Green> <On|Off|Toggle>)
;    <Prim> ::= <BasicPrim> 
;             | call | subcall | timed-call
;             | is_scheduled | deschedule | is_present | evict

;   The call forms are all just plain prims.  This reflects the fact
;   that they are permitted to have dynamically computed token
;   arguments, and also dynamically computed times for timed-call.


;;;; Output Grammar:                                                   <br><br>

;;;; Subcall's and "return" statements are gone.                       <br>


;; TODO: USE NULLK
;; TODO: Add the hack that it just doesn't touch a handler that has no SUBCALL.


;; NOTE: Everiwhere that "OPTIMIZE" occurs is a (potentially
;; dangerous) optimization that could be turned off.

; --------------------------------------------------------------------------------

;; I accumulate tests through mutation throughout this file.
;; This method allows me to test internal functions whose definitions
;; are not exposed at the top level.
(define these-tests '())

(define cps-tokmac
  (let ()
    
    (define (id x) x)

    (define (tb->body tb)
      (mvlet ([(tok subid args stored constbinds body) (destructure-tokbind tb)])
	     body))

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
		[,x (loop x)]))
       (lambda (ls _) (apply append ls))
       e))

    ;; Returns a list of tokens referenced in (tok _ _) expressions.
    ;; The consumer probably wants to convert the output to a set.
    (define (toks-referenced e)
      (tml-generic-traverse
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
		 (match x 
			[(tok ,t ,n) (guard (integer? x)) '()]
			[(tok ,t ,[e]) e]
			[,x (loop x)])))])
	(tml-generic-traverse
	 ;; driver, fuser, expression
	 (lambda  (x loop)
	   (define allowed (allowed1 loop))
	   (match x
;		  [,x (guard (begin (printf "~ntoks-escaped looping: ") (display-constrained (list x 50)) (newline) #f)) 3]
		  [(ext-ref ,[allowed -> t] ,v) t]
		  [(ext-set! ,[allowed -> t] ,v ,[e]) (append t e)]
		  [(,call ,[allowed -> rator] ,[rands] ...)
		   (guard (memq call '(bcast call call-fast)))
		   (apply append rator rands)]
		  [(timed-call ,t ,[allowed -> rator] ,[rands] ...)
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

  ;; Traverse the expression looking for tokens tainted by "subcalls"
  (define (get-tainted expr)
    (tml-generic-traverse
     (lambda (x loop)
       (match x
	      [(subcall (tok ,t ,[e]) ,[args] ...)
	       (apply append (list t) e args)]
	      [,e (loop e)]))
     (lambda (ls _) (apply append ls))
     expr))

  (define (has-subcall? expr)
    (tml-generic-traverse
     (lambda (x loop)
       (match x
	      [(subcall (tok ,t ,e) ,args ...) #t]
	      [,e (loop e)]))
     (lambda (ls _) (ormap id ls))
     expr))
  (define (has-kcall? expr)
    (tml-generic-traverse
     (lambda (x loop)
       (match x
	      [(kcall ,k ,v) #t]
	      [,e (loop e)]))
     (lambda (ls _) (ormap id ls))
     expr))

  ;; Traverse the expression checking to see if it has a (return _) statement.
  (define (has-return? expr)
    (let ((result
    (tml-generic-traverse
       (lambda  (x loop)
	 (match x 
		[(return ,e) #t]
		[,e (loop e)]))
       (lambda (ls _) (ormap id ls))
       expr)))
      ;(break)
      result
    ))

  ;; Verify that every program path ends in a (return _) statement.
  (define (valid-returns? expr)
    (let ((nonret (lambda (expr)
		    (tml-generic-traverse
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
	  (mvlet ([(tok subid args stored constbinds body) (destructure-tokbind tb)])
	    (let* ([k (unique-name 'k)]	    
		   [desugar-return 
		    (lambda (expr)
		      (tml-generic-traverse
		       (lambda (expr loop) ;; driver
			 (match expr
			   ; [2005.11.02] Disabling: now original return statements do nothing:
;				[(return ,[x]) `(kcall ,k ,x)]
			   [(return ,x) x]


				;; [2005.10.26] What does this do??  Looks like junk: removing  ;; FIXME!!!
;				[(set! ,v ,[e])
;				 `(begin (set! ,v ,e) (call ,k (void)))]
				[,x (loop x)]))
		       (lambda (expls reconstruct) (apply reconstruct expls)) ;; fuser
		       expr))])
	      (values
	       `[,tok ,subid (,k ,@args) (stored ,@stored)
		      ,(desugar-return body)]
	       k)))))
		           
    (define add-kargs
      (lambda (expr tainted)
	(tml-generic-traverse
	 (lambda (expr loop) ;; driver
	   (match expr
	     [(timed-call ,[time] (tok ,tok ,[subtok]) ,[args] ...)
	      (if (memq tok tainted)
		  `(timed-call ,time (tok ,tok ,subtok) ,NULLK ,@args)
		  `(timed-call ,time (tok ,tok ,subtok) ,@args))]
	     [(,call (tok ,t ,[subtok]) ,[args] ...)
	      (guard (memq call '(call bcast call-fast )))
	      ;; If it's a tainted token we insert a continuation argument:
	      (if (memq t tainted)
		  `(,call (tok ,t ,subtok) ,NULLK ,@args)
		  `(,call (tok ,t ,subtok) ,@args))]
	     ;; Anonymous call:
	     [(,call ,[opera*] ...)
	      (guard (memq call '(call bcast call-fast timed-call)))	     
	      (if (eq? call 'timed-call)
		  `(timed-call ,(car opera*) ,(cadr opera*) ,NULLK ,(cddr opera*))
		  `(,call ,(car opera*) ,NULLK ,(cdr opera*)))]

	     ;; This is just a hack to stop add-kargs from adding a *second* k argument:
	     [(kcall-fast ,[x*] ...)
	      `(call-fast ,x* ...)]

	     [,x (loop x)]))
	 (lambda (expls reconstruct) (apply reconstruct expls)) ;; fuser
	 expr)))

    ;; expand-subcalls expands out all subcalls 
    ;; Best way to represent a thing with a hole in it is a continuation:
    ;; (Using CPS to convert to CPS.)
    ;; Thus this pass itself carries a continuation representing the context of the current expression.
    (define expand-subcalls
      (lambda (expr k-arg)
    ;---------------------------------------
    (define generate-k
      (lambda ()
        (unique-name 'k)))  ;; This could just return 'k
    ;---------------------------------------
    ;;Continuation Lambda: this closes up a continuation (like a clam ;) ).
    (define (CLAM E)
        (let ((formal (unique-name 'HOLE)))
          (let ((body (E formal)))
	    (match body
	      ;; [2005.11.02] This is a quick little eta-reduce OPTIMIZATION:
	      ;; It can effectively serve as tail call optimization:
	      [(kcall ,k ,v) (guard (eq? v formal) (or (symbol? k) (equal? k NULLK)))  k]
	      ;; [2005.11.02] Hopefully we'll do better opts in our inline pass
	      ;; but this OPTIMIZATION removes generation of unnecessary closures:
	      [,v (guard (symbol? v) (eq? formal v))    NULLK]
	      [,_  `(lambda (,formal) ,body)]))))
    ;---------------------------------------
    (define possible-letk
      (lambda (cont-exp abs-body)
	;; [2005.11.03] RRN OPTIMIZE aliases:
	(if (or (symbol? cont-exp)
		(equal? cont-exp NULLK))
	    (abs-body cont-exp)
	    (let ([k (generate-k)])
	      (let ([body (abs-body k)])
		`(let ((,k ,cont-exp))
		   ,body))))))
    ;---------------------------------------
    (define make-kcall
      (lambda (k v)
	(if (equal? k NULLK)
	    v
	    `(kcall ,k ,v))))
    ;---------------------------------------
    (define cps*
      (lambda (N* E)
        (cond
          ((null? N*) (E '()))
          (else (cps (car N*)
                     (lambda (a)
                       (cps* (cdr N*)
                             (lambda (d) (E (cons a d))))))))))
    ;---------------------------------------
    (define cps
      (lambda (t E)
	;; E is the continuation representing the enclosing expression to the current being processed.
        (match  t
	  [,c (guard (constant? c)) (E c)]
          [(quote ,c) (E `(quote ,c))]
	  [(leds ,what ,which) (E `(leds ,what ,which))]

          [,x (guard (symbol? x)) (E x)]	  
	  [(set! ,v ,x)  (cps x (lambda (x) (E `(set! ,v ,x))))]
	  [(tok ,t ,n) (guard (integer? n)) (E `(tok ,t ,n))]
	  [(tok ,t ,x) (cps x (lambda (x) (E `(tok ,t ,x))))]
	  [(ext-ref ,x ,v) (cps x (lambda (x) (E `(ext-ref ,x ,v))))]
	  [(ext-set! ,e1 ,v ,e2) 
	   (cps e1 (lambda (e1)   
		     (cps e2 (lambda (e2)
			       (E `(ext-set! ,e1 ,v ,e2))))))]
	  [(begin) (E '(void))]
          [(begin ,x) (cps x E)]
          [(begin ,expr ,expr* ...)	   
;	   (printf "DOING BEGIN: ~s ~s Here's clam: ~s\n" expr expr* (CLAM E))
           (cps expr
                (lambda (v)
		  (make-begin `(begin ,v ,(cps `(begin ,expr* ...) E)))))]
          [(if ,test-exp ,true-exp ,false-exp)
	   (define (default-case)
	     (possible-letk (CLAM E)
			    (lambda (k)
			      (cps test-exp
				   (lambda (v)
				     `(if ,v
					  ,(cps true-exp (lambda (v) (make-kcall k v)))
					  ,(cps false-exp (lambda (v) (make-kcall k v))))
				     )))))
	   ;; OPTIMIZATION
	   (if (and (not (has-subcall? true-exp))
		    (not (has-subcall? false-exp))
		    ;#f ;; DISABLING
		    )
	       ;; TODO FIXME: HACK [2005.11.03] - 
	       ;; if there are no subcalls on either branch, then there's
	       ;; no reason to grab the continuation.
	       ;; Need to verify that this is correct, and also think 
	       ;; about what problems it doesn't solve.
	       (let ((left (cps true-exp (lambda (v) v)))
		     (right (cps false-exp (lambda (v) v))))
		 (if (or (has-kcall? left)
			 (has-kcall? right))
		     ;; Then this optimization is not legal, revert to default:
		     (default-case)
		     ;; Otherwise it's safe, let's go ahead.
		     (cps test-exp (lambda (v) (E `(if ,v ,left ,right))))))
	       (default-case))]

          [(let ((,x ,N) ...) ,body)
           (possible-letk (CLAM E)
                          (lambda (k)
                            (cps* `(,N ...)
                                  (lambda (w*)
                                    `(let ,(map list `(,x ...) w*)
                                       ,(cps body (lambda (v) (make-kcall k v))))))))]
	  
	  

	  ;; [2005.10.31] Changed to use high-priority scheduling for subcalls:
	  ;; This is where we CLAM the continuation:
	  [(subcall ,tok ,args ...)
	   (cps tok (lambda (t)
		      (cps* args
			    (lambda (w*)
			      `(kcall-fast ,t ,(CLAM E) ,@w*)))))]
	  
	  [(,call ,opera* ...)
	   (guard (memq call '(call bcast call-fast timed-call)))
	   (cps* opera*
		 (lambda (opera*)
		   (E `(,call ,opera* ...))
		   ))]

	  [(kcall ,t ,v)
	   ;(error 'cps-tokmac "what's this doing here?  found a kcall in cps-tokmac..: ~s" `(kcall ,t v))]
	   (cps t (lambda (t)
		    (cps v (lambda (v)
			     (E `(kcall ,t ,v))))))]

	  [(,prim ,rands ...)
	   (guard (or (token-machine-primitive? prim)
		      (basic-primitive? prim)))
	   (cps* rands 
		 (lambda (ls)
		   (E `(,prim ,@ls))))]

	  [(app ,opera* ...)
	   (warning 'cps-tokmac  "arbitrary application of rator: ~s" opera*);(car opera*))
	   (cps* opera*  (lambda (ls) (E (cons 'app ls))))]

	  [,otherwise (error 'cps-tokmac:expand-subcalls "bad expression: ~s" otherwise)]
	  )))
    ;---------------------------------------	
	;; expand-subcalls returns an expression and new handlers
      (let ((newexpr (cps expr (lambda (x) (if k-arg `(kcall ,k-arg ,x) x)))))
	newexpr)))
;	    (values newexpr 
;		    new-handlers)))))

      (define process-tokbind
	(lambda (tokenbind tainted k-arg)
	  (disp "TAINTED" tainted)
	  (mvlet ([(tok subid args stored constbinds body) (destructure-tokbind tokenbind)])
	    (if (not (null? constbinds)) (error 'cps-tokmac "Not expecting local constbinds!"))
	    ;; Splitting this into two traversals:
	    (let* ([newexpr1 ;; OPTIMIZATION: don't bother cps'ing at all if there are no subcalls.
		    (if (has-subcall? body)
			(expand-subcalls body k-arg)
			(if k-arg `(kcall ,k-arg ,body) body))]
		   [newexpr2 (add-kargs newexpr1 tainted)])
	      `(,tok ,subid ,args (stored ,@stored) ,newexpr2)))))

 
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
	     [(,free-vars '(begin '0 '1 (let ([z '3]) z)))
	      ()]
	     	     

	     ["Testing toks escaped"
	      (,toks-escaped '(begin '1 (let ([kind_4 '22])
					 (begin '11
						(call t (tok K_5 kind_4) '2)))))
	      (K_5)]
	     [(,toks-escaped '(begin '1 (let ([kind_4 '22])
					 (begin '11
						(call (tok K_5 kind_4) '2)))))
	      ()]
	     [(,toks-escaped '(if (= flag '0)
				 (if (= subtok_ind '0) (void) (begin))
				 (begin (begin fv0 '3) (evict (tok K_5 0)))))
	      ()]
	     [(,toks-escaped '(if (= flag '0)
				 (if (= subtok_ind '0) (void) (begin))
				 (begin (begin fv0 '3) (tok K_5 0))))
	      (K_5)]
	     
	     [" and its companion: toks-referenced "
	      (,toks-referenced '(begin '1
				    (let ([kind_4 '22])
				      (begin '11
					     (call (tok K_5 kind_4) '2)))))
	      (K_5)]
        
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

	     ["Test expand-subcalls"
	      (reunique-names
	       (let ([expr 
		      (,add-kargs (,expand-subcalls '(begin '1 (subcall (tok t 0) '2) '3) #f)
				  '(t))])
		 (let ([lam (deep-assq 'lambda expr)])
		   (list 
		    (,toks-referenced expr)
		    (,toks-escaped expr)
		   (,free-vars lam)
		   (,free-vars expr)
		   ))))
	      ((t) () (HOLE) (HOLE))]


;; Maybe use in next pass:     
#;	     ["Test expand-subcalls"
	      (call-with-values 
		  (lambda () (,expand-subcalls '(begin '1 (subcall (tok t 0) '2) '3)))
		(lambda (expr) ;ktbs)
		  (or 
		   (and (= 1 (length ktbs))
			(mvlet ([(tok subid args stored constbinds body) (destructure-tokbind (car ktbs))])
			       (list (equal? (,toks-referenced body) (list tok))
				     (equal? (,toks-escaped body) '())
				     (set-equal? (list->set (,free-vars body)) '(flag subtok_ind fv0))
				     (,free-vars expr))))
		   ;; If the test fails, give information:
		   (list expr ktbs)
		   )))
	      (#t #t #t (t))]


	      
	     ) these-tests))


    ;; Main body of cps-tokmac
    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...)
			  (nodepgm (tokens ,nodetoks ...))))
	 (let ([alltoks (map car nodetoks)]
	       [returntokbinds (filter (lambda (tb) (has-return? (tb->body tb))) nodetoks)]
	       [subcalledtoks (apply append (map get-tainted (map tb->body nodetoks)))])
	   (let ([returntoks (map car returntokbinds)])
	   ;; Verify that the return toks have valid returns:
	   (for-each (lambda (x) 
		       (unless (valid-returns? (tb->body x))
			 (pretty-print x)
			 (error 'cps-tokmac 
				"token handler(s) ^^^ does not have a 'return' statement on all code paths: \n~s"
				x)))
		     returntokbinds)
             (let ([subonly (difference subcalledtoks returntoks)]
		   [retonly (difference returntoks subcalledtoks)])
	       ;(if (regiment-verbose)
	       ;(disp "Results, sub&ret: " (intersection subcalledtoks returntoks)  subonly retonly)
	       
	     ;; Issue warnings for tokens that are subcalled but don't have return statements.
	     (if (not (null? subonly))
		 (for-each 
		  (lambda (x)
		    (if (memq x alltoks)
			(warning 'cps-tokmac
				 "Token handlers for ~a are subcalled but do not have return statements!"
				 x)
			(warning 'cps-tokmac
				 "Subcall to nonexistent token handler!: ~a" x)))
		  subonly))
	     ;; It's also worth a warning if we have useless returns:
	     (if (not (null? retonly))
		 (warning 'cps-tokmac
			  "Token handlers for ~a have return statements but there are no subcalls to them!"
			  retonly))

	     ;; For now we're going to be lenient and put in implicit return statements:
	     ;; Then we need to update our list of tokbinds carrying returns.
	     (set! nodetoks (map (lambda (tb)
				   (if (memq (car tb) subonly)
				       (match tb 
					      [(,tok ,subid ,args (stored ,st* ...) ,bod)
					      `(,tok ,subid ,args (stored ,st* ...) (return ,bod))])
				       tb))
				 nodetoks))
	     ;; This line is inefficient but safe:
	     (set! returntokbinds (filter (lambda (tb) (has-return? (tb->body tb))) nodetoks))
	     (set! returntoks (map car returntokbinds))

	     ;(disp "  postupdate: Results, sub&ret: " (intersection subcalledtoks returntoks) subcalledtoks returntoks)
	     )


	   ;; Now for the real work.
	   (let* ([newtbs
		   (map (lambda (tb)
			  (if (memq (car tb) returntoks)
			      (mvlet ([(tb k-arg) (kify-tokbind tb)])
;				(printf "AFTER KIFY: \n") (pretty-print tb)
				(process-tokbind tb returntoks k-arg))
			      (process-tokbind tb returntoks #f)))
		     nodetoks)])
			  
	     (let ((final 
		    ;; Next we process all tokbinds to expand out subcalls:
		    `(,lang '(program (bindings ,constbinds ...)
			       (nodepgm (tokens ,@newtbs))))))

	       #; ;; [2005.11.02] Removing this because it's no longer true that we make no changes to non-subcalling programs.
	       (DEBUGMODE ;; If we're in debug mode, let's make sure the pass had no effect on non-subcall programs.
		(and (not (deep-assq 'subcall prog))
		     (not (equal? prog final))
		     (begin (printf "Input: \n")
			    (define-top-level-value 'a prog)
			    (pretty-print prog)
			    (printf "Output: \n")
			    (define-top-level-value 'b final)
			    (pretty-print final)
		     (error 'cps-tokmac "BUG: Changed input program ^^^ (bound to 'a', output bound to 'b') even though it had no subcalls."))))
	       final
	       ))))]))
))


(define (testcps-expr e)
  (match e 
	 [(tokens ,tbs ...)
	  (cps-tokmac `(foolang '(program (bindings) (nodepgm (tokens ,@tbs)))))]
	 [,expr
	  (cps-tokmac `(foolang '(program (bindings) (nodepgm (tokens (toknought subtokid () (stored) ,expr))))))]))

;; Finally we add some tests for the whole module -- for the externally visible parts of the module.
(set! these-tests
  (append these-tests
   `(
    
    ["Put an empty test through." 
     (cps-tokmac
      '(cps-tokmac-lang
	'(program
	  (bindings )
	  (nodepgm (tokens)))))
     ,(lambda (_) #t)]

    ["Now test with two tokens and one subcall."
     (reunique-names
      (cps-tokmac '(foolang
		    '(program
			 (bindings)
		       (nodepgm
			   (tokens (tok1 () (+ '1 (subcall (tok tok2 0) '2)))
				   (tok2 (x) (return (* '2 x)))))))))
     ;; This might not be safe under different implementations.
     ;; Different arg evaluation and map ordering could very well change the unique-naming below.
     (foolang
      '(program
	(bindings)
	(nodepgm
	 (tokens
	  (tok1 subtok_ind
		()
		(stored)
		(call-fast (tok tok2 0) (lambda (HOLE) (+ '1 HOLE)) '2))
	  (tok2 subtok_ind (k x) (stored) (kcall k (* '2 x)))))))]
     
;; Maybe move to the next pass in some fashion:
#;    ["Now test with two tokens and one subcall."
     (cps-tokmac '(foolang
		   '(program
		     (bindings)
		     (nodepgm
		      (tokens (tok1 () (+ '1 (subcall (tok tok2 0) '2)))
			      (tok2 (x) (return (* '2 x))))))))
     ;; This oracle procedure just abstracts over varying generated names.  
     ;; Instead we should just reset the name generator.
     ,(lambda (x)
	(match x 
	  [(foolang
	    '(program
	      (bindings)
	      (nodepgm
	       (tokens
		(tok1 subtok_ind
		      ()
		      (stored)
		      (let ([,kind1
			     (if (token-present? (tok ,k1 0))
				 (let ([new (+ '1 (ext-ref (tok ,k2 0) kcounter))])
				   (begin (ext-set! (tok ,k3 0) kcounter new)
					  new))
				 (begin (call (tok ,k4 0) '0) '1))])
			(begin (call (tok ,k5 ,kind2) '0)
                        (call (tok tok2 0) (tok ,k6 ,kind3) '2))))
		(,k7 subtok_ind
		     (flag fv0)
		     (stored (kcounter 0))
		     (if (= flag '0)
			 (if (= subtok_ind '0) (void) (begin))
			 (begin (+ '1 fv0) (evict (tok ,k8 subtok_ind)))))
		(tok2 subtok_ind (,otherk1 x) (stored) (call ,otherk2 (* '2 x)))))))
	   (guard (andmap (lambda (x) (eq? x k1)) (list k2 k3 k4 k5 k6 k7 k8))
		  (andmap (lambda (x) (eq? x kind1)) (list kind2 kind3))
		  (eq? otherk1 otherk2))
	   #t]
	  [,other #f]))]

    
    ["Testing using 'testcps-expr': an if in begin."
     (deep-assq 'if (,testcps-expr '(begin a (if b c d))))
     ,(lambda (x)
	(match x
	       [(if b (kcall ,k c) (kcall ,k_ d))
		(eq? k k_)]
	       [(if b c d) #t]
	       [,else #f]))]
    ["Testing using 'testcps-expr': an if in begin with non-identity continuation"
     (deep-assq 'lambda (,testcps-expr '(begin a (if b c d) e)))
     ,(lambda (x) (disp "FO" x)
	(match x 
	       [#f #t] ;; Did not cps it -- that's fine.
	       [(lambda (,hole) (begin ,hole_ e)) #t]
	       [,else #f]))]    
    ["Testing using 'testcps-expr': a begin in an if branch."
     (deep-assq 'if (,testcps-expr '(if a b (begin c d))))
     ,(lambda (x)
	(match x
	       ;; It should push the kcall into the begin:
	       [(if a b (begin c d)) #t]
	       [(if a (kcall ,k b) (begin c (kcall ,k_ d))) #t]
	       [,else #f]))]
    ["Testing using 'testcps-expr': same thing, testing the left branch."
     (deep-assq 'if (,testcps-expr '(if a (begin b c) d)))
     ,(lambda (x)
	(match x
	       ;; It should push the kcall into the begin:
	       [(if a (begin b c) d) #t]
	       [(if a (begin b (kcall ,k_ c)) (kcall ,k d)) #t]
	       [,else #f]))]


    ["Testing using 'testcps-expr': an if in a (+) (w SUBCALL to force cps)"
     (length (deep-all-matches
      ,(lambda (x)
	 (match x
		[(if b (kcall ,k c) (call-fast (tok tok2 0) ,k_)) #t]
		[,else #f])) 
      (cps-tokmac 
       (cleanup-token-machine 
	'(tokens (tok1 () (+ '1 (if b c (subcall tok2)))) 
		 (tok2 () (return 3)))))))
     1]
    ["Testing using 'testcps-expr': an if in begin (w SUBCALL to force cps)"
     (length 
      (deep-all-matches 
       ,(lambda (x)
	  (match x
		 [(if b (kcall ,k c) (call-fast (tok tok2 0) ,k_)) #t]
		 [,else #f]))
       (cps-tokmac 
	(cleanup-token-machine 
	 '(tokens (tok1 () (begin (if b c (subcall tok2)) d))
		  (tok2 () (return 3)))))))
     1]
    
    ["Make sure we don't lift begin clauses out in an inappropriate way."
     (let ((result (cps-tokmac 
			     (cleanup-token-machine 
			      '(tokens 
				[tok1 () (let ((y '3)) (let ((x y)) (begin (set! y '4) '99)))]
				[tok2 (x) (call (tok tok2 0) (begin '88 '99))]
				)))))
       (list (car (reverse (deep-assq 'tok1 result)))
	     (car (reverse (deep-assq 'tok2 result)))))
     ;; Some unnecessary bindings get generated, hopefully we'll optimize these away in a later pass.
     ,(lambda (p) 
	(match p 
	  [((let ([,k1 ,topk])
	      (let ([y '3])
		(let ([,k2 ,k3])
		  (let ([x y]) (begin (set! y '4) (kcall ,k4 '99))))))
	    (begin '88 (call (tok tok2 0) '99))) 
	   #t]
	  [((let ([y '3]) (let ([x y]) (begin (set! y '4) '99)))
	   (call (tok tok2 0) (begin '88 '99)))
	   #t]
	  [((let ([y '3]) (let ([x y]) (begin (set! y '4) '99)))
	    (begin '88 (call (tok tok2 0) '99)))
	   #t]
	  [,_ #f]))
     #;     
     ((let ((y '3)) (let ((x y)) (begin (set! y '4) '99)))
      (call (tok tok2 0) (begin '88 '99)))]

    ["Make sure we do extract subcalls from begin contexts appropriately."
     (match (deep-assq 'begin (cps-tokmac (cleanup-token-machine 
					   '(tokens       
						[tok1 () (begin '1 (subcall tok1) '2 (return '3))]))))
       [(begin '1 (call-fast ,',tok (lambda ,',_ ...))) #t])
     #t]

    ["Look at how we treat lets."
     (deep-assq 'let 
		(cps-tokmac 
		 (cleanup-token-machine 
		  '(tokens 
		       [SOC-start () (soc-return (subcall tok1))]
		     [tok1 () (let ((y '3)) (let ((x y)) (return (+ x y))))]))))
     , (lambda (res) 
	 (match res
	   [(let ([y ,_]) (let ([x ,__]) ,b)) #t]
	   [(let ([,k1 ,topk])
	      (let ([y '3])
		(let ([,k2 ,k3])
		  (let ([x y]) (kcall ,k4 (+ x y))))))
	    #t]
	   [,else #f]))]
    
    ["Currently subcalls in if branches make useless continuations."
     ;; Could try to optimize these away in a variety of manners:
     (cps-tokmac (cleanup-token-machine '(if #t (subcall (tok sum 0) val acc_7))))
     ;; Actually now they just make NULLK
     ,(lambda (p)
	(= 1 (length (deep-assq-all 'NULLK p))))]
;; Currently produces: [2005.10.30]
#;
(cleanup-token-machine-lang
  '(program
     (bindings)
     (nodepgm
       (tokens
         (SOC-start subtok_ind () (stored) (void))
         (node-start
           subtok_ind
           ()
           (stored)
           (let ([k_88 (lambda (HOLE_89) HOLE_89)])
             (if '#t
                 (call
                   (tok sum 0)
                   (lambda (HOLE_90) (kcall k_88 HOLE_90))
                   val
                   acc_7)
                 (kcall k_88 (void)))))))))

    
         
   )))


(define test-this (default-unit-tester
		    "27: CPS-Tokmac: use CPS on blocking calls."
		    these-tests))

(define test27 test-this)
(define tests27 these-tests)
(define test-cps-tokmac test-this)
(define tests-cps-tokmac these-tests)


;(cps-tokmac '(toheu '(program (bindings) (nodepgm (tokens (tok1 () (begin '1 (subcall tok2 '9) '3)) (tok2 (x) x))))))

'(cps-tokmac '(toheu 
	      '(program (bindings) 
			(nodepgm (tokens 
				  (tok1 () (begin '1 (subcall tok2 '9) '3)) 
				  (tok2 (x) x)
				  (tok3 () (dbg "woot %d\n" (subcall tok1)))
				  )))))

(define x 
  '(foolang
  '(program
     (bindings)
     (nodepgm
       (tokens
         (SOC-start
           subtok_ind
           ()
           (stored)
           (let ([kind_21
                  (if (token-present? (tok K_22 0))
                      (let ([new (+ '1 (ext-ref (tok K_22 0) kcounter))])
                        (begin (ext-set! (tok K_22 0) kcounter new) new))
                      (begin (call (tok K_22 0) '0) '1))])
             (begin (call (tok K_22 kind_21) '0)
                    (call (tok tok2 0) (tok K_22 kind_21) '2))))
         (K_22 subtok_ind
               (flag fv0)
               (stored (kcounter 0))
               (if (= flag '0)
                   (if (= subtok_ind '0) (void) (begin))
                   (begin (dbg '"foo %d\n" (+ '1 fv0))
                          (evict (tok K_22 subtok_ind)))))
         (tok2 subtok_ind (k_20 x) (stored) (call k_20 (* '2 x)))))))
)




;; FIXME: BUG
;; This (non-minimal) program currently does not CPS correctly.
;; The state mutation gets moved around in a broken way.
#;
(desugar-macros-lang
  '(program
     (bindings)
     (nodepgm
       (tokens
         (SOC-start subtok_ind () (stored) (gemit (tok tree 0)))
         (tree subtok_ind () (stored) (grelay (tok tree subtok_ind)))
         (node-start
           subtok_ind
           ()
           (stored)
           (timed-call 500 (tok start-election 0)))
         (start-election
           subtok_ind
           ()
           (stored)
           (begin
             (subcall (tok leaderstorage_2 0))
             (printf
               '" (Launching: ~s) \n"
               (ext-ref (tok leaderstorage_2 0) mycred_4))
             (gemit
               (tok compete_1 (my-id))
               (ext-ref (tok leaderstorage_2 0) mycred_4))
             (timed-call 1000 amiwinner_7)))
         (leaderstorage_2
           ()
           (stored
             (curleader_3 'leader-uninitialized)
             (mycred_4 'mycreds-uninitialized))
           (begin
             (set! curleader_3 (my-id))
             (let ([tmp_5 (subcall (tok fun 0))])
               (begin
                 (printf '" (INIT ~s) \n" tmp_5)
                 (set! mycred_4 tmp_5)))))
         (compete_1
           subtokid_8
           (val_9)
           (begin
             '"If storage has not been allocated, do it now."
             '"This means the message just got to a new node."
             (if (token-present? (tok leaderstorage_2 0))
                 (void)
                 (subcall (tok leaderstorage_2 0)))
             (if (> val_9 (ext-ref (tok leaderstorage_2 0) mycred_4))
                 (begin
                   '"If they beat the local leader, then the new winner is them."
                   (printf
                     '"(~a ~a) "
                     val_9
                     (ext-ref (tok leaderstorage_2 0) mycred_4))
                   (ext-set!
                     (tok leaderstorage_2 0)
                     curleader_3
                     subtokid_8)
                   (ext-set! (tok leaderstorage_2 0) mycred_4 val_9)
                   '"And since they won, we bear their flag onward:"
                   (grelay (tok compete_1 subtokid_8)))
                 (begin
                   '"If they don't change our mind about who's leading, we do nothing."
                   (printf
                     '"~a "
                     (ext-ref (tok leaderstorage_2 0) curleader_3))))))
         (amiwinner_7
           ()
           (begin
             (if (= (ext-ref (tok leaderstorage_2 0) curleader_3)
                    (my-id))
                 (begin
                   (printf
                     '"\n(winner ~a at time ~a)\n"
                     (my-id)
                     (my-clock))
                   (call (tok lead 0)))
                 (void))))
         (lead
           subtok_ind
           ()
           (stored)
           (greturn (my-id) (to (tok SOC-return-handler 0))
             (via (tok tree 0)) (seed '#f) (aggr #f)))
         (fun subtok_ind () (stored) (my-id))))))