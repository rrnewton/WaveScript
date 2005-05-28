;; [2005.02.10]

;; cps-tokmac
;; This pass does the CPS transformation for the token machine.

;; Introduces lambda's and kcall.


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

(define cps-tokmac
  (let ()

    (define (id x) x)

    (define (tb->body tb)
      (mvlet ([(tok subid args stored constbinds body) (destructure-tokbind tb)])
	     body))

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
		    [(return ,[loop -> x])         (fuse (list x) `(return ,x))]
		    [(,prim ,[loop -> rands] ...)
		     (guard (or (token-machine-primitive? prim)
				(basic-primitive? prim)))
		     (fuse rands `(,prim ,rands ...))]

		    ;; Allowing lambda's.  
		    ;; These aren't in the input language but are in the output language.
		    [(lambda (,vars ...) ,[loop -> expr])
		     (fuse (list expr) `(lambda ,vars ,expr))]

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
	  (mvlet ([(tok subid args stored constbinds body) (destructure-tokbind tb)])
	    (let* ([k (unique-name 'k)]	    
		   [desugar-return 
		    (lambda (expr)
		      (generic-traverse
		       (lambda (expr loop) ;; driver
			 (match expr
				[(return ,[x]) `(kcall ,k ,x)]
				[(set! ,v ,[e])
				 `(begin `(set! ,v ,e) (call ,k (void)))]
				[,x (loop x)]))
		       (lambda (_ exp) exp) ;; fuser
		       expr))])
	      `[,tok ,subid (,k ,@args) (stored ,@stored)
		     ,(desugar-return body)]))))
		           
    ;; expand-subcalls expands out all subcalls 
    ;; Best way to represent a thing with a hole in it is a continuation:
    ;; Thus this pass itself carries a continuation representing the context of the current expression.
    (define expand-subcalls
      (lambda (expr)
      ;; Continuation code must reside in new handlers:
;      (let ([new-handlers '()]) ;; MUTABLE
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

	     ;; Alright, let's think through conditionals:
	     
	     ;; What should this generate?
	     ;; (if (subcall a) (subcall b) (subcall c))	     
	     ;; -> ... (A K1)
	     ;; (def K1 (x) (if x (b K2) (c K2)))

	     ;; (if a (subcall b) (subcall c))
	     ;; -> ... (if a (b K1) (c K1))
	     
	     ;; THIS REQUIRES NAMING THE CONTEXT: (or duplicating the code)
	     ;; (if a b (subcall c))
	     ;; -> ... (if a (K1 b) (c K1))	     
	     ;;
	     ;; Can we *detect* when this happens?  We don't want to
	     ;; always cps convert conditionals...
	     ;; In our case it seems difficult to recover through optimization.
              [(if ,test ,conseq ,altern)
	       (loop test
		     (lambda (t)
		       (let* ([k (unique-name 'k)]
			      [v (unique-name 'HOLE)])		
			 `(let ([,k (lambda (,v) ,(pvk v))])
			    (if ,t
				,(loop conseq (lambda (c) `(kcall ,k ,c)))
				,(loop altern (lambda (a) `(kcall ,k ,a))))))))]

	     [(let ((,lhs ,rhs)) ,body)
	      (loop rhs
		 (lambda (r)
		   (loop body
			 (lambda (b)
			   (pvk `(let ([,lhs ,r]) ,b))))))]

	     
	     [(subcall ,tok ,args ...)
	      (let* ([valvar (unique-name 'HOLE)]
		     ;; This expression represents the continuation of the subcall:
		     [broken-off-code (pvk valvar)] ;(pvk RETVAL_VARNAME)]
		     )		
		(loop-list args
			   (lambda (ls)
			     `(call ,tok (lambda (,valvar) ,broken-off-code) ,@ls))))]


	     ;; These can be treated as normal primitives:
	     [(,call ,tok ,args* ...)
	      (guard (memq call '(call bcast)))
	      (loop-list args* 
			 (lambda (ls) (pvk `(,call ,tok ,@ls))))]
	     [(timed-call ,time ,tok ,args* ...)
	      (loop-list args* 
			 (lambda (ls) (pvk `(timed-call ,time ,tok ,@ls))))]


	     [(kcall ,t ,v)
	      (loop t
		    (lambda (t)
		      (loop v
			    (lambda (v)
			      `(kcall ,t ,v)))))]


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
	      (error 'cps-tokmac:expand-subcalls
		     "bad expression: ~s" otherwise)]
	     )))
		
	;; expand-subcalls returns an expression and new handlers
	  (let ((newexpr (loop expr (lambda (x) x))))
	    newexpr)))
;	    (values newexpr 
;		    new-handlers)))))


    ;; Applies expand-subcalls to all tokbinds
      (define process-tokbinds
	(lambda (tokenbinds)
	  (let loop ([tbs tokenbinds] [tbacc '()])
	    (if (null? tbs)
		(reverse! tbacc)
		(mvlet ([(tok subid args stored constbinds body) (destructure-tokbind (car tbs))])
		  (if (not (null? constbinds)) (error 'cps-tokmac "Not expecting local constbinds!"))
;		  (mvlet ([(newexpr newtokbinds) (expand-subcalls body)])
		  (let ([newexpr (expand-subcalls body)])
			 (loop (cdr tbs)
;				(append newtokbinds 
					(cons 
					 `(,tok ,subid ,args (stored ,@stored) ,newexpr)
					 tbacc))))))))

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
				 (if (= subtokind '0) (void) (begin))
				 (begin (begin fv0 '3) (evict (tok K_5 0)))))
	      ()]
	     [(,toks-escaped '(if (= flag '0)
				 (if (= subtokind '0) (void) (begin))
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
	      (parameterize ((unique-name-counter 0))
	      (let ([expr (,expand-subcalls '(begin '1 (subcall (tok t 0) '2) '3))])
		(let ([lam (deep-assq 'lambda expr)])
		  (list 
		   (,toks-referenced expr)
		   (,toks-escaped expr)
		   (,free-vars lam)
		   (,free-vars expr)
		   ))))
	      ((t) () (HOLE_1) (HOLE_1))]


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
				     (set-equal? (list->set (,free-vars body)) '(flag subtokind fv0))
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
		       (if (not (valid-returns? (tb->body x)))
			   (error 'cps-tokmac 
				  "token handler(s) does not have a 'return' statement on all code paths: ~a"
				  x)))
		     returntokbinds)
             (let ([subonly (difference subcalledtoks returntoks)]
		   [retonly (difference returntoks subcalledtoks)])
	       (disp "Results, sub&ret: " (intersection subcalledtoks returntoks)  subonly retonly)
	       
	     (if (not (null? subonly))
		 (for-each 
		  (lambda (x)
		    (if (memq x alltoks)
			(error 'cps-tokmac
				 "Token handlers for ~a are subcalled but do not have return statements!"
				 x)
			(warning 'cps-tokmac
				 "Subcall to nonexistent token handler!: ~a" x)))
		  subonly))
	     (if (not (null? retonly))
		 (warning 'cps-tokmac
			  "Token handlers for ~a have return statements but there are no subcalls to them!"
			  retonly)))
	   ;; Now for the real work.
	   ;; First we add k-arguments to any tainted tokens:
	   (let ([newtbs (map kify-tokbind returntokbinds)]
		 [oldtbs (filter (lambda (tb) (not (memq (car tb) returntoks))) nodetoks)])
	     ;; Next we process all tokbinds to expand out subcalls:
	     `(,lang '(program (bindings ,constbinds ...)
			  (nodepgm (tokens ,@(process-tokbinds (append oldtbs newtbs))))))
	     )))]))))


(define (testcps-expr e)
  (match e 
	 [(tokens ,tbs ...)
	  (cps-tokmac `(foolang '(program (bindings) (nodepgm (tokens ,@tbs)))))]
	 [,expr
	  (cps-tokmac `(foolang '(program (bindings) (nodepgm (tokens (toknought () ,expr))))))]))

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
     (parameterize ((unique-name-counter 0))
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
		(call (tok tok2 0) (lambda (HOLE_2) (+ '1 HOLE_2)) '2))
	  (tok2 subtok_ind (k_1 x) (stored) (kcall k_1 (* '2 x)))))))]
     
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
		(,k7 subtokind
		     (flag fv0)
		     (stored (kcounter 0))
		     (if (= flag '0)
			 (if (= subtokind '0) (void) (begin))
			 (begin (+ '1 fv0) (evict (tok ,k8 subtokind)))))
		(tok2 subtok_ind (,otherk1 x) (stored) (call ,otherk2 (* '2 x)))))))
	   (guard (andmap (lambda (x) (eq? x k1)) (list k2 k3 k4 k5 k6 k7 k8))
		  (andmap (lambda (x) (eq? x kind1)) (list kind2 kind3))
		  (eq? otherk1 otherk2))
	   #t]
	  [,other #f]))]

    
    ["Testing using 'testcps-expr': an if in begin."
     (,testcps-expr '(begin a (if b c d)))
     ,(lambda (x)
	(match (deep-assq 'if x)
	       [(if b (kcall ,k c) (kcall ,k_ d))
		(eq? k k_)]
	       [,else #f]))]

    ["Testing using 'testcps-expr': an if in begin with non-identity continuation"
     (,testcps-expr '(begin a (if b c d) e))
     ,(lambda (x)
	(match (deep-assq 'lambda x)
	       [(lambda (,hole) (begin ,hole_ e)) #t]
	       [,else #f]))]
    
    ["Testing using 'testcps-expr': a begin in an if branch."
     (,testcps-expr '(if a b (begin c d)))

     ,(lambda (x)
	(match (deep-assq 'if x)
	       ;; It should push the kcall into the begin:
	       [(if a (kcall ,k b) (begin c (kcall ,k_ d))) #t]
	       [,else #f]))]    
    ["Testing using 'testcps-expr': same thing, testing the left branch."
     (,testcps-expr '(if a (begin b c) d))
     ,(lambda (x)
	(match (deep-assq 'if x)
	       ;; It should push the kcall into the begin:
	       [(if a (begin b (kcall ,k_ c)) (kcall ,k d)) #t]
	       [,else #f]))]

          
    )
   ))


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
         (K_22 subtokind
               (flag fv0)
               (stored (kcounter 0))
               (if (= flag '0)
                   (if (= subtokind '0) (void) (begin))
                   (begin (dbg '"foo %d\n" (+ '1 fv0))
                          (evict (tok K_22 subtokind)))))
         (tok2 subtok_ind (k_20 x) (stored) (call k_20 (* '2 x)))))))
)
