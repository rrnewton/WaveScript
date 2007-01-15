
;;;; .title Generic Tree Walker for TML (tml_generic_traverse.ss)

;;;; This is used by several passes which operate on TML (Token Machine
;;;; Language) code.  It enables very easy traversal of TML code, for
;;;; when you only need to match against and change a single case, and
;;;; you want to avoid writing the "boilerplate" code that traverses the
;;;; other cases.
;;;; <br><br>

;;;; It could be used by all the passes; they all do tree-walks.  And I
;;;; may write more passes to use it.  But it's best for cases when you
;;;; only touch a small fraction of the grammar's variants.
;;;; <br><br>

;;;; WIDE GRAMMAR: Currently, this is used in multiple passes
;;;; (desugar-let-stored, desugare-soc-return, cps-tokmac,
;;;; convert-closure, but it could be more).  As such, it basically
;;;; supports the union of the grammars.  This hopefully will not cause
;;;; bugs for us, because they are distinct forms that can't be confused
;;;; with one another.  But watch out.
;;;; <br><br>

;;;; As we use this for more passes we'll widen the grammar further.
;;;; <br><br>

;;;; [2005.10.02] Just factored this out into this file. <br>

;;;; [2006.01.12] Adding support for dynamically targetted ext-ref and ext-set! <br>

; =======================================================================


(module tml_generic_traverse  mzscheme  
  (require (lib "include.ss")  
           "../generic/constants.ss"
           "../plt/iu-match.ss"
	   "../plt/prim_defs.ss"
	   ;(lib "compat.ss")
           (all-except "../util/helpers.ss" test-this these-tests filter)
           (all-except "../plt/regiment_helpers.ss" test-this these-tests filter)
           )
  
  ;; Insure provision of verify-regiment:
  (provide tml-generic-traverse
           tml-simple-pass
           tml-free-vars
	   tml-potential-escaped-tokens
           test-this test-generic-traverse)

  (chezimports prim_defs
	       (except helpers test-this these-tests)
	       ;regiment_helpers
	       )

;; This is confusing, but there are so many small traversals of
;; the program tree that it is much easier to reuse this tree walk:
;; It's complex, but saves a lot of boilerplate. (See peyton jones "boilerplate" papers.)
;;
;;
;; NOTE: A common failure mode when using this is invoking the
;; wrong loop when making a recursive pattern match.  Be careful. <br><br>
(define tml-generic-traverse 
  ;; .param driver driver
  ;; .param fuser  fuser
  ;; .param expr   expression
  ;;
  ;; The "driver" takes the first shot at an expression, transforms the
  ;; subcases that it wants to, and then hands the rest on to its
  ;; continuation to do the automated traversal. The automated
  ;; traversal, in turn, uses the "fuse" function to glue back together
  ;; the parts of the tree.  The fuse function is passed a list of child
  ;; exprss and another continuation representing the "default fuser" which
  ;; just puts the expression back together like it was before (given the child terms).
  ;;       <br><br>
  ;; Types:    <br>
  ;;   driver : expr, (expr -> 'intermediate) -> 'result)                <br>
  ;;   fuse : 'intermediate list, (expr list -> expr) -> 'intermediate)  <br>
  ;;   e : expr                                                          <br>
  ;; Return value: 'result                                               <br>
  (let ()
  (define (build-traverser driver fuse e)
  (let loop ((e e))
    (define (tokonly x)
      (match x
	     [(tok ,_ ,__) (loop x)]
 	     [,_ (error tml-generic-traverse 
			"This expression was expected to be a (tok <t> <e>) form: ~s" x)]))
    (driver e 
      (lambda (expression)
	(define dotok 
	  (lambda (tok fun)
	      (match tok
		[(tok ,t ,n) (guard (integer? n))
		 (fuse () (lambda () (fun `(tok ,t ,n))))]
		[(tok ,t ,[loop -> e])
		 (fuse (list e) (lambda (n) (fun `(tok ,t ,n))))]
		[,e (error 'foo "~a" e)])))
	
	(match  expression
	  ;[,x (guard (begin (printf "~nGenTrav looping: ") (display-constrained (list x 50)) (newline) #f)) 3]

	  [,const (guard (constant? const)) (fuse () (lambda () const))]

          ;; This is for debugging, we just don't touch it:
	  [(BLACKBOX ,expr ...)             (fuse () (lambda () `(BLACKBOX ,expr ...)))]

	  ;; We don't put any restrictions on what can be in a quoted constant:
	  [(quote ,const)                (fuse ()      (lambda () `(quote ,const)))]
	  [,var (guard (symbol? var))    (fuse ()      (lambda () var))]
	  [(tok ,tok)                    (fuse ()      (lambda () `(tok ,tok)))]
	  [(tok ,tok ,n) (guard (integer? n)) (fuse () (lambda () `(tok ,tok ,n)))]
	  [(tok ,tok ,[loop -> expr])    (fuse (list expr) (lambda (x) `(tok ,tok ,x)))]

	  ;; [2005.10.31] May decide to not restrict these to token expressions only in the future:
	  ;; [2006.01.12] Unrestricting now:
	  [(ext-ref ,[loop -> tok] ,var)      
	   (fuse (list tok) (lambda (tok) `(ext-ref ,tok ,var)))]
	  [(ext-set! ,[loop -> tok] ,var ,[loop -> expr])
	   (fuse (list tok expr) (lambda (t x) `(ext-set! ,t ,var ,x)))]

	  [(set! ,v ,[loop -> e])        (fuse (list e)    (lambda (x) `(set! ,v ,x)))]
	  [(leds ,what ,which)           (fuse ()      (lambda () `(leds ,what ,which)))]

	  ;; Always run make-begin, hope this is safe:
	  [(begin ,[loop -> xs] ...)     (fuse xs       (lambda ls (make-begin `(begin ,ls ...))))]
	  [(if ,[loop -> a] ,[loop -> b] ,[loop -> c])
	   (fuse (list a b c) (lambda (x y z) `(if ,x ,y ,z)))]

	  [(let ([,lhs ,[loop -> rhs]]) ,[loop -> bod])
	   (fuse (list rhs bod) 
		 (lambda (x y) `(let ([,lhs ,x]) ,y)))]
	  ;; [2005.11.09] SOME passes allow this as a shorthand, but not all.
	  [(let* ([,lhs* ,[loop -> rhs*]] ...) ,[loop -> bod])
	   (fuse (cons bod rhs*)
		 (lambda (x . y*) `(let ([,lhs* ,y*] ...) ,x)))]
	  [(let-stored ([,lhs* ,[loop -> rhs*]] ...) ,[loop -> bod])
	   (fuse (append (list bod) rhs*)
		 (lambda (bod . rhs*)
		   `(let-stored ([,lhs* ,rhs*] ...) ,bod)))]

	  
	  ;; Handling gradient forms as well.

;	  [(gdist (tok ,t ,n)) (guard (integer? n)) 
;	  [(gdist (tok ,t ,[loop -> ind]))         
	  ;; Gradient forms are a bit annoying to handle properly:
	  [(gdist ,t)   (dotok t (lambda (x) `(gdist ,x)))]
	  [(gparent ,t)   (dotok t (lambda (x) `(gparent ,x)))]
	  [(gorigin ,t)   (dotok t (lambda (x) `(gorigin ,x)))]
	  [(ghopcount ,t) (dotok t (lambda (x) `(ghopcount ,x)))]
	  [(gversion ,t)  (dotok t (lambda (x) `(gversion ,x)))]

	  ;; [2005.10.31] Simplifying this by simply exposing the token expression
	  ;; as a valid EXPR visible to this generic traversal.  Before I was hiding it.
	  [(gemit ,[tokonly -> t] ,[loop -> rands] ...)
	   (fuse (cons t rands) (lambda (tok . rands) `(gemit ,tok ,@rands)))]
	  [(grelay ,[tokonly -> t] ,[loop -> rands] ...)
	   (fuse (cons t rands) (lambda (t . rands) `(grelay ,t ,@rands)))]
	  [(grelay ,other ...)
	   (error 'tml-generic-traverse
		  "bad grelay form: ~s" `(grelay ,other ...))]

	  [(greturn ,[loop -> e]
		    (to ,[tokonly -> t1])
		    (via ,[tokonly -> t2])
		    (seed ,[loop -> e3])
		    (aggr ,aggr))
	   (define (f e t1 t2 e3 aggr)
	     `(greturn ,e 
		       (to ,t1) 
		       (via ,t2) 
		       (seed ,e3)
		       (aggr ,aggr)))
	   (if (not aggr)
	       (fuse (list e t1 t2 e3)      (lambda (a b c d)   (f a b c d #f)))
	       (fuse (list e t1 t2 e3 (tokonly aggr)) (lambda (a b c d e) (f a b c d e))))]

	  [(,call ,[loop -> rator] ,[loop -> rands] ...)
	   (guard (memq call '(bcast subcall direct-subcall call activate call-fast)))
	   (fuse (cons rator rands) (lambda (x . ls) `(,call ,x ,ls ...)))]


	  [(timed-call ,time ,[loop -> rator] ,[loop -> rands] ...)
	   (guard (number? time))		     
	   (fuse (cons rator rands) (lambda (x . ls) `(timed-call ,time ,x ,ls ...)))]
	  ;; Return is a prim, this is unnecessary:
;	  [(return ,[loop -> e])         (fuse (list e) (lambda (x) `(return ,x)))]
	  [(app ,[loop -> rator] ,[loop -> rands] ...)
	   (fuse (cons rator rands) (lambda (x . ls)`(app ,x ,ls ...)))]

	  ;; For the convert-closure pass, processing the output of cps-tokmac
	  [(lambda (,v) ,[loop -> e])    (fuse (list e) (lambda (x) `(lambda (,v) ,x)))]
	  [(kclosure ,fvs ,v ,[loop -> e]) (fuse (list e) (lambda (x) `(kclosure ,fvs ,v ,x)))]
	  [(kcall ,[loop -> k] ,[loop -> e]) (fuse (list k e) (lambda (x y) `(kcall ,x ,y)))]
	  [(build-kclosure ,kname (,fvs ...)) 
	   (fuse () (lambda () `(build-kclosure ,kname (,fvs ...))))]

	  
	  [(dbg ,[loop -> rand*] ...)  (fuse rand* (lambda rand* `(dbg ,rand* ...)))]

	  [(,prim ,[loop -> rands] ...)
	   (guard (or (token-machine-primitive? prim)
		      (basic-primitive? prim)))
	   (fuse rands (lambda ls `(,prim ,ls ...)))]
	  [(,[loop -> rator] ,[loop -> rands] ...)
	   (warning 'tml-generic-traverse "allowing arbitrary application of rator: ~a" rator)
	   (fuse (cons rator rands) (lambda (x . ls)`(app ,x ,ls ...)))]
	  [,otherwise
	   (error 'tml-generic-traverse
		  "bad expression: ~s" otherwise)])))))

  ;; Main body of tml-generic-traverse:
  (case-lambda 
    [(d f e) (build-traverser d f e)]
    [(d f) (lambda (e) (build-traverser d f e))])))


;; This construct allows us to build simple passes by defining only
;; the process-expr function.  It just encapsulates some boilerplate.
;; .form (tml-simple-pass <process-expr>)
;; .form (tml-simple-pass <process-expr> <process-tokbind>)
;; .form (tml-simple-pass <process-expr> <process-tokbind> <output-lang-name>)
(define tml-simple-pass
  (let (;; Default proces token binding:
	[def-ptb (lambda (pe)
		   (lambda (tb)
		     (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
		       `[,tok ,id ,args (stored ,@stored )
			      ,(pe ;(map car constbinds) 
				   body)])))]
	;; Default process program:
	[def-pp (lambda (ptb newlang)
		  (lambda (prog)
		    (match prog
		      [(,lang '(program (bindings ,constbinds ...)
				 (nodepgm (tokens ,[ptb -> toks] ...))))
		       `(,(if newlang newlang lang)
			 '(program (bindings ,constbinds ...)
			    (nodepgm (tokens ,toks ...))))])))])
    (case-lambda
      [(pe) (def-pp (def-ptb pe) #f)]
      [(pe ptb) (def-pp ptb #f)]
      [(pe ptb newlang) (def-pp ptb newlang)]
      )))


;; This is a generic free-vars which can be shared by different passes that have non-conflicting grammars.
(define (tml-free-vars e)
  (tml-generic-traverse
   ;; driver, fuser, expression
   (lambda  (x loop) 
     (match x
       [,v (guard (symbol? v)) (list v)]
       [(let ([,lhs ,[rhs]]) ,[bod])
	(append rhs (remq lhs bod))
	]
       [(lambda (,lhs* ...) ,[bod])
	(difference bod ;(list->set (apply append bod*)) 
		    lhs*)]
       [(let-stored ((,lhs ,[rhs])) ,[bod])
	(append rhs (remq lhs bod))]

       [,x (loop x)]))
   (lambda (ls _) (apply append ls))
   e))


;; Returns those tokens referenced in (tok _ _) expressions which
;; might escape.  That is, token "values" that are not immediately
;; consumed by primitives.
;; This is a conservative estimate.  It could get much better with some dataflow analysis.
(define tml-potential-escaped-tokens
  (let ()
    (define (toks-escaped e)
      (let ([allowed1
	     (lambda (loop)
	       (lambda (x)
		 (match x 
			[(tok ,t ,n) (guard (integer? x)) '()]
			[(tok ,t ,[loop -> e]) e]
			[,x (loop x)])))])
	(tml-generic-traverse
	 ;; driver, fuser, expression
	 (lambda  (x loop)
	   (define allowed (allowed1 loop))


	   ;; CERTAIN primitives that take tokens as input are allowed (non-escaping).
	   ;; Not all should be allowed, for example, if we had
	   ;; cons, cons would not be allowed.  
	   ;; 
	   ;; token->subtokid IS allowed currently, but this is debatable.	   
	   (define allowed-prims
	     '(elect-leader 
	       flood
	       call call-fast timed-call subcall direct-subcall bcast ucast ucast-wack
	       token-scheduled? token-deschedule token-present? evict evict-all
	       token->subid))

	   (define disallowed-prims
	     '(
	       ; token->subid
	       ))


	   ;; Handles (allowed, non-escaping) primitives that take token args.
	   (define do-primargs-w-tokens
	     (lambda (prim args)
	       (map-prim-w-types 
		(lambda (arg type)
		  (match (cons type arg)
		    [(Token . (tok ,tok ,e)) (allowed e)]
		    [(,other . ,e)           (loop e)]))
		prim args)))
	   
	   (match x
;		  [,x (guard (begin (printf "~ntoks-escaped looping: ") (display-constrained (list x 50)) (newline) #f)) 3]

	          ;; Refs and sets are not escapes, because the user can't store the token itself.
		  [(ext-ref ,[allowed -> t] ,v) t]
		  [(ext-set! ,[allowed -> t] ,v ,[e]) (append t e)]

		  ;; Calls are certainly allowed.
		  [(,call ,[allowed -> rator] ,[rands] ...)
		   (guard (memq call '(bcast call call-fast)))
		   (apply append rator rands)]
		  [(timed-call ,t ,[allowed -> rator] ,[rands] ...)
		   (guard (number? t))
		   (apply append rator rands)]

		  ;; Non-escaped token-accepting prims.
		  [(,prim ,args* ...)
		   (guard (and (memq prim allowed-prims)
			       (token-machine-primitive? prim)
			       ;(basic-primitive? prim)
			       ))		  
		   (apply append (do-primargs-w-tokens prim args*))]

		  ;; Escaped token-accepting prims.
		  [(,prim ,[args*] ...)
		   (guard (and (memq prim disallowed-prims)
			       (token-machine-primitive? prim)))
		   (apply append args*)]

		  ;; Non-(directly)-token processing primitives: always count as escapes.
		  [(,prim ,[args*] ...)
		   (guard (token-machine-primitive? prim))
		   (if (memq 'Token (get-primitive-entry prim))
		       (error 'tml-potential-escaped-tokens
			      "This token-accepting primitive was not classified as escaping or non-escaping: ~a"
			      prim))
		   (apply append args*)]

		  ;; Now anything left are token escapes!!
		  [(tok ,t ,n) (guard (integer? n)) (list t)]
		  [(tok ,t ,[e]) (cons t e)]

		  [,x (loop x)]))
       (lambda (ls _) (apply append ls))
       e)))
    ;; Main procedure body:
    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...)
		   (nodepgm (tokens ,nodetoks ...))))
	 (let ([escapes (map (lambda (tb)
			       (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
				 (toks-escaped body)))
			  nodetoks)])
	   (apply append escapes))]))))

; ================================================================================

(define these-tests
  `(	     
    ["Do a little verification of generic-traverse, check datatypes"
     (call/cc (lambda (esc)
		(,tml-generic-traverse
		 (lambda (x loop)
		   (or (procedure? loop) (esc `(driver-non-procedure-k)))
		   (cond
		    [(and (list? x) (eq? (car x) 'return)) #t]
		    [else (loop x)]))
		 (lambda (ls k) (or (procedure? k) (esc `(fuser-non-procedure-k ,k)))
			 (ormap (lambda (x) x) ls))
		 '(+ '1 (subcall (tok tok2 0) '2)))
		#f))
     #f]

    ["Test the gradient forms a bit because they're confusing:"
     (,sort <
     (,tml-generic-traverse
      (lambda (x k) 
	(if (and (list? x) (= 2 (length x)) (eq? 'quote (car x)))
	    (list (cadr x))
	    (k x)))
      (lambda (ls k) (filter number? (apply append ls)))
      '(begin
	 (grelay (tok t 1))
	 (grelay (tok t '2))
	 (gdist (tok t 3))
	 (gdist (tok t '4))
	 (gemit (tok t '5) '6 '7)
	 (gemit (tok t 8))
	 (gemit (tok t 9))
	 (gemit (tok t '10))
	 (greturn (void) 
		  (to (tok t '11))
		  (via (tok t '12))
		  (seed (void))
		  (aggr #f))
	 (greturn (void) 
		  (to (tok t 13))
		  (via (tok t 14))
		  (seed (void))
		  (aggr (tok t 15)))
	 (greturn '16 (to (tok t 17))
		  (via (tok t '18))
		  (seed (void))
		  (aggr (tok t 19)))
	 )))
     (2 4 5 6 7 10 11 12 16 18)]
	

    ))

(define test-this (default-unit-tester
		    "TML Generic-Traverse: abstracts tree-walks over TML code."
		    these-tests))

(define test-generic-traverse test-this)

) ;; End module


