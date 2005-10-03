
;; tml_generic_traverse.ss

;; This is used by several passes which operate on TML (Token Machine
;; Language) code.  It enables very easy traversal of TML code, for
;; when you only need to match against and change a single case, and
;; you want to avoid writing the "boilerplate" code that traverses the
;; other cases.

;; It could be used by all the passes; they all do tree-walks.  And I
;; may write more passes to use it.  But it's best for cases when you
;; only touch a small fraction of the grammar's variants.

;; WIDE GRAMMAR: Currently, this is used in multiple passes
;; (desugar-let-stored, desugare-soc-return, cps-tokmac,
;; convert-closure, but it could be more).  As such, it basically
;; supports the union of the grammars.  This hopefully will not cause
;; bugs for us, because they are distinct forms that can't be confused
;; with one another.  But watch out.

;; As we use this for more passes we'll widen the grammar further.

;; [2005.10.02] Just factored this out into this file.

;; ======================================================================

;; This is confusing, but there are so many small traversals of
;; the program tree in this pass that it is much easier to reuse this tree walk:
;; It's complex, but saves a lot of boilerplate. (See peyton jones "boilerplate" papers.)
;;
;;
;; NOTE: A common failure mode when using this is invoking the
;; wrong loop when making a recursive pattern match.  Be careful.
(define (tml-generic-traverse driver fuse e)
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
;(disp "TMLGENTRAVERSE: " driver fuse)
  (let loop ((e e))
    (driver e 
      (lambda (expression)
	(match expression
	  ;[,x (guard (begin (printf "~nGenTrav looping: ") (display-constrained (list x 50)) (newline) #f)) 3]
	  [,const (guard (constant? const)) (fuse () (lambda () const))]
	  [(quote ,const)                (fuse ()      (lambda () `(quote ,const)))]
	  [,var (guard (symbol? var))    (fuse ()      (lambda () var))]
	  [(tok ,tok)                    (fuse ()      (lambda () `(tok ,tok)))]
	  [(tok ,tok ,n) (guard (integer? n)) (fuse () (lambda () `(tok ,tok ,n)))]
	  [(tok ,tok ,[loop -> expr])    (fuse (list expr) (lambda (x) `(tok ,tok ,x)))]
	  [(ext-ref ,tok ,var)           (fuse ()      (lambda () `(ext-ref ,tok ,var)))]
	  [(ext-set! ,tok ,var ,[loop -> expr])  
	   (fuse (list expr) (lambda (x) `(ext-set! ,tok ,var ,x)))]
	  [(set! ,v ,[loop -> e])        (fuse (list e)    (lambda (x) `(set! ,v ,x)))]
	  [(leds ,what ,which)           (fuse ()      (lambda () `(leds ,what ,which)))]
	  [(begin ,[loop -> xs] ...)     (fuse xs       (lambda ls `(begin ,ls ...)))]
	  [(if ,[loop -> a] ,[loop -> b] ,[loop -> c])
	   (fuse (list a b c) (lambda (x y z) `(if ,x ,y ,z)))]
	  [(let ([,lhs ,[loop -> rhs]]) ,[loop -> bod])
	   (fuse (list rhs bod) 
		 (lambda (x y) `(let ([,lhs ,x]) ,y)))]
	  [(let-stored ([,lhs* ,[loop -> rhs*]] ...) ,[loop -> bod])
	   (fuse (append (list bod) rhs*)
		 (lambda (bod . rhs*)
		   `(let-stored ([,lhs* ,rhs*] ...) ,bod)))]
	   ;; "activate" and the gradient calls have already been desugared:
	  [(,call ,[loop -> rator] ,[loop -> rands] ...)
	   (guard (memq call '(bcast subcall call)))
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
	  [(kcall ,[loop -> k] ,[loop -> e]) (fuse (list k e) (lambda (x y) `(kcall ,x ,y)))]
	  [(build-kclosure ,kname (,fvs ...)) 
	   (fuse () (lambda () `(build-kclosure ,kname (,fvs ...))))]

	  ;; For now we just don't touch the insides of a dbg statement:
	  ;; It's treated as a non-traversable atom:
;	  [(dbg ,rand ...)  (fuse () `(dbg ,rand ...))]

	  [(,prim ,[loop -> rands] ...)
	   (guard (or (token-machine-primitive? prim)
		      (basic-primitive? prim)))
	   (fuse rands (lambda ls `(,prim ,ls ...)))]
	  [(,[loop -> rator] ,[loop -> rands] ...)
	   (warning 'closure-convert "allowing arbitrary application of rator: ~a" rator)
	   (fuse (cons rator rands) (lambda (x . ls)`(app ,x ,ls ...)))]
	  [,otherwise
	   (error 'generic-traverse
		  "bad expression: ~s" otherwise)])))))




#;    (define (tml-generic-traverse driver fuse e)
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
      (trace-let loop ((e e))
	;(disp " Loop " e)
	(driver e 
	   (trace-lambda fallthrough (x)
	     ;(disp " Fell through to cont, with" x)
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
		    [(let-stored ([,lhs* ,[loop -> rhs*]] ...) ,[loop -> bod])
		                                   (fuse (append rhs* (list bod)) 
							 `(let-stored ([,lhs* ,rhs*] ...) ,bod))]
		    ;; "activate" and the gradient calls have already been desugared:
		    [(,call ,[loop -> rator] ,[loop -> rands] ...)
		     (guard (memq call '(bcast subcall call)))
		     (fuse (cons rator rands) `(,call ,rator ,rands ...))]
		    [(timed-call ,time ,[loop -> rator] ,[loop -> rands] ...)
		     (guard (number? time))		     
		     (fuse (cons rator rands) `(timed-call ,time ,rator ,rands ...))]
		    [(return ,[loop -> x])         
		     (guard (disp "DAMN RETURN:"))
		     (fuse (list x) `(return ,x))]
		    [(,prim ,[loop -> rands] ...)
		     (guard (or (token-machine-primitive? prim)
				(basic-primitive? prim))
			    (disp "prim" prim)
			    )
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


(define these-tests
  `(	     
    ["Do a little verification of generic-traverse, check datatypes"
     (call/cc (lambda (esc)
		(tml-generic-traverse
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
    ))

(define test-this (default-unit-tester
		    "TML Generic-Traverse: abstracts tree-walks over TML code."
		    these-tests))
