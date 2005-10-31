
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
  (let loop ((e e))
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
	(match expression
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
	  [(ext-ref ,tok ,var)           (fuse ()      (lambda () `(ext-ref ,tok ,var)))]
	  [(ext-set! ,tok ,var ,[loop -> expr])  
	   (fuse (list expr) (lambda (x) `(ext-set! ,tok ,var ,x)))]
	  [(set! ,v ,[loop -> e])        (fuse (list e)    (lambda (x) `(set! ,v ,x)))]
	  [(leds ,what ,which)           (fuse ()      (lambda () `(leds ,what ,which)))]

	  ;; Always run make-begin, hope this is safe:
	  [(begin ,[loop -> xs] ...)     (fuse xs       (lambda ls (make-begin `(begin ,ls ...))))]
	  [(if ,[loop -> a] ,[loop -> b] ,[loop -> c])
	   (fuse (list a b c) (lambda (x y z) `(if ,x ,y ,z)))]
	  [(let ([,lhs ,[loop -> rhs]]) ,[loop -> bod])
	   (fuse (list rhs bod) 
		 (lambda (x y) `(let ([,lhs ,x]) ,y)))]
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

	  [(gemit (tok ,t ,n) ,[loop -> rands] ...) (guard (integer? n))
	   (fuse rands (lambda rands `(gemit (tok ,t ,n) ,@rands)))]
	  [(gemit (tok ,t ,[loop -> ind]) ,[loop -> rands] ...)
	   (fuse (cons ind rands) (lambda (n . rands) `(gemit (tok ,t ,n) ,@rands)))]
	  [(grelay (tok ,t ,n)) (guard (integer? n)) (fuse () (lambda () `(grelay (tok ,t ,n))))]
	  [(grelay (tok ,t ,[loop -> ind]))          (fuse `(,ind) (lambda (n) `(grelay (tok ,t ,n))))]
	  [(greturn ,[loop -> e]
		    (to (tok ,t1 ,e1))
		    (via (tok ,t2 ,e2))
		    (seed ,[loop -> e3])
		    (aggr ,aggr))
	   (define (f e e1 e2 e3)
	     `(greturn ,e 
		       (to (tok ,t1 ,e1)) 
		       (via (tok ,t2 ,e2)) 
		       (seed ,e3) 
		       (aggr ,aggr)))
	   ;; This is more than a little annoying:
	   (cond
	    [(and (integer? e1) (integer? e2))
	     (fuse (list e e3) (lambda ls (match ls [(,e ,e3) (f e e1 e2 e3)])))]
	    [(integer? e1)
	     (fuse (list e e2 e3) (lambda ls (match ls [(,e ,e2 ,e3) (f e e1 e2 e3)])))]
	    [(integer? e2)
	     (fuse (list e e1 e3) (lambda ls (match ls [(,e ,e1 ,e3) (f e e1 e2 e3)])))]
	    [else
	     (fuse (list e e1 e2 e3) (lambda ls (match ls [(,e ,e1 ,e2 ,e3) (f e e1 e2 e3)])))])]
	  [(greturn ,xs ...)
	   (error 'tml-generic-traverse
		  "bad gradient return statement: ~a" `(greturn ,@xs))]



	  [(,call ,[loop -> rator] ,[loop -> rands] ...)
	   (guard (memq call '(bcast subcall call activate)))
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

    ["Test the gradient forms a bit because they're confusing:"
     (sort <
     (tml-generic-traverse
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