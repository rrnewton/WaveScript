


(module desugar-pattern-matching mzscheme
  (require )
  (provide desugar-pattern-matching
	   test_desugar-patterns)  
  (chezimports)

;; Desugar pattern matching within lambda's, letrecs, and "match" statements. <br>
;; TODO: This should really not go in the source_loader.
(define desugar-pattern-matching 
  (let ([break-pattern 
	 (lambda (pat)
	   (match pat
	     [,s (guard (symbol? s)) 
		 (values s '())]
	     [#(,[pv* binds*] ...)
	      (let ([v (unique-name 'pattmp)]
		    [len (length pv*)])
		(let ([newbinds 
		       `([,pv* (tupref ,(iota len) ,(make-list len len) ,(make-list len v))] ...)])		  
		  (values v
			  `( ,@(filter (lambda (b) (not (eq? (car b) '_))) newbinds)
			     ,@(apply append binds*)
			     ))))]
	   ))])
  (lambda (expr)
  (let loop ([expr expr] [env '()] [subst '()])
  (match expr 
    [,c (guard (constant? c)) c]
    [(quote ,d) `(quote ,d)]
    [,var (guard (symbol? var))
	  (let ((entry (assq var subst)))
	    (if entry (cadr entry) var))]          
    [(if ,[test] ,[conseq] ,[altern])
     (guard (not (memq 'if env)))
     `(if ,test ,conseq ,altern)]
    ;; Pre type checking!!
    [(lambda (,[break-pattern -> formal* binds* ] ...) ,expr)
     (guard (not (memq 'lambda env)))     
     (let ([bod (loop expr
		 (append formal* env)
		 ;(append (apply append subst*) subst)
		 subst
		 )]
	   [binds (apply append binds*)])
       (if (null? binds)
	   `(lambda (,formal* ...)  ,bod)
	   `(lambda (,formal* ...)  (letrec (,binds ...) ,bod))))]
    [(letrec ((,[break-pattern -> lhs* binds*] ,[rhs*]) ...) ,[bod])
     (guard (not (memq 'letrec env)))
     
     `(letrec ( [,lhs* ,rhs*] ...
		,@(apply append binds*)
	       )
	,bod)]
    ;; Only handles one-armed matches right now:
;    [(match ,[x] [ ,[break-pattern -> var* binds*]  ,[rhs*] ] ...)
    [(match ,[x] (,[break-pattern -> var binds] ,[rhs]))
     `(letrec ([,var ,x]
	       ,binds ...)
	  ,rhs)]    

    ;; This is extremely dangerous... just looping down all constructs...
    ;; This means for-loops, etc, are just treated as applications.
    [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
    )))))


; ================================================================================

(define these-tests
  `(["Run a basic test of the pattern match expander."
     (reunique-names (,desugar-pattern-matching '(lambda (#(foo #(bar baz)) x) foo)))
     (lambda (pattmp x)
       (letrec ([foo (tupref 0 2 pattmp)]
		[pattmp_1 (tupref 1 2 pattmp)]
		[bar (tupref 0 2 pattmp_1)]
		[baz (tupref 1 2 pattmp_1)])
	 foo))]

    [(,desugar-pattern-matching '(match 3 [x x]))
     (letrec ([x 3]) x)]

    ))

(define test-this (default-unit-tester "source_loader.ss: For reading regiment source files." these-tests))
(define test_desugar-patterns test-this)

)
