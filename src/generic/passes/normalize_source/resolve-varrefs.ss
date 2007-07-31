;;;; Note: as an additional little hack, we use this opportunity to
;;;; expand "ref" to "Mutable:ref".  But we do it hygenically,
;;;; avoiding capture.

; I wonder how inefficent this "fluid-let" method is.

(module resolve-varrefs mzscheme
  (require "../../../plt/common.ss")
  (provide resolve-varrefs 
	   resolve-varrefs-grammar
           ;test-this these-tests test01 tests01
           test-resolve-varrefs
	   )
  (chezimports)

  ;; This is a bit of a hack... really should split resolve-varrefs into
  ;; two separate passes for the two places it's used.
  (define resolve-varrefs-grammar 
    ;; Kill the using construct, wait it's only in the sugared grammar:
    ;; Wait, we still need user type constructors, those haven't been desugared:    
    (snoc 
     `[Type ,type?]
     ;`[Type (,(lambda (x) (and (symbol? x) (not (memq x '(NUM quote ->))))) Type ...)]
     (filter (lambda (x) (not (eq? 'Type (car x))))
       initial_regiment_grammar)))

  (define (explode-id id) 
    (map string->symbol
      (string-split (symbol->string id) #\:)))
  (define (contract-id id)
    (string->symbol
     (apply string-append
	    (insert-between 
	     ":" (map symbol->string id)))))
  
  ;; 'using' constructs may be nested.  This brings up some interesting scenarios:
  ;; namespace Foo {
  ;;   namespace Foo {
  ;;     x = 3;
  ;;   }
  ;; }

  ;; { using Foo; using Bar; ... }
  ;; This enables using bindings in *either* namespace, right?

  ;; { using Foo; using Foo; ... }
  ;; But this gives you access to the nested namespace????

  ;; Or should that be this?
  ;; { using Foo; using Foo:Foo; ... }
  
  ;; I think probably the latter.  But with the implementation I'm
  ;; going to use it could be either.


  ;; This is not reentrant for two reasons: the var/exploded-table
  ;; state, and the fact that it uses the unique name counter.
  (define-pass resolve-varrefs
      (define relevant-prims 
	(filter id
	   (map (lambda (prm) 
		  ;; Err... these really might should have different names:
		  (if (memq (car prm) '(+: *: -: /: ^:))
		      #f
		      (let ([boom (explode-id (car prm))])
			(if (not (null? (cdr boom)))
			    (cons (car prm) boom)
			    #f))))
	     (regiment-primitives))))
      (define exploded-table (map cdr relevant-prims))

      ;; This is used for two purposes.  It keeps track of all bindings so we can recognize free variables.
      ;; It also keeps track of variable renamings that are to be performed as a result of using statements.
      ;; Seed this with prims, but don't rename prims:
      (define var-table (map (lambda (p) (list (car p) (car p))) relevant-prims))
      ;; Mutated below:
      (define type-constructors '())
      ;; Mutated below:
      ;(define mapping '())
      
      (define (Var pos var)
	(cond

	 ;; DESUGARING:
	 ;; We can't dealias some of these aliases without making
	 ;; sure they aren't shadowed in the environment.
	 [(and (eq? var 'ref) (not (assq 'ref var-table)))
	  ;(inspect "GOT REF")
	  'Mutable:ref]

	 ;[(eq? var 'ref) (printf "GOT REF IN SCOPE\n") 'ref]
	 
	 [(memq var type-constructors) 
	  ;;(error 'resolv-varrefs "type constructor not directly applied: ~s" var)
	  var]
	 [(assq var var-table) => 
	  (lambda (pr) 
	    (let ([v2 (cadr pr)])
	      (if (and (symbol? v2) (not (eq? v2 var)))
		  (Var pos v2)
		  v2)))]
	 [(regiment-primitive? var) var]
	 [else (error 'resolve-varrefs 
		      "variable was not bound!: ~a\n\nEnvironment Context: ~s\n~a"
		      var (map car var-table)
		      (if pos
			  (format "\nSource location:\n  ~a\n\n" (src-pos->string pos))
			  ""))]))

      (define (driver x fallthru)
	(match x
	  ;; There should be a better way of doing this:
	  [(src-pos ,p ,var) (guard (symbol? var))  (Var p var)]
	  [,var (guard (symbol? var)) (Var #f var)]

;; [2007.06.01] Moving this functionality to eta-primitives.
#;
	  [(app ,tc ,[rand]) (guard (memq tc type-constructors))
	   ;; From here on out we use a special form:
	   `(construct-data ,tc ,rand)]

	  ;; The automatic traversal won't do the variable (it's not an expression):
	  [(set! ,[v] ,[rhs]) `(set! ,v ,rhs)]
	  [(using ,M ,bod)
	   (let* ([imported
		   (filter (lambda (ev) (eq? M (car ev)))
		     exploded-table)]
		  [chopped (map cdr imported)])

	     ;; Here we perform some surgery on the var-table
	     (fluid-let ([var-table
			  (append 
			   (map list 
			     (map contract-id chopped)
			     (map contract-id imported))
			   var-table)]
			 [exploded-table (append chopped  exploded-table)])
	       (driver bod fallthru)))]
	  
	  [,oth (fallthru oth)]))

    [OutputGrammar resolve-varrefs-grammar]
    [Expr driver]
    [Bindings 
     (lambda (vars types exprs reconstr exprfun)
       ;(define newvars (map unique-name vars))
       ;; Disabling renaming here and doing that as a separate pass.
       ;; This is so that we can print the types with the original names.
       (define newvars vars)
       ;(inspect (vector 'newvars newvars))
       (reconstr newvars types		 
           (fluid-let ([var-table (append (map list vars newvars)
					  var-table)]
		       [exploded-table 
			(append (map explode-id vars)
				exploded-table)])
	     (map exprfun exprs))))]
    [Program 
     (lambda (p doExpr)
       (unique-name-counter 0)
       (match p
	 [(,input-language (quote (program ,body ,other ... ,type)))
	  (fluid-let ([type-constructors 
		       (match (or (assq 'union-types other ) '(union-types))
			 [(union-types [,name* [,tycon** . ,_] ...] ...)
			  (apply append tycon**)])])
	    `(resolve-varrefs-language '(program ,(doExpr body) ,other ... ,type)))]))]
    )





	
;==============================================================================
  
;; Some of these tests are ripped from rename-var:
(define-testing these-tests
     `(

       [(,resolve-varrefs '(some-lang '(program (letrec ((x Int 1)) 
					    (+_ (app (lambda (x) (Int) x) 3) x)) (union-types) Int)))
	,(lambda (p)
	   (match p
	     [(resolve-varrefs-language
	       '(program
		    (letrec ([,x_1 Int 1])
		      (+_ (app (lambda (,x_2) (Int) ,x_2b) 3) ,x_1b))
		  (union-types) Int))
	      (and (eq? x_1 x_1b) (eq? x_2 x_2b))]
	     [,else #f]))]
       
       ;; Might not be portable, assumes particular numbering:
       ["check on for loops"
	(,resolve-varrefs '(some-lang '(program (lambda (f woot) ((Int -> Int) Int)
						    (for (i 1 (app f woot)) 0)) 
					 (union-types) Int)))
        ,(lambda (v)
           (match v
             ((resolve-varrefs-language '(program (lambda (,F1 ,W1) ((Int -> Int) Int)
					       (for (,I 1 (app ,F2 ,W2)) 0)) 
					   (union-types) Int))
              (and ;(not (eqv? I 'i))
                   (eqv? F1 F2)
                   (eqv? W1 W2)))
             (,else #f)))]
       ["check on set!" 
	(,resolve-varrefs '(some-lang '(program (letrec ([v Int 3]) 
					    (set! v 39)) 
					 (union-types) Int)))
	(resolve-varrefs-language
	 '(program (letrec ([unspecified Int 3]) (set! unspecified 39)) (union-types) Int))]
       

       ["Basic using declaration"
	(reunique-names 
	 (resolve-varrefs '(lang '(program (letrec ([M:x 't '3]) (using M x)) (union-types) Int))))

	(resolve-varrefs-language '(program (letrec ((M:x (quote t) (quote 3))) M:x) (union-types) Int))
#;
	(resolve-varrefs-language
	 '(program
	      (letrec ([unspecified 't '3]) (letrec ([x 'type unspecified]) x))
	    (union-types) Int))]

       
              
       ))

(define-testing test-this
  (default-unit-tester 
    " 2: Resolve-Varrefs: Pass to rename variabless."
    these-tests))

(define test-resolve-varrefs test-this)

;==============================================================================

)

