

(module resolve-varrefs mzscheme
  (require "../../../plt/common.ss")
  (provide resolve-varrefs 
           ;test-this these-tests test01 tests01
           test-resolve-varrefs
	   )
  (chezimports)

  ;; This is a bit of a hack... really should split resolve-varrefs into
  ;; two separate passes for the two places it's used.
  (define resolve-varrefs-grammar   initial_regiment_grammar)

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
      (define var-table '())
      (define exploded-table '())
      (define (driver x fallthru)
	(match x
	  [,var (guard (symbol? var))
		(cond
		 [(assq var var-table) => cadr]
		 [(regiment-primitive? var) var]
		 [else (error 'resolve-varrefs 
			      "variable was not bound!: ~a\n environment: ~s"
			      var var-table)])]
	  ;; The automatic traversal won't do the variable (it's not an expression):
	  [(set! ,[v] ,[rhs]) `(set! ,v ,rhs)]
	  [(using ,M ,e)
	   (let ([imported
		  (filter (lambda (ev) (eq? M (car ev)))
		    exploded-table)])
	     ;(inspect (vector 'imported imported))

	     ;; TODO: It might be somewhat more efficent if we brought
	     ;; these substitutions down with us, rather than
	     ;; introducing this code bloat:
	     (driver
	      `(letrec ([,(map contract-id (map cdr imported))
			 ,(map (lambda (_) `(quote ,(unique-name 'type))) imported)
			 ,(map contract-id imported)] ...)
		 ,e)
	      fallthru))]
	  
	  [,oth (fallthru oth)]))

    [OutputGrammar resolve-varrefs-grammar]
    [Expr driver]
    [Bindings 
     (lambda (vars types exprs reconstr exprfun)
       (define newvars (map unique-name vars))
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
	 [(,input-language (quote (program ,[doExpr -> body] ,type)))
	  `(resolve-varrefs-language '(program ,body ,type))]))]    
    )


  ;; Module stack:
  ;; ( [Foo x y z]
  ;;   [Bar x y w]
  ;;   [#f a b c])











#;
       (match p
	 [(,input-language (quote (program ,body ,type)))
	  (let ([body (process-expr body '())])
	    `(resolve-varrefs-language '(program ,body ,type)))])
       

#;  
  (define (process-expr expr var-table exploded-table)
    (define (driver x fallthru)
      (match x
	[,var (guard (symbol? var))
	      (cond
	       [(assq var var-table) => cdr]
	       [(regiment-primitive? var) var]
	       [else (error 'resolve-varrefs "variable was not bound, how can this happen?: ~a ~a"
			    var var-table)])]	

	[(using ,M ,e)
	 (let ([imported
		(filter (lambda (ev) (eq? M (car ev)))
		  exploded-table)])
	   (driver 
	    `(letrec ([,(map contract-id (map cdr imported)) 
		       ,(map contract-id imported)] ...)
	       ,e)
	    fallthru))]

#;
	[,bf (guard (binding-form? bf))
	     
	     ]

	[(lambda (,v* ...) (,t* ...) ,expr)
	 (guard (not (assq 'lambda var-table)))
	 (let* ([new-v* (map unique-name v*)]
		[new-table (append (map cons v* new-v*) var-table)])
	   (let ([expr (process-expr expr new-table)])
	     `(lambda ,new-v* ,t* ,expr)))]
	[(for (,i ,[st] ,[en]) ,body)
	 (guard (not (assq 'for var-table)))
	 (let* ([newi (unique-name i)]
		[var-table `((,i . ,newi) . ,var-table)])
	   `(for (,newi ,st ,en) 
		,(process-expr body var-table)))]
	;; The automatic traversal won't do the variable (it's not an expression):
	[(set! ,[v] ,[rhs]) `(set! ,v ,rhs)]
	
	[(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
	 (guard (not (assq 'letrec var-table)))
	 (let* ([new-lhs* (map unique-name lhs*)]
		[var-table (append (map cons lhs* new-lhs*) var-table)])
	   (let ([rhs* (map (lambda (x) (process-expr x var-table)) rhs*)]
		 [expr (process-expr expr var-table)])
	     `(letrec ([,new-lhs* ,type* ,rhs*] ...) ,expr)))]

	[,other (fallthru other)]))
    
    (core-generic-traverse driver (lambda (ls k) (apply k ls)) expr))





	
;==============================================================================
  
;; Some of these tests are ripped from rename-var:
(define-testing these-tests
  (append (map
	      (lambda (x)
		(let ((prog (car x)) (res (cadr x)))
		  `[(,resolve-varrefs '(some-lang '(program ,prog Int)))
		    (resolve-varrefs-language '(program ,res Int))]))
	    `([3 3]    
	      [(letrec ((x Int 1)) x) (letrec ([x_1 Int 1]) x_1)]          
	      ))
     `(

       [(,resolve-varrefs '(some-lang '(program (letrec ((x Int 1)) 
					    (+_ (app (lambda (x) (Int) x) 3) x)) Int)))
	,(lambda (p)
	   (match p
	     [(resolve-varrefs-language
	       '(program
		    (letrec ([,x_1 Int 1])
		      (+_ (app (lambda (,x_2) (Int) ,x_2b) 3) ,x_1b))
		  Int))
	      (and (eq? x_1 x_1b) (eq? x_2 x_2b))]
	     [,else #f]))]
       
       ;; Might not be portable, assumes particular numbering:
       ["check on for loops"
	(,resolve-varrefs '(some-lang '(program (lambda (f woot) ((Int -> Int) Int)
						    (for (i 1 (app f woot)) 0)) Int)))
        ,(lambda (v)
           (match v
             ((resolve-varrefs-language '(program (lambda (,F1 ,W1) ((Int -> Int) Int)
					       (for (,I 1 (app ,F2 ,W2)) 0)) Int))
              (and (not (eqv? I 'i))
                   (eqv? F1 F2)
                   (eqv? W1 W2)))
             (,else #f)))]
       ["check on set!" 
	(,resolve-varrefs '(some-lang '(program (letrec ([v Int 3]) 
					    (set! v 39)) Int)))
	(resolve-varrefs-language
	 '(program (letrec ([v_1 Int 3]) (set! v_1 39)) Int))]
       
       ["Basic using declaration"
	(reunique-names 
	 (resolve-varrefs '(lang '(program (letrec ([M:x 't '3]) (using M x)) Int))))
	(resolve-varrefs-language
	 '(program
	    (letrec ([M_x 't '3]) (letrec ([x 'type M_x]) x))
	    Int))]
              
       )))

(define-testing test-this
  (default-unit-tester 
    " 2: Resolve-Varrefs: Pass to rename variabless."
    these-tests))

(define test-resolve-varrefs test-this)

;==============================================================================

)




#!eof

(resolve-varrefs 
 `(lang 
   '(program 
	,(pass_desugar-pattern-matching
	  (verify-regiment
	   (ws-postprocess 
	   '((namespace Foo (define s0 (app timer 3.0)))
	    (namespace Bar (namespace Baz (define s1 (using Foo s0))))
	    (define s2 Bar:Baz:s1)
	    (define f
	      (lambda (s) (let-as (s2 (x y) s) (dot-project (x x y) s2))))
	    (<- BASE
		(app f
		     (iterate
		      (lambda (#() ___VIRTQUEUE___)
			(begin (emit ___VIRTQUEUE___ (tuple 1 2)) ___VIRTQUEUE___))
		      s2)))))))
      (Stream (Int * Int * Int)))))







(desugar-pattern-matching-language
  '(program
     (letrec ([Foo:s0 'type_11 (app timer 3.0)])
       (letrec ([Bar:Baz:s1 'type_10 (using Foo s0)])
         (letrec ([s2 'type_9 Bar:Baz:s1])
           (letrec ([f 'type_8 (lambda (s)
                                 ('type_5)
                                 (letrec ([s2 '#<procedure notype> s])
                                   (letrec ([:s2:x 'notypeyet_18 (lambda #0=(x)
                                                                   ('notypeyet_20)
                                                                   (tupref
                                                                     0
                                                                     2
                                                                     .
                                                                     #1=(x)))]
                                            [:s2:y 'notypeyet_17 (lambda #0#
                                                                   ('notypeyet_19)
                                                                   (tupref
                                                                     1
                                                                     2
                                                                     .
                                                                     #1#))])
                                     (iterate
                                       (lambda (___tmp____13 ___vq____14)
                                         ('notypeyet_15 'notypeyet_16)
                                         (begin
                                           (emit
                                             ___vq____14
                                             (tuple
                                               (app :s2:x ___tmp____13)
                                               (app :s2:x ___tmp____13)
                                               (app :s2:y ___tmp____13)))
                                           ___vq____14))
                                       s2))))])
             (app f
                  (iterate
                    (lambda (pattmp_12 ___VIRTQUEUE___)
                      ('type_7 'type_6)
                      (begin
                        (emit ___VIRTQUEUE___ (tuple 1 2))
                        ___VIRTQUEUE___))
                    s2))))))
     toptype))



====================================================================================================

(resolve-varrefs-language
  '(program
     (letrec ([Foo_s0_1 'type_11 (app timer 3.0)])
       (letrec ([Bar_Baz_s1_2 'type_10 (letrec ([s0_4 'type_3 Foo_s0_1])
                                         s0_4)])
         (letrec ([s2_5 'type_9 Bar_Baz_s1_2])
           (letrec ([f_6 'type_8 (lambda (s_7)
                                   ('type_5)
                                   (letrec ([s2_8 '#<procedure notype> s_7])
                                     (letrec ([_s2_x_10 'notypeyet_18 (lambda (x_12)
                                                                        ('notypeyet_20)
                                                                        (tupref
                                                                          0
                                                                          2
                                                                          x_12))]
                                              [_s2_y_9 'notypeyet_17 (lambda (x_11)
                                                                       ('notypeyet_19)
                                                                       (tupref
                                                                         1
                                                                         2
                                                                         x_11))])
                                       (iterate
                                         (lambda (___tmp____14 ___vq____13)
                                           ('notypeyet_15 'notypeyet_16)
                                           (begin
                                             (emit
                                               ___vq____13
                                               (tuple
                                                 (app _s2_x_10
                                                      ___tmp____14)
                                                 (app _s2_x_10
                                                      ___tmp____14)
                                                 (app _s2_y_9
                                                      ___tmp____14)))
                                             ___vq____13))
                                         s2_8))))])
             (app f_6
                  (iterate
                    (lambda (pattmp_16 ___VIRTQUEUE____15)
                      ('type_7 'type_6)
                      (begin
                        (emit ___VIRTQUEUE____15 (tuple 1 2))
                        ___VIRTQUEUE____15))
                    s2_5))))))
     toptype))
