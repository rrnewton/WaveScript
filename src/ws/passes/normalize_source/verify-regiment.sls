#!r6rs

;;;; Pass 00: verify-regiment

;;;; This pass verifies that the input is in the regiment lanuguage.
;;;; It also wraps the program in the boilerplate '(<lang> '(program <Exp>)) form.

;;;; It's not *just* a verification pass though.  It does one teensy
;;;; bit of normalization.  It includes "blank" types ('a) where there
;;;; are no user type annotations.  This removes the annoying types/notypes
;;;; ambiguity in lambda/let/letrec/etc.

;;;; Really this is pretty unnecessary now because the type checker
;;;; should catch most of the problems that verify-regiment does.

(library (ws passes normalize_source verify-regiment)   
  (export verify-regiment test-verify-regiment test00)
  (import (except (rnrs (6)) error) (ws common))  

; ----------------------------------------

(define verify-regiment
  (build-compiler-pass ;; This wraps the main function with extra debugging
   'verify-regiment
   `(input)
   `(output (grammar ,sugared_regiment_grammar PassInput))
   (let ()

     (define (assert-valid-name! v)
       ;; [2007.08.11] Changing this:
       #;
       (IFWAVESCOPE 
	(if (regiment-primitive? v)
	    (error 'verify-regiment 
		   "for the time being you cannot bind/mutate variables that use the same names as primitives: '~s'" v))
	(if (regiment-keyword? v)
	    (error 'verify-regiment 
		   "for the time being you cannot bind/mutate variables that use the same names as keywords: '~s'" v))
	)
       (void)
       )

     (define (pattern! p)
       (match p
	 [,v (guard (symbol? v))  (assert-valid-name! v)]
	 [(assert-type ,t ,[v]) (guard (type? t)) (void)]
	 [#(,p* ...) (for-each pattern! p*)]
	 [(data-constructor ,tc ,arg ...) (guard (symbol? tc)) (for-each pattern! arg)]
	 [,other (error 'verify-regiment "bad binding pattern: ~s" other)]
	 ))

     (define (pattern->vars p)
       (match p
	 [_  '()] ;; Don't return "ignored" slots.
	 [(assert-type ,_ ,[p]) p]
	 [,v (guard (symbol? v))  (list v)]
	 [#(,[v*] ...) (apply append v*)]
	 ))

     (define (blank-type) `(quote ,(unique-name 'type)))

     ;; .param env is just a list of symbols in scope.
     ;; (Really this is outdated because the type-checker will catch problems
     ;; with unbound variables.)
     (define process-expr
       (lambda (expr env)
	 (match expr

	   ;; TODO: should check the types for validity also.
	   [(assert-type ,t ,[e]) `(assert-type ,t ,e)]
	   [(src-pos ,p ,[e]) `(src-pos ,p ,e)]	   
	   
	   [,const (guard (and (not (symbol? const)) (or (simple-constant? const) (string? const)))) const]
	   [(quote ,datum)
	    (guard (not (memq 'quote env)) (datum? datum))
	    `(quote ,datum)]
	   [,var (guard (symbol? var))
		 ;; [2007.03.11] Don't currently enforce
		 ;; bound-variables here, it's done in resolve-varref:
		 var
		 #;
		 (if (and (not (memq var env))
			  (not (regiment-primitive? var)))
		     (error 'verify-regiment (format "unbound variable: ~a\n" var))
		     var)]	   
	   
	  [(tuple ,[e] ...) `(tuple ,e ...)]
	  [(tupref ,n ,len ,[e])
	   (unless (qinteger? n) (error 'verify-regiment "bad index to tupref: ~a" n))
	   (unless (qinteger? len) (error 'verify-regiment "bad length argument to tupref: ~a" len))
	   `(tupref ,n ,len ,e)]
 
          [(lambda (,v* ...) ,optionaltypes ... ,expr)
           (guard (not (memq 'lambda env)))
	   (for-each pattern! v*)	   
	   (let ([types (match optionaltypes 
                     [() (map (lambda (_) (blank-type)) v*)]
                     [((,t* ...)) t*])]
            [vars (apply append (map pattern->vars v*))])	     
	     (unless (list-is-set? vars)
	       (error 'verify-regiment "argument list has repeats: ~a" vars))
	     `(lambda ,v* ,types ,(process-expr expr (union vars env))))]

	  ;; Should eventually remove ifs as subsumed by case statements.
          [(if ,[test] ,[conseq] ,[altern])
           (guard (not (memq 'if env)))
	   `(if ,test ,conseq ,altern)]

	  ;; TODO: should check the form of the patterns:
	  [(wscase ,[val] (,pat* ,[rhs*]) ...)
	   (for-each pattern! pat*)
	   `(wscase ,val ,@(map list pat* rhs*))]

	  [(letrec ([,lhs* ,optional ... ,rhs*] ...) ,expr)
	   (guard (not (memq 'letrec env)))
	   (for-each pattern! lhs*)
	   (let* ([vars (apply append (map pattern->vars lhs*))]
		  [newenv (union vars env)]
		  [rands (map (lambda (r) (process-expr r newenv)) rhs*)]
		  [body  (process-expr expr newenv)]
		  [types (map (lambda (opt)
				(match opt
				  [() (blank-type)]
				  [(,t) t]))
			   optional)])
	     (ASSERT (list-is-set? vars))
	     `(letrec ([,lhs* ,types ,rands] ...) ,body))]
	  
	  ;; This is long-winded, handling all these let-variants:
	  [(let* ([,lhs* ,optional ... ,rhs*] ...) ,body)
	   (guard (not (memq 'let* env)))
	   (for-each pattern! lhs*)
	   (let loop ([env env] [lhs* lhs*] [rhs* rhs*] [opt optional] [acc '()])
	     (if (null? lhs*)
		 `(let* ,(reverse acc) ,(process-expr body env))
		 (let* ([vars (pattern->vars (car lhs*))]
			[rhsnew (process-expr (car rhs*) env)]
			[newenv (union vars env)]
			[type (match (car opt) [() (blank-type)] [(,t) t])])
		   (ASSERT (list-is-set? vars))
		   (loop newenv (cdr lhs*) (cdr rhs*) (cdr opt)
			 `([,(car lhs*) ,type ,rhsnew] . ,acc)
			 ))))]
	 [(let ([,lhs* ,optional ... ,[rhs*]] ...) ,body)
	  (guard (not (memq 'let env)))	  
	  (for-each pattern! lhs*)
	  (let ([vars (apply append (map pattern->vars lhs*))]
		[types (map (lambda (opt)
			      (match opt
				[() (blank-type)]
				[(,t) t]))
			 optional)])
	    (ASSERT (list-is-set? vars))
	    `(let ([,lhs* ,types ,rhs*] ...) 
	       ,(process-expr body (union lhs* env))))]

	 ;; This is temporary until it's desugared:
	 [(let-as (,lhs ,pat ,[rhs]) ,body)
	  (guard (not (memq 'let-as env)))
	  (ASSERT symbol? lhs)
	  ;; "pat" is really just a list of symbols for now.
	  (ASSERT (curry andmap symbol?) pat)
	  (ASSERT (list-is-set? pat))
	  `(let-as (,lhs ,pat ,rhs)
		   ,(process-expr body (union (cons lhs pat) env)))]

	 ;; Another sugar:
	 [(dot-project (,proj* ...) ,[src])
	  (ASSERT (curry andmap symbol?) proj*)
	  `(dot-project (,proj* ...) ,src)]

	 [(dot-record-project (,proj* ...) ,[src])
	  (ASSERT (curry andmap symbol?) proj*)
	  `(dot-record-project (,proj* ...) ,src)]

	 ;; verify-regiment can't track the bindings that should be introduced here:
	 [(using ,M ,[e])  (ASSERT symbol? M)  `(using ,M ,e)]

    ;; [2006.07.25] Adding effectful constructs for WaveScope:
    [(begin ,[e] ...) `(begin ,e ...)]
    [(set! ,v ,[e]) (guard (symbol? v)) 
     (if (and (not (memq v env))
              (not (regiment-primitive? v)))
         (error 'verify-regiment (format "set! unbound variable: ~a\n" v)))
     (assert-valid-name! v)
     `(set! ,v ,e)]
    [(for (,v ,[e1] ,[e2]) ,e3) (guard (symbol? v))
     (assert-valid-name! v)
     `(for (,v ,e1 ,e2) 
          ,(process-expr e3 (cons v env)))]
    [(while ,[e1] ,[e2]) `(while ,e1 ,e2)]
    ;; ========================================

    [(iterate ,annot ,[f] ,[s])
     `(iterate ,annot ,f ,s)]
    
    [(,prim ,[rand*] ...)
     (guard (not (memq prim env))
            (regiment-primitive? prim))
     `(,prim ,rand* ...)]
    
    [(app ,[rator] ,[rand*] ...)  `(app ,rator ,rand* ...)]

    [(empty-wsrecord) `(empty-record)]
    [(wsrecord-extend ,name ,[x] ,[rec]) `(wsrecord-extend ,name ,x ,rec)]
    [(wsrecord-select ,name ,[rec])      `(wsrecord-select ,name ,rec)]
    [(wsrecord-restrict ,name ,[rec])    `(wsrecord-restrict ,name ,rec)]
    
    [,unmatched
     (error 'verify-regiment "invalid syntax ~s" unmatched)])))
     
     (define (process-program prog)

	 ;; This is ugly, but for this very first pass, we pretend
	 ;; that these are primitives.  They are desugared in the next pass.
	 (parameterize ([regiment-primitives
			 (append 
			  '((+ (Int Int) Int)
			    (- (Int Int) Int) 
			    (* (Int Int) Int) 
			    (/ (Int Int) Int) 
			    (^ (Int Int) Int))
			  (regiment-primitives))])
           (match prog
	     ;; The input is already wrapped with the metadata:
	     [(,input-language (quote (program ,body ,?type ...)))	      
	      `(verify-regiment-language 
		'(program ,(process-expr body '())
		   ,@(if (null? ?type) '('toptype)
			 ?type)))]
	     ;; Nope?  Well wrap that metadata:
	     [,body (process-program `(base-language '(program ,body)))]
	     )
	   ))

     ;; Main body:
     process-program
     )))


;==============================================================================
     

(define-testing test-verify-regiment
  (default-unit-tester 
    " 0: Verify-Regiment: Pass to verify initial regiment language."
  '( [(verify-regiment '(some-lang '(program 3)))
      (verify-regiment-language (quote (program 3 'toptype)))]
      
     [(verify-regiment '(some-lang '(program
       (letrec ((a (anchor-at 30 40)))
       (letrec ((r (circle a 50.))
		(f (lambda (next tot)
		     (cons (_+_ (car tot) (sense "temperature" next))
			   (cons (_+_ (car (cdr tot)) 1)
				 '()))))
		(g (lambda (tot) (/_ (car tot) (car (cdr tot))))))
	 (smap g (rfold f '(0 0) r)))))))
      unspecified]
      
     [(verify-regiment '(some-lang '(program
       (letrec ((R (circle-at 30 40 50.))
	      (f (lambda (next tot)
		   (cons (_+_ (car tot) (sense "temperature" next))
			 (cons (_+_ (car (cdr tot)) 1)
			       '()))))
	      (g (lambda (tot) (/_ (car tot) (car (cdr tot))))))
       (letrec ((avg (smap g (rfold f (cons 0 (cons 0 '())) R))))
	 (runtil (swhen-any (lambda (x) (> x 15)) avg)
		 R
		 (circle-at 0 0 100.)))))))
      unspecified]
     )))
 
(define test00 test-verify-regiment)

;==============================================================================


) ; End module
