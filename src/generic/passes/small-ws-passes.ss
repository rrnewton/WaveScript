

;;;; This defines a handful of small passes used by WaveScript that
;;;; don't quite each deserve their own files.


(module small-ws-passes mzscheme
  (require "../../plt/common.ss"
	   "../../plt/hashtab.ss"	   
	   "normalize_query/ws-remove-complex-opera.ss"
	   "../compiler_components/type_environments.ss"
	   )
  (provide 
           introduce-lazy-letrec
	   lift-polymorphic-constant
	   unlift-polymorphic-constant
	   strip-irrelevant-polymorphism
	   strip-src-pos
	   ;purify-letrec  ;; Disabled
	   standardize-iterate
	   kill-polymorphic-types
	   ;ws-add-return-statements  ;; Disabled
	   resolve-type-aliases
	   ws-normalize-context
	   generate-comparison-code

           ; --mic
	   propagate-copies
           )
  (chezimports)
  (require-for-syntax "../../plt/common.ss")

;; [2007.09.06] little helper pass used in ws.early:
(define-pass strip-src-pos
  [Expr (lambda (e fallthru)
	  (match e
	    [(src-pos ,_ ,[e]) e]
	    [,oth (fallthru oth)]))])


;; [2007.08.02] This kind of thing should not be done in the actual
;; code generators if it can be helped.
(define-pass generate-comparison-code
  (define (build-comparison ty e1 e2)
    (match ty
      [(Array ,elt) 
       (let ([el1  (unique-name "arrel1")]
	     [el2  (unique-name "arrel2")]
	     [i    (unique-name "i")]
	     [stop (unique-name "stop")]
	     [len  (unique-name "len")])
	 `(if (wsequal? (assert-type Int (Array:length ,e1)) (Array:length ,e2))
	      (let ([,i (Ref Int) (Mutable:ref '0)]
		    [,stop (Ref Bool) (Mutable:ref '#f)]
		    [,len Int (Array:length ,e1)])
		(begin
		  (while (if (< (deref ,i) ,len) (not (deref ,stop)) '#f)
			 (let ([,el1 ,elt (Array:ref ,e1 ,i)])
			 (let ([,el2 ,elt (Array:ref ,e2 ,i)])
			     (if ,(build-comparison elt el1 el2)
				 (set! ,i (+_ (deref ,i) '1))
				 (set! ,stop '#t)
				 ))))
		  (not (deref ,stop))))
	      '#f))]
      ;; For the simple case we just allow the wsequal? to stick around.
      [,_ `(wsequal? (assert-type ,ty ,e1) ,e2)]))
  [Expr 
   (lambda (x fallthru)
     (match x
       [(wsequal? (assert-type ,ty ,[e1]) ,[e2])
	(ASSERT simple-expr? e1)
	(ASSERT simple-expr? e2)
	(build-comparison ty e1 e2)	
	#;
	(maybe-let e1 ty
	  (lambda (e1)
	    (maybe-let )
	    ))
	]
       [(wsequal? . ,_) 
	(error 'generate-comparison-code 
	       "wsequal? was missing type annotation: ~s"
	       `(wsequal? . ,_))]
       [,oth (fallthru oth)]
       ))])


;; [2007.05.01] This pulls complex constants up to the top of the program.
;(define-pass lift-complex-constants)

;; This is superficial.
;; This cuts out all the meta data but the union-types, which is all we need towards the end.
#;
(define (prune-meta-data input-gram)
  (let ()
    (define gram 
      (cons '(Program ((quote program) Query ('union-types ((Var Type ...) [Var Type ...] ...) ...) Type))	    
	    (remq (assq 'Program input-gram))))
    (define-pass prune-meta-data	
      [OutputGrammar gram])
    prune-meta-data))

;; Simply transforms letrec into lazy-letrec.
(define-pass introduce-lazy-letrec
    [Expr (lambda (x fallthru)
	    (match x
	      [(free ,_ ,[e]) e]
	      [,other 
	       (match (fallthru other)
		 [(letrec ,rest ...) `(lazy-letrec ,rest ...)]
		 [,other other]) ]))])

;; [2007.03.17] Including Array:makeUNSAFE here even though it's not "constant"
(define-pass lift-polymorphic-constant
    [Expr (lambda (x fallthru)
	    (define (f x) 
	      (let ([tmp (unique-name 'tmp)]
		    [t   (unique-name 'alpha)])
		`(let ([,tmp (quote ,t) ,x]) ,tmp)))
	    (match x
	      [nullseg (f x)]
	      [Array:null (f x)]
	      ['()  (f x)]
	      [(Array:makeUNSAFE ,[n]) (f `(Array:makeUNSAFE ,n))]
	      ;; Don't touch these:
	      [(foreign        ,x ,y) `(foreign        ,x ,y)]
	      [(foreign_source ,x ,y) `(foreign_source ,x ,y)]

	      [,other (fallthru other)]))])

(define-pass unlift-polymorphic-constant
    (define (pconst? x) 
      (match x
	[nullseg #t]
	[Array:null #t]
	['()     #t]
	[()     #t]
	[(Array:makeUNSAFE ,n) #t]
	[,else   #f]))
  [Expr (lambda (x fallthru)
	  (match x

	    ;; Don't touch these:
	    [(foreign        ,x ,y) `(foreign        ,x ,y)]
	    [(foreign_source ,x ,y) `(foreign_source ,x ,y)]

	    [(let ([,v1 ,t ,c]) ,v2)
	       (guard (eq? v1 v2) (pconst? c))
;; [2007.07.08] Removing this assert because we clean up below:
;	       (ASSERT (lambda (t) (not (polymorphic-type? t))) t)
	       `(assert-type ,t ,c)]

	    [,c (guard (pconst? c))
		(error 'unlift-polymorphic-constant "missed polymorphic const: ~s" c)]

	    ;; Don't touch these:
	    [(foreign ,x ,y) `(foreign ,x ,y)]
	    [,other (fallthru other)]))])


;; [2007.07.08]
;; Remaining polymorphism at this phase of the compiler is
;; "irrelevent" in the sense that it describes only uninspected values.
;; Thus it is equivalent to insert unit in all such places.
(define-pass strip-irrelevant-polymorphism
    (define (data-source? e)
      (let ([expr (peel-annotations e)])
	(and (pair? expr) (memq (car expr) '(readFile dataFile)))))
    (define (Type ty)
      (match ty
	  [(,qt ,v) (guard (memq qt '(quote NUM)))
	   (ASSERT symbol? v)
	   #()]
	  [,s (guard (symbol? s)) s]
	  [#(,[t*] ...)                    (list->vector t*)]
	  [(,[arg*] ... -> ,[ret])        `(,@arg* -> ,ret)]
	  ;; Including Ref:
	  [(,C ,[t*] ...) (guard (symbol? C) (not (memq C '(quote NUM))))    (cons C t*)]
	  [,s (guard (string? s)) s] 
	  [,oth (error 'strip-irrelevant-polymorphism "unhandled type: ~s" oth)]))
    (define Expr
      (lambda (x fallthru)
	  (match x
	    ;; Strip an assert-type unless it's around a data source.
	    ;; HACK: quoted numbers also need their type annotations.
#;
	    [(assert-type ,t ,e) (guard (or (data-source? e) (quoted-num? e)))
	     `(assert-type ,t ,(Expr e fallthru))]

	    ;; New hack: we keep any annotations that have NO
	    ;; polymorphism.  These will not be a problem.
	    [(assert-type ,t ,[e]) (guard (not (polymorphic-type? t)))
	     `(assert-type ,t ,e)]

	    ;; Otherwise we remove the type assertion entirely.
	    [(assert-type ,_ ,[e]) 
	     ;(printf "GOT ASSERT: ~s\n" ty)
	     ;`(assert-type ,ty ,e)
	     e
	     ]
	    [,oth (fallthru oth)])))
  [Expr Expr]
  [Bindings 
   (lambda (vars types exprs reconstr exprfun)
     (reconstr vars (map Type types) (map exprfun exprs)))])


;; Purify-letrec: makes sure letrec's only bind functions.
#;
(define-pass purify-letrec
    [Expr (lambda (x fallthru)
	    (match x
	      [(letrec ([,v* ,ty* ,[e*]] ...) ,[bod])	       
	       (cond
		[(lambda? e) ]
		[(no-lambda? e) ]
		
		)]

	      [(free ,_ ,[e]) e]
	      [,other 
	       (match (fallthru other)
		 [(letrec ,rest ...) `(lazy-letrec ,rest ...)]
		 [,other other]) ]))])

;; This little pass handles only iterate cases.
(define-pass standardize-iterate
    (define process-expr
      (lambda (x fallthru)
	(match x
	  [(iterate (let ,binds (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,[strm])
	   `(iterate (let ,binds (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,strm)]

	  ;; [2007.03.25] This is a bit hackish... but at this
	  ;; late point in the game the program's already been
	  ;; typechecked several times, so it should be ok to
	  ;; throw away this ascription:
	  [(iterate (assert-type ,t ,lam) ,src)
	   (process-expr `(iterate ,lam ,src) fallthru)]
	  
	  ;; OPTIMIZATION:
	  ;; This doesn't recursively process the inside of iterates.
	  ;; That's because we can't find iterates within iterates.
	  ;; This does preclude using fuse-passes on this pass.
	  [(iterate (lambda (,x ,y) (,tyx ,tyy) ,bod) ,[strm])
	   `(iterate (let () (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,strm)]
	  [(iterate ,_ ...)
	   (error 'standardize-iterate "shouldn't have missed this iterate: ~s" `(iterate ,_ ...))]
	  [,oth (fallthru oth)])
	))
    [Expr process-expr]
    ;[Props 'incomplete-ast-coverage]
    )

(define-pass kill-polymorphic-types
    (define (Type t)
      (match t
	;; Here we turn any remaining type vars into unit:
	[',n  '#()]
	;; Any remaining numeric type vars become Int:
	[(NUM ,v) (guard (symbol? v)) 'Int]

	[,s    (guard (symbol? s))           s]
	[(,[arg*] ... -> ,[res])           `(,arg* ... -> ,res)]
	[(,s ,[t] ...) (guard (symbol? s)) `(,s ,t ...)]
	[#(,[t*] ...)                       (apply vector t*)]
	[,other (error 'kill-polymorphic-types "bad type: ~s" other)]))
  #;
  [Expr (lambda (x fallthru)
	  (match x 
	    [(assert-type ,[Type -> t] ,[e])
	     `(assert-type ,t ,e)]
	    [,oth (fallthru oth)]))]
  [Bindings (lambda (var* ty* expr* reconstr Expr)
	      (reconstr var* (map Type ty*) (map Expr expr*)))])


;; UNUSED
(define-pass ws-add-return-statements
    (define (doit fallthru)
      (lambda (x)	
	(match x 
	  [,x (guard (simple-expr? x)) `(return ,x)]
	  ;[(assert-type ,t ,e)	   ]
	  [(if ,a ,[b] ,[c])      `(if ,a ,b ,c)]
	  [(begin ,e ... ,[last]) `(begin ,@e ,last)]
	  [(let ,binds ,[body])   `(let ,binds ,body)]
	  [(for ,decl ,[body])    `(for ,decl ,body)]

	  [,oth `(return ,(fallthru oth))]
	  )))  
  [OutputGrammar 
   (cons ;'(LetOrSimple ('return Simple))
    '(Expr ('return Simple))
    ws-remove-complex-opera*-grammar)
   ]
  [Expr (lambda (x fallthru)
	    (match x
	      [(iterate (let ,binds (lambda (,x ,y) (,tyx ,tyy) ,[(doit fallthru) -> bod])) ,strm)
	       `(iterate (let ,binds (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,strm)]
	      [,oth (fallthru oth)])
	    )])

;; [2007.03.14]
;; This desugars all types within the program by applying all type aliases.
(define-pass resolve-type-aliases
    (define aliases '())    ;; Mutated below:
    (define union-types #f) ;; Mutated below:
    (define Type (lambda (t) (export-type (dealias-type aliases union-types (instantiate-type t)))))
    ;(define Type (lambda (t) (dealias-type aliases t)))
    [Bindings (lambda (v* t* e* reconst Expr)
		(reconst v* (map Type t*) (map Expr e*)))]
    [Expr (lambda (x fallthru)
	    (match x [(assert-type ,[Type -> t] ,[e]) `(assert-type ,t ,e)]
		   [,oth (fallthru oth)]))]
    [Program (lambda(prog Expr)	  
	       (match prog
		 [(,inputlang '(program ,bod ,meta* ... ,type))
		  (fluid-let ([aliases (cdr (or (assq 'type-aliases meta*) 
						'(type-aliases)))]
			      [union-types (ASSERT (assq 'union-types meta*))])
		    `(resolve-type-aliases-language
		      '(program ,(Expr bod) 
			        ;,@(remq (assq 'type-aliases meta*) meta*)
			        ,@meta*
				,type)))]))]
    ;; Now we're free of sugars and can use the initial grammar.
    [OutputGrammar initial_regiment_grammar])


;; Pass properties: complete-ast-coverage
;; 
;; Handles: set!, for, effectful prims
(define-pass ws-normalize-context
    [Expr (lambda (x fallthru)
	    (match x  
	      ;; This catches all effectful prims/constructs and puts them in effect context.
	      ;[(break)           `(begin (break)       (tuple))]
	      ;[(emit ,[vq] ,[e]) `(begin (emit ,vq ,e) (tuple))]
	      [(set! ,v ,[e])    `(begin (set! ,v ,e)  (tuple))]
	      [(for (,i ,[st] ,[en]) ,[bod]) `(begin (for (,i ,st ,en) ,bod) (tuple))]
	      [(,prim ,[simple] ...) (guard (assq prim wavescript-effectful-primitives))
	       (let ([entry (assq prim wavescript-effectful-primitives)])
		 (match (caddr entry)
		   [(quote ,v) `(begin (,prim . ,simple) 'BOTTOM)]
		   [#()        `(begin (,prim . ,simple) 'UNIT)]))]
	      [,oth (fallthru oth)]))])


; --mic
(define (first-true ls p)
  (cond ((null? ls) #f)
        ((p (car ls)) (car ls))
        (else (first-true (cdr ls) p))))

; --mic
(define-pass propagate-copies
    ; FIXME: count all var. lookup stats
    [Expr (letrec ((do-expr
                    ; substs is a hash table of variable substitions
                    (lambda (x fallthru substs)

                      (match x

                        [(let ((,v1 ,t1 ,v0)) ,body)
                         (guard (and (symbol? v0)
                                     (not (and (pair? t1)
                                               (eq? (car t1) 'Ref)))))
                         (begin
                           (let ((subst-binding (first-true substs (lambda (b) (eq? v0 (car b))))))
                             (fallthru
                              body
                              (lambda (x f)
                                (do-expr x f (cons `(,v1 . ,(if subst-binding (cdr subst-binding) v0))
                                                   substs)))))
                           )]
                        
                        [,var
                         (guard (symbol? var))
                              
                         ; newest
                         (let ((subst-binding (first-true substs (lambda (b) (eq? var (car b))))))
                           (if subst-binding
                               (cdr subst-binding)
                               (fallthru x)))]
                        
                        [,oth (fallthru oth)]))))

            (lambda (x f) (do-expr x f ())) )]
)



#;
     [Program (lambda (p E)
	       (fluid-let ([substs (make-default-hash-table)])
		 )
	       
	       )]
  
) ;; End module
