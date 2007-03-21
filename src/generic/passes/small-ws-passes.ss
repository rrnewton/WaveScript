

;;;; This defines a handful of small passes used by WaveScript that
;;;; don't quite each deserve their own files.


(module small-ws-passes mzscheme
  (require "../../plt/common.ss"
	   "normalize_query/ws-remove-complex-opera.ss"
	   )
  (provide 
           introduce-lazy-letrec
	   lift-polymorphic-constant
	   unlift-polymorphic-constant
	   ;purify-letrec  ;; Disabled
	   standardize-iterate
	   kill-polymorphic-types
	   ;ws-add-return-statements  ;; Disabled
	   resolve-type-aliases
	   ws-normalize-context
           )
  (chezimports)
  (require-for-syntax "../../plt/common.ss")


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
	      ['()     (printf "Lifting null!\n") (f x)]
	      [(Array:makeUNSAFE ,[n]) (f `(Array:makeUNSAFE ,n))]
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
	    [(let ([,v1 ,t ,c]) ,v2)
	       (guard (eq? v1 v2) (pconst? c))
	       (ASSERT (not (polymorphic-type? t)))
	       `(assert-type ,t ,c)]
	    [,c (guard (pconst? c))
		(error 'unlift-polymorphic-constant "missed polymorphic const: ~s" c)]
	    [,other (fallthru other)]))])



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

;; This little pass 
(define-pass standardize-iterate
    [Expr (lambda (x fallthru)
	    (match x
	      [(iterate (let ,binds (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,[strm])
	       `(iterate (let ,binds (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,strm)]
	      [(iterate (lambda (,x ,y) (,tyx ,tyy) ,bod) ,[strm])
	       `(iterate (let () (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,strm)]
	      [(iterate ,_ ...)
	       (error 'standardize-iterate "shouldn't have missed this iterate: ~s" `(iterate ,_ ...))]
	      [,oth (fallthru oth)])
	    )])

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
  [Bindings (lambda (var* ty* expr* reconstr Expr)
	      (reconstr var* (map Type ty*) (map Expr expr*)))])

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
    (define aliases '())
    (define (Type t)
      (match t
	[,s (guard (symbol? s))                   
	    (let ([entry (or (assq s aliases)
			     (assq s regiment-type-aliases))])
	      (if entry 
		  (begin (DEBUGASSERT (= 2 (length entry)))
			 (cadr entry))
		  s))]
	[',n                                     `(quote ,n)]
	;;['(,n . ,v)                               (if v (Type v) `(quote ,n))]
	[(NUM ,v) (guard (symbol? v))            `(NUM ,v)]
	[(NUM (,v . ,t))                          (if t (Type t) `(NUM ,v))]

	;; This is simple substitition of the type arguments:
	[(,s ,[t*] ...) (guard (symbol? s))
	 (let ([entry (or (assq s aliases)
			  (assq s regiment-type-aliases))])
	   (match entry
	     [#f `(,s ,t* ...)]
	     [(,v ,rhs) (error 'resolve-type-aliases 
			       "alias ~s should not be instantiated with arguments!: ~s" 
			       s (cons s t*))]
	     [(,v (,a* ...) ,rhs)
	      ;; We're lazy, so let's use the existing machinery
	      ;; to do the substition.  So what if it's a little inefficient.
	      (match (instantiate-type `(Magic #(,a* ...) ,rhs))
		[(Magic #(,cells ...) ,rhs)
		 ;; Now use the unifier to set all those mutable cellS:
		 (for-each (lambda (x y) (types-equal! x y "<resolve-type-aliases>"))
		   cells t*)
		 (export-type rhs)])]))]
	[(,[arg*] ... -> ,[res])                 `(,arg* ... -> ,res)]
	[#(,[t*] ...)                            (apply vector t*)]
	[,other (error 'resolve-type-aliases "bad type: ~s" other)])
      )
    [Bindings (lambda (v* t* e* reconst Expr)
		(reconst v* (map Type t*) (map Expr e*)))]
    [Expr (lambda (x fallthru)
	    (match x [(assert-type ,[Type -> t] ,[e]) `(assert-type ,t ,e)]
		   [,oth (fallthru oth)]))]
    [Program (lambda(prog Expr)	  
	       (match prog
		 [(,inputlang '(program ,bod ,type)) prog]
		 [(,inputlang '(program ,bod (type-aliases ,alias* ...) ,type))
		  (fluid-let ([aliases alias*])		    
		    `(resolve-type-aliases-language
		      '(program ,(Expr bod) ,type)))]))]
    ;; Now we're free of sugars and can use the initial grammar.
    [OutputGrammar initial_regiment_grammar])


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

  
) ;; End module
