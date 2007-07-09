
;===============================================================================
;;;;      ---- Pass Anihilate Higher-Order  ---- 
;;;;
;;;; Now that we are post-elaboration, it should be safe to inject
;;;; some more second-class refs.  This pass reduces higher order
;;;; procedures over plain data (array, list, *not* Stream) into first
;;;; order code.  Generally into first order code involving for/while
;;;; loops and side effects.
;;;;
;;;; .author Ryan Newton

;===============================================================================

(module anihilate-higher-order mzscheme
  (require "../../../plt/common.ss"
	   "../static_elaborate/verify-elaborated.ss")
  (provide anihilate-higher-order anihilate-higher-order-grammar )
  (chezimports)
  
  (define anihilate-higher-order-grammar
    ;; Add Ref type:
    (cons '(Type ('Ref Type))
	  (filter (lambda (x)
		    (match x
		      [(Prim ',p) (not (assq p higher-order-primitives))]
		      [,else #t]))
	    verify-elaborated-grammar)))

  ;; This labels the mutable iterator state variables as "refs".
  ;; Accordingly, references to them must be dereferences.
  (define-pass anihilate-higher-order
    (define (Expr x fallthru)
      (match x

	[(List:map (lambda (,v) (,ty) ,[e1]) ,[e2])
	 (let ([out (unique-name 'outls)]
	       [newty (unique-name 'notyy)]
	       [ptr (unique-name 'ptr)]
	       [i (unique-name 'i)])		
	   `(let ([,ptr (Ref (List ,ty)) (Mutable:ref ,e2)]
		  [,out (Ref (List ',newty)) (Mutable:ref '())])
	      (begin 
		;; Inefficient!  Tests list length here.  Use while loop!
		;; (Or could have primitive to reverse AND return length...)
		(for (,i '1 (List:length (deref ,ptr)))
		    (begin 
		      (set! ,out 
			    (cons 
			     (let ([,v ,ty (car (deref ,ptr))]) ,e1)
			     (deref ,out)))
		      (set! ,ptr (cdr (deref ,ptr)))))
		;; Return reverse of the accumulator:
		;; (Shouldbe reverse!))
		(List:reverse (deref ,out)))))]
	[(List:fold (lambda (,acc ,v) (,ty1 ,ty2) ,[e1]) ,[zer] ,[e2])
	 (let ([x (unique-name 'acc)]
	       [ptr (unique-name 'ptr)]
	       [i (unique-name 'i)])		
	   `(let ([,ptr (Ref (List ,ty2)) (Mutable:ref ,e2)]
		  [,x   (Ref ,ty1) (Mutable:ref ,zer)])
	      (begin 
		;; Inefficient!  Test list length here.
		;; (Could have primitive to reverse and return length...)
		(for (,i '1 (List:length (deref ,ptr)))
		    (begin 
		      (set! ,x
			    (let ([,v ,ty2 (car (deref ,ptr))]
				  [,acc ,ty1 (deref ,x)])
			      ,e1))
		      (set! ,ptr (cdr (deref ,ptr)))))
		(deref ,x))))]

	;; This is not higher-order, but what the heck, it fits:
	[(Array:toList ,[arr])
	 (let ([notype (unique-name 'notype)]
	       [tmp (unique-name 'tmp)]
	       [out (unique-name 'out)]
	       [len (unique-name 'len)]
	       [i (unique-name 'i)])
	   `(let ([,tmp (Array ',notype) ,arr]
		  [,out (Ref (List ',notype)) (Mutable:ref '())])
	      (let ([,len Int (Array:length ,tmp)])
		(begin 
		  (for (,i '1 ,len)
		   (set! ,out (cons (Array:ref ,tmp (-_ ,len ,i)) (deref ,out))))
		  (deref ,out)))))]

	[(Array:map (lambda (,v) (,ty) ,[e1]) ,[e2])
	 (let ([tmp (unique-name 'tmp)]
	       [out (unique-name 'outls)]
	       [newty (unique-name 'noty)]
	       [i (unique-name 'i)])		
	   ;; Need Array:makeZeroed or Array:makeUNSAFE !!
	   `(let ([,tmp (Array ,ty) ,e2])
	      (if (wsequal? ,tmp Array:null)
		  Array:null
		  (let ([,out (Array ',newty)
			      ;(Array ,ty)
			      (Array:makeUNSAFE (Array:length ,tmp) 
					  ;(Array:ref ,tmp '0)
					  )])
		    (begin 
		      (for (,i '0 (-_ (Array:length ,tmp) '1))
			  (Array:set ,out ,i
		             (let ([,v ,ty (Array:ref ,tmp ,i)]) ,e1)))
		      ,out)))))]
	[(Array:fold (lambda (,acc ,v) (,ty1 ,ty2) ,[e1]) ,[zer] ,[e2])
	 (let ([tmp (unique-name 'tmp)]
	       [x (unique-name 'acc)]
	       [i (unique-name 'i)])
	   `(let ([,tmp (Array ,ty2) ,e2]
		  [,x (Ref ,ty1) (Mutable:ref ,zer)])
	      (begin 
		(for (,i '0 (-_ (Array:length ,tmp) '1))
		    (set! ,x 
		       (let ([,acc ,ty1 (deref ,x)]
			     [,v ,ty2 (Array:ref ,tmp ,i)])
			 ,e1)))
		(deref ,x))))]

	[(Array:andmap (lambda (,v) (,ty) ,[e1]) ,[e2])
	 (let ([tmp (unique-name 'tmp)]
	       [x (unique-name 'flag)]
	       [i (unique-name 'i)]
	       [len (unique-name 'len)])
	   `(let ([,tmp (Array ,ty) ,e2]
		  [,x (Ref Bool) (Mutable:ref '#t)]
		  [,i (Ref Int) (Mutable:ref '0)])
	      (let ([,len Int (Array:length ,tmp)])
		(begin 
		  (while (if (deref ,x) (< (deref ,i) ,len) '#f)
			 (begin (if (let ([,v ,ty (Array:ref ,tmp (deref ,i))]) ,e1)
				    (tuple)
				    (set! ,x '#f))
				(set! ,i (+_ (deref ,i) '1))))
		  (deref ,x)))
	      ))]

	[(Array:build ,[n] (lambda (,v) (Int) ,[e1]))
	 (let ([notype (unique-name 'notype)]
	       [out (unique-name 'outls)]
	       [len (unique-name 'len)]
	       [i (unique-name 'i)])
	   `(let ([,len Int ,n])
	      (let ([,out (Array ',notype) (Array:makeUNSAFE ,len)])
		(begin 
		  (for (,i '0 (-_ ,len '1))
		      (Array:set ,out ,i (let ([,v Int ,i]) ,e1)))
		  ,out))))]
	
;     ;;(List:filter (('a -> Bool) (List 'a)) (List 'a))

;     (Array:andmap      (('a -> Bool) (Array 'a))            Bool)

	;; Safety net:
	[(,prim ,rands ...) (guard (assq prim higher-order-primitives))
	 (error 'anihilate-higher-order "missed this, possible not fully elaborated:\n  Code: ~a\n  Location: ~a\n" 
		(get-snippet `(,prim ,@rands))
		(get-location `(,prim ,@rands))
		)]
	
	[,oth (fallthru oth)]))

    [Expr Expr]
    [OutputGrammar anihilate-higher-order-grammar])
  
) ;; End module



#;
(check-grammar "" '(unionN s_1 s_2) anihilate-higher-order-grammar 'Expr)

