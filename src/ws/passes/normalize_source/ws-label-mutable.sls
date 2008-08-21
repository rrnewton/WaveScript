#!r6rs

;===============================================================================
;;;;      ---- Pass Label Mutable  ---- 
;;;;
;;;; This labels the mutable iterator state variables as "refs".
;;;; Accordingly, references to them must be dereferences.
;;;;
;;;; [2008.08.15] Now this has been extended so that any variable may be mutated, and promoted to "ref".
;;;; iterate bindings are not special at all.  This behaves like Scheme.
;;;;
;;;; .author Ryan Newton

;===============================================================================

(library (ws passes normalize_source ws-label-mutable)
  (export ws-label-mutable   ws-label-mutable-grammar )
  (import (except (rnrs (6)) error) (ws common))
  
  (define ws-label-mutable-grammar
    ;; Add Ref type:
    (cons '(Type ('Ref Type))
	  initial_regiment_grammar))

  (define (decorate-rhs rhs)
    ;; The ref cannot be contained within
    ;; any other expression because refs are
    ;; second class!  It must be bound directly to a var.
    (match rhs ;; no match recursion
      [(app Mutable:ref ,e) `(Mutable:ref ,e)]
      [(Mutable:ref ,e)      rhs]
      [(,annot ,p ,e) (guard (annotation? annot))
       (if (decorated? e) e
	   `(Mutable:ref ,rhs))
       ;`(,annot ,p ,(decorate-rhs e))
       ]
      [,else                `(Mutable:ref ,rhs)]))

  (define (decorated? rhs)
    ;; The ref cannot be contained within
    ;; any other expression because refs are
    ;; second class!  It must be bound directly to a var.
    (match rhs ;; no match recursion
      [(app Mutable:ref ,e) #t]
      [(Mutable:ref ,e)     #t]
      [(,annot ,p ,[e]) (guard (annotation? annot)) e]
      [,else                #f]))

  ;; This finds all the variables affected by "set!"
  ;; It tags them at their binding sites (if they aren't already).
  (define-pass ws-gather-mutable 
      ;; The driver in this case returns a vector of two values: 
      ;;  (1) The new expression.
      ;;  (2) The list (set) of set!'d vars 
      [Expr (lambda (xp fallthru)
	    (match xp
	      [(set! ,v ,[_e])   
	       (match _e
		 [#(,e ,mut) (vector `(set! ,v ,e) (set-cons:list v mut))])]
	      [(,lett ([,lhs* ,ty* ,[_rhs*]] ...) ,[_bod])
		(guard (eq-any? lett 'let 'letrec))
		(match _bod
		  [#(,bod ,bod_setbanged)
		   (match _rhs*
		     [(#(,rhs* ,rhs_setbanged*) ...)
		      (define allrhs_banged (apply union rhs_setbanged*))
		      (when (eq? lett 'letrec)
			(for-each (lambda (lhs)
				    (when (memq lhs allrhs_banged)
				      (error 'ws-label-mutable
					     "Invalid set! to variable bound in recursive binding: ~a" lhs)))
			  lhs*))
		      (vector
		       `(,lett ,(map (lambda (lhs ty rhs)				       
				       (if (memq lhs bod_setbanged)					   
					   (list lhs 
						 (if (decorated? rhs) ty `(Ref ,ty))
						 (decorate-rhs rhs))
					   (list lhs ty rhs)))
				  lhs* ty* rhs*)
			  ,bod)
		       ;; Pass upwards set-banged free variables:
		       (difference (union allrhs_banged bod_setbanged)
				   lhs*))])])]
	      [,oth (fallthru oth)]))]
    [Fuser (lambda (vecs k)
	     (match vecs
	       [(#(,exp* ,mutable*) ...)
		(vector (apply k exp*)
			(apply union mutable*))]))]
    [Program 
     (lambda (prog Expr)
       (apply-to-program-body (lambda (x) (vector-ref (Expr x) 0)) prog))])
 
  ;; This walks down the tree and brings with it a list of variables that are mutable.
  ;; All references to them are wrapped in "deref"s.  The input program may not have any derefs.
  (define-pass ws-add-derefs
    (define Expr
      (lambda (exp mutable-at-bind)
	  (core-generic-traverse 
	   (lambda (x fallthru)
	     (match x
	       [,v (guard (symbol? v)) (if (memq v mutable-at-bind) `(deref ,v) v)]	       	      
	       [(,letform ([,lhs* ,ty* ,rhs*] ...) ,bod)  (guard (memq letform '(let letrec)))
		(define mutable-lhs*
		  (filter id
		    (map (lambda (lhs rhs) (if (decorated? rhs) lhs #f))
			      lhs* rhs*)))
		(define new-mutable (append mutable-lhs* mutable-at-bind))
		`(,letform ,(map (lambda (lhs ty rhs)
				   (list lhs ty 
					 (Expr rhs (if (eq? letform 'letrec)
						       new-mutable  mutable-at-bind))))
			      lhs* ty* rhs*)
			   ,(Expr bod new-mutable))]
	       [(deref ,v) (error 'ws-label-mutable "The input program should not already have any derefs: ~a" `(deref ,v))]
	       [,oth (fallthru oth)]))
	   (lambda (ls k) (apply k ls)) ;; generic fuser
	   exp)))
    [OutputGrammar ws-label-mutable-grammar]
    [Program 
     (lambda (prog __)
       (apply-to-program-body (lambda (exp) (Expr exp '())) prog))])
  
  ;(define ws-label-mutable (compose ws-add-derefs ws-gather-mutable))
  (define (ws-label-mutable  prog)
    (let ([x (ws-gather-mutable prog)])
      ;(inspect x);(inspect (strip-annotations x 'src-pos))
      (ws-add-derefs x)
      ;x
      ))

) ;; End module

