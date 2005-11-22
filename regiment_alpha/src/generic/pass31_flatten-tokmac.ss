;; [2005.11.22] NOTE! leds forms are dissappearing through here!!


;; Pass Flatten Token Machine

;; TODO: Going to need to associate types with local vars at some point.

;; [2005.11.09] 
;; This pass is a port of my haskell code.  
;; I want to consolidate things into Scheme since the haskell port never happened.





(define flatten-tokmac
  (build-compiler-pass ;; This wraps the main function with extra debugging
   'flatten-tokmac
   `(input)
   `(output );(grammar ,full_but_clean_tml PassInput))
   (let () 
     ;(reg:define-struct (block decls stmts))
     
     ;; We rename all the local vars just to be safe.
     (define (rename-let-vars expr)
       (let loop ((expr expr) (subst '()))
	 (tml-generic-traverse
	  (lambda (x k) ;; Tree walker
	    (match x
	      [,v (guard (symbol? v)) 
		  (let ((entry (assq v subst)))
		    (if entry (cadr entry) v))]
	      [(let ((,lhs ,[rhs])) ,bod)
	       (let ((new (unique-name (deunique-name lhs))))
		 `(let ((,new ,rhs))
		    ,(loop bod (cons (list lhs new) subst))))]
	      [,o (k o)]))
	  (lambda (ls k) (apply k ls))
	  expr)))

     ;; The value returned by effect-only primitives/constructs
     (define voidval #f)

     ;; This is a seperate loop for things that occur in effect context.
;      (define Effect
;        (tml-generic-traverse
; 	;; We just take the normal value result:
; 	(lambda (x autoloop) (Value x))
; 	;; Then we make it side-effect only
; 	(lambda (ls f)
; 	  (let-match ((#(,vs ,ss ,x) ls)

     ;; Returns: #(vars statmnts retval)
     (define Value
       (tml-generic-traverse
	;; TREE WALKER:
	(lambda (x autoloop)
	  ;; The fuser really does most of the work here.  This walker simply
	  ;; handles the special cases:
	  (match x	    

	    ;; This is handled special because it SHOULD be considered basic even
	    ;; though it might have a sub-expr:
	    [(tok ,t ,[ind])
	     (let-match ([#(,vs ,ss ,x) ind])
	       (vector vs ss `(tok ,t ,x)))]

	    ;; These are sent off because they have side-effects only:

;; FIXME: [2005.11.09]  For now these can just return whatever.  It's unspecified after all.
	    
; 	    [(leds ,what ,which)  (vector () `(leds ,what ,which) voidval)]
; 	    [(set! ,v ,[sub]) ;(Effect `(set! ,v ,x))]
; 	     (let-match ((#(,vs ,ss ,x) sub))
; 	       (vector vs (snoc `(set! ,v ,x) ss) voidval))]
; 	    [(ext-set! ,[t] ,v ,[rhs])
; 	     (let-match ([#(,vs1 ,ss1 ,x1) t]
; 			 [#(,vs2 ,ss2 ,x2) rhs])
; 	       (vector (append vs1 vs2)
; 		       (append ss1 ss2 
; 			       `((ext-set! ,x1 ,v ,x2)))
; 		       voidval))]
			  
	    ;; Let dissappears entirely:
	    [(let ((,lhs ,[rhs])) ,[bod])
	     (let-match ([#(,v1 ,s1 ,x1) rhs]
			 [#(,v2 ,s2 ,x2) bod])
	       (vector (cons lhs (append v1 v2))
		       (append s1 `((set! ,lhs ,x1)) s2)
		       x2))]
	    ;; Begin doesn't introduce variables for its ignored effect prims:
	    ;; This could be handled automatically by the fuser below, but that would make more vars.
	    [(begin ,[exps] ...)
	     (let-match ([(#(,vs* ,ss* ,xs) ...) exps])
	       (vector (apply append vs*)
		       (apply append ss*)
		       ;; Return only value of last expression:
		       (rac xs)))]
	    ;; IF is special and must still stick around because it of course effects control flow:
	    [(if ,[t] ,[c] ,[a])
	     (let-match ([#(,vt ,st ,xt) t]
			 [#(,vc ,sc ,xc) c]
			 [#(,va ,sa ,xa) a])
	       (let ([tmp (unique-name 'if_ret)])		 
		 (vector (cons tmp (append vt vc va))
			 (append st 
				 `((if ,xt
				       (begin ,@sc (set! ,tmp ,xc))
				       (begin ,@sa (set! ,tmp ,xa)))))
			 tmp)))]
	    
            [,o (autoloop o)]
	    ))

	;; FUSER:
	(lambda (results reconstruct)
	  ;; If the results are null then this is a const/var/etc.  E.g. *it's BASIC*.
	  (if (null? results)
	      (vector () () (reconstruct))

	  (let-match (((#(,vs* ,ss* ,xs) ...)
		       ;; This is corny, but we just auto-wrap simple cases  -- assuming they're 
		       ;; effect free values -- so we don't have to handle them above:  
		       ;; This should catch const, vars, tokens: 
		       (map (lambda (x) (if (vector? x) x
					    (begin (disp "Wrapping:" x)
						   (vector () () x))))	    
			 results)))
	    ;; Now we construct a generic fused result.  
	    ;; Evaluate subexprs, bind to new vars, then bind final result to yet another new var.
	    (let (;(rands (map unique-name (make-list (length results) 'rand)))
		  (res   (unique-name 'tmp)))
	      (vector (apply append (list res) vs*)
		      ;; Put all the code blocks together:
		      (append (apply append ss*) ; `((set! ,(car rands) ,(car xs)
			      ;; Reconstruct the expression with our basic operands:
			      `((set! ,res ,(apply reconstruct xs))))
		      res)))))))

     (define (Tokbind tb)       
       (mvlet ([(tok id args stored bindings body) (destructure-tokbind tb)])
	 ;(disp "RENAMED:") (pp (rename-let-vars body))
	 (match (Value (rename-let-vars body))
	   [#(,vs ,ss ,x) 
	    `(,tok ,id ,args 
		   (stored ,@stored)
		   ;; The local variables become simple bindings:
		   ;; After this pass the "bindings" are the local variables for this function block:
		  ;; [2005.11.09] (Right now this bindings slot is unused, so there shouldn't be a problem.)
					;(bindings ,@(append `((,vs '0) ...)  bindings))
		   ;; Changed my mind on that... just one big let*:
		   (let* ([,vs '0] ...)
		     ;; [2005.11.09] For the time being we do return a value.
		     ;; That's just for the simulator (and if we're not using CPS).
		     (begin ,@ss ,x)))])))

     
     ;; This fills in some more boilerplate.
     (tml-simple-pass Value Tokbind 'flatten-tokmac-lang))))
