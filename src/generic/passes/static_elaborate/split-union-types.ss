
;; To have a monomorphic program, it is necessary that we replicate
;; the union-type declarations for each type they are instantiated at.

;; FIXME [2007.10.11] UNFINISHED

;; CURRENTLY THIS ONLY PRUNES OUT UNUSED UNION TYPE DECLS!

(module split-union-types mzscheme
  (require "../../../plt/common.ss"
           "../normalize_source/typecheck.ss"
	   "../static_elaborate/static-elaborate.ss"
	   )
  (provide split-union-types)
  (chezprovide )
  (chezimports )
  
  (define-pass split-union-types
    (define instancecounts 'uninit) ;; Counts instances of each type.
    (define varianttable 'uninit) ;; Maps constructor name to type name.
    (define (Type ty)
      (match ty
	[(Sum . ,_) (inspect _)]
	
	[,s (guard (symbol? s)) s]
	[#(,[t*] ...) (list->vector t*)]
	[(,[arg] ... -> ,[ret]) `(,@arg -> ,ret)]
	[(,qt ,v) (guard (memq qt '(quote NUM)) (symbol? v)) `(,qt ,v)]
	[(,C ,[t*] ...) (guard (symbol? C)) (cons C t*)])
      )
    [Bindings 
     (lambda (vars types exprs reconstr exprfun)
       (reconstr vars (map Type types) (map exprfun exprs)))]
    [Expr (lambda (x fallthru)
	    (match x 
	      ;; FIXME:: WE JUST DONT TOUCH THE ASCRIPTIONS:
	      [(assert-type ,ty ,[e])  `(assert-type ,ty ,e)]
	      [,other (fallthru other)]))]
    [Program 
     (lambda(prog Expr)	  
       (match prog
	 [(,inputlang '(program ,bod ,meta* ... ,type))
	  (let ([union-types (cdr (or (assq 'union-types meta*) '(union-types)))])

	    (fluid-let ([varianttable (make-default-hash-table (* 5 (length union-types)))]
			[instancecounts (make-default-hash-table (* 5 (length union-types)))])
	      (for-each (match-lambda (((,name . ,_) . ,variants))		
		(hashtab-set! instancecounts name 0)
		(for-each (match-lambda ((,tc ,ty))			    
			    (hashtab-set! varianttable tc name)
			    ) variants))
		union-types)
	      
	      (let ([newty (Type type)]
		    [newbod (Expr bod)])		
		(define pruned
		  (filter (lambda (entry)
			    (not (zero? (hashtab-get instancecounts (caar entry)))))
		    union-types))
		`(,inputlang '(program ,newbod (union-types ,@pruned) ,(remq 'union-types meta*) ... ,newty)))
	      )
	    )]))]
    ) ;; End pass

) ;; End module
