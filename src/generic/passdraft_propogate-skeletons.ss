;; [2004.07.30] Pass propogate-skeletons

;; FOR NOW I USE THE GLOBAL TREE!!


;; This pass addresses the problem that a fold may use the
;; gradient-skeleton from a different token than the one from which it
;; gets its values.  There's a seperate *What* and *Where*.

;; It changes primapps of this type:
;; (fold <Simple> <Simple> <Simple>)

;; into this:
;; (foldwith <Token> <Simple> <Simple> <Simple>)

;; Where the "with token" means "using the back-pointer skeleton from
;; that token channel".

(define propogate-skeletons
  (lambda (expr)
    (match expr
	   [(annotate-heartbeats-lang (quote (program (props ,proptable ...) 
						      ,(lazy-letrec ,binds ,fin))))

    (define (simple? expr)
      (match expr
	     [,var (guard (symbol? var)) #t]
	     [(quote ,const) (guard (or (symbol? const) (constant? const))) #t]
	     [,else #f]))

    (define (check-prop p s)
	     (let ((entry (assq s proptable)))
	       (if entry (memq p (cdr entry))
		   (error 'pass10_deglobalize:check-prop
			  "This should not happen!  ~nName ~s has no entry in ~s."
			  s proptable))))    

    (define process-let
      (lambda (expr)
        (match expr
	  [(lazy-letrec ([,lhs* ,heartbeat* ,[process-expr -> rhs*]] ...) ,fin)
	   `(lazy-letrec ([,lhs* ,heartbeat* ,rhs*] ...) ,fin)])))

    ;; This only works because of the harsh restrictions on the
    ;; language. We have the whole datapath right there for us to
    ;; trace.  As the language becomes more flexible, we'll need to do
    ;; this *dynamically*.
    (define (find-skeleton name)
;      ;; If it's a *region* that means (FOR NOW) that means it has a skeleton.
;      ;; <TODO> intersection and stuff will screw this up and require
;      ;; a more principled approach.
;;      (if (check-prop 'area name)
;	  (match (assq name binds)

;;FOR NOW I USE THE GLOBAL TREE...		 
		 
    (define process-expr
      (lambda (expr)
        (match expr
	  [,exp (guard (simple? exp)) exp]
          [(lambda ,formalexp ,[process-let -> letexp])
	   `(lambda ,formalexp ,letexp)]
          [(if ,test ,conseq ,altern)
	   `(if ,test ,conseq ,altern)]
          [(fold ,fun ,seed ,region)
	   `(foldwith ,(find-skeleton ,region) ,fun ,seed ,region)]
          [(,prim ,rand* ...)
           (guard (regiment-primitive? prim))
	   `(,prim ,rand* ...)]
          [,unmatched
	   (error 'TEMPLATE "invalid syntax ~s" unmatched)])))


	 ;; Body of match case:    
	 `(propogate-skeletons-lang
	   (quote (program (props ,proptable ...)
			   ,(process-let `(lazy-letrec ,binds ,fin)))))])))))

