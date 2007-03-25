



(module explicit-stream-wiring mzscheme
  (require "../../../plt/common.ss")
  (provide explicit-stream-wiring )
  (chezimports)

;; This pass makes the forward-links explicit in the stream graph.
;; 
;; The output of this pass is no longer an expression in the original sense.
;; It's four things:
;;  1) A set of constant bindings (which may, in the future, include functions).
;;  2) A set of Sources: [v ty (prim const ...) (downstream-links ...)]
;;  3) A set of Iterates: [v ty fundef upstream (downstream-links ...)]
;;  4) A designated name of a source/iterate that returns to BASE<-
(define-pass explicit-stream-wiring 
    (define (Expr x aliases)
      (define (dealias v)
	(let ([entry (assq v aliases)])
	  (if entry (dealias (cadr entry)) v)))
      ;(unless (null? aliases) (printf "ALIASES: ~s\n" aliases))
      (match x
	;; Operators:
	[(let ([,v ,ty (iterate ,f ,[dealias -> sig])]) ,[bod])
	 (cons `[,sig -> ,v ,ty ,f] bod)]
	;; Sources:
	[(let ([,v (Stream ,ty) (,prim ,rands* ...)]) ,[bod])
	 (guard (assq prim wavescript-stream-primitives))
	 (cons `[-> ,v (Stream ,ty) (,prim . ,rands*)] bod)]
	;; Sources: (Annoying)
	[(let ([,v (Stream ,ty) (assert-type ,t (,prim ,rands* ...))]) ,[bod])
	 (guard (assq prim wavescript-stream-primitives))
	 (cons `[-> ,v (Stream ,ty) (,prim . ,rands*)] bod)]

	;; Alias:
	[(let ([,v1 (Stream ,ty) ,v2]) ,bod) (guard symbol? v2)
	 (Expr bod (cons (list v1 v2) aliases))]
	;; Constants:
	[(let ([,v ,ty ,rhs]) ,[bod])
	 (ASSERT (lambda (t) (not (deep-assq 'Stream t))) ty)
	 (cons `[CONST ,v ,ty ,rhs] bod)]
	;; Sink:
	[,v (guard (symbol? v)) `([BASE ,(dealias v)])]
	[,oth (error 'explicit-stream-wiring "unmatched query construct: ~s" oth)]
	))
  (define (decl->upstream d)
    (match d
      [(-> . ,_)            #f]
      [(,src -> ,dest . ,_) src]
      [(BASE  ,src)         src]
      [(CONST ,_)           #f]))
  (define (decl->name d) 
    (match d
      [(-> ,v . ,_)         v]
      [(,src -> ,dest . ,_) dest]
      [(BASE  ,src)         #f]
      [(CONST ,_)           #f]))
  (define (gather-refs v decls)
    (match decls
      [() ()]
      [((-> . ,_) . ,[rest])     rest]
      [((,src -> ,dest . ,_) . ,[rest]) 
       (guard (eq? v src))
       (cons dest rest)]
      [((BASE ,src) . ,[rest]) (guard (eq? v src))  (cons 'BASE rest)]
      [(,_ . ,[rest]) rest]))
  [Program 
   (lambda (p _)
     (match p
       [(,lang '(program ,e (Stream ,t)))
	(let ([decls (Expr e '())])
	  (define c*
	    (map cdr(filter (lambda (x) (eq? (car x) 'CONST)) decls)))
	  ;; Here we gather all the forward wirings. Quadratic:
	  (define src*
	    (apply append 
		   (map (lambda (d)
			  (match d
			    [(-> ,v ,ty ,app) `((,v ,ty ,app ,(gather-refs v decls)))]
			    [,_ ()]))
		     decls)))
	  (define iter*
	    (apply append 
		   (map (lambda (d)
			  (match d
			    [(,src -> ,v ,ty ,f) 
			     `((,v ,ty ,f ,src ,(gather-refs v decls)))]
			    [,_ ()]))
		     decls)))
	  (define base (cadr (assq 'BASE decls)))
	  `(explicit-stream-wiring-language
	    '(graph (const . ,c*)
		    (sources . ,src*)
		    (iterates . ,iter*)
		    (sink ,base ,t))))]))]
  )


) ;; End module
