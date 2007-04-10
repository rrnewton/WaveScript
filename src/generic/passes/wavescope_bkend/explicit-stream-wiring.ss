



(module explicit-stream-wiring mzscheme
  (require "../../../plt/common.ss")
  (provide explicit-stream-wiring )
  (chezimports)

;; This pass makes the forward-links explicit in the stream graph.
;; 
;; The output of this pass is no longer an expression in the original sense.
;; It's five things:
;;  1) A set of constant bindings (which may, in the future, include functions).
;;  2) A set of Sources:  [v ty (prim const ...) (downstream-links ...)]
;;  3) A set of Iterates: [v ty fundef upstream (downstream-links ...)]
;;  4) A set of UnionNs:   [v ty (upstream-link ...) (downstream-links ...)]
;;  5) A designated name of a source/iterate that returns to BASE<-
(define-pass explicit-stream-wiring 

    ;; This makes a first pass over the spine of the query and
    ;; gathers/tags the appropriate bits.
    (define (Expr x aliases)
      (define (dealias v)
	(let ([entry (assq v aliases)])
	  (if entry (dealias (cadr entry)) v)))
      ;(unless (null? aliases) (printf "ALIASES: ~s\n" aliases))
      (match x
	;; Operators:
	[(let ([,v ,ty (iterate ,f ,[dealias -> sig])]) ,[bod])
	 (cons `[,sig -> ,v ,ty ,f] bod)]
	;; UnionN: 
	[(let ([,v (Stream ,ty) (unionN ,[dealias -> S*] ...)]) ,[bod])
	 (cons `(,S* U-> ,v ,ty) bod)]
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

  ;; This gathers all the downstream operators that use a given named stream.
  (define (gather-refs v decls . unionedges)
    (let ([unionedges (if (null? unionedges) '() (car unionedges))])
      (match decls
	[() ()]
	[((-> . ,_) . ,[rest])     rest]
	[((,src ,arr ,dest . ,_) . ,[rest]) 
	 (guard (memq arr '(-> U->)) (eq? v src))
	 (cons dest rest)]
	[((BASE ,src) . ,[rest]) (guard (eq? v src))  
	 (cons 'BASE rest)]
	[(,_ . ,[rest]) rest])))


  ;; Tag the down edges that go into unionN with their indices.
  (define (add-indices src down* unionedges)
    (map (lambda (down)
	   (match (assq down unionedges)
	     [(,name (,up* ...))
	      (list (list-index down up*) dest)]
	     [,else down]))
      down*))


  [Program 
   (lambda (p _)
     (match p
       [(,lang '(program ,e ,meta* ... (Stream ,t)))
	(let ([decls (Expr e '())])

	  ;; Now we need to make a second pass to separate the
	  ;; different types of construct and to gather references.
	  (define cb* (map cdr (filter (lambda (x) (eq? (car x) 'CONST)) decls)))

	  ;; Here we gather all the forward wirings. Quadratic:
	  
	  ;; First we get these, because we use them below:
	  (define unionedges
	    (apply append 
		   (map (lambda (d)
			  (match d
			    [(,src* U-> ,v ,ty ,f) `((,v ,src*))]
			    [,_ ()]))
		     decls)))

	  ;; Currently only unionN supported...
	  (define union*
	    (apply append 
		   (map (lambda (d)
			  (match d
			    [(,src* U-> ,v ,ty)
			     `((,v ,ty ,src* 
				   ,(add-indices v (gather-refs v decls) unionedges)))]
			    [,_ ()]))
		     decls)))
	  (define src*
	    (apply append 
		   (map (lambda (d)
			  (match d
			    [(-> ,v ,ty ,app) `((,v ,ty ,app 
						    ,(add-indices v (gather-refs v decls) unionedges)))]
			    [,_ ()]))
		     decls)))
	  (define iter*
	    (apply append 
		   (map (lambda (d)
			  (match d
			    [(,src -> ,v ,ty ,f)
			     `((,v ,ty ,f ,src ,(add-indices v (gather-refs v decls) unionedges)))]
			    [,_ ()]))
		     decls)))
	 
	  (define base (cadr (assq 'BASE decls)))

	  (ASSERT (curry apply =) (list (length decls) 
					(+ 1 (length iter*) (length src*) (length union*) (length cb*))))

	  `(explicit-stream-wiring-language
	    '(graph (const     . ,cb*)
		    (sources   . ,src*)
		    (iterates  . ,iter*)
		    (unionNs    . ,union*)
		    (sink ,base ,t))))]))]
  )


) ;; End module
