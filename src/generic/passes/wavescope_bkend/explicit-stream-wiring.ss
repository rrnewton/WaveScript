

;;; NOTE: This is currently unnecessarily complexified by the
;;; existence of unionN AND merge.  Reduce these to one operator.

(module explicit-stream-wiring mzscheme
  (require "../../../plt/common.ss")
  (provide explicit-stream-wiring )
  (chezimports)

;; These are the types of objects we dig up in the code as we process it.

(reg:define-struct (constbind name type code))
(reg:define-struct (initcode code))
(reg:define-struct (source name type code))
(reg:define-struct (sink name))
(reg:define-struct (operator name type code src*))

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
    
    ;; Sort out which primitives are sources vs. intermediate points
    ;; in the query graph.
    (define source-prims
      (map car
        (filter (match-lambda ((,name ,args ,result)) (not (deep-assq 'Stream args)))
          wavescript-stream-primitives)))
    (define nonsource-prims
       (map car 
	(filter (match-lambda ((,name ,args ,result)) (deep-assq 'Stream args))
	  wavescript-stream-primitives)))

    (define (unionN? pr) (and (pair? pr) (eq? (car pr) 'unionN)))
    (define (merge? pr) (and (pair? pr) (eq? (car pr) '_merge)))
    (define (annotated-prim? pr)
      (and (pair? pr) (memq (car pr) '(iterate _merge unionN __readFile))))

    (define (sourceapp? pr)    (and (pair? pr) (memq (car pr) source-prims)))
    (define (nonsourceapp? pr) (and (pair? pr) (memq (car pr) nonsource-prims)))

    ;; This makes a first pass over the spine of the query and
    ;; gathers/tags the appropriate bits.
    (define (Expr x aliases)
      (define (dealias v)
        (ASSERT symbol? v)
        (let ([entry (assq v aliases)])
          (if entry (dealias (cadr entry)) v)))
      (unless (null? aliases) (printf "ALIASES: ~s\n" aliases))
      (match x
	;; Operators: This includes iterate:
	[(let ([,v ,ty ,rhs]) ,[bod])
	 (guard (let ([peeled (peel-annotations rhs)])
		  (or (nonsourceapp? peeled)
		      (unionN? peeled)
            (annotated-prim? peeled))))
	 (define (prim-entry op rand*)
	   (if (eq? op 'unionN)
	       `(,op ((List Annotation) . ,(map (lambda (_) '(Stream 'a)) (cdr rand*)))
                ((List Annotation) (Stream #(Int 'a))))
	       (get-primitive-entry op)))
	 (match (peel-annotations rhs)

      ;; added to peel off the (new) annotations from iterates
      [(iterate ,annot ,rand* ...)
       (match (prim-entry 'iterate `(,annot . ,rand*))
         [(,_ (,annotty . ,argty*) ,result)
          (let ([src*
                 (apply append
                   (map (lambda (rand argty)
                          (match argty
                            [(Stream ,_)
                             (ASSERT symbol? rand)
                             (list (dealias rand))]
                            [,oth (DEBUGASSERT (not (deep-assq 'Stream oth))) ()]))
                     rand* argty*))])
            (cons (make-operator v ty `(iterate ,annot . ,rand*) src*)
                  bod))])]

      ;; added to peel off the (new) annotations from any prim.
      [(,prim ,annot ,rand* ...)
       (guard (and (pair? annot) (eq? 'annotations (car annot))))
       (match (prim-entry prim `(,annot ,@rand*))
         [(,_ ,argty* ,result)
          (let ([src*
                 (apply append
                        (map (lambda (rand argty)
                               (match argty
                                 [(Stream ,_)
                                  (ASSERT (symbol? rand))
                                  (list (dealias rand))]
                                 [,oth (DEBUGASSERT (not (deep-assq 'Stream oth))) ()]))
                          rand* (cdr argty*)))])
            (cons (make-operator v ty `(,prim ,annot . ,rand*) src*)
                  bod))])]

	   [(,prim ,rand* ...)
	    (match (prim-entry prim rand*)
	      [(,_ ,argty* ,result)
	       #;
	       (unless (= (length argty*) (length rand*))
            (inspect (vector rand* argty*)))
	       (let ([src* 
                 (apply append 
                        (map (lambda (rand argty)
                               (match argty
                                 [(Stream ,_) 
                                  (ASSERT symbol? rand)
                                  (list (dealias rand))]
                                 [,oth (DEBUGASSERT (not (deep-assq 'Stream oth)))
                                       ()]))
                          rand* argty*))])
            (cons (make-operator v ty `(,prim . ,rand*) src*)
                  bod))])])]

	[(let ([,v ,ty (iterate ,annot . ,_)]) ,[bod])
	 (inspect (cons 'iterate _))]

	;; Sources
	[(let ([,v (Stream ,ty) ,rhs]) ,[bod])
	 (guard (sourceapp? (peel-annotations rhs)))
	 (cons (make-source v `(Stream ,ty) (peel-annotations rhs))
	       bod)]

	;; Alias:
	[(let ([,v1 (Stream ,ty) ,v2]) ,bod) (guard (symbol? v2))	 
	 (Expr bod (cons (list v1 v2) aliases))]

	;; This is not necessarily the only way to do this.  We gather
	;; all start-up time effects.  Later we will bundle them into
	;; one initialization function.
	[(begin ,effect* ... ,[bod])
	 (cons (make-initcode effect*) bod)]

	;; Constants: Theoretically we could wrap up side effecting
	;; code (from a 'begin') into the bindings for the constants.
	;; That would be a bit limiting.  For example, there's nowhere
	;; to put a for-loop that fills TWO constant array bindings.
	;;
	[(let ([,v ,ty ,rhs]) ,[bod])
	 (ASSERT (lambda (t) (not (deep-assq 'Stream t))) ty)
	 (cons (make-constbind v ty rhs) bod)]

	;; Sink:
	[,v (guard (symbol? v)) 
	    (ASSERT symbol? v)
	    (list (make-sink (dealias v)))]

	;; Safety net:
	[(,op ,_ ...) (guard (or (memq op '(UnionN iterate))
				 (assq op wavescript-stream-primitives)))
	 (error 'explicit-stream-wiring "missed a construct: ~s" op)]

	[,oth (error 'explicit-stream-wiring "unmatched query construct: ~s" oth)]
	))
  (define (decl->upstream d)
    (cond 
     [(operator? d) (operator-src* d)]
     [(source? d)   #f]
     [(sink? d)     (list (sink-name d))]
     [(constbind? d) #f]
     [(initcode? d) #f]
     [else (error 'decl->upstream "bad decl: ~s" d)]))
  (define (decl->name d)
    (cond 
     [(operator? d) (operator-name d)]
     [(source? d)   (source-name   d)]
     [(sink? d)      'BASE]
     [(constbind? d) #f]
     [(initcode? d)  #f]
     [else (error 'decl->upstream "bad decl: ~s" d)]))

  ;; This gathers all the downstream operators that use a given named stream.
  (define (gather-refs v decls . unionedges)
    (let ([unionedges (if (null? unionedges) '() (car unionedges))])
      (filter id
	(map (lambda (d) 
	       (let ([upstrm (decl->upstream d)])
		 (if (and upstrm (memq v upstrm))
		      (decl->name d)
		      #f)))
	  decls))))

  ;; Tag the down edges that go into unionN with their indices.
  (define (add-indices src down* unionedges)
    (map (lambda (down)
	   ;; If one of our out edges goes to a union:
	   (match (assq down unionedges)
	     [(,name (,up* ...))
	      (list (ASSERT (list-find-position src up*)) down)]
	     [,else down]))
      down*))

  [Program 
   (lambda (p _)
     (match p
       [(,lang '(program ,e ,meta* ... (Stream ,t)))
	(let ([union-types (cdr (ASSERT (assq 'union-types meta*)))]
	      [decls (Expr e '())])

;	  (define ___ (inspect decls))

	  ;; Now we need to make a second pass to gather references
	  ;; and unpack what we found into an Sexp.
	  (define cb*   
	    (map (lambda (cb) 
		   `(,(constbind-name cb) ,(constbind-type cb) ,(constbind-code cb)))
	      (filter constbind? decls)))
	  (define init* (map initcode-code  (filter initcode? decls)))
	  
	  ;; Here we gather all the forward wirings. Quadratic:
	  
	  ;; First we get these, because we use them below:

	  (define unionedges
	    (apply append 
		   (map (lambda (d)
			  (if (and (operator? d) 
				   (eq? 'unionN (car (operator-code d))))
			      `((,(operator-name d) ,(operator-src* d)))
			      ()))
		     decls)))

#;
	  ;; Currently only unionN, merge supported...
	  (define union*
	    (apply append 
		   (map (lambda (d)
			  (match d
			    [(,src* U-> ,v ,ty)
			     `((union 
				(name ,v) 
				(output-type ,ty) 
				(incoming ,@src*)
				(outgoing ,@(add-indices v (gather-refs v decls) unionedges))))]
			    [(,a ,b M-> ,v ,ty)
			     `((merge 
				(name ,v) 
				(output-type ,ty) 
				(incoming ,a ,b)
				(outgoing ,@(add-indices v (gather-refs v decls) unionedges))))]
			    [,_ ()]))
		     decls)))

	  (define src*
	    (apply append 
		   (map (lambda (d)
			  (if (source? d) 
			      `(((name ,(source-name d))
				 (output-type ,(source-type d))
				 (code ,(source-code d))
				 (outgoing ,@(add-indices (source-name d) 
							  (gather-refs (source-name d) decls)
							  unionedges))
				 ))
			      ()))
		     decls)))

	  (define op*
	    (apply append 
		   (map (lambda (d)
                          (if (operator? d)
			      `(
				(,(car (operator-code d))
				 (name ,(operator-name d)) 
				 (output-type ,(operator-type d))
				 (code ,(operator-code d))
				 (incoming ,@(operator-src* d))
				 (outgoing ,@(add-indices (operator-name d)
							  (gather-refs (operator-name d) decls)
							  unionedges))))
			      ()))
		     decls)))
          
	  (define basename
	    (let ([ls (filter sink? decls)])
	      (ASSERT (fx= (length ls) 1))
	      (sink-name (car ls))))	  

	  (ASSERT (curry apply =) 
		  (list (length decls) 
			(+ 1 (length op*) (length src*) 
                           ;(length union*)
                           (length cb*) (length init*))))

	  `(explicit-stream-wiring-language
	    '(graph (const     . ,cb*)
		    (init      . ,(apply append init*))
		    (sources   . ,src*)
		    (operators  . ,op*)
;		    (iterates  . ,iter*)
;		    (unions    . ,union*)
		    (sink ,basename ,t)
		    (union-types . ,union-types)
		    )))]))]
  )


) ;; End module
