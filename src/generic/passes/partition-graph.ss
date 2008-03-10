
(module partition-graph mzscheme
    (require "../../plt/common.ss")
    (provide 
     	   partition-graph-by-namespace
	   refine-node-partition
	   refine-server-partition
	   merge-partitions
	   partition->opnames)
    (chezimports)

(define (make-cutpoint ty src dest) 
  `(cutpoint (incoming ,src) (outgoing ,dest) (output-type ,ty)))

;; A hack to do program partitioning based on the names of top-level streams.
(define-pass partition-graph-by-namespace
    (define (node-name? nm)
      (define str (symbol->string nm))
      (define len (string-length "Node:"))
      (and (> (string-length str) len)
	   (or ;(equal? "Node:" (substring str 0 len))
	       (equal? "Node_" (substring str 0 len)))))
    ;; TODO: Need to do this recursively (scan for downstream node-side ops)
    (define (Source node-ops)
      (define all-incoming (apply append (map (lambda (op) (cdr (assq 'incoming (cdr op)))) node-ops)))
      (printf "ALL NODE INCOMING: ~s\n" all-incoming)
      (lambda (xp)	
	(match xp
	  [((name ,nm) (output-type ,ty) (code ,code) (outgoing ,down* ...))
	   (if (or (node-name? nm) (memq nm all-incoming))
	       (values (list xp) '())
	       (values '() (list xp)))])))
    (define (Operator op)
       (let ([name (cadr (assq 'name (cdr op)))]
	     [type (cadr (assq 'output-type (cdr op)))]
	     [incoming (cdr (assq 'incoming (cdr op)))]
	     [outgoing (cdr (assq 'outgoing (cdr op)))])
	 (define me? (node-name? name))
	 ;(define up?* (map node-name? incoming))
	 ;(define all-up? (andmap id up?*))
	 ;(define some-up? (ormap id up?*))
	 (define down?* (map node-name? outgoing))
	 (define all-down? (andmap id down?*))
	 (define some-down? (ormap id down?*))
	 #;
	 (when (and (not all-up?) some-up?)
	   (error 'partition-graph-by-namespace 
		  "operator cannot currently have some input streams node-side and some server: ~a ~s" 
		  name incoming))
	 (cond
	  ;[(and all-up? me?)              (values (list op) '())] ;; Purely node.

	  ;; A cut point, node-side:
	  [(and me? (not all-down?))
	   (let ([serv-downstrm (filter id (map (lambda (x y) (and (not x) y)) down?* outgoing))])
	     (printf "GENERATING CUT POINTS: ~s ~s ~s ~s\n" name serv-downstrm down?* outgoing)
	     (values (cons op (map (lambda (down) (make-cutpoint type  name down)) ;; node cuts
				serv-downstrm))
		     (map (lambda (down) (make-cutpoint type name down)) ;; server cuts
		       serv-downstrm)))]

	  [(or me? (ormap id down?*))      (values (list op) '())] ;; Node.
	  ;[(and (not all-up?) some-up?)   (values '() (list op))] ;; Server, e.g. merge.
	  [else                           (values '() (list op))]) ;; Server.
	))
    [Program 
     (lambda (prog Expr)
       (match prog
	 [(,input-language 
	   '(graph (const ,cnst* ...)
		   (init  ,init* ...)
		   (sources ,src* ...)
		   (operators ,[Operator -> node-oper** server-oper**] ...)
		   (sink ,base ,basetype)	,meta* ...))
	  (define nodeops (apply append node-oper**))
	  (let-match ([(,[(Source nodeops) -> node-src** server-src**] ...) src*])
	  
	    ;(inspect (vector 'nodesrc node-src**))
	    ;(inspect (vector 'servsrc server-src**))
	    ;(inspect (vector 'nodeop node-oper**))
	    ;(inspect (vector 'servop server-oper**))
	    (printf "DONE\n")
	    ;; TODO: Should filter the constants appropriately:
	    (vector
	     `(,input-language 
	       '(graph (const ,cnst* ...) (init ,@init*)
		       (sources ,@(apply append node-src**))
		       (operators ,@nodeops)
		       (sink #f #f)
		       ,@meta*))
	     `(,input-language 
	       '(graph (const ,cnst* ...) (init ,@init*)
		       (sources ,@(apply append server-src**))
		       (operators ,@(apply append server-oper**))
		       (sink ,base ,basetype)
		       ,@meta*)))
	    )]))]

    )


;; The initial cut (based on namespace) provides some information --
;; in particular, foreign calls in the node partition must happen on
;; the node.  Next, we separate out those operators which are flexible
;; -- *mobile* -- that can live on either side of the partition.
(define-pass refine-node-partition     
  (define (Operator Expr)
    (lambda (op)
      (let ([code (assq 'code (cdr op))])
	(if code 
	    (Expr (cadr code))
	    (match op
	      ;; Prune out cutpoints for now:
	      [(cutpoint . ,_) (values '() '())])
	    ))))
  ;; This returns true if the operator is "clean" -- if it doesn't
  ;; contain anything that would force it to be on the embedded node.
  [Expr (lambda (xp fallth)
	  (match xp
	    [(,frgn . ,_)
	     (guard (eq-any? frgn 'foreign '__foreign 'foreign-app
			     ;; For now printing is disabled to:
			     'print
			     ))
	     #f]
	    [,oth (fallth oth)]))]
  [Fuser (lambda (ls k) (and-list ls))]
  [Program 
     (lambda (prog Expr)
       (match prog
	 [(,input-language 
	   '(graph (const ,cnst* ...)
		   (init  ,init* ...)
		   (sources ,src* ...)
		   (operators ,oper* ...)
		   (sink ,base ,basetype)	,meta* ...))
	  (define cutpoints
	    (filter (lambda (op) (eq? (car op) 'cutpoint)) oper*))
	  (define (lookup name)
	    (let loop ([ops oper*])	      
	      (if (null? ops) #f
		  (let ([entry (assq 'name (cdr (car ops)))])
		    (if (and entry (eq? (cadr entry) name))
			(car ops)
			(loop (cdr ops)))))))
	  (define (get-downstream op) 
	    (lookup (cadr (ASSERT (assq 'outgoing (cdr op))))))
	  (define (get-upstream op)
	    (lookup (cadr (ASSERT (assq 'incoming (cdr op))))))
	  (define next-down (filter id (map get-downstream cutpoints)))
	  (define next-up   (filter id (map get-upstream   cutpoints)))
	  ;; From each cutpoint, walk until we hit something that's not mobile:
	  (define (trace get-next next-of-kin)
	    (let loop ([tracepoints next-of-kin] [acc '()])
	      (match tracepoints
		[() acc]
		[(,head . ,tail)
		 ;(printf "  Considering: ~s\n" (assq 'name (cdr head)))
		 (if ((Operator Expr) head)
		     (loop (let ([x (get-next head)]) (if x (cons x tail) tail))
			   (cons head acc))
		     ;; When we reach a stopping point, put in a cutpoint:
		     (begin
		       ;(inspect head)
		       (loop tail 
			     acc
			     #;
			     (cons 
			      (make-cutpoint in out ty)
			      acc))
		       ))])))

	  ;; Hack, making this work in both directions:
	  ;; (Thus we can use it for the node and server.)
	  (define mobile 
	    (append (trace get-downstream next-down)
		    (trace get-upstream   next-up)))
	  (define nodeops (difference oper* mobile))
	  (vector
	   `(,input-language 
	     '(graph (const ,@cnst*) (init ,@init*) (sources) ;; All sources stay on the node for now.
		     (operators ,@mobile)
		     (sink #f #f) ,@meta*))
	   `(,input-language 
	     '(graph (const ,@cnst*) (init ,@init*) (sources ,@src*)
		     (operators ,@nodeops)
		     (sink #f #f) ,@meta*))
	   )
	  ]))])


;; For the time being, the mobility criteria for the server side is
;; the *same* as the node-side (are there foreign calls?).  In the
;; future, this may include other distinctions, such as using floating
;; arithmetic and so on.  Also, we ultimately need to enforce some
;; limit on very expensive operators.  It could be a disaster to even
;; try to benchmark really expensive operators on the motes.  We
;; should use the benchmark results from the server side to estimate
;; whether we should at all attempt benchmarking on the node.
(define refine-server-partition refine-node-partition)

;; Takes the "base" portion, the metadata, the init, and the constants from p1...  
(define (merge-partitions p1 p2)
  (match (vector p1 p2)
    [#((,input-language 
	'(graph (const ,cnst* ...)  (init  ,init* ...)  (sources ,src* ...)
		(operators ,oper* ...)  (sink ,base ,basetype) ,meta* ...))
       (,___
	'(graph (const ,cnst2* ...) (init  ,init2* ...) (sources ,src2* ...)
		(operators ,oper2* ...) (sink ,base2 ,2basetype) ,2meta* ...)))
     `(,input-language 
       '(graph (const ,@cnst*)  ; ,@cnst2*
	       (init ,@init*) ; ,@init2*
	       (sources ,@src* ,@src2*)
	       (operators ,@oper* ,@oper2*)
	       (sink ,base ,basetype) ,@meta*))]))

(define (partition->opnames part)
  (define (opname op) 
    (let ([entry (assq 'name (cdr op))])
      (if entry (cadr entry) #f)))
  (match part
    [(,input-language 
      '(graph (const ,cnst* ...)
	      (init  ,init* ...)
	      (sources ,src* ...)
	      (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))
     (filter id 
       (append (map opname src*)
	       (map opname oper*)))
     ]))


) ;; End module