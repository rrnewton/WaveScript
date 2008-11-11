

;;;; This pass replaces records with tuples.  It works because the
;;;; program is already monomorphic.  It batches together multiple
;;;; consecutive record extension/restrictions.  But it does this only
;;;; when they are syntactically adjacent... it does no non-local
;;;; optimization.

;;;; Currently this does heavy list sorting... we'll have to watch for
;;;; this becoming a bottleneck.

;;;; .author Ryan Newton [2008.11.11]

(library (ws passes normalize_query desugar-records)
  (export desugar-records)
  (import (except (rnrs (6)) error) (ws common) )


  (define (row->labels row)
    (match row
      [(Row ,name ,_ ,[tail]) (cons name tail)]
      [',v '()]  ;; empty-wsrecord will cause this.  TODO: treat it like other polymorphic constants.
      [#() '()]))
  (define (row->sorted row) (list-sort symbol<? (row->labels row)))
  (define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))
  (define (symbol<=? a b) (string<=? (symbol->string a) (symbol->string b)))

  (define (row->alist row)
    (match row
      [(Row ,name ,ty ,[tail]) (cons (cons name ty) tail)]
      [#() '()]))

  (define (insert-into-sorted fn x ls)
    (let loop ([ls ls])
      (cond 
       [(null? ls) (list x)]
       [(fn x (car ls)) (cons x ls)]
       [else (cons (car ls) (loop (cdr ls)))])))

  (define-pass desugar-record-ops
    [Expr/Types (lambda (x tenv fallthru)		  
		  (let loop ([x x])
		    (match x ;; no recursion
		      [(empty-wsrecord) '(tuple)]
		      [(wsrecord-select ,name ,rec)
		       ;(printf "SELECT FROM: ~a , type ~a \n" rec (recover-type rec tenv))
		       (let-match ([',name name]
				   [(Record ,row) (recover-type rec tenv)])
			 (define labels (row->sorted row))
			 `(tupref ,(list-find-position name labels) ,(length labels) ,rec))]

		      [(,recordop ,args ...) (guard (eq-any? recordop 'wsrecord-extend 'wsrecord-restrict))
		       ;; This opchain reflects the order in which extension/restrictions are applied:
		       (define-values (root opchain)
			 (let accum ([x x] [chain '()])			   
			   (match x 
			     [(wsrecord-restrict ',name ,rec)               (accum rec (cons (vector 'restrict name) chain))]
			     [(wsrecord-extend ',name ,[loop -> val] ,rec)  (accum rec (cons (vector 'extend name val) chain))]
			     [,oth (values oth chain)])))
		       (define rootty (recover-type root tenv))
		       ;(define _ (printf "ROOT ~a  TYPE : ~a\n" root rootty))
		       (define sorted (match rootty [(Record ,row) (row->sorted row)]))
		       (define rootlen (length sorted))
		       
		       ;(printf "OPCHAIN ~a root ty  ~a\n" opchain sorted)
		      		       
		       (define (buildtup root)
			 (let loop ([chain opchain] [index_map (mapi (lambda (i x) (cons x i)) sorted)])
					;(printf "MAP ~a\n" index_map)
			 (if (null? chain)
			     ;; Now take the final index-map and produce an expression:			     
			     `(tuple ,@(map (match-lambda ((,nm . ,rhs))
					      (if (integer? rhs)
						  `(tupref ,rhs ,rootlen ,root)
						  rhs))
					 index_map))
			     (match (car chain)
			       [#(extend ,nm ,v)
				(loop (cdr chain)
				      (insert-into-sorted (lambda (a b) (symbol<=? (car a) (car b))) (cons nm v) index_map))]
			       [#(restrict ,nm)
				(loop (cdr chain)
				      (let loop2 ([map index_map])
					(cond 
					 [(null? map) (error 'desugar-records "must be a bug, could not find label ~a in record type" nm)]
					 [(eq? nm (caar map)) (cdr map)]
					 [else (cons (car map) (loop2 (cdr map)))])))]
			       ))))
		       (if (simple-expr? root) 
			   (buildtup root)
			   (let ([tmp (unique-name "tmprec")])
			     `(let ([,tmp ,rootty ,root])
				,(buildtup tmp))))]

		      [,oth (fallthru oth tenv)])))]
    )

  ;; 'Bindings' doesn't mix with 'Expr/Types' so this is currently broken up into two passes.
  (define-pass desugar-record-types
      ;; Convert record types to a tuple types:
      ;; Hmm. still no generic traverse for types it looks like.  [2008.11.11] 
      (define (Type orig) 
	(match orig ;; No recursion.
	  [(,qt ,v) (guard (tvar-quotation? qt) (symbol? v)) orig]
	  [,s (guard (symbol? s)) s]    
	  [#(,[t*] ...) (apply vector t*)]
	  [(,[arg*] ... -> ,[res]) `(,@arg* -> ,res)]
	  [(Record ,row)
	   (define sorted (list-sort (lambda (a b) (symbol<? (car a) (car b))) (row->alist row)))
	   (list->vector (map cdr sorted))]
	  ;; Including Ref:
	  [(,s ,[t*] ...) (guard (symbol? s)) `(,s ,@t*)]
	  [,s (guard (string? s)) s] ;; Allowing strings for uninterpreted C types.
	  [,other (error 'desugar-records "bad type: ~s" other)]))
    [Expr (lambda (x fallthru)
	    (match x 
	      [(assert-type ,[Type -> ty] ,[e]) `(assert-type ,ty ,e)]
	      [,oth (fallthru oth)]))]
    [Bindings (lambda (vars types exprs reconstr exprfun)
		(reconstr vars (map Type types) (map exprfun exprs)))])

  (define desugar-records (compose desugar-record-types desugar-record-ops))

) ;; End module
