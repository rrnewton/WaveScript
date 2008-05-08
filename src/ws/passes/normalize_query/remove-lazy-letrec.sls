#!r6rs

;;;; .title Remove Lazy Letrec

;;;; [2006.11.16]

;;;; This is relatively nasty hack.  We reorder a lazy-letrec's
;;;; bindings to respect its dependencies.  Thereafter it can be
;;;; implemented with letrecs.  This disallows mutual recursion.
;;;; (or currently with LET, which disallows all recursion)

;;;; Since this *is* a hack, it doesn't really implement *lazy*
;;;; letrec.  It would have to thunk everything.


(library (ws passes normalize_query remove-lazy-letrec)
  (export remove-lazy-letrec)
  (import (rnrs) (ws common) (ws util tsort))

(define-pass remove-lazy-letrec
    [Expr (lambda (x fallthru)
	    (match (fallthru x)
	      ;; Don't bother for zero or one binding.
	      [(lazy-letrec ([,lhs* ,ty* ,[rhs*]] ...) ,[body])
	       (match (reorder-bindings (map list lhs* ty* rhs*))
		 [() body]
		 [(,bind . ,[rest]) `(let (,bind) ,rest)])
	       
;	       `(letrec ,(reorder-bindings (map list lhs* ty* rhs*)) ,body)
	       ]
	      [,other other]))]
  )

;; This uses free-vars to find dependencies, then does a topological sort.
(define reorder-bindings
  (lambda (binds)
    (match binds
      [([,lhs* ,ty* ,rhs*] ...)

       (let* ([fv** (map core-free-vars rhs*)]
	      ;; These only look at dependencies among this set of bindincgs.
	      [edges (map (lambda (lhs fv*) 
			    (cons lhs (filter (lambda (v) (memq v lhs*)) 
					fv*)))
		       lhs* fv**)]
	      )
	 ;; NOTE: this currently doesn't allow even self-recursive functions.
	 (if (cyclic? edges)
	     (error 'remove-lazy-letrec:reorder-bindings
		    "Not allowed to have cycles in lazy-letrec bindings: \n~s"
		    (map list lhs* ty* rhs*)))                
	      
	 ;; Return the appropriately sorted bindings:
	 (map (lambda (v) (assq v binds))
	   (reverse (topological-sort edges)))
	 )
       ]
      )))

#;
; ======================================================================
;; Takes a set of lazy-letrec bindings and orders it (verifying that
;; there are no cycles) so that it will work with eager evaluation.
;; .param origbindings  A list of lazy-letrec declarations.
;; .param otherrefs  A list of symbols, these are external references to the list of bindings.  
;; Used for reference counting.
(define delazy-bindings
  (lambda (origbindings otherrefs)
    ;; Takes a list of (,name ,const), (,name ,name), (,name (,prim ,args ...)), or (,name (if ,x ,y ,z))
    ;; FIXME: TODO: check for cycles.
    
    (define (ref-counts binds)
      (let ((hash (make-default-hash-table)))
	;; First take into account any "external" refs to this block of bindings.
	(for-each 
	    (lambda (s) (let ((cur (hashtab-get hash s)))
			  (hashtab-set! hash s (if cur (fx+ 1 cur) 1))))
	  otherrefs)
	(for-each (lambda (b)
		    ;; Make sure there's an entry for every bound symbol:
		    (if (not (hashtab-get hash (car b))) (hashtab-set! hash (car b) 0))

		    (let ([name (car b)] [rhs (rac b)] [extras (rdc (cdr b))])
		      (match rhs
				   [,sym (guard (symbol? sym))
					 (let ((entry (hashtab-get hash sym)))
					   (if entry
					       (hashtab-set! hash sym (fx+ 1 entry))
					       (hashtab-set! hash sym 1)))]
				   [(quote ,const) (guard (or (simple-constant? const) (symbol? const))) (void)]
				   ;; This is node-local code now, check against TML prims:
				   [(,prim ,[args] ...)
				    (guard (or (token-machine-primitive? prim)
					       ;(regiment-primitive? prim)
					       (basic-primitive? prim)))
				    (void)]
				   [(if ,[x] ,[y] ,[z]) (void)]
				   ;[(tupref ,n ,m ,[x]) (void)]
				   ;[(tuple ,[args] ...) (void)]

				   [,other (error 'deglobalize:delazy-bindings
						  "trying to refcount, bad rhs subexpr: ~s" other)])))
	  binds)
	hash))

    (define firstcounts (ref-counts origbindings))

    ;; Simply inline everything that has only one reference:
    (define substituted-binds
	(begin ;(hashtab-for-each (lambda (k c) ( printf "  Got count: ~s ~s\n" k c)) firstcounts)
	(let loop ((curbinds origbindings))
	  (if (null? curbinds) '()
	      ;; [2006.04.02] Having problems with this usage of ... currently:
;	      (let-match ([(,name ,extras ... ,rhs) (car curbinds)])
	      (let* ([b (car curbinds)] [name (car b)] [rhs (rac b)] [extras (rdc (cdr b))])
		;; This inlines varrefs and creates a new rhs:
		(let ((newrhs (let inner ((xp rhs))
					 (match xp
					   [,sym (guard (symbol? sym))
						 ;( printf "Doing it to ~s... ~s ~s\n" sym (hashtab-get firstcounts sym) (assq sym origbindings))
						 (if (eq? 1 (hashtab-get firstcounts sym)) ;; Could be #f
						     (let ((entry (assq sym origbindings)))
						       (if entry
							   (inner (cadr (assq sym origbindings)))
							   ;; Otherwise it's a free variable!  Nothing to do with that.
							   sym))
						     sym)]
					   [(quote ,c) (guard (or (simple-constant? c) (symbol? c))) `(quote ,c)]
					   [(if ,[x] ,[y] ,[z]) `(if ,x ,y ,z)]
					   [(,prim ,[args] ...) 
					    (guard (or (token-machine-primitive? prim)
						       (basic-primitive? prim)))
					    `(,prim ,args ...)]
					   [,other (error 'deglobalize:delazy-bindings 
							  "trying to inline, bad rhs subexpr: ~s" other)]
					   ))))
		  (cons `(,name ,@extras ,newrhs) 
			(loop (cdr curbinds)))))))))

    (define newcounts (ref-counts substituted-binds))
    
    ;; Now recount the refs and remove anything that is unreferenced.
    (define pruned-binds
      (begin ;(hashtab-for-each (lambda (k c) ( printf "Got new counts: ~s ~s\n" k c)) newcounts)
      (filter (match-lambda ((,name ,rhs))
		;; HACK: FIXME..
		;; We don't nix it unless WE made it unreferenced via our inlining.
		(not (and (eq? 0 (hashtab-get newcounts name))
			  (not (eq? 0 (hashtab-get firstcounts name))))))
	  substituted-binds)))

    ;; Check if IF is fixed up sufficiently.
    (for-each (match-lambda ((,name ,rhs))
		(define (nono rhs)
		  (match rhs
		    [,sym (guard (symbol? sym))
			  ;; Cannot currently delazify this, and we won't generate incorrect code:
			  (if (assq sym origbindings)
			      (error 'deglobalize:delazy-bindings
				     "IF construct currently cannot have lazy references in conseq/altern: reference to ~s in ~s"
				     sym rhs))]
		    [(quote ,const) (guard (or (symbol? const) (simple-constant? const))) (void)]
		    [(,prim ,[args] ...) (guard (or (token-machine-primitive? prim)
						    (basic-primitive? prim))) (void)]
		    [(if ,[x] ,[y] ,[z]) (void)]))
		(define (ok rhs)
		  (match rhs
		    [,sym (guard (symbol? sym)) (void)]
		    [(quote ,const) (guard (or (symbol? const) (simple-constant? const))) (void)]
		    [(,prim ,[args] ...) (guard (or (token-machine-primitive? prim)
						    (basic-primitive? prim))) (void)]
		    [(if ,[x] ,[nono -> y] ,[nono -> z]) (void)]))
		(ok rhs))
      pruned-binds)
		
    ;; Check if ordering is satisfied.
    (let ((edges (map (match-lambda ((,name ,rhs))
			(apply list name (filter (lambda (v) (assq v pruned-binds))
					   (tml-free-vars rhs))))
		   pruned-binds)))
      (if (cyclic? edges)
	  (error 'deglobalize:delazy-bindings
		 "Not allowed to have cycles in node-code let bindings: \n~s"
		 pruned-binds))
      
      ;; Return the appropriately sorted bindings:
      (map (lambda (v) (assq v pruned-binds))
	(reverse! (topological-sort edges))))
;    pruned-binds
    ))

) ; End module
