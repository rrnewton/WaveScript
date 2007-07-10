
;; At this stage in the compiler letrec can be removed.  There are no
;; more recursive function definitions, and the graph of stream
;; connections must be acyclic.  Thus, any given letrec may be sorted.

(module ws-remove-letrec mzscheme
  (require "../../../plt/common.ss"
	   "reduce-primitives.ss"
           (all-except "../../util/tsort.ss" test-this these-tests)	   
	   )
  (provide remove-letrec
	   remove-letrec-grammar
	   )
  (chezimports tsort)

(define remove-letrec-grammar 
  (filter (lambda (prod)
	    (match prod
	      [(Expr ('letrec ,_ ...)) #f]
	      ;;[(Expr ('lazy-letrec ,_ ...)) #f]
	      [,_ #t]))
    reduce-primitives-grammar))

;; This handles only the letrec case.
(define-pass remove-letrec 
    [OutputGrammar remove-letrec-grammar]
    [Expr (lambda (x fallthru)
	    (match x
	      ;; Should be no "lazy" letrec at this point.
	      [(letrec ([,v* ,ty* ,[e*]] ...) ,[bod])
	       (let* ([fv** (map core-free-vars e*)]
		      [graph (map cons v* fv**)]
		      [binds (map list v* ty* e*)])
#;
		 (unless (null? (intersection v* (apply append fv**)))
		   (inspect 
		    (map-filter (lambda (v) (assq v binds)) 
			      (reverse (topological-sort graph))))
		   #;
		   (inspect (topological-sort graph)))

		 ;`(let ([,v* ,ty* ,e*] ...) ,bod)
		 (make-nested-lets 
		  (map-filter (lambda (v) (assq v binds)) 
			      (reverse (topological-sort graph)))
		  bod))]

	      [(iterate (letrec ([,lhs* ,ty* ,[rhs*]] ...) ,[bod]) ,[src])
	       (DEBUGASSERT (null? (intersection lhs* (apply append (map core-free-vars rhs*)))))
	       `(iterate (let ,(map list lhs* ty* rhs*) ,bod) ,src)]

	      [(letrec ,_ ...) (error 'remove-letrec "missed letrec: ~s" `(letrec ,_ ...))]
	      [,oth (fallthru oth)])
	    )])

(ASSERT (= (length remove-letrec-grammar) (sub1 (length reduce-primitives-grammar))))

) ;; End module
