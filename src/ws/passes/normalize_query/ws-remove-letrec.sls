#!r6rs

;; At this stage in the compiler letrec can be removed.  There are no
;; more recursive function definitions, and the graph of stream
;; connections must be acyclic.  Thus, any given letrec may be sorted.

;; [2007.10.02] This also does the job of flattening normal let
;; statements into nested chains of lets.  (Giving them a let*
;; semantics.)

(library (ws passes normalize_query ws-remove-letrec)
  (export remove-letrec
	  remove-letrec-grammar
	  topo-sort-bindings
	  )
  (import (except (rnrs (6)) error) (ws common)
	  (ws passes normalize_query reduce-primitives)
	  (ws util tsort))

(define remove-letrec-grammar 
  (cons 
 ;; Let's only have one binding after this:
 `[Expr ('let ((LHS Type Expr)) Expr)]
 (filter (lambda (prod)
	   (match prod
	     [(Expr ('letrec ,_ ...)) #f]
	     [(,__   ('let ,_ ...))   #f]
	     ;;[(Expr ('lazy-letrec ,_ ...)) #f]
	     [,_ #t]))
   reduce-primitives-grammar)))

(define ProcessExpr 
  (lambda (x fallthru)
    (match x
      
      [(let ([,lhs* ,ty* ,[rhs*]] ...) ,[bod])
       (make-nested-lets (map list lhs* ty* rhs*) bod)
       #; #;
       (warning 'ws-remove-letrec "Shouldn't get a normal let in input: ~s"
		`(let ,_ ,__))
       (inspect `(let ,_ ,__))]

      ;; Should be no "lazy" letrec at this point.
      [(letrec ([,v* ,ty* ,[e*]] ...) ,[bod])
       (make-nested-lets (topo-sort-bindings v* ty* e*) bod)]

      [(iterate ,annot (letrec ([,lhs* ,ty* ,[rhs*]] ...) ,[bod]) ,[src])
       (DEBUGASSERT (null? (intersection lhs* (apply append (map core-free-vars rhs*)))))
       `(iterate ,annot (let ,(map list lhs* ty* rhs*) ,bod) ,src)]

      [(letrec ,_ ...) (error 'remove-letrec "missed letrec: ~s" `(letrec ,_ ...))]
      [,oth (fallthru oth)])
    ))

(define (topo-sort-bindings v* ty* rhs*)
  (let* ([fv** (map core-free-vars rhs*)]
	 [graph (map cons v* fv**)]
	 [binds (map list v* ty* rhs*)])
    #;
    (unless (null? (intersection v* (apply append fv**)))
      (inspect 
       (map-filter (lambda (v) (assq v binds)) 
		   (reverse (topological-sort graph))))
      #;
      (inspect (topological-sort graph)))

    (map-filter (lambda (v) (assq v binds))
		(reverse (topological-sort graph)))))

;; This handles only the letrec case.
(define-pass remove-letrec 
    [OutputProps single-bind-let]
    [OutputGrammar remove-letrec-grammar]
    [Expr ProcessExpr])

(ASSERT (= (length remove-letrec-grammar) (sub1 (length reduce-primitives-grammar))))

) ;; End module
