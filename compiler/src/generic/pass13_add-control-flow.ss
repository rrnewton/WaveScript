;; [2004.08.20]
;; This creates a separate and parallel control flow graph.

;; (amap f (circle (anchor-at '(30 40)) 50))
;;  -> (soc anchor circle amap)

;; (amap f (union r1 r2))
;;  -> (soc r1 amap)
;;  -> (soc r2 amap)


(define add-control-flow
  (lambda (expr)
    (match expr
	   [(annotate-heartbeats-language (quote (program (props ,proptable ...) ,letexpr)))

	    (let ([check-prop 
		   (lambda (p s)
		     (let ((entry (assq s proptable)))
		       (if entry (memq p (cdr entry))
			   (error 'pass10_deglobalize:check-prop
				  "This should not happen!  ~nName ~s has no entry in ~s."
				  s proptable))))])

    ;; Returns control flow graph
    (define (process-let expr)
      (match expr
	 [ (lazy-letrec ([,lhs* ,heartbeat* ,[expr-dependencies -> deps*]] ...) ,expr)
	   (apply append
		  (map (lambda (lhs deps)
			 (map (lambda (x) `(,x ,lhs)) deps))
		       lhs* deps*)) ]
	 [,other (error 'add-control-flow:process-let "bad lazy-letrec expression: ~s" other)]))

    (define expr-dependencies
      (lambda (expr)
        (match expr
          [(quote ,const) '()]
          [,var (guard (symbol? var) (not(regiment-constant? var)))
		(if (check-prop 'local var)
		    '()
		    (list var))]
          [(lambda ,formalexp ,expr)
	   '() ;; CHECK UP ON THIS; MAYBE TAKE FREE-VARS??
	   ]
	  ;; Hmm... if I can tell at compile time I should narrow this!

          [(if ,[test] ,[conseq] ,[altern])
	   (append test conseq altern)]
	  
          [,prim (guard (regiment-constant? prim)) '()]
          [(,prim ,[rand*] ...)
           (guard (regiment-primitive? prim))
	   (apply append rand*)]
          [,unmatched
	   (error 'addplaces:process-let "invalid syntax ~s" unmatched)])))


    (let ([leaves (map car (filter (lambda (entry) (memq 'leaf entry)) proptable))])
      `(add-control-flow-language
	(quote (program (props ,proptable ...)
			(control-flow
			 ,@(map (lambda (x) `(SOC ,x)) leaves)
			 ,@(process-let letexpr))
			,letexpr
			)))))]
	   )))




'(add-control-flow '(annotate-heartbeats-language
		     '(program
		       (props (result_1 local final))
		       (lazy-letrec ((result_1 #f '3)) result_1))))


