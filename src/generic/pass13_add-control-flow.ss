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
	   [(annotate-heartbeats-lang (quote (program (props ,proptable ...) ,letexpr)))

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
          [,var (guard (symbol? var)) (list var)]
          [(lambda ,formalexp ,expr)
	   '() ;; CHECK UP ON THIS; MAYBE TAKE FREE-VARS??
	   ]
	  ;; Hmm... if I can tell at compile time I should narrow this!

          [(if ,[test] ,[conseq] ,[altern])
	   (append test coseq altern)]
          [(,prim ,[rand*] ...)
           (guard (regiment-primitive? prim))
	   (apply append rand*)]
          [,unmatched
	   (error 'addplaces:process-let "invalid syntax ~s" unmatched)])))


    (let ([leaves (map car (filter (lambda (entry) (memq 'leaf entry)) proptable))])
      (disp "LEAVES" leaves)
      `(add-control-flow-lang (quote (program (props ,proptable ...)
					      ,(append 
						(map (lambda (x) `(SOC ,x)) leaves)
						(process-let letexpr))
					      ,letexpr
					))))]
	   )))




'(add-control-flow '(annotate-heartbeats-lang
		     '(program
		       (props (result_1 local final))
		       (lazy-letrec ((result_1 #f '3)) result_1))))


