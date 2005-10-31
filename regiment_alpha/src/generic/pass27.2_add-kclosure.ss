

(define add-kclosure
  (let ()
    
    (define (free-vars e)
      (list->set 
       (tml-generic-traverse
	;; driver, fuser, expression
	(lambda  (x loop) 
	  (match x
;		 [,x (guard (begin (printf "Driver loop: ~a~n" x) #f)) 3]
		 [,v (guard (symbol? v)) (list v)]
		 [(let ([,lhs ,[rhs]]) ,[bod])
		  (append rhs (remq-all lhs bod))]
		 [(lambda (,v) ,[e]) (remq-all v e)]		 
		 [,x (loop x)]))
	(lambda (ls _) (apply append ls))
	e)))

   ;; Process expr:
    (define process-expr
   (lambda (consts x)
     (tml-generic-traverse
      (lambda (x k)
	(match x
	  [(lambda (,v) ,bod)
	   `(kclosure ,(remq v (free-vars bod))
		      ,v ,(process-expr consts bod))]
	  [,x (k x)]))
      (lambda (xs k) (apply k xs))
      x
      )))
    
    
	      
;; FACTOR OUT THIS BOILER PLATE:
 (define (process-tokbind tb)
  (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
    `[,tok ,id ,args (stored ,@stored )
	   ,(process-expr (map car constbinds) body)]))

 ;; Main body of this pass:
 (lambda (prog)
   (match prog
     [(,lang '(program (bindings ,constbinds ...)
		(nodepgm (tokens ,[process-tokbind -> toks] ...))))
      `(,lang
	'(program (bindings ,constbinds ...)
	   (nodepgm (tokens ,toks ...))))]))
    ))