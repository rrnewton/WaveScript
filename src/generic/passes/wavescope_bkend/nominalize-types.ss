
;;;; .title WaveScript Nominalize Types
;;;; .author Ryan Newton

;;;; This pass replaces the ML-like algebraic types and replaces them with suitable C-types.



(module wavescript_nominalize-types  mzscheme 
  (require "helpers.ss")
  (provide nominalize-types test-this test-nominalize-types)
  (chezprovide )
  (chezimports (except helpers test-this these-tests)
	       (except reg_core_generic_traverse test-this these-tests))

  (define nominalize-types
    (let ()
      ;; The generic traversal returns an intermediate value of type
      ;; #(Expr TypeDefs) where TypeDefs is list of [Name [StructFieldType ...]] pairs.
      (reg:define-struct (result expr tydefs))

      ;; An association list accumulating new struct types.
      ;; 
      (define struct-table '())
      
      ;; We avoid the boilerplate by defining this as a "generic traversal"
      (define Expr 
	(core-generic-traverse/types
	 ;; Driver
	 (lambda (expr tenv loop)
	   (define (default x) (loop x tenv))
	   (trace-match USERDRVR expr

	     ;; Tuple statements HAVE to be type annotated now.
	     [(tuple ,[default -> arg*] ...)
	      (printf "TUPLE!! \n")
	      ;(inspect tenv)
	      (id;inspect/continue 
	       (make-result 
		`(make-struct FOOTYPE ,arg* ...)
		`((FOOTYPE ,(map (lambda (arg) (recover-type arg tenv)) arg*)))))]
	     
	     [,other (default other)]
	     ))
	 ;; Fuser
	 (lambda (ls k)
	   (printf "FUSING: ~s\n\n" ls)
	   (make-result (apply k (map result-expr ls))
			(apply append (map result-tydefs ls))))
	 ))

      ;; Main body:
      (lambda (prog) 
	(match prog 
	  [(,lang '(program ,[Expr -> body] ,type))
	   
	   (printf "Here's annotation:\n" )
	   (pretty-print body)

	   ;; Running the type-checker/inferencer isn't going to work on this output any longer:
	   `(nominalize-types-language
	     '(program ,(result-expr body) 
		      ;; We stick the type definitions here:
		      (struct-defs ,@(result-tydefs body)) ,type))]
	  ))))

  (define these-tests  `())
  (define test-this (default-unit-tester "" these-tests))
  (define test-nominalize-types test-this)

) ;; End module.





