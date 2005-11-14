;; source_loader.ss 

;; [2005.11.11] This module loads regiment source from a ".rs" file
;; and desugars it according to a few simple conventions that I've
;; followed.

(define read-regiment-source-file
  (lambda (fn)
    (define (desugar exps)
      (let loop ((ls exps))
	(match ls
	  [() (error 'read-regiment-source-file "file has no return expression.")]
	  [((define ,x ,y) ,rest ...)
	   `(letrec ((,x ,y)) ,(loop rest))]
	  [(,retexp) retexp]
	  [(,other ,rest ...) (error 'read-regiment-source-file
				     "invalid expression in definition context: ~s" other)])))

    (match (file->slist fn)
      [((parameters ,p ...) ,exps ...)
       (values (desugar exps) p)]
      [(,prog ...) 
       (values (desugar prog) ())])))
