
;; Pass: Sever Continuation State


(define sever-cont-state
  (let ()    

    (define (process-expr this-tok this-stored expr)
;      (disp (hashtab->list this-stored))
      (letrec ([outside (tml-generic-traverse
			 ;; Main transformer:
			 (lambda (x autoloop)
			   (match x
			     [(lambda ,v ,[inside -> e]) 
			      (if (not (= 1 (length v))) 
				  (error 'sever-cont-state "all lambda's should be continuations, bad formals list: ~s" v))
			      `(lambda ,v ,e)]
			     [,o (autoloop o)]))
			 ;; Result fuser:
			 (lambda (xs k) (apply k xs)))]
	       [inside (tml-generic-traverse
			(lambda (x autoloop)
			  (match x
			    ;; Tranform a local ref to an external ref if we're inside a continuation.
			    [,v (guard (symbol? v) (hashtab-get this-stored v))
				`(ext-ref ,this-tok ,v)]
			    [(set! ,v ,[e]) (guard (hashtab-get this-stored v))
			     `(ext-set! ,this-tok ,v ,e)]
			    [,o (autoloop o)]))
			(lambda (results recombine) (apply recombine results)))])
	(outside expr)))

    (define process-tokbind
      (lambda (tb)
	(mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
	  `[,tok ,id ,args (stored ,@stored )
		 ,(process-expr `(tok ,tok ,id) (set->hashtab (map car stored)) body)])))
    
    (tml-simple-pass  process-expr process-tokbind)
    ))


(define these-tests
  `(

    ["Make sure it severs the state references for a simple example."
     (sever-cont-state '(desugar-gradient-lang
			 '(program
			      (bindings)
			    (nodepgm
				(tokens
				  (SOC-start
				   subtok_ind
				   ()
				   (stored (x '0))
				   (begin (set! x '0) (printf "~s\n" x)
					  (call tok1 
						(lambda (v)
						  (begin 
						    (set! x '0) 
						    (printf "~s\n" x))))))
				  (tok1 (k) (kcall k 'foo)))))))
     (desugar-gradient-lang
      '(program
	   (bindings)
	 (nodepgm
	     (tokens
		 (SOC-start
		  subtok_ind
		  ()
		  (stored (x '0))
		  (begin
		    (set! x '0)
		    (printf "~s\n" x)
		    (call
		     tok1
		     (lambda (v)
		       (begin
			 (ext-set! #0=(tok SOC-start subtok_ind) x '0)
			 (printf "~s\n" (ext-ref #0# x)))))))
	       (tok1 subtok_ind (k) (stored) (kcall k 'foo))))))]

    ))