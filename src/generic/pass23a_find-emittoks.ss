
;;;; .title Find-Emittoks  (pass23a_find-emittoks.ss)

;;;; [2006.01.15] <br><br>

;;;; I'm refactoring desugar-gradients to simplify it.  Part of this
;;;; effort is factoring out this pass.  
;;;;   <br> <br>

;;;; This pass finds all the tokens that might possibly be emitted in a
;;;; "gemit" statement.  This is necessarily a conservative estimate.
;;;; These are the tokens that will later be augmented to carry gradient
;;;; information in their argument list.
;;;;   <br> <br>

;;;; Input language:   <br><br>

;;;; Output language:  <br>
;;;; - Contains an extra form storing the tainted "emittoks".



;; This is the compiler pass.
(define find-emittoks  
  (let ()

    ;; This returns all tokens which will or could be gradientized (tainted).
    ;; If we ever have a first class reference to a token name, it is potentially tainted.
    ;; This is a conservative estimate.
    (define (process-expr expr)
      (tml-generic-traverse
       (lambda (x autoloop)
	 (match x
	        ;[,x (guard (begin (printf "FindEmitToks matching: ~a~n" x) #f)) 3]
	   [(tok ,t ,n) (guard (number? n)) (list t)]
	   [(tok ,t ,[e]) (cons t e)]

	   [(ext-ref (tok ,t ,[ls]) ,v) ls]
	   [(ext-set! (tok ,t ,[ls]) ,v ,[ls2]) (append ls ls2)]

	   ;; "Direct call":  
	   [(gemit (tok ,t ,[e]) ,[args*] ...)  (cons t (apply append e args*))]
	   ;; Indirect gemit call... could consider restricting these.
	   [(gemit ,[e] ,[args*] ...)
	    (error 'pass23_desugar-gradients "not allowing dynamically targeted emits atm: ~s" x)
					;(apply append e args*)
	    ]
	   
	   ;; The to's and the vias are static! Aggr has no subtok index!
	   [(greturn ,[expr] (to (tok ,t ,tn)) (via (tok ,v ,vn)) (seed ,[seed_val]) (aggr ,a))
	    ;; The via requires that a tree be there, and hence it be gradientized.
	    (cons v (append expr seed_val))]
	   
	   ;; Static calls are allowed:
	   [(call (tok ,t ,[e]) ,[args*] ...) (apply append e args*)]
	   ;; Anything more dynamic makes us think the operand is potentially emitted.

	   ;; Tokens fed to primitives don't count as "escaped".
	   [(,prim ,args* ...)
	    (guard (or (token-machine-primitive? prim)
		       (basic-primitive? prim)))
	    (define do-primargs-w-tokens;; Handles primitives that take token args.
	      (lambda (prim args)
		(map-prim-w-types 
		 (lambda (arg type)
		   (match (cons type arg)
		     [(Token . (tok ,tok ,e)) (process-expr e)]
		     [(,other . ,e)           (process-expr e)]))
		 prim args)))
	    (apply append (do-primargs-w-tokens prim args*))
	    ]
	   
	   [,other (autoloop other)]))
       ;; Fuser
       (lambda (results recombine)
	 (apply append results))
       ;; Start expression:
       expr))

    (define process-tokbinds
      (lambda (tbs)
	(if (null? tbs) '()
	    (mvlet ([(_ __ ___ ____ _____ body) (destructure-tokbind (car tbs))])
		   (append (process-expr body)
			   (process-tokbinds (cdr tbs)))))))

    ;; Main body of Find-emittoks
    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...) 
		   (nodepgm (tokens ,toks ...))))
	 
	 (let ([tainted (list->set (process-tokbinds toks))])
	   `(,lang
	     '(program 
		  (bindings ,constbinds ...)
		  (nodepgm (tokens ,toks ...))
		  (emittoks ,tainted ...))
	     ))]))
    ))
