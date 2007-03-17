
;===============================================================================
;;;;      ---- Pass Label Mutable  ---- 
;;;;
;;;; This labels the mutable iterator state variables as "refs".
;;;; Accordingly, references to them must be dereferences.
;;;;
;;;; .author Ryan Newton

;===============================================================================

(module ws-label-mutable mzscheme
  (require "../../../plt/common.ss")
  (provide ws-label-mutable   ws-label-mutable-grammar )
  (chezimports)
  
  (define ws-label-mutable-grammar
    ;; Add Ref type:
    (cons '(Type ('Ref Type))
	  initial_regiment_grammar))

  ;; This labels the mutable iterator state variables as "refs".
  ;; Accordingly, references to them must be dereferences.
  (define-pass ws-label-mutable
    ;; Not threadsafe because of static state:
    (define mutable '())
    (define Expr
         (lambda (x fallthru)
	   (match x
	     [,v (guard (symbol? v)) 
		 (if (memq v mutable) `(deref ,v) v)]
	     [(set! ,v ,[e]) 
	      (unless (memq v mutable)
		(error 'ws-label-mutable "Assignment to variable that is not iterator state!: ~s~a~s" 
		       `(set! ,v ,e) "\nMutable vars in scope: " mutable))
	      `(set! ,v ,e)]
	     ;; Hmm... this is really treating it as a *let* not a letrec.
	     ;; TODO: Change this to let!
	     [(iterate (letrec ([,lhs* ,ty* ,[rhs*]] ...) ,lamb) ,[src])
	      (printf "MUTABLE!! ~s\n" lhs*)
	      (fluid-let ([mutable (append lhs* mutable)])
		`(iterate (letrec ([,lhs* (Ref ,ty*) (ref ,rhs*)] ...)
			    ,(Expr lamb fallthru)) ,src))]
	     [,oth (fallthru oth)])))
    [Expr Expr]
    [OutputGrammar ws-label-mutable-grammar])
  
) ;; End module

