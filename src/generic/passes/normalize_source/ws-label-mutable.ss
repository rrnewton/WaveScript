
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
		(error 'ws-label-mutable "Assignment to variable that is not iterator state!: \nSource Location:\n  ~a\n Expression:\n ~a\n~a~s" 
		       (get-location `(set! ,v ,e))
		       (get-snippet `(set! ,v ,e))
		       "\nMutable vars in scope: " mutable))
	      `(set! ,v ,e)]
	     ;; Hmm... this is really treating it as a *let* not a letrec.
	     ;; TODO: Change this to let!
	     [(iterate (letrec ([,lhs* ,ty* ,[rhs*]] ...) ,lamb) ,[src])
	      ;(printf "  MUTABLE!! ~s\n" lhs*)
	      (fluid-let ([mutable (append lhs* mutable)])
		`(iterate (letrec ([,lhs* (Ref ,ty*) (Mutable:ref ,rhs*)] ...)
			    ,(Expr lamb fallthru)) ,src))]
	     
	     ;; [2007.03.18] Ok, for now we're going to allow the user
	     ;; to introduce Refs too... The deref's are inserted
	     ;; automatically.  I haven't figured out exactly what
	     ;; story I want for side-effects yet.
	     [(,letform ([,lhs* ,ty* ,[rhs*]] ...) ,bod)
	      (guard (memq letform '(let letrec)))
	      (let ([mutable-lhs*
		     (filter id
		       (map (lambda (lhs rhs)
			      ;; The ref cannot be contained within
			      ;; any other expression because refs are
			      ;; second class!  It must be bound directly to a var.
			      (match rhs
				[(app Mutable:ref ,e) lhs]
				[(Mutable:ref ,e) lhs]
				[(src-pos ,p ,[e]) (and e lhs)]
				;; Should maybe allow assert-type too:
				[(assert-type ,p ,[e]) (and e lhs)]
				[,else #f]))
			 lhs* rhs*))])
		;(unless (null? mutable-lhs*) (inspect mutable-lhs*))
		(fluid-let ([mutable (append mutable-lhs* mutable)])
		  `(,letform ([,lhs* ,ty* ,rhs*] ...)
			     ,(Expr bod fallthru))))]

	     [,oth (fallthru oth)])))
    [Expr Expr]
    [OutputGrammar ws-label-mutable-grammar])
  
) ;; End module

