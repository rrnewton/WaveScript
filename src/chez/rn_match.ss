;; [2007.04.06] CODE DUPLICATION:
;;
;; This file is the same as "match.r5rs" also in the repository.
;; EXCEPT, I've hacked this one to use syntax-case for one purpose:
;; real ellipses.  The syntax-rules version had to settle for "_..."

;; Also I removed the "PATMATCH_" name mangling.



;; How far can we get doing match with syntax-rules?

;; Portability:
;; Chez -- ok
;; PLT -- ok
;; SCM -- ok (remember to run with -r 5)
;; gambit -- ok, load "~~/syntax-case.scm"
;; guile -- ok "(use-syntax (ice-9 syncase))"

;; stklos --

;; sisc   ??
;; gauche ?? 
;; kawa   ?? 

;; MIT -- works on some tests (eval doesn't, though)
;;        Breaks down on the first "->" test.
;;        Some weird thing wherein it tries to *apply* the results of the cata.
;; bigloo -- some kind of call-with-values error on the multiple value test
;; larceny -- gets a wrong number of arguments error on the same test as bigloo
;; gauche -- same wrong number of arguments

(module rn_match ((match match-help bind-dummy-vars bind-popped-vars exec-body 
			 build-list ellipses-helper 
			 vecref-helper vecref-helper2 convert-pat))

(define-syntax match
  (syntax-rules ()
    ((_ Exp Clause ...)
     (let f ((x Exp))
       (match-help _ f x Clause ...)))))

;; Certain implementations have difficulty with delaying multple values.
;; (including gauche, stklos, bigloo, scheme48, larceny)
#;
(define-syntax delay-values
  (syntax-rules ()
    ((_ e)
     (let ((vals #f))
       (lambda ()
	 (if vals (apply values vals)
	     (call-with-values (lambda () e)
	       (lambda args (set! vals args) (apply values args))))
	 )))))

;; For Chez, etc:
#;
(define-syntax delay-values
  (syntax-rules ()
    ((_ e) (delay e))))

;; Do I really need a promise?
(define-syntax delay-values
  (syntax-rules ()
    ((_ e) (lambda () e))))


;; This walks over the clauses and uses dispatches another helper to
;; walk over each patern.
(define-syntax match-help
  (syntax-rules (guard)
    ((_ Template Cata Obj )  (error 'match "no next clause"))
    ;; Guarded pattern:
    ((_ Template Cata Obj (Pat (guard G ...) B0 Bod ...) Rest ...)
     (let ((next (lambda () (match-help Template Cata Obj Rest ...))))
       (convert-pat ((Obj Pat) ())
		    exec-body 	   
		    (begin B0 Bod ...) (and G ...)
		    Cata next () ())))
    ;; Unguarded:
    ((_ Template Cata Obj (Pat B0 Bod ...) Rest ...)
     (let ((next (lambda () (match-help Template Cata Obj Rest ...))))
       (convert-pat ((Obj Pat) ()) 
		    exec-body 		   
		    (begin B0 Bod ...) #t
		    Cata next () ())
       ))))




;; Results of recursive matches can't be used in guards.
(define-syntax bind-dummy-vars
  (syntax-rules ()
    ((_ () Bod)  Bod)
    ((_ (V0 . V*) Bod)
     (let ((V0 'match-error-cannot-use-cata-var-in-guard))
       (bind-dummy-vars V* Bod)))))

;; Here we perform the recursive matches.  We use the reuse the variable names.
(define-syntax bind-popped-vars
  (syntax-rules ()
    ((_ () Bod)  Bod)
    ((_ (V0 . V*) Bod)
     (let ((V0 (V0)))
       (bind-popped-vars V* Bod)))))


(define-syntax exec-body
  (syntax-rules ()
    ((_  Bod Guard NextClause Vars CataVars)
     (bind-popped-vars Vars
         (if (bind-dummy-vars CataVars Guard)
	     (bind-popped-vars CataVars Bod)
	     (NextClause)
	     )))))

;; Like exec-body but just builds a list of all the vars.
;; This puts CataVars first in the list.
(define-syntax build-list 
  (syntax-rules ()
    ((_ __ #t #f () ())   '())
    ((_ __ #t #f (V . Vars) CataVars)
     (cons V (build-list __ #t #f Vars CataVars)))
    ((_ __ #t #f () (V . CataVars))
     (cons V (build-list __ #t #f () CataVars)))
    ((_ __ Guard NextClause Vars CataVars)
     (if (bind-popped-vars Vars
          (bind-popped-vars CataVars Guard))
	 (build-list __ #t #f Vars CataVars)
	 (NextClause)))))

(define-syntax bind-cata
  (syntax-rules ()
    ((_ Bod Promise Args ())  Bod)

    ;; Optimization, handle one argument, no call-with-values:
    ((_ Bod Promise (V0) __)
     (let ([V0 Promise]) Bod))

    ((_ Bod Promise Args (V0 . V*))
     (let ((V0 (lambda ()
		 ;; Maybe Inefficient:
		 (call-with-values Promise ;(lambda () (force-values Promise))
		   ;; Select out just our variable from the results:
		   (lambda Args V0)))))
       (bind-cata Bod Promise Args V*)))))

;; This does the job of "collect-vars", but it also carries a
;; continuation (of a sort) with it.
(define-syntax ellipses-helper
  (syntax-rules (unquote ->)
    ((_ (Vars ...) (CataVars ...) (Bod ...))
     (lambda (CataVars ... Vars ...)
       (Bod ... (Vars ...) (CataVars ...))))
    
    ((_ Vars (CataVars ...) Bod (unquote (FUN -> V* ...)) . Rest)
     (ellipses-helper Vars (V* ... CataVars ...) Bod  . Rest))
    ((_ Vars (CataVars ...) Bod (unquote (V* ...)) . Rest)
     (ellipses-helper Vars (V* ... CataVars ...) Bod  . Rest))
    ((_ Vars CataVars Bod (unquote V) . Rest)
     (ellipses-helper (V . Vars) CataVars Bod  . Rest))
    
    ((_ Vars CataVars Bod () . Rest)
     (ellipses-helper Vars CataVars Bod  . Rest))
    
    ((_ Vars CataVars Bod (P0 . P*) . Rest)
     (ellipses-helper Vars CataVars Bod P0 P* . Rest))
    ;; Otherwise just assume its a literal:
    ((_ Vars CataVars Bod LIT . Rest)
     (ellipses-helper Vars CataVars Bod . Rest))
    ))

(define-syntax vecref-helper
  (syntax-rules ()
    ((_ vec ind acc (Stack . stuff)) (vecref-helper2 acc stack stuff))
    ((_ vec ind acc stuff P0 P* ...)
     (vecref-helper vec (fx+ 1 ind) 
			     (((vector-ref vec ind) P0) . acc)
			     stuff P* ...))))

;; This (verbosely) does a reversal of the patterns for the vector.
;; This goal is to get a left-to-right order, which I believe to be
;; more efficient for my uses.  (Generally, the first element is a
;; symbol that tags the variant represented by the vector.)
(define-syntax vecref-helper2
  (syntax-rules ()
    ((_ () stack stuff) (convert-pat stack . stuff))
    ((_ (B0 . B*) stack stuff)
     (vecref-helper2 B* (B0 stack) stuff))))


;; Convert a pattern into a function that will test for a match.
;;
;; This takes several arguments:
;;   Stack -- Objs&Patterns left to match.  Objs should be just vars.
;;            Note, each frame of the stack is a list with two elements: 
;;              1: head of stack, 2: rest of stack.
;;   Exec -- Rator to apply to Bod and Vars when we reach a termination point.
;;   Bod -- the expression to execute if the pattern matches
;;   Guard -- guard expression (NOT FINISHED)
;;   Cata -- the name of the function that will reinvoke this match
;;   Nextclause -- abort this clause and go to the next.
;;   Vars -- Accumulator for vars bound by the pattern.
;;   CataVars -- Accumulator for vars bound by the pattern, with transformers applied.
;;
;; If match, the body is evaluated, otherwise "nextclause" is called.
;; All pattern variables are lazy "thunked" so as to defer any Cata's.
(define-syntax convert-pat
  (lambda (x)

    (define ellipsis?
      (lambda (x)
        (and (identifier? x) (literal-identifier=? x #'(... ...)))))

    (syntax-case x (unquote ->)

      ;; Termination condition:
      ;; Now check the guard and (possibly) execute the body.
      ((_ () Exec Bod Guard Cata NextClause Vars CataVars)
       #'(Exec Bod Guard NextClause Vars CataVars))
      
      ;; Cata redirect: 
      ((_ ((Obj (unquote (f -> V0 V* ...))) Stack) Exec Bod Guard Cata NextClause Vars (CataVars ...))
       #'(let ((promise (delay-values (f Obj))))
	 ;(inspect f)
	 (bind-cata 
	  (convert-pat Stack Exec Bod Guard Cata
		       NextClause Vars (V0 V* ... CataVars ...))
	  promise
	  (V0 V* ...) (V0 V* ...))))
      
      ;; Unquote Pattern, Cata: recursively match
      ((_ ((Obj (unquote (V0 V* ...))) Stack) Exec Bod Guard Cata NextClause Vars (CataVars ...))
       #'(let ((promise (delay-values (Cata Obj))))
	 (bind-cata 
	  (convert-pat Stack Exec Bod Guard Cata
		       NextClause Vars (V0 V* ... CataVars ...))
	  promise
	  (V0 V* ...) (V0 V* ...))))
	
      ;; Unquote Pattern: bind a pattern variable:
      ((_ ((Obj (unquote V)) Stack) Exec Bod Guard Cata NextClause Vars CataVars)
       #'(let ((V (lambda () Obj)))
	 (convert-pat Stack Exec Bod Guard Cata NextClause (V . Vars) CataVars)))

      ;; Ellipses:
      ((_ ((Obj (P0 Dots)) Stack) Exec Bod Guard Cata NextClause (Vars ...) (CataVars ...))
       (ellipsis? #'Dots)
       #'(call-with-current-continuation
	(lambda (escape)
	  (let* ((failed (lambda () (escape (NextClause))))
		 ;; Bind a pattern-matcher for one element of the list.	
		 (project (lambda (VAL)
			    (convert-pat ((VAL P0) ()) build-list 
					 IGNORED Guard Cata failed (Vars ...) (CataVars ...)))))
	    
	    ;; TODO, HANDLE NULL CASE:
; 	    (if (null? Obj) 
; 		(bind-vars-null (collect-vars () P0 ())
; 				(bind-cata-null (collect-cata-vars P0)
; 						Bod Guard)))

	    (let loop ((ls Obj) (acc '()))
	      (cond
	       ((null? ls)
		(let* ((rotated (apply map list (reverse acc))))
		  (apply 
		   (ellipses-helper (Vars ...) (CataVars ...)
				    (convert-pat Stack exec-body Bod Guard Cata NextClause)
				    P0)
		   ;; When we pop the cata-var we pop the whole list.
		   (map (lambda (ls) (lambda () (map (lambda (th) (th)) ls)))
		     rotated)
		   )))
	       (else (loop (cdr ls) 
			   (cons (project (car ls)) acc)))))))))
	
	;; Pair pattern:  Do car, push cdr onto stack.
	((_ ((Obj (P0 . P1)) Stack) Exec Bod Guard Cata NextClause Vars CataVars)
	 #'(if (pair? Obj)
	     (let ((head (car Obj))
		   (tail (cdr Obj)))
	       (convert-pat ((head P0) ((tail P1) Stack))
			    Exec Bod Guard Cata NextClause Vars CataVars))
	     (NextClause)))

	;; Vector pattern
	((_ ((Obj #(Pat ...)) Stack) Exec Bod Guard Cata NextClause Vars CataVars)
	 ;; Hope the compiler manages to evaluate this 'length' call:
	 #'(if (and (vector? Obj) (fx= (vector-length Obj) (length '(Pat ...))))
	     ;; This creates redundant vector 
	     (vecref-helper Obj 0 () (Stack Exec Bod Guard Cata NextClause Vars CataVars) Pat ...)
	     ;(convert-pat Stack ___ )
	     ;(convert-pat Stack ___ Exec Bod Guard Cata NextClause Vars CataVars)
	     (NextClause)))

	;; Literal pattern.
	;; Since we're using syntax-rules here we can't tell much.
	((_ ((Obj LIT) Stack) Exec Bod Guard Cata NextClause Vars CataVars)
	 #'(begin 
	   ;; Hopefully this happens at compile-time:

; 	   (ASSERT 
; ;	   (DEBUGASSERT
; 	           (or (symbol? (quote LIT))
; 		       (null? (quote LIT))
; 		       (boolean? (quote LIT))
; 		       (string? (quote LIT))
; 		       (number? (quote LIT))))


	   (if (equal? Obj (quote LIT))
	       (convert-pat Stack Exec Bod Guard Cata NextClause Vars CataVars)
	       (NextClause))))

	;; Otherwise, syntax error.
		 
	)))

) ;; End module




#!eof




  (collect 4)(define val '(foo 1 2 (bar 3 4 5)))
  (time (rep 10000000 (match val [(foo ,x ,y) 'no]
     [(bar ,[x] ,[y] ,[z]) `(bar ,x ,y ,z)] [(foo ,[x] ,[y] ,[z]) `(foo ,x ,y ,z)] [,_ 0])))


  (collect 4)(define val #(foo 1 2 #(bar 3 4 5)))
  (time (rep 10000000 (match val [#(foo ,x ,y) 'no]
     [#(bar ,[x] ,[y] ,[z]) `#(bar ,x ,y ,z)] [#(foo ,[x] ,[y] ,[z]) `#(foo ,x ,y ,z)] [,_ 0])))

