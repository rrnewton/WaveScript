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

(module rn-match ((match match-help bind-dummy-vars bind-popped-vars exec-body 
			 build-list bind-cata ellipses-helper delay-values 
			 countup-vars countup-elements wrap-lambda-at-the-end 
			 vecref-helper vecref-helper2 convert-pat
			 extend-backquote my-backquote
			 pop force-and-select
			 )
		  my-backquote
		  (extend-backquote my-backquote)
		  test-match 
		  tm2)
  
(define-syntax match
  (syntax-rules ()
    ((_ Exp Clause ...)
     (let Rematch ((Orig Exp))
       (match-help _ Rematch Orig Clause ...)))))

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
(define-syntax delay-values
  (syntax-rules ()
    ((_ e) (delay e))))

#;
;; Do I really need a promise?
;; [2007.04.08] ACK! At least for now I still do...
(define-syntax delay-values
  (syntax-rules ()
    ((_ e) (lambda () e))))

#|
;; Debugging:
(define-syntax delay-values
  (syntax-rules ()
    ((_ e) (let ([run? #f])
	     (lambda ()
	       (if run? (error 'delay-values "tried to pop cata again! ~s" #'e))
	       (set! run? #t)
	       e)))))
(define-syntax force-and-select
  (syntax-rules ()
    ((_ Promise Args V0)
     (pop Promise (lambda Args V0)))))
|#
(define (pop promise fun)
  (printf "Popping! ~s\n" promise)
  (call-with-values promise fun))


(define-syntax force-and-select
  (syntax-rules ()
    ((_ Promise Args V0)
     (call-with-values Promise
       ;; Select out just our variable from the results:
       (lambda Args V0)))))

(define-syntax MATCH_ASSERT
  (syntax-rules ()
    ((_ expr) (or expr (error 'ASSERT " failed: ~s" 'expr)))))


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
		    (extend-backquote Template B0 Bod ...)    ;; Total body.
		    (extend-backquote Template (and G ...))   ;; Guard expression
		    Cata next () ())))
    ;; Unguarded:
    ((_ Template Cata Obj (Pat B0 Bod ...) Rest ...)
     (let ((next (lambda () (match-help Template Cata Obj Rest ...))))
       (convert-pat ((Obj Pat) ()) 
		    exec-body 		   
		    (extend-backquote Template B0 Bod ...)    ;; Total body.
		    #t                                        ;; Guard expression
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
    ;; This case executes first:
    ((_ __ Guard NextClause Vars CataVars)
     (if (bind-popped-vars Vars
	  ;; We're still in matching phase, can't really pop the cata's yet:
          (bind-dummy-vars CataVars Guard))
	 (build-list __ #t #f Vars CataVars)
	 (NextClause)))))

(define-syntax bind-cata
  (syntax-rules ()
    ((_ Bod Promise Args ())  Bod)

    ;; Optimization, handle one argument, no call-with-values:
#;
    ((_ Bod Promise (V0) __)
     (let ([V0 Promise]) Bod))
    
    ;; This is where we multiply pop a promise just to get at one of the values each time:
    ((_ Bod Promise Args (V0 . V*))
     (let ((V0 (lambda ()
		 ;; Maybe Inefficient:
		 (force-and-select Promise Args V0))))
       (bind-cata Bod Promise Args V*)))))


;; This does the job of "collect-vars", but it also carries a
;; continuation (of a sort) with it.
(define-syntax ellipses-helper
  (syntax-rules (unquote ->)
    ((_ (Vars ...) (CataVars ...) (Bod ...))
     (Bod ... (Vars ...) (CataVars ...)))
    
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

    ((_ Vars CataVars Bod #(P* ...) . Rest)
     (ellipses-helper Vars CataVars Bod P* ... . Rest))

    ;; Otherwise just assume its a literal:
    ((_ Vars CataVars Bod LIT . Rest)
     (ellipses-helper Vars CataVars Bod . Rest))
    ))

(define-syntax wrap-lambda-at-the-end
  (syntax-rules ()
    ((_ (Bod ...) (Vars ...) (CataVars ...))
     (lambda (Vars ... CataVars ...)
       (Bod ... (Vars ...) (CataVars ...))))))


(define-syntax countup-elements
  (syntax-rules ()
    ((_ ) 0)
    ((_ X X* ...)
     (fx+ 1 (countup-elements X* ...)))))

(define-syntax countup-vars
  (syntax-rules ()
    ((_ () ()) 0)
    ((_ () (C CataVars ...))
     (fx+ 1 (countup-vars () (CataVars ...))))
    ((_ (V Vars ...) (CataVars ...))
      (fx+ 1 (countup-vars (Vars ...) (CataVars ...)))
     )))


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
      ;; We set up the binding for a Cata, but we shouldn't execute it until weget to the body.
      ((_ ((Obj (unquote (f -> V0 V* ...))) Stack) Exec Bod Guard Cata NextClause Vars (CataVars ...))
       #'(let ((promise (delay-values (f Obj))))
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
      ;; TEMP: we thunk these pattern variables even though we shouldn't have to.
      ;; This is for consistency with the Cata vars.
      ((_ ((Obj (unquote V)) Stack) Exec Bod Guard Cata NextClause Vars CataVars)
       #'(let ((V (lambda () Obj)))
	 (convert-pat Stack Exec Bod Guard Cata NextClause (V . Vars) CataVars)))

      ;; Ellipses with something following:
      ((_ ((Obj (P0 Dots P1 P* ...)) Stack) Exec Bod Guard Cata NextClause Vars CataVars)
       (ellipsis? #'Dots)
       ;; This case is pretty inefficient right now:
       #'(if (list? Obj)
	     (let ([len (length Obj)]
		   [remains (countup-elements P1 P* ...)])
	       (if (< len remains)
		   (NextClause)
		   (let ([hd (list-head Obj (- len remains))]
			 [tl (list-tail Obj (- len remains))])
		     (convert-pat ((hd (P0 Dots)) ((tl (P1 P* ...)) Stack)) 
				  Exec Bod Guard Cata NextClause Vars CataVars))))
	     (NextClause)))

      ;; Need to implement full ellipses for vector patterns, for now just doing this limited form:
      ;; Ellipses necessitate turning it into a list:
      ((_ ((Obj #(P0 Dots P* ...)) Stack) Exec Bod Guard Cata NextClause Vars CataVars)
       (ellipsis? #'Dots)
       #'(if (vector? Obj)
	     (let ([v2ls (vector->list Obj)])
	       (convert-pat ((v2ls (P0 Dots P* ...)) Stack) Exec Bod Guard Cata NextClause Vars CataVars))
	     (NextClause)))

#|


(begin 
  (print-graph #f)(optimize-level 2)(print-gensym #f)(import rn-match)(pretty-line-length 170)
  (define val '(let ([x Int (+. '1 '2)]) bod))
  (expand '(match val 
    [(,let ([,id* ,t* ,[rhs*]] ...) ,[bod]) 
     (guard (printf "GUARDING ~s \n" id*) #f)
     (printf " LET: ~s\n" id*)]
    [(,prim ,[rand*] ...)
     (guard (regiment-primitive? prim))
     (printf "  HMM ~s\n" prim)
     'prim]
    [,oth 'fail])))


(begin 
  (print-graph #f)(optimize-level 2)(print-gensym #f)(import rn-match)(pretty-line-length 170)
  (define val '(let ([x Int (+. '1 '2)]) bod))
  (eval '(match val 
    [(,let ([,id* ,t* ,[rhs*]] ...) ,[bod]) 
     (guard (printf "GUARDING ~s \n" id*) #f)
     (printf " LET: ~s\n" id*)]
    [(,prim ,[rand*8] ...)
     (guard (regiment-primitive? prim))
     (printf "  HMM ~s\n" prim)
     'prim]
    [,oth 'fail])))



|#

      ;; Ellipses:
      ((_ ((Obj (P0 Dots)) Stack) Exec Bod Guard Cata NextClause (Vars ...) (CataVars ...))
       (ellipsis? #'Dots)
       #'(call-with-current-continuation
	(lambda (escape)
	  (let* ((failed (lambda () (escape (NextClause))))
		 ;; Bind a pattern-matcher for one element of the list.	
		 ;; It returns the pattern variables' bindings in a list:
		 (project (lambda (VAL)
			    (convert-pat ((VAL P0) ())
					 build-list ;; Replacement for exec-body
					 'IGNORED #t Cata failed () ()))))
	    ;; Here is the code that loops through the list at runtime.
	    ;; Projecting out a set of variables from each list element.
	    (if (or (null? Obj) (pair? Obj))
		(let ellipses-loop ((ls Obj) (acc '()))
		  (cond
		   ((null? ls)
		    (apply 
		     ;; First we gather just the variables in this ellipses pattern.
		     (ellipses-helper () ()
		        (wrap-lambda-at-the-end ;; We take those in as a list.
			 ;; If we get past this pattern we're on to the next one.
			 ;; But P0's variables are already bound.
			 (ellipses-helper (Vars ...) (CataVars ...)
					  (convert-pat Stack exec-body Bod Guard Cata NextClause) P0))
			P0)
		     (if (null? Obj)
			 ;; Build a list of (thunked) nulls of the right length:
			 (make-list 
			  (ellipses-helper () () (countup-vars) P0)
			  (lambda () '()))
			 ;; When we pop the cata-var we pop the whole list.
			 (map (lambda (ls) (lambda () (map (lambda (th) (th)) ls)))
			   ;; Rotate:
			   (apply map list (reverse acc)))
			 )))
		 (else (ellipses-loop (cdr ls) 
			 (cons (project (car ls)) acc)))))
		(NextClause))
	    ))))
	
	;; Pair pattern:  Do car, push cdr onto stack.
	((_ ((Obj (P0 . P1)) Stack) Exec Bod Guard Cata NextClause Vars CataVars)
	 #'(if (pair? Obj)
	     (let ((head (car Obj))
		   (tail (cdr Obj)))
	       (convert-pat ((head P0) ((tail P1) Stack))
			    Exec Bod Guard Cata NextClause Vars CataVars))
	     (NextClause)))

	;; Vector pattern
	((_ ((Obj #(P0 Pat ...)) Stack) Exec Bod Guard Cata NextClause Vars CataVars)
	 ;; Hope the compiler manages to evaluate this 'length' call:
	 #'(if (and (vector? Obj) (fx= (vector-length Obj) (length '(P0 Pat ...))))
	     ;; This creates redundant vector 
	     (vecref-helper Obj 0 () (Stack Exec Bod Guard Cata NextClause Vars CataVars) P0 Pat ...)
	     ;(convert-pat Stack ___ )
	     ;(convert-pat Stack ___ Exec Bod Guard Cata NextClause Vars CataVars)
	     (NextClause)))

	;; Literal pattern.
	;; Since we're using syntax-rules here we can't tell much.
	((_ ((Obj LIT) Stack) Exec Bod Guard Cata NextClause Vars CataVars)
	 (begin
	   (MATCH_ASSERT (not (ellipsis? #'LIT)))
	   (MATCH_ASSERT 
	    (or (symbol? (syntax-object->datum #'LIT))
		(null?   (syntax-object->datum #'LIT))
		(boolean? (syntax-object->datum #'LIT))
		(string?  (syntax-object->datum #'LIT))
		(number?  (syntax-object->datum #'LIT))
		(vector?  (syntax-object->datum #'LIT)) ;; Letting empty vector fall through to here.
		))
	   #'(begin 
	       (if (equal? Obj (quote LIT))
		   (convert-pat Stack Exec Bod Guard Cata NextClause Vars CataVars)
		   (NextClause)))))

	;; Otherwise, syntax error.
		 
	)))

(define (test-match)
  (for-each 
      (lambda (pr)
	(display "   Test: ") (write (car pr)) (newline)
	(if (equal? (eval (car pr) (interaction-environment)) ;(scheme-report-environment 5)
		    (cadr pr))
	    (begin (display "-- Passed." ) (newline))
	    (begin (display "-- FAILED." ) (newline))
	    ))
    '(   
      ((match 3 (,x x)) 3)

      ((match '(1 2) ((,x ,y) (+ x y))) 3)
      
      ((match '(1 2) ((,x ,y ,z) (+ x x y)) ((,x ,y) (* 100 y))) 200)
      
      ((match '(1 2) ((,x ,y ,z) (+ x x y)) (,v v)) (1 2))

      ((match '(1 2) ((3 ,y) (* 1000 y)) ((1 ,y) (* 100 y))) 200)

      ((match '(1 2) ((,(x) ,(y)) (list x y)) (1 3) (2 4)) (3 4))

      ((match '(1 2) ((,(x y) ,(z w)) (list x y z w)) (1 (values 3 4)) (2 (values 5 6)))
       (3 4 5 6))

      ((match '(1 . 2) ((,x . ,y) y)) 2)

      ((match '(1 2 3) ((1 ,x* ...) x*)) (2 3))
      ((match '((a 1) (b 2) (c 3)) (((,x* ,y*) ...) (vector x* y*))) #((a b c) (1 2 3)))
      ((match '((a 1) (b 2) (c 3 4)) (((,x* ,y*) ...) (vector x* y*)) (,_ 'yay)) yay)

      ;; Redirect:
      ((match '(1 2 3) ((1 ,(add1 -> x) ,(add1 -> y)) (list x y))) (3 4))

      ;; Basic guard:
      ((match 3 (,x (guard (even? x)) 44) (,y (guard (< y 40) (odd? y)) 33)) 33)

      ;; Redirect and ellipses.
;     ((match '(1 2 3) ((1 ,(add1 -> x*) ...) x*)) (3 4))

;       ;; Make sure we keep those bindings straight.
;       ((match '((a 2 9) (b 2 99) (c 2 999))
; 	 (((,x 2 ,(y)) _...) (vector x y))
; 	 (,n (add1 n)))
;        )

      )))

(define (tm2)
  (match '(letrec () 'hmm)
      [(,let ([,id* ,t* ,[rhs*]] ...) ,[bod]) 
       (guard (memq let '(let letrec lazy-letrec)))
       (vector let id* t* rhs* bod)]
    [,oth 99]))



;; TEMP: 
;; Using the quasiquote extension from match.ss
;; I need to rewrite this in a portable way or remove my dependency on it:

(define-syntax my-backquote
  (lambda (x)
    (define ellipsis?
      (lambda (x)
        (and (identifier? x) (literal-identifier=? x #'(... ...)))))
    (define-syntax with-values
      (syntax-rules ()
        ((_ P C) (call-with-values (lambda () P) C))))
    (define-syntax syntax-lambda
      (lambda (x)
        (syntax-case x ()
          ((_ (Pat ...) Body0 Body ...)
           (with-syntax (((X ...) (generate-temporaries #'(Pat ...))))
             #'(lambda (X ...)
                 (with-syntax ((Pat X) ...)
                   Body0 Body ...)))))))
    (define-syntax with-temp
      (syntax-rules ()
        ((_ V Body0 Body ...)
         (with-syntax (((V) (generate-temporaries '(x))))
           Body0 Body ...))))
    (define-syntax with-temps
      (syntax-rules ()
        ((_ (V ...) (Exp ...) Body0 Body ...)
         (with-syntax (((V ...) (generate-temporaries #'(Exp ...))))
           Body0 Body ...))))
    (define destruct
      (lambda (Orig x depth)
        (syntax-case x (quasiquote unquote unquote-splicing)
          ;; inner quasiquote
          ((quasiquote Exp)
           (with-values (destruct Orig #'Exp (add1 depth))
             (syntax-lambda (Builder Vars Exps)
               (if (null? #'Vars)
                   (values #''(quasiquote Exp) '() '())
                   (values #'(list 'quasiquote Builder) #'Vars #'Exps)))))
          ;; unquote
          ((unquote Exp)
           (zero? depth)
           (with-temp X
             (values #'X (list #'X) (list #'Exp))))
          ((unquote Exp)
           (with-values (destruct Orig #'Exp (sub1 depth))
             (syntax-lambda (Builder Vars Exps)
               (if (null? #'Vars)
                   (values #''(unquote Exp) '() '())
                   (values #'(list 'unquote Builder) #'Vars #'Exps)))))
          ;; splicing
          (((unquote-splicing Exp))
           (zero? depth)
           (with-temp X
             (values #'X (list #'X) (list #'Exp))))
          (((unquote-splicing Exp ...))
           (zero? depth)
           (with-temps (X ...) (Exp ...)
             (values #'(append X ...) #'(X ...) #'(Exp ...))))
          (((unquote-splicing Exp ...) . Rest)
           (zero? depth)
           (with-values (destruct Orig #'Rest depth)
             (syntax-lambda (Builder Vars Exps)
               (with-temps (X ...) (Exp ...)
                 (if (null? #'Vars)
                     (values #'(append X ... 'Rest)
                             #'(X ...) #'(Exp ...))
                     (values #'(append X ... Builder)
                             #'(X ... . Vars) #'(Exp ... . Exps)))))))
          ((unquote-splicing Exp ...)
           (with-values (destruct Orig #'(Exp ...) (sub1 depth))
             (syntax-lambda (Builder Vars Exps)
               (if (null? #'Vars)
                   (values #''(unquote-splicing Exp ...) '() '())
                   (values #'(cons 'unquote-splicing Builder)
                           #'Vars #'Exps)))))
          ;; dots
          (((unquote Exp) Dots)
           (and (zero? depth) (ellipsis? #'Dots))
           (with-temp X
             (values #'X (list #'X) (list #'Exp))))
          (((unquote Exp) Dots . Rest)
           (and (zero? depth) (ellipsis? #'Dots))
           (with-values (destruct Orig #'Rest depth)
             (syntax-lambda (RestBuilder RestVars RestExps)
               (with-syntax ((TailExp
                               (if (null? #'RestVars)
                                   #''Rest
                                   #'RestBuilder)))
                 (with-temp X
                   (values #'(append X TailExp)
                           (cons #'X #'RestVars)
                           (cons #'Exp #'RestExps)))))))
          ((Exp Dots . Rest)
           (and (zero? depth) (ellipsis? #'Dots))
           (with-values (destruct Orig #'Exp depth)
             (syntax-lambda (ExpBuilder (ExpVar ...) (ExpExp ...))
               (if (null? #'(ExpVar ...))
                   (syntax-error Orig "Bad ellipsis")
                   (with-values (destruct Orig #'Rest depth)
                     (syntax-lambda (RestBuilder RestVars RestExps)
                       (with-syntax ((TailExp
                                       (if (null? #'RestVars)
                                           #''Rest
                                           #'RestBuilder))
                                     (Orig Orig))
                         (values #'(let f ((ExpVar ExpVar) ...)
                                     (if (and (pair? ExpVar) ...)
                                         (cons
                                           (let ((ExpVar (car ExpVar)) ...)
                                             ExpBuilder)
                                           (f (cdr ExpVar) ...))
                                         (if (and (null? ExpVar) ...)
                                             TailExp
                                             (error 'unquote
                                               "Mismatched lists in ~s"
                                               Orig))))
                                 (append #'(ExpVar ...) #'RestVars)
                                 (append #'(ExpExp ...) #'RestExps)))))))))
	  ;; Vectors
	  (#(X ...)
	   (with-values (destruct Orig #'(X ...) depth)
	     (syntax-lambda (LsBuilder LsVars LsExps)
	       (values #'(list->vector LsBuilder) #'LsVars #'LsExps))))
          ;; random stuff
          ((Hd . Tl)
           (with-values (destruct Orig #'Hd depth)
             (syntax-lambda (HdBuilder HdVars HdExps)
               (with-values (destruct Orig #'Tl depth)
                 (syntax-lambda (TlBuilder TlVars TlExps)
                   (with-syntax ((Hd (if (null? #'HdVars)
                                         #''Hd
                                         #'HdBuilder))
                                 (Tl (if (null? #'TlVars)
                                         #''Tl
                                         #'TlBuilder)))
                     (values #'(cons Hd Tl)
                             (append #'HdVars #'TlVars)
                             (append #'HdExps #'TlExps))))))))
          (OtherThing
            (values #''OtherThing '() '())))))
    ;; macro begins
    (syntax-case x ()
      ((_ Datum)
       (with-values (destruct #'(quasiquote Datum) #'Datum 0)
         (syntax-lambda (Builder (Var ...) (Exp ...))
           (if (null? #'(Var ...))
               #''Datum
               #'(let ((Var Exp) ...)
                   Builder))))))))

(define-syntax extend-backquote
  (lambda (x)
    (syntax-case x ()
      ((_ Template Exp ...)
       (with-syntax ((quasiquote
                       (datum->syntax-object #'Template 'FLUB)))
         #'(let-syntax ((quasiquote
                          (lambda (x)
                            (syntax-case x ()
                              ((_ Foo) #'(my-backquote Foo))))))
             Exp ...))))))







) ;; End module




#!eof




  (collect 4)(define val '(foo 1 2 (bar 3 4 5)))
  (time (rep 10000000 (match val [(foo ,x ,y) 'no]
     [(bar ,[x] ,[y] ,[z]) `(bar ,x ,y ,z)] [(foo ,[x] ,[y] ,[z]) `(foo ,x ,y ,z)] [,_ 0])))


  (collect 4)(define val #(foo 1 2 #(bar 3 4 5)))
  (time (rep 10000000 (match val [#(foo ,x ,y) 'no]
     [#(bar ,[x] ,[y] ,[z]) `#(bar ,x ,y ,z)] [#(foo ,[x] ,[y] ,[z]) `#(foo ,x ,y ,z)] [,_ 0])))






(match '(timer 3.0)
  [(,prim ,[rand*] ...)
   (guard (regiment-primitive? prim))
   9999]
  [,oth 78])


(define (print-var-types exp max-depth . p)
  (IFCHEZ (import rn-match) (void))
  (let ([port (if (null? p) (current-output-port) (car p))])
    
    (trace-define (get-var-types exp)
)

   
    (let loop ([x (get-var-types exp)] [depth 0] [indent " "])
      (if (= depth max-depth) (void)
	  (match x
	    [() (void)]
	    [(type ,v ,t ,subvars)
	     (unless (eq? v '___VIRTQUEUE___) 	 ;; <-- HACK: 
	       (fprintf port "~a~a :: " indent v)
	       (print-type t port) (newline port))
	     (loop subvars (fx+ 1 depth) (++ indent "  "))]
	    [,ls (guard (list? ls))
		 (for-each (lambda (x) (loop x depth indent))
		   ls)]
	    [,other (error 'print-var-types "bad result from get-var-types: ~a" other)])))
      ))




      (match '(timer 3.0)

       [(,lang '(program ,[body] ,meta ... ))
	 (append body `((type BASE ,(last meta) ())))]

       [,c (guard (simple-constant? c)) '()]
       [,var (guard (symbol? var))  `()]       
       [(quote ,c)       '()]
       [(assert-type ,t ,[e]) e]
       [(set! ,v ,[e]) e]
       [(begin ,[e*] ...) (apply append e*)]
       [(for (,i ,[s] ,[e]) ,[bodls]) (cons `[type ,i Int ()] (append s e bodls))]
       [(while ,[tstls] ,[bodls]) (append tstls bodls)]

       [(if ,[t] ,[c] ,[a]) (append t c a)]
       [(tuple ,[args] ...) (apply append args)]
       [(tupref ,n ,m ,[x]) x]
       [(unionN ,[args] ...) (apply append args)]

       [(,let ([,id* ,t* ,[rhs*]] ...) ,[bod]) 
	(guard (memq let '(let letrec lazy-letrec)))
	(append (apply append 
		       (map (lambda (id t rhsls)
			      `([type ,id ,t ,rhsls]))
			 id* t* rhs*))
		bod)]
       [(lambda ,v* ,t* ,[bodls])   bodls]
       [(,app ,[rat] ,[rand*] ...) (guard (memq app '(app construct-data)))
	(apply append rat rand*)]
       [(,prim ,[rand*] ...)
	 (guard (regiment-primitive? prim))
	 (apply append rand*)]
	[,other (error 'print-var-types "bad expression: ~a" other)])


(begin (print-gensym #f)(print-graph #f)(expand '(match 3 [3 4])))
(begin (optimize-level 2)(print-gensym£h #f)(print-graph #f)(expand/optimize '(match 3 [3 4])))




  (collect 4)
  (time (rep 50000 ))

(define val '(foo 1 2 (bar 3 4 5)))
(match val [('foo x y) 'no] 
       [('bar x y z) `(bar ,(f x) ,(f y) ,(f z))]
       [('foo x y z) `(foo ,(f x) ,(f y) ,(f z))]
       [_ 0])




(import rn-match)(match '() [(,z ...) (vector x y z)]))

