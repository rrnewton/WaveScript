
;; How far can we get doing match with syntax-rules.


;; match-lambda
;; let-match
;; trace-match... etc.

(eval-when (compile eval load) (case-sensitive #t))

(module (match match-help convert-pat  test ASSERT
	       bind-popped-vars ellipses-helper build-lambda
	       bind-dummy-vars bind-cata
	       exec-body build-list
	       )

(define-syntax ASSERT
  (syntax-rules ()
    [(_ expr) (or expr (error 'ASSERT " failed: ~s" 'expr))]))

(define-syntax match
  (syntax-rules ()
    [(_ Exp Clause ...)
     (let f ((x Exp))
       (match-help _ f x Clause ...))]))

(define-syntax match-help
  (syntax-rules (guard)
    [(_ Template Cata Obj )  (error 'match "no next clause")]
    [(_ Template Cata Obj (Pat (guard G ...) B0 Bod ...) Rest ...)
     (let ([next (lambda () (match-help Template Cata Obj Rest ...))])
       (convert-pat ((Obj Pat))
		    exec-body 	   
		    (begin B0 Bod ...) (and G ...)
		    Cata next () ()))]
    [(_ Template Cata Obj (Pat B0 Bod ...) Rest ...)
     (let ([next (lambda () (match-help Template Cata Obj Rest ...))])
       (convert-pat ((Obj Pat)) 
		    exec-body 		   
		    (begin B0 Bod ...) #t
		    Cata next () ())
       )]))

(define-syntax bind-popped-vars
  (syntax-rules ()
    [(_ () Bod)  Bod]
    [(_ (V0 . V*) Bod)
     (let ([V0 (V0)])
       (bind-popped-vars V* Bod))]))

(define-syntax bind-dummy-vars
  (syntax-rules ()
    [(_ () Bod)  Bod]
    [(_ (V0 . V*) Bod)
     (let ([V0 'match-error-cannot-use-cata-var-in-guard])
       (bind-popped-vars V* Bod))]))

(define-syntax exec-body
  (syntax-rules ()
    [(_  Bod Guard NextClause Vars CataVars)
     (bind-popped-vars Vars
         (if (bind-dummy-vars CataVars Guard)
	     (bind-popped-vars CataVars Bod)
	     (NextClause)
	     ))]))

;; Like exec-body but just builds a list of all the vars.
;; This puts CataVars first in the list.
(define-syntax build-list 
  (syntax-rules ()
    [(_ __ #t #f () ())   ()]
    [(_ __ #t #f (V . Vars) CataVars)
     (cons V (build-list __ #t #f Vars CataVars))]
    [(_ __ #t #f () (V . CataVars))
     (cons V (build-list __ #t #f () CataVars))]
    [(_ __ Guard NextClause Vars CataVars)
     (if (bind-popped-vars Vars
          (bind-popped-vars CataVars Guard))
	 (build-list __ #t #f Vars CataVars)
	 (NextClause))]))

(define-syntax bind-cata
  (syntax-rules ()
    [(_ Bod Promise Args ())  Bod]
    [(_ Bod Promise Args (V0 . V*))
     (let ([V0 (lambda ()
		 (call-with-values (lambda () (force Promise))
		   (lambda Args V0)))])
       (bind-cata Bod Promise Args V*))]))

;; This does the job of "collect-vars", but it also carries a
;; continuation (of a sort) with it.
(define-syntax ellipses-helper
  (syntax-rules (unquote ->)
    [(_ (Vars ...) (CataVars ...) (Bod ...))
     (lambda (CataVars ... Vars ...)
       (Bod ... (Vars ...) (CataVars ...)))]
    
    [(_ Vars (CataVars ...) Bod (unquote (FUN -> V* ...)) . Rest)
     (ellipses-helper Vars (V* ... CataVars ...) Bod  . Rest)]
    [(_ Vars (CataVars ...) Bod (unquote (V* ...)) . Rest)
     (ellipses-helper Vars (V* ... CataVars ...) Bod  . Rest)]
    [(_ Vars CataVars Bod (unquote V) . Rest)
     (ellipses-helper (V . Vars) CataVars Bod  . Rest)]
    
    [(_ Vars CataVars Bod () . Rest)
     (ellipses-helper Vars CataVars Bod  . Rest)]
    
    [(_ Vars CataVars Bod (P0 . P*) . Rest)
     (ellipses-helper Vars CataVars Bod P0 P* . Rest)]
    ;; Otherwise just assume its a literal:
    [(_ Vars CataVars Bod LIT . Rest)
     (ellipses-helper Vars CataVars Bod . Rest)]
    ))

(define-syntax build-lambda (syntax-rules () [(_) lambda]))

;; Convert a pattern into a function that will test for a match.
;;
;; This takes several arguments:
;;   Stack -- Objs&Patterns left to match.  Objs should be just vars.
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
    (syntax-rules (unquote .... ->)

      ;; Termination condition:
      ;; Now check the guard and (possibly) execute the body.
      [(_ () Exec Bod Guard Cata NextClause Vars CataVars)
       (Exec Bod Guard NextClause Vars CataVars)]
      
      ;; Cata redirect: 
      [(_ ([Obj (unquote (f -> V0 V* ...))] . Stack) Exec Bod Guard Cata NextClause Vars (CataVars ...))
       (let ([promise (delay (f Obj))])
	 ;(inspect f)
	 (bind-cata 
	  (convert-pat Stack Exec Bod Guard Cata
		       NextClause Vars (V0 V* ... CataVars ...))
	  promise
	  (V0 V* ...) (V0 V* ...)))]
      
      ;; Unquote Pattern, Cata: recursively match
      [(_ ([Obj (unquote (V0 V* ...))] . Stack) Exec Bod Guard Cata NextClause Vars (CataVars ...))
       (let ([promise (delay (Cata Obj))])
	 (bind-cata 
	  (convert-pat Stack Exec Bod Guard Cata
		       NextClause Vars (V0 V* ... CataVars ...))
	  promise
	  (V0 V* ...) (V0 V* ...)))]
	
      ;; Unquote Pattern: bind a pattern variable:
      [(_ ([Obj (unquote V)] . Stack) Exec Bod Guard Cata NextClause Vars CataVars)
       (let ([V (lambda () Obj)])
	 (convert-pat Stack Exec Bod Guard Cata NextClause (V . Vars) CataVars))]

      ;; Ellipses:
      [(_ ([Obj (P0 ....)] . Stack) Exec Bod Guard Cata NextClause (Vars ...) (CataVars ...))
       (call/cc 
	(lambda (escape)
	  (let* ([failed (lambda () (escape (NextClause)))]
		 ;; Bind a pattern-matcher for one element of the list.	
		 [project (lambda (VAL)
			    (convert-pat ([VAL P0]) build-list IGNORED Guard Cata failed (Vars ...) (CataVars ...)))])
	    
	    ;; TODO, HANDLE NULL CASE:
	    #;
	    (if (null? Obj) 
		(bind-vars-null (collect-vars () P0 ())
				(bind-cata-null (collect-cata-vars P0)
						Bod Guard)))

	    (let loop ([ls Obj] [acc '()])
	      (cond
	       [(null? ls)
		(let* ([rotated (apply map list (reverse! acc))])
		  (apply 
		   (ellipses-helper (Vars ...) (CataVars ...)
				    (convert-pat Stack exec-body Bod Guard Cata NextClause)
				    P0)
		   ;; When we pop the cata-var we pop the whole list.
		   (map (lambda (ls) (lambda () (map (lambda (th) (th)) ls)))
		     rotated)
		   ))]
	       [else (loop (cdr ls) 
			   (cons (project (car ls)) acc))])))))]
	
	;; Pair pattern:  Do car, push cdr onto stack.
	[(_ ([Obj (P0 . P1)] . Stack) Exec Bod Guard Cata NextClause Vars CataVars)
	 (if (pair? Obj)
	     (let ([head (car Obj)]
		   [tail (cdr Obj)])
	       (convert-pat ([head P0] [tail P1] . Stack)
			    Exec Bod Guard Cata NextClause Vars CataVars))
	     (NextClause)
	     )]
		
	;; Literal pattern.
	;; Since we're using syntax-rules here we can't tell much.
	[(_ ([Obj LIT] . Stack) Exec Bod Guard Cata NextClause Vars CataVars)
	 (begin 
	   ;; Hopefully this happens at compile-time:
	   (ASSERT (or (symbol? (quote LIT))
		       (null? (quote LIT))
		       (boolean? (quote LIT))
		       (string? (quote LIT))
		       (number? (quote LIT))))
	   (if (equal? Obj (quote LIT))
	       (convert-pat Stack Exec Bod Guard Cata NextClause Vars CataVars)
	       (NextClause)))]

	;; Otherwise, syntax error.
	))


(define (test)
  (for-each 
      (lambda (pr)
	(printf "   Test: ~a\n" (car pr))
	(if (equal? (eval (car pr)) (cadr pr))
	    (printf "-- Passed.\n" )
	    (printf "-- FAILED.\n" )
	    ))
    '(   
      [(match 3 [,x x]) 3]

      [(match '(1 2) [(,x ,y) (+ x y)]) 3]
      
      [(match '(1 2) [(,x ,y ,z) (+ x x y)] [(,x ,y) (* 100 y)]) 200]
      
      [(match '(1 2) [(,x ,y ,z) (+ x x y)] [,v v]) (1 2)]

      [(match '(1 2) [(3 ,y) (* 1000 y)] [(1 ,y) (* 100 y)]) 200]

      [(match '(1 2) [(,[x] ,[y]) (list x y)] [1 3] [2 4]) (3 4)]

      [(match '(1 2) [(,[x y] ,[z w]) (list x y z w)] [1 (values 3 4)] [2 (values 5 6)])
       (3 4 5 6)]

      [(match '(1 . 2) [(,x . ,y) y]) 2]

;      [(expand '(collect-vars () (a b) (d ,e f (,g)))) (g e a b)]
      
      [(match '(1 2 3) [(1 ,x* ....) x*]) (2 3)]
      [(match '((a 1) (b 2) (c 3)) [([,x* ,y*] ....) (vector x* y*)]) #((a b c) (1 2 3))]
      [(match '((a 1) (b 2) (c 3 4)) [([,x* ,y*] ....) (vector x* y*)] [,_ 'yay]) yay]
      
      [(match '(1 2 3) [(1 ,[add1 -> x] ,[add1 -> y]) (list x y)]) (3 4)]
;      [(match '(1 2 3) [(1 ,[add1 -> x*] ....) x*]) (3 4)]

      ;; Basic guard:
      [(match 3 [,x (guard (even? x)) 44] [,y (guard (< y 40) (odd? y)) 33]) 33]

#;
      ;; Make sure we keep those bindings straight.
      [(match '((a 2 9) (b 2 99) (c 2 999))
	 [([,x 2 ,[y]] ....) (vector x y)]
	 [,n (add1 n)])
       ]


      )))

  (printf "TESTING: ~a\n" (test))
;; End module:
)


#!eof


;; here's something that takes apart quoted stuff. 

(define destruct
  (lambda (datum)
    (match datum
      (() `'())
      ((,[X] . ,[Y])`(cons ,X ,Y))
;      (#(,[X] ...) `(vector ,X ...))
#;
      (,thing
	(guard (symbol? thing))
	`',thing)
      (,thing
	thing))))

;; examples using explicit Catas

(define sumsquares
  (lambda (ls)
    (define square 
      (lambda (x)
        (* x x)))
    (match ls 
      [(,[a*] ...) (apply + a*)]
      [,[square -> n] n])))

(define sumsquares
  (lambda (ls)
    (define square 
      (lambda (x)
        (* x x)))
    (let ([acc 0])
      (match+ (acc) ls 
        [(,[] ...) acc]
        [,[(lambda (acc x) (+ acc (square x))) ->] acc]))))

;;; The following uses explicit Catas to parse programs in the
;;; simple language defined by the grammar below

;;; <Prog> -> (program <Stmt>* <Expr>)
;;; <Stmt> -> (if <Expr> <Stmt> <Stmt>)
;;;         | (set! <var> <Expr>)
;;; <Expr> -> <var>
;;;         | <integer>
;;;         | (if <Expr> <Expr> <Expr>)
;;;         | (<Expr> <Expr*>)


(define parse
  (lambda (x)
    (define Prog
      (lambda (x)
        (match x
          [(program ,[Stmt -> s*] ... ,[Expr -> e])
           `(begin ,s* ... ,e)]
          [,other (error 'parse "invalid program ~s" other)])))
    (define Stmt
      (lambda (x)
        (match x
          [(if ,[Expr -> e] ,[Stmt -> s1] ,[Stmt -> s2])
           `(if ,e ,s1 ,s2)]
          [(set! ,v ,[Expr -> e])
           (guard (symbol? v))
           `(set! ,v ,e)]
          [,other (error 'parse "invalid statement ~s" other)])))
    (define Expr
      (lambda (x)
        (match x
          [,v (guard (symbol? v)) v]
          [,n (guard (integer? n)) n]
          [(if ,[e1] ,[e2] ,[e3])
           `(if ,e1 ,e2 ,e3)]
          [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
          [,other (error 'parse "invalid expression ~s" other)])))
    (Prog x)))
;;; (parse '(program (set! x 3) (+ x 4)))) => (begin (set! x 3) (+ x 4))

