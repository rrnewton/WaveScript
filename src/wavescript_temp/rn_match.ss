
;; How far can we get doing match with syntax-rules.


;; match-lambda
;; let-match
;; trace-match... etc.

(eval-when (compile eval load) (case-sensitive #t))

(module (match match-help convert-pat exec-body test ASSERT
	       list-up-all-vars list-vars-helper traverse-vars
	       list-up-vars list-up-cata-vars 
	       collect-vars collect-cata-vars
	       build-list-binder)

(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) #'(or expr (error 'ASSERT "failed: ~s" #'expr))]
      ;; This form is (ASSERT integer? x) returning the value of x.
      [(_ fun val) #'(let ([v val])
		       (if (fun v) v			   
			   (error 'ASSERT "failed: ~s\n Value which did not satisfy above predicate: ~s" #'fun 
				  v)))]
      )))

(define-syntax match
  (syntax-rules ()
    [(_ Exp Clause ...)
     (let f ((x Exp))
       (match-help _ f x Clause ...))]))

(define-syntax match-help
  (syntax-rules ()
    [(_ Template Cata Obj )  (error 'match "no next clause")]
    [(_ Template Cata Obj (Pat Bod) Rest ...)
     (let ([next (lambda () (match-help Template Cata Obj Rest ...))])
       ;; convert-pat returns a function that we apply to the value.
       (convert-pat ((Obj Pat)) 
		    exec-body 		   
		    Bod 
#;
		    (lambda (collect-vars Pat) 
		      (lambda (collect-cata-vars Pat)
			(exec-body Bod (collect-cata-sets Pat))))
		    Cata next () ())
       )]))

;(define exec (lambda catavars (lambda vars Body)))

(define-syntax exec-body
  (syntax-rules ()
    [(_ Bod ()) Bod]
    [(_ Bod (V0 . V*))
     (let ([V0 (V0)])
       (exec-body Bod V*))]))

;; Like exec-body but just builds a list of all the vars
;; Put Cata vars before normal Vars:
(define-syntax build-list 
  (syntax-rules ()
    [(_ b ()) ()]
    [(_ b ((__ V* ...) CataSets ...) Vars)
     (append (list V* ...) (build-list b (CataSets ...)))]
    [(_ b () (V . Vars)) 
     (cons V (build-list b () Vars))]    ))

;; Obsoleted:
(define-syntax build-list-binder
  (syntax-rules ()
    [(_ (Bod Rotated) (CataVar ...) (Vars ...))
     ;(inspect Rotated)
     (apply (lambda (CataVar ... Vars ...) Bod)
	    Rotated)
     ]))

  (define (test)
    (list 

     (match '(1 2) [(,x ,y) (+ x y)])
     
     (match '(1 2) [(,x ,y ,z) (+ x x y)] [(,x ,y) (* 100 y)])
          
     (match '(1 2) [(,x ,y ,z) (+ x x y)] [,v v])

     (match '(1 2) [(3 ,y) (* 1000 y)] [(1 ,y) (* 100 y)])

     (match '(1 2) [(,[x] ,[y]) (list x y)] [1 3] [2 4])

     (match '(1 2) [(,[x y] ,[z w]) (list x y z w)] [1 (values 3 4)] [2 (values 5 6)])

     (match '(1 . 2) [(,x . ,y) y])

      (print-graph #f)
      (print-gensym #f)

;      (pretty-print  (expand '(match '(1 2 3) [(,x* ....) x*])))

;      (match '(1 2 3) [(1 ,x* ....) x*])

;     (match '((a 1) (b 2) (c 3)) [([,x* ,y*] ....) (vector x* y*)])

     ))


(define-syntax syntax-flatten
  (syntax-rules ()
    [(_ Acc) Acc]
    [(_ Acc () Rest ...) 
     (syntax-flatten Acc Rest ...)]
    [(_ Acc (F . More) Rest ...)
     (syntax-flatten Acc F More Rest ...)]
    [(_ (Acc ...) F Rest ...)
     (syntax-flatten (Acc ... F) Rest ...)]
    ))
;(expand '(syntax-flatten () (1) (2 (3 ((8))))))



;; Attemp at abstracting this a bit:
(define-syntax list-up-all-vars
  (syntax-rules ()
    [(_ P) (traverse-vars () P list-vars-helper)]))
(define-syntax list-vars-helper
  (syntax-rules ()
    [(_ (Acc ...)) (list Acc ...)]))
(define-syntax traverse-vars
  (syntax-rules (unquote )
    [(_ Acc Exec) (Exec Acc)]
    [(_ Acc Exec (unquote V) . Rest)
     (traverse-vars (V . Acc) Exec . Rest)]
    [(_ Acc Exec () . Rest)
     (traverse-vars Acc Exec . Rest)]
    [(_ Acc Exec (P0 . P*) . Rest)
     (traverse-vars Acc Exec P0 P* . Rest)]
    [(_ Acc Exec LIT . Rest) 
     (traverse-vars Acc Exec . Rest)]
    ))

;; Highly redundant, might be some good way to combine, but I ran into
;; trouble the first time I tried.
(define-syntax list-up-vars
  (syntax-rules (unquote )
    [(_ Acc ) (list . Acc)]
    [(_ Acc  (unquote V) . Rest)
     (list-up-vars (V . Acc)  . Rest)]
    [(_ Acc  () . Rest)
     (list-up-vars Acc  . Rest)]
    [(_ Acc  (P0 . P*) . Rest)
     (list-up-vars Acc  P0 P* . Rest)]
    [(_ Acc  LIT . Rest) 
     (list-up-vars Acc  . Rest)]
    ))
(define-syntax list-up-cata-vars
  (syntax-rules (unquote -> )
    [(_ Acc ) (list . Acc)]
    [(_ Acc  (unquote (f -> V* ...)) . Rest)
     (list-up-cata-vars (V* ... . Acc)  . Rest)]
    [(_ Acc  (unquote (V* ...)) . Rest)
     (list-up-cata-vars (V* ... . Acc)  . Rest)]
    [(_ Acc  () . Rest)
     (list-up-cata-vars Acc  . Rest)]
    [(_ Acc  (P0 . P*) . Rest)
     (list-up-cata-vars Acc  P0 P* . Rest)]
    [(_ Acc  LIT . Rest) 
     (list-up-cata-vars Acc  . Rest)]
    ))
(define-syntax collect-vars
  (syntax-rules (unquote )
    [(_ Acc ) Acc]
    [(_ Acc  (unquote V) . Rest)
     (collect-vars (V . Acc)  . Rest)]
    [(_ Acc  () . Rest)
     (collect-vars Acc  . Rest)]
    [(_ Acc  (P0 . P*) . Rest)
     (collect-vars Acc  P0 P* . Rest)]
    [(_ Acc  LIT . Rest) 
     (collect-vars Acc  . Rest)]
    ))
(define-syntax collect-cata-vars
  (syntax-rules (unquote -> )
    [(_ Acc ) Acc]
    [(_ Acc  (unquote (f -> V* ...)) . Rest)
     (collect-cata-vars (V* ... . Acc)  . Rest)]
    [(_ Acc  (unquote (V* ...)) . Rest)
     (collect-cata-vars (V* ... . Acc)  . Rest)]
    [(_ Acc  () . Rest)
     (collect-cata-vars Acc  . Rest)]
    [(_ Acc  (P0 . P*) . Rest)
     (collect-cata-vars Acc  P0 P* . Rest)]
    [(_ Acc  LIT . Rest) 
     (collect-cata-vars Acc  . Rest)]))
#;
(define-syntax collect-cata-sets
  (syntax-rules (unquote -> )
    [(_ Cata Acc ) Acc]
    [(_ Cata Acc  (unquote (f -> V0 V* ...)) . Rest)
     (collect-cata-sets Cata ((f V0 V* ...) . Acc)  . Rest)]
    [(_ Cata Acc  (unquote (V0 V* ...)) . Rest)
     (collect-cata-sets Cata ((Cata V0 V* ...) . Acc)  . Rest)]
    [(_ Cata Acc  () . Rest)
     (collect-cata-sets Cata Acc  . Rest)]
    [(_ Cata Acc  (P0 . P*) . Rest)
     (collect-cata-sets Cata Acc  P0 P* . Rest)]
    [(_ Cata Acc  LIT . Rest) 
     (collect-cata-sets Cata Acc  . Rest)]))


(define-syntax bind-cata 
  (syntax-rules ()
    [(_ Bod Promise Args ())  Bod]
    [(_ Bod Promise Args (V0 . V*))
     (let ([V0 (lambda ()
		 (call-with-values promise
		   (lambda Args V0)))])
       (bind-cata Bod Promise Args V*))]))

;; Convert a pattern into a function that will test for a match.
;;
;; This takes several arguments:
;;   Stack -- Objs&Patterns left to match.  Objs should be just vars. 2
;;   Bod -- the expression to execute if the pattern matches
;;   Cata -- the name of the function that will reinvoke this match
;;   Nextclause -- abort this clause and go to the next.
;;   CataVars -- Sets of vars that will result from catas (if pattern) matches
;;
;; If match, the body is evaluated, otherwise "nextclause" is called.
;; All pattern variables are lazy "thunked" so as to defer any Cata's.
(define-syntax convert-pat
    (syntax-rules (unquote .... )

	;; Termination condition:
      [(_ () Exec Bod Cata NextClause Vars)
       (Exec Bod Vars)
       ;Bod
       ]
      
	;; Cata redirect: 
	;; todo

	;; Unquote Pattern, Cata: recursively match
	[(_ ([Obj (unquote (V0 V* ...))] . Stack) Exec Bod Cata NextClause (Vars ...))
	 (let ([promise (delay (Cata Obj))])
	   (bind-cata 
	    (convert-pat Stack Exec Bod Cata
			 NextClause (V0 V* ... Vars ...))
	    promise
	    (V0 V* ...) (V0 V* ...)))]
	
	;; Unquote Pattern: bind a pattern variable:
	[(_ ([Obj (unquote V)] . Stack) Exec Bod Cata NextClause Vars)
	 (let ([V (lambda () Obj)])
	   (convert-pat Stack Exec Bod Cata NextClause (V . Vars)))]

	;; Ellipses:
#;	[(_ ([Obj (P0 ....)] . Stack) Exec Bod Cata NextClause CataVars Vars)
	 (call/1cc 
	  (lambda (escape)	    
	    (let* ([failed (lambda () (escape (NextClause)))]
		   ;; Bind a pattern-matcher for one element of the list.	
		   [project (lambda (VAL)
			      (convert-pat ([VAL P0]) build-list IGNORED Cata failed CataVars Vars))]
#;
		   [project (lambda (VAL)
			      ;; When the pattern matches, build a list of the vars:				
			      (convert-pat ([VAL P0]) Exec
					   (list-up-all-vars () P0)
					   Cata failed CataVars Vars)
			      )]
		   )


	      #;
	      [(null? ls) 
		  (bind-vars-null (collect-vars P0)
				  (bind-cata-null (collect-cata-vars P0)
						  Bod))]

	      (let loop ([ls Obj] [acc '()] [cataacc '()])
		(cond
		 
		 [(null? ls)
		  (let* ([rotated_vars (apply map list (reverse! acc))]
			 [rotated_catavars (apply map list (reverse! cataacc))])
		    
		    (apply (lambda (collect-vars P0)
			     (apply (lambda (collect-cata-vars P0)
				      (convert-pat Stack exec-body Bod Cata NextClause 				 
						   CataVars
						   ((collect-vars P0) Vars ... )))
				    rotated_catavars))
			   rotated_vars)

		    #;
		    ((Bod (collect-cata-sets P0))
		     (collect-vars P0))

					;(convert-pat ([Obj P0]) build-list-binder (Bod rotated) Cata NextClause () ())
		    )]

#;
		 [(null? (cdr ls))
		  (let* ([final (cons (project (car ls)) acc)]
			 [rotated (apply map list (reverse! final))])
		    (convert-pat ([Obj P0]) build-list-binder (Bod rotated) Cata NextClause () ())
		    )]

		 [else (loop (cdr ls) 
			     (cons (project (car ls)) acc))]
		 )))))]
	
	;; Pair pattern:  Do car, push cdr onto stack.
	[(_ ([Obj (P0 . P1)] . Stack) Exec Bod Cata NextClause Vars)
	 (if (pair? Obj)
	     (let ([head (car Obj)]
		   [tail (cdr Obj)])
	       (convert-pat ([head P0] [tail P1] . Stack)
			    Exec Bod Cata NextClause Vars))
	     (NextClause)
	     )]
		
	;; Literal pattern.
	;; Since we're using syntax-rules here we can't tell much.
	[(_ ([Obj LIT] . Stack) Exec Bod Cata NextClause Vars)
	 (begin 
	   ;; Hopefully this happens at compile-time:
;	   (ASSERT (or (symbol? (quote LIT))
;		       (null? (quote LIT))
;		       (string? (quote LIT))
;		       (number? (quote LIT))))
	   (if (equal? Obj (quote LIT))
	       (convert-pat Stack Exec Bod Cata NextClause Vars)
	       (NextClause)))]

	;; Otherwise, syntax error.
	))

  (printf "TESTING: ~a\n" (test))
;; End module:
)



#;
(lambda (x)
    (syntax-case x ()
		 


      ((_ Template Cata Obj ThreadedIds)
       ;(inspect `(HRM ,(datum Template) ,(datum Cata) ,(datum Obj) ,(datum ThreadedIds)))
       ;(inspect #'Template)
       #'(error 'match "Unmatched datum.\n  Datum: ~s\n  Source-Location: ~s\n" Obj #'Template))

      ((_ Template Cata Obj ThreadedIds (Pat B0 B ...) Rest ...)
       #'(convert-pat Pat
           (match-help1 Template Cata Obj ThreadedIds 
             (B0 B ...)
             Rest ...)))

      ))



#!eof

;;; examples of passing along threaded information.

;;; Try (collect-symbols '(if (x y 'a 'c zz) 'b 'c))
;;; Note that it commonizes the reference to c. 

(define-syntax with-values
  (syntax-rules ()
    ((_ P C) (call-with-values (lambda () P) C))))
(define collect-symbols
  (lambda (exp)
    (with-values (collect-symbols-help exp)
      (lambda (symbol-decls exp)
        (match symbol-decls
          (((,symbol-name . ,symbol-var) ...)
           ;`(let ((,symbol-var (quote ,symbol-name)) ...) ,exp)
	   `(let ,(map list symbol-var (map (lambda (x) `(quote ,symbol-name)))) ,exp)
	   ))))))
(define collect-symbols-help
  (lambda (exp)
    (let ((symbol-env '()))
      (match+ (symbol-env) exp
        (,x
          (guard (symbol? x))
          (values symbol-env x))
        ((quote ,x)
         (guard (symbol? x))
         (let ((pair/false (assq x symbol-env)))
           (if pair/false
               (values symbol-env (cdr pair/false))
               (let ((v (gensym)))
                 (values (cons (cons x v) symbol-env)
                         v)))))
        ((quote ,x)
         (values symbol-env `(quote ,x)))
        ((if ,[t] ,[c] ,[a])
         (values symbol-env `(if ,t ,c ,a)))
        ((,[op] ,[arg] ...)
         (values symbol-env `(,op ,arg ...)))))))

;;; the grammar for this one is just if-exprs and everything else

(define collect-leaves
  (lambda (exp acc)
    (match+ (acc) exp
      ((if ,[] ,[] ,[])
       acc)
      ((,[] ,[] ...)
       acc)
      (,x
        (cons x acc)))))

;; here's something that takes apart quoted stuff. 

(define destruct
  (lambda (datum)
    (match datum
      (() `'())
      ((,[X] . ,[Y])`(cons ,X ,Y))
      (#(,[X] ...) `(vector ,X ...))
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

