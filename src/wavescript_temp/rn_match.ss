
;; How far can we get doing match with syntax-rules.


;; match-lambda
;; let-match
;; trace-match... etc.

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

(module (match match-help convert-pat exec-body test) 

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
       (convert-pat ((Obj Pat)) Bod Cata next ())
       )]))

#;
(define-syntax match-help1
  (syntax-rules (guard)
    ((_ PatLit Vars Cdecls Template Cata Obj ThreadedIds
       ((guard G ...) B0 B ...) Rest ...)
     (let ((ls/false (sexp-dispatch Obj PatLit)))
       (if (and ls/false (apply (lambda Vars
                                  (guard-body Cdecls
                                    (extend-backquote Template (and G ...))))
                           ls/false))
           (apply (lambda Vars
                    (clause-body Cata Cdecls ThreadedIds
                      (extend-backquote Template B0 B ...)))
             ls/false)
           (match-help Template Cata Obj ThreadedIds Rest ...))))
    ((_ PatLit Vars Cdecls Template Cata Obj ThreadedIds
       (B0 B ...) Rest ...)
     (let ((ls/false (sexp-dispatch Obj PatLit)))
       (if ls/false
           (apply (lambda Vars
                    (clause-body Cata Cdecls ThreadedIds
                      (extend-backquote Template B0 B ...)))
             ls/false)
           (match-help Template Cata Obj ThreadedIds Rest ...))))))

  (define-syntax exec-body
    (syntax-rules ()
      [(_ Bod ()) Bod]
      [(_ Bod ((fun Var ...) CataSets ...))
       (call-with-values
	   (lambda () (fun))
	 (lambda (Var ...)
	   (exec-body Bod (CataSets ...))
	   ))]))

  (define (test)
    (list 

     (match '(1 2) [(,x ,y) (+ x y)])
     
     (match '(1 2) [(,x ,y ,z) (+ x x y)] [(,x ,y) (* 100 y)])
          
     (match '(1 2) [(,x ,y ,z) (+ x x y)] [,v v])

     (match '(1 2) [(3 ,y) (* 1000 y)] [(1 ,y) (* 100 y)])

     (match '(1 2) [(,[x] ,[y]) (list x y)] [1 3] [2 4])

     (match '(1 2) [(,[x y] ,[z w]) (list x y z w)] [1 (values 3 4)] [2 (values 5 6)])

     (match '(1 . 2) [(,x . ,y) y])

     ))


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
      (syntax-rules (unquote)

	;; Termination condition:
	[(_ () Bod Cata NextClause CataVars)
	 (exec-body Bod CataVars)]

	;; Cata redirect: 
	;; todo

	;; Unquote Pattern, Cata: recursively match
	[(_ ([Obj (unquote (V0 . V*))] . Stack) Bod Cata NextClause CataVars)
	 (let ([cataset (lambda () (Cata Obj))])
	   (convert-pat Stack Bod Cata 
			NextClause ((cataset V0 . V*) . CataVars)))]
	
	;; Unquote Pattern: bind a pattern variable:
	[(_ ([Obj (unquote V)] . Stack) Bod Cata NextClause CataVars)
	 (let ([V Obj])
	   (convert-pat Stack Bod Cata NextClause CataVars))]
	
	;; Pair pattern:
	[(_ ([Obj (P0 . P1)] . Stack) Bod Cata NextClause CataVars)
	 ;; Do car, push cdr onto stack.
	 (if (pair? Obj)
	     (let ([head (car Obj)]
		   [tail (cdr Obj)])
	       (convert-pat ([head P0] [tail P1] . Stack)
			    Bod Cata NextClause CataVars))
	     (NextClause)
	     )]
	
	;; Ellipses:
	[(_ ([Obj (P0 ellipses)] . Stack) Bod Cata NextClause CataVars)
	 ;; Do car, push cdr onto stack.
	 (if (pair? Obj)
	     (let ([head (car Obj)]
		   [tail (cdr Obj)])
	       (convert-pat ([head P0] [tail P1] . Stack)
			    Bod Cata NextClause CataVars))
	     (NextClause)
	     )]
	
	;; Literal pattern.
	;; Since we're using syntax-rules here we can't tell much.
	[(_ ([Obj LIT] . Stack) Bod Cata NextClause CataVars)
	 (begin 
	   ;; Hopefully this happens at compile-time:
	   (ASSERT (or (symbol? LIT)
		       (null? LIT)
		       (string? LIT)
		       (number? LIT)))
	   (if (equal? Obj LIT)
	       (convert-pat Stack Bod Cata NextClause CataVars)	       
	       (NextClause)))]

	;; Otherwise, syntax error.
	))

  (printf "TESTING: ~a\n" (test))
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

      )))




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

