;;; Pass X??: verify-core

;;; This pass verifies that the input is in the simplified core
;;; language.  (Uber simplified!)


;;; <Pgm>  ::= <Let>
;;; <Let>  ::= (let (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= (quote <datum>)
;;;          | <var>
;;;          | (if <var> <var> <var>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <var>*)
;;; <Formalexp> ::= (<var>*)

;; No variable capture is allowed at this point.

;;; The implementation requires constant?, datum?, keyword?,
;;; blanko-primitive?, set?, formalexp?, get-formals, and the list
;;; blanko-primitives from helpers.ss.

(define verify-core 
  (let ()

    (define (process-let expr env)
      (match expr
          [(let ([,lhs* ,[process-expr -> rhs*]] ...) ,expr)
	   
	   (guard (not (memq 'let env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
	   (if (ormap (lambda (s) (memq s env)) lhs*)
	       (error 'verify-core "no variable	capture at this point."))
	   #t]))

    (define process-expr
      (lambda (expr env)
        (match expr
          [,const (guard (constant? const)) const]
          [(quote ,datum)
           (guard (not (memq 'quote env)) (datum? datum))
           `(quote ,datum)]
          [,var (guard (symbol? var))
            var]

          [(if ,test ,conseq ,altern)
           (guard (not (memq 'if env)))
	   
	   
	   
		  (and (symbol? test) (symbol? conseq) (symbol? altern)))
	   
           `(if ,test ,conseq ,altern)]


          [(begin ,[first-expr] ,[rest-expr*] ...)
           (guard (not (memq 'begin env)))
           `(begin ,first-expr ,rest-expr* ...)]
          [(lambda ,formalexp ,expr ,expr* ...)
           (guard (formalexp? formalexp)
                  (not (memq 'lambda env)))					
;	   (check-lambda-clause formalexp expr expr* env)
           `(lambda ,formalexp ,expr ,expr* ...)]

          [(case-lambda [,formalexp* ,expr* ,expr** ...] ...)
           (guard (andmap formalexp? formalexp*)
                  (not (memq 'case-lambda env)))
           (for-each (lambda (f e e*)
                       (check-lambda-clause f e e* env))
                     formalexp* expr* expr**)
           `(case-lambda [,formalexp* ,expr* ,expr** ...] ...)]

          [(letrec ([,lhs* ,rhs*] ...) ,expr ,expr* ...)
           (guard (not (memq 'letrec env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
           (let ([env (append lhs* env)])
             (let ([rhs* (process-expr* rhs* env)]
                   [expr (process-expr expr env)]
                   [expr* (process-expr* expr* env)])
               `(letrec ([,lhs* ,rhs*] ...) ,expr ,expr* ...)))]

          [(,keyword ,form* ...)
           (guard (not (memq keyword env))
                  (keyword? keyword))
           (error 'verify-scheme "invalid syntax ~s" `(,keyword ,form* ...))]
          
          ;; Optimize level specifier, n, is optional
          [(\#primitive ,prim)
           (guard (scheme-primitive? prim))
           `(\#primitive ,(snet-optimize-level) ,prim)]
          [(\#primitive ,n ,prim)
           (guard (scheme-primitive? prim))
           `(\#primitive ,n ,prim)]
          
          [(\#primitive ,n ... ,prim)
           (error 'verify-scheme
                  "(#primitive ...) used, but ~s~a~n"
                  prim " isn't one of our primitives: " )]
          
          [((\#primitive ,n ,prim) ,[rand*] ...)
           (guard (not (memq prim env))
                  (scheme-primitive? prim))
           (check-primitive-numargs prim rand*)
           `((\#primitive ,n ,prim) ,rand* ...)]
          [(,prim ,[rand*] ...)
           (guard (>= (snet-optimize-level) 2)
                  (not (memq prim env))
                  (scheme-primitive? prim))
           (check-primitive-numargs prim rand*)
           `(,prim ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
|#

          [,unmatched
            (error 'verify-scheme "invalid syntax ~s" unmatched)])))

    (lambda (expr)
      (display "Running on ") (display expr) (newline)
      
      (match expr
	     ;; Doesn't change the input language... 
        [(,input-language (quote (program ,body)))

           (let ([body (process-expr body '())])
             `(,input-language '(program ,body)))]))))  
  