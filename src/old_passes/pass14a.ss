;;; Pass 12a: find-call/cc
;;; January 2001



;;; The output language differs in that lambda expressions can appear
;;; only on the right-hand sides of letrec bindings.

;;; <Program> ::= (let () <output-language-definition> <Exp>)
;;; <Exp>  ::= (quote <imm>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (let ((<var> <Exp>)*) <Exp>)
;;;          | (letrec ((<var> <Lambda>)*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;; <Lambda> ::= (lambda (<var>*) <Exp>)

;;; The implementation requires scheme-primitive? and unique-name
;;; from helpers.ss.

(define find-call/cc
  (let ()
    
(define Expr
  (lambda (expr)
    (match expr
      [(quote ,constant) #f]
      [,var (guard (symbol? var)) #f]
      [(if ,[test] ,[conseq] ,[altern])
       (or test conseq altern)]
      [(begin ,[expr*] ...) (ormap (lambda (x) x) expr*) ]
      [(lambda (,formal* ...) ,[body]) body]
      [(let ([,lhs* ,[rhs*]] ...) ,[body])
       (ormap (lambda (x) x) (cons body rhs*))]
      [(letrec ([,lhs* (lambda (,formal** ...) ,[lbody*])] ...) ,[body])
       (ormap (lambda (x) x) (cons body lbody*))]
      [(call/cc ,fun) #t]
      [(,prim ,[rand*] ...)
       (guard (scheme-primitive? prim)) #f]
      [(,[rator] ,[rand*] ...)
       (ormap (lambda (x) x) (cons rator rand))]
      [,unmatched
        (error 'find-call/cc "invalid expression: ~s"
               unmatched)])))
(define output-language-definition
  '(begin
     (define-syntax contains-call/cc
       (syntax-rules ()
         [(_ (bool) expr) expr]))))
(lambda (prog)
  (match prog
    [(let () ,input-language-definition ,expr)
     (let ([call/cc-flag (Expr expr)])
       `(let ()
          ,output-language-definition
          (contains-call/cc (,call/cc-flag) ,expr)))]))))

(define f find-call/cc)


