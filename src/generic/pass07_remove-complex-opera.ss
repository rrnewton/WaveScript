
;===============================================================================
;;---- Pass 07 ---- Remove Complex Opera*
;This pass guarantees that each subexpression (operator or operand) of a call
;or primitive call is either a lexical variable or a constant. When an
;expression is complex (neither a lexical variable nor a constant), this pass
;replaces it with a new lexical variable and binds the variable to the
;expression's value in an enclosing let expression. When more than one
;subexpression is complex, the new variables are bound in parallel by a single
;let expression.
;   Ryan Newton
;===============================================================================

;;; Input Language

;;; <Pgm>  ::= (<language-name> (quote (program <Let>)))
;;; <Let>  ::= (lazy-letrec (<Decl>*) <Exp>))
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= 
;;;            (quote <imm>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Exp>*)
;;; <Formalexp> ::= (<var>*)

;;; Output Language

;;; <Pgm>  ::= (<language-name (quote (program <Let>)))
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Simple> ::= (quote <imm>) | <var> 
;;; <Formalexp> ::= (<var>*)
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)


;===============================================================================

;;; <Prog>    ::= (let () <LanguageDefn>
;;;                 (letrec ((<Var> <Proc>)*)
;;;                   <Exp>))
;;; <Proc>    ::= (lambda (<Var> <Var>*) <Exp>)
;;; <Closure> ::= (make-closure <Var> <Var>*)
;;; <Exp>     ::= (quote <Lit>)
;;;             | <Var>
;;;             | (if <Exp> <Exp> <Exp>)
;;;             | (begin <Exp> <Exp>)
;;;             | (let ((<Var> <Exp>)+) <Exp>)
;;;             | (let ((<Var> <Closure>)+)
;;;                 (begin (closure-set! <Var> <Number> <Var>)+
;;;                        <Exp>))
;;;             | (<Prim> <Exp>*)
;;;             | (anonymous-call <Exp> <Exp>)
;;;             | (<Var> <Exp>*)

;;; Output Language

;;; <Prog>    ::= (let () <LanguageDefn>
;;;                 (letrec ((<Var> <Proc>)*)
;;;                   <Exp>))
;;; <Proc>    ::= (lambda (<Var> <Var>*) <Exp>)
;;; <Closure> ::= (make-closure <Var> <Var>*)
;;; <Exp>     ::= (quote <Lit>)
;;;             | <Var>
;;;             | (if <Exp> <Exp> <Exp>)
;;;             | (begin <Exp> <Exp>)
;;;             | (let ((<Var> <Exp>)+) <Exp>)
;;;             | (let ((<Var> <Closure>)+)
;;;                 (begin (closure-set! <Var> <Number> <Var>)+
;;;                        <Exp>))
;;;             | (<Prim> <SimpleExp>*)
;;;             | (anonymous-call <SimpleExp> <SimpleExp>)
;;;             | (<Var> <SimpleExp>*)
;;; <SimpleExp> ::= <Lit> | <Var>

;===============================================================================

(define remove-complex-opera*
  (let ()
    ;===========================================================================
    ;process-opera* takes a list of opera*, and returns 2 values as follows:
    ;1) a new list of opera* (with complex opera* replaced by var refs)
    ;2) a list of bindings, containing bindings which bind new names to
    ;complex expressions.  There is a binding of this sort for each complex
    ;opera* that occured in the input list.
    [define process-opera*
      (lambda (operalst)
        (if (null? operalst)
            (values '() '())
            (match (car operalst)
                   [(quote ,lit)
                    (let-values
                      ([(ops binds) (process-opera* (cdr operalst))])
                      (values (cons (car operalst) ops) binds))]
                   [,var
                     (guard (symbol? var))
                     (let-values
                       ([(ops binds) (process-opera* (cdr operalst))])
                       (values (cons (car operalst) ops) binds))]
                   [,else
                     (let ((new-var (gen-symbol 'tmp)))
                       (let-values
                         ([(ops binds) (process-opera* (cdr operalst))])
                         (values
                           (cons new-var ops)
                           (cons `[,new-var ,(car operalst)] binds))))])))]
    ;===========================================================================
    [define process-exp
      (lambda (expr)
        (match expr
               [(quote ,lit)
                `(quote ,lit)]
               [,var
                 (guard (symbol? var))
                 var]
               [(if ,[test] ,[conseq] ,[altern])
                `(if ,test ,conseq ,altern)]
               [(begin ,[exprs] ...)
                `(begin ,@exprs)]
               [(let ((,local* ,[rhs*]) ...) ,[body])
                `(let ,(map list local* rhs*) ,body)]
               [(,prim ,[rand*] ...)
                (guard (primitive? prim))
                (let-values ([(new-opera* binds) (process-opera* rand*)])
                            (if (null? binds)
                                `(,prim ,@new-opera*)
                                `(let ,binds
                                   (,prim ,@new-opera*))))]
               [(anonymous-call ,[opera**] ...)
                (let-values ([(new-opera* binds) (process-opera* opera**)])
                            (if (null? binds)
                                `(anonymous-call ,@new-opera*)
                                `(let ,binds
                                   (anonymous-call ,@new-opera*))))]
               [(,rator ,[rand*] ...)
                (let-values ([(new-opera* binds) (process-opera* rand*)])
                            (if (null? binds)
                                `(,rator ,@new-opera*)
                                `(let ,binds
                                   (,rator ,@new-opera*))))]
               [else (error 'remove-complex-opera* "~s: " expr)]
               ))]
    ;===========================================================================
    [define process-letrec
      (lambda (letrec-exp)
        (match letrec-exp
               [(letrec ((,code-name* (lambda (,formals* ...) ,pbody*)) ...)
                  ,body)
                `(letrec ,(map (lambda (name formals pbody)
                                 `[,name (lambda ,formals
                                           ,(process-exp pbody))])
                               code-name* formals* pbody*)
                   ,(process-exp body))]
               [,else (error
                        'remove-complex-opera*
                        "top level letrec expression is incorrectly formatted:~s"
                        letrec-exp)]))]
    ;===========================================================================
    [define LANGUAGE-DEFN
      '(begin
         (define make-closure
           (lambda (code i) (make-procedure code (make-vector i))))
         (define closure-ref
           (lambda (cp i) (vector-ref (procedure-env cp) i)))
         (define closure-set!
           (lambda (cp i v) (vector-set! (procedure-env cp) i v)))
         (define-syntax anonymous-call
                        (syntax-rules ()
                                      [(_ e0 e1 ...)
                                       (let ([t e0])
                                         ((procedure-code t) t e1 ...))]))
         (define-record procedure ((immutable code) (immutable env))))]
    ;===========================================================================
    (lambda (program)
      (match program
             [(let () ,old-language-defn ,letrec-exp)
              `(let () ,LANGUAGE-DEFN
                 ,(process-letrec (cadddr program)))]
             [,else (error 'remove-complex-opera*
                           "Language definitions not set up correctly: ~s"
                           program)]))))
;===============================================================================