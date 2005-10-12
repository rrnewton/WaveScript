;;; Pass 18: introduce-closure-primitives
;;; September 2001
;===============================================================================

;;; This pass removes the syntactic forms bind-free and closures,
;;; replacing them with closure primitives that do explicitly what the
;;; closures and bind-free forms do implicitly.
;;;
;;; Each closures form is replaced by code that explicitly creates the
;;; specified set of closures, using make-closure, and inserts into
;;; these closures the values of their free variables, using closure-set!.
;;;
;;; Within the body of each bind-free form, references to the named
;;; free variables are replaced by calls to closure-ref.  Since it is
;;; no longer needed after this, the bind-free form is dropped.
;;;
;;; Anonymous calls are expanded to extract the closure code explicitly
;;; from the closure and invoke it on the closure plus the original
;;; arguments.  A let binding for the closure is wrapped around the call
;;; to avoid evaluating the closure expression twice.

;;; [01.09.20] :
;;; NOTE: NOW THAT THINGS ARE CLASS-STYLE, THERE IS NO LONGER ANY NEED
;;; TO PASS A CLOSURE ARGUMENT TO PROCEDURES, AND AS SUCH, THAT EXTRA
;;; ARG IS REMOVED IN THIS PASS.

;;; (letrec ([anon$7
;;;           (lambda (cp.8 y.3) (bind-free (cp.8 x.2) (* x.2 y.3)))]
;;;          [f$4
;;;           (lambda (cp.9 x.2)
;;;             (bind-free (cp.9)
;;;               (closures ([anon.7 (label anon$7) x.2]) anon.7)))]
;;;          [map$1
;;;           (lambda (cp.10 f.6 ls.5)
;;;             (bind-free (cp.10 map.1)
;;;               (if (null? ls.5)
;;;                   '()
;;;                   (cons (f.6 (car ls.5))
;;;                         ((label map$1) map.1 f.6 (cdr ls.5))))))])
;;;   (closures ([map.1 (label map$1) map.1])
;;;     (closures ([f.4 (label f$4)])
;;;       ((label map$1)
;;;        map.1
;;;        ((label f$4) f.4 '7)
;;;        (cons '1 (cons '2 (cons '3 '())))))))
;;;
;;; becomes
;;;
;;; (letrec ([anon$7
;;;           (lambda (cp.8 y.3) (* (closure-ref cp.8 '0) y.3))]
;;;          [f$4
;;;           (lambda (cp.9 x.2)
;;;             (let ([anon.7 (make-closure (label anon$7) '1)])
;;;               (begin (closure-set! anon.7 '0 x.2) anon.7)))]
;;;          [map$1
;;;           (lambda (cp.10 f.6 ls.5)
;;;             (if (null? ls.5)
;;;                 '()
;;;                 (cons (let ([tmp.11 f.6])
;;;                         ((closure-code tmp.11) tmp.11 (car ls.5)))
;;;                       ((label map$1)
;;;                        (closure-ref cp.10 '0)
;;;                        f.6
;;;                        (cdr ls.5)))))])
;;;   (let ([map.1 (make-closure (label map$1))])
;;;     (begin
;;;       (closure-set! map.1 '0 map.1)
;;;       (let ([f.4 (make-closure (label f$4) '0)])
;;;         ((label map$1)
;;;          map.1
;;;          ((label f$4) f.4 '7)
;;;          (cons '1 (cons '2 (cons '3 '()))))))))

;;; The input language is the same as the output language of Pass 14.

;;; The output language differs in that the bind-free and closures
;;; forms are gone.

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>   ::= (letrec ((<var> <ClassDef>)*) <Exp>)
;;; <ClassDef> ::= (class-def (<freevar> ...) <Lambda>)
;;; <Lambda>   ::= (lambda <Formalexp> <Exp>)
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)
;;; <Exp>       ::= (quote <imm>)
;;;               | (class <var>)
;;;               | <var>
;;;               | (if <Exp> <Exp> <Exp>)
;;;               | (begin <Exp> <Exp>*)
;;;               | (let ((<var> <Exp>)*) <Exp>)
;;;               | (<primitive> <Exp>*)
;;;               | (<Exp> <Exp>*)
;;;               | (toplvl-varref <var>)
;;;               | (toplvl-varassign! <var> <Exp>)


;;; In addition, the set of primitives now includes make-closure,
;;; closure-code, closure-freevar, and closure-set!.

;;; The implementation uses iota, scheme-primitive?, and unique-name
;;; from helpers.ss.

(define introduce-closure-primitives
  (let ()
    (define list-index
      (lambda (x ls)
        (let loop ([i 0] [ls ls])
          (and (not (null? ls))
               (if (eq? (car ls) x)
                   i
                   (loop (+ i 1) (cdr ls)))))))
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,imm) `(quote ,imm)]
          [(label ,lab) `(class ,lab)]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(toplvl-varassign! ,var ,[rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[expr*] ...) `(begin ,expr* ...)]
          [(let ([,lhs* ,[rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
          [(closures ([,name* (label ,class-name*) ,free** ...] ...) ,[body])
           (let ([rhs*
                   (map (lambda (class-name)
                          `(make-closure (class ,class-name)))
                        class-name*)])
             `(let ([,name* ,rhs*] ...)
                ,(make-begin
                   (append
                     (apply
                       append
                       (map (lambda (class-name name free*)
                              (map (lambda (free)
                                     `(closure-set! ',class-name ',free
                                                    ,name ,free))
                                   free*))
                            class-name*
                            name*
                            free**))
                     (list body)
                     ))))]
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map process-class-defn class-defn*)])
             `(let-class ,class-defn* ,body))]
          [(new ,class-name ,[args*] ...)
           `(new ,class-name ,args* ...)]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,obj ,class-name ,body)]
          [(object-reference ,obj ,class ,field)
           `(object-reference ,obj ,class ,field)]
          [(invoke-method ,obj ,class ,method (,[arg*] ...))
           `(invoke-method ,obj ,class ,method (,arg* ...))]
          [(foreign-call ,name ,type-sig (,[arg*] ...))
           `(foreign-call ,name ,type-sig (,arg* ...))]
          [(static-ref ,class ,field)
           `(static-ref ,class ,field)]
          [(this-ref ,class ,field)
           `(this-ref ,class ,field)]
          [(set-field! ,field ,[rhs])
           `(set-field! ,field ,rhs)]
          [(invoke-static-method ,class ,method-name ,type-sig (,[arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [(,prim ,[rand*] ...)
           (guard (extended-scheme-primitive? prim))
           `(,prim ,rand* ...)]
          [((label ,lab) ,[rand*] ...)
           `(direct-call (class ,lab) ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           ;(disp "Processing app" `(,rator ,rand* ...))
           `(anon-call ,rator ,rand* ...)]
          [,expr (error 'explicit-closure "invalid expression: ~s" expr)])))
    (define process-class-defn
      (lambda (cdef)
        (match cdef
          [(define-class ,name ,base
             ,ctor
             (fields (,fmods** ... ,fname*) ...)
             (methods (,mmods** ... ,mtype*
                        (,mname* ,mbody*)) ...))
           (let ([ctor (process-method ctor)]
                 [mbody* (map process-method mbody*)])
             `(define-class
                ,name ,base ,ctor
                (fields (,fmods** ... ,fname*) ...)
                (methods (,mmods** ... ,mtype*
                           (,mname* ,mbody*)) ...)))]
          [,unmatched (error 'explicit-closure
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (let ([body (Letrec body)])
             `(lambda ,args ,body))])))
    (define Lambda
      (lambda (expr)
        (match expr
          [(lambda (,cparg . ,formalexp)
             (bind-free (,cp ,free* ...)
                        ,body))
           `(class-def (,free* ...)
                       (lambda ,formalexp
                         ,(Expr body)))])))
    (define Letrec
      (lambda (expr)
        (match expr
          [(letrec ([,lhs* ,[Lambda -> rhs*]] ...) ,body)
           `(letrec ([,lhs* ,rhs*] ...)
              ,(Expr body))])))
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (let ([body (Letrec body)])
             `(introduce-closure-primitives-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))