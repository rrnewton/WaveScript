;;; Pass 23: lift-letrec-body
;;; September 2001
;===============================================================================

;;; Output grammar:

;;; This pass, which is really not a pass at all, makes a (first-order)
;;; procedure out of the letrec body:

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>  ::= (letrec ((<var> <>)*) (entry-point <var>))

(define lift-letrec-body
  (let ()
    (define Letrec
      (lambda (expr)
        (match expr
          [(letrec ([,lhs* ,rhs*] ...) ,body)
           (let ([main (code-name 'main)])
             `(letrec ([,lhs* ,rhs*] ...
                       [,main
                         (class-def () (lambda () ,body))])
                (entry-point ,main)))])))
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([body (Letrec body)])
           `(lift-letrec-body-language
              '(program ,sym* ,pkg* ,class-defns* ... ,body)))]))))
