;; OUT OF USE RIGHT NOW

;;; Pass 13: rename-k
;;; This pass should not really be necessary.  Dan first thought that having
;;; one name for k wouldn't cause any problems, but I don't know what to do
;;; about converting:
;;;   (let ([k (... k ...)])
;;; into:
;;;   (letrec ([k (... k ...)])
;;; This transformation of let's to letrecs requires distinct variable names
;;; to avoid capture.

;;; This pass (if it continues to exist), will take all the 'k's produced by
;;; introduce-cps and give them distinct names.
;===============================================================================

(define rename-k
  (let ()
    ;; This takes an expression and a thunk that, when evaluated, returns
    ;; the name of the current 'k'
    (define Expr
      (lambda (expr kthunk)
        (match expr
          ;; Here we catch a variable reference to 'k:
          [k (kthunk)]
          ;; If there is a k argument, it will be on the end:
          ;; (this pass must *directly* follow introduce-cps)
          [(lambda (,formal* ...) ,body)
           (guard (eq? (rac formal*) 'k))
           (let ([kname (unique-name 'k)])
             `(lambda (,@(rdc formal*) ,kname)
                ,(Expr body (lambda () kname))))]
          ;	  [(lambda (,formal* ... k) ,body)
          ;	   (let ([kname (unique-name 'k)])
          ;	     `(lambda (,formal* ... ,kname)
          ;		,(Expr body (lambda () kname))))]
          ;; If k does occur on the lhs of a let, it will be all alone:
          [(let ([k ,[rhs]]) ,body)
           (let* ([kname (unique-name 'k)]
                  [kth (lambda () kname)])
             `(let ([,kname ,rhs])
                ,(Expr body kth)))]
          ;---------------------------------------
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(toplvl-varassign! ,var ,[rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(quote ,imm) `(quote ,imm)]
          [,var (guard (symbol? var)) var]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[expr*] ...) `(begin ,expr* ...)]
          [(lambda (,formal* ...) ,[body])
           `(lambda (,formal* ...) ,body)]
          [(let ([,lhs* ,[rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
          [(letrec ([,lhs* ,[rhs*]] ...) ,[body])
           `(letrec ([,lhs* ,rhs*] ...) ,body)]
          [(,prim ,[rand*] ...)
           (guard (extended-scheme-primitive? prim))
           `(,prim ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
          [,unmatched
            (error 'rename-k! "invalid expression: ~s"
                   unmatched)])))
    (define output-language-definition
      '(begin))
    (lambda (prog)
      (match prog
        [(let () ,input-language-definition ,expr)
         `(let ()
            ,output-language-definition
            ,(Expr expr
                   (lambda ()
                     (error 'rename-k
                            "'k' occurs as a free variable in this input program!:~a"
                            prog))))]))))
