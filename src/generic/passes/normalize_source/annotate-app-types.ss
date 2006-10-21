

;;;; This pass just explicitely annotates the types of application expressions.

(define annotate-app-types-grammar
  (let ([newg (remq (member '(Expr ('app Expr ...))) remove-unquoted-constant-grammar)])
    (ASSERT (< (length newg) (length remove-unquoted-constant-grammar)))
    (cons '[Expr ('typed-app Type Expr ...)] newg)))

(define-pass annotate-app-types
    [OutputGrammar annotate-app-types-grammar]
    [Expr/Types do-expr]
  (define (do-expr expr tenv fallthrough)
    (match expr
      [(app ,rator ,[rand*] ...)
       `(typed-app ,(recover-type rator tenv) ,(do-expr rator) ,rand* ...)]
      [,other (fallthrough other)])))

