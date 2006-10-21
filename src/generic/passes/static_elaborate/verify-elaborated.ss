

;;;; This pass just explicitely annotates the types of application expressions.

#;
(define annotate-app-types-grammar
  (let ([newg (remq (car (member '(Expr ('app Expr ...)) remove-unquoted-constant-grammar))
		    remove-unquoted-constant-grammar)])
    (ASSERT (< (length newg) (length remove-unquoted-constant-grammar)))
    (cons '[Expr ('typed-app Type Expr ...)] newg)))

(define-pass verify-elaborated
;    [OutputGrammar annotate-app-types-grammar]
    [Expr/Types 
     (lambda (expr tenv fallthrough)
       (match expr
	 [(app ,[rator] ,[rand*] ...)		     
	  (let ([type (recover-type rator tenv)])
	    (if ;(or (deep-assq 'Signal type) (deep-assq 'Region type))
	        #t ; (distributed-type? type)
		(error 'verify-elaborated
		       "post-elaboration expression should not contain arrow types containing monads.\n  Type: ~s\n  Rator: ~s\n"
		       type rator))
	    `(app ,rator ,rand* ...))]
	 [,other (fallthrough other tenv)]))]
  )
