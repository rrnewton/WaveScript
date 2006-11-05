

;;;; This pass just explicitely annotates the types of application expressions.
#;
(define annotate-app-types-grammar
  (let ([newg (remq (car (member '(Expr ('app Expr ...)) remove-unquoted-constant-grammar))
		    remove-unquoted-constant-grammar)])
    (ASSERT (< (length newg) (length remove-unquoted-constant-grammar)))
    (cons '[Expr ('typed-app Type Expr ...)] newg)))




;; Verifies that there are no polymorphic types left on the programs variable bindings.
;; Also verifies that there are no disallowed applications.
(define-pass verify-elaborated
;    [OutputGrammar annotate-app-types-grammar]
    [Expr/Types 
     (lambda (expr tenv fallthrough)
       (match expr
	 [(app ,[rator] ,[rand*] ...)		     
	  (let ([type (recover-type rator tenv)])
	    (if ;(or (deep-assq 'Signal type) (deep-assq 'Region type))
	     ;(distributed-type? type)	
	     #t ;; <-- Harshest version: no functions at all.
	     (error 'verify-elaborated
		    "post-elaboration expression should not contain arrow types containing monads.\n  Type: ~s\n  Rator: ~s\n"
		    type rator))
	    `(app ,rator ,rand* ...))]
	 [,other (fallthrough other tenv)]))]
  
  ;; TODO: FIXME VERIFY THAT THERE ARE *NO* POLYMORPHIC TYPES LEFT:
  ;[Bindings ]

  )
