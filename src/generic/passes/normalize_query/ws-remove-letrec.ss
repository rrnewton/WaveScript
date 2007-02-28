

(module ws-remove-letrec mzscheme
  (require "../../../plt/common.ss"
	   "reduce-primitives.ss")
  (provide remove-letrec
	   remove-letrec-grammar
	   )
  (chezimports)

(define remove-letrec-grammar 
  (filter (lambda (prod)
	    (match prod
	      [(Expr ('letrec ,_ ...)) #f]
	      ;;[(Expr ('lazy-letrec ,_ ...)) #f]
	      [,_ #t]))
    reduce-primitives-grammar))

(define-pass remove-letrec 
    [OutputGrammar remove-letrec-grammar]
    [Expr (lambda (x fallthru)
	    (match x
	      [(letrec ([,v* ,ty* ,[e*]] ...) ,[bod])
	       (ASSERT null? (intersection v* (map core-free-vars e*)))
	       `(let ([,v* ,ty* ,e*] ...) ,bod)
	       ]
	      [(letrec ,_ ...) (error 'remove-letrec "missed letrec: ~s" `(letrec ,_ ...))]
	      [,oth (fallthru oth)])
	    )])

(ASSERT (= (length remove-letrec-grammar) (sub1 (length reduce-primitives-grammar))))

) ;; End module
