
;; [2004.08.06]
;; This outputs a token machine in Haskell concrete syntax.

;; Uses the pretty-printer.
;; Requires case-sensitivity.


(define haskellize-tokmac
  (let ()

    (define (hlist ls)
      (apply string-append
	     `("[" ,@(insert-between ", " (map (lambda (x) (format "~a" x)) ls))
	       "]")))

    (define (hprim p)
      (case p
	[(rmap) "Pamap"]
	[(rfold) "Pafold"]
	[else (string-append "P" (symbol->string p))]))

    (define hbegin
      (lambda (expr*)		
	(let loop ([expr (match `(begin ,@expr*)
				[(begin ,[expr*] ...) (apply append expr*)]
				[,expr (list expr)])])	  
	  (match expr
	     [(,x) x]
	     [(,x ,x* ...)
	      (format "(Eseq ~a ~a)" x (loop x*))]))))
    
    (define (htok t) (format "(Token \"~a\")" t))
    (define (hid id) (format "(Id \"~a\")" id))

    (define (process-constbind cbind)
      (match cbind
	[(,[hid -> id] ,[process-expr -> rhs])
	 (format "(~a, ~a)" id rhs)]))
    
    (define (process-expr expr)
      (match expr
	[(quote ,const) (format "(Econst ~a)" const)]
	[,var (guard (symbol? var)) (format "(Evar ~a)" (hid var))]
	    
	;; Both of these take timing arguments in the Haskell AST:
	[(emit ,tok ,[args*] ...)
	 (format "(Eemit Nothing ~a ~a)" (htok tok) (hlist args*))]
	[(call ,tok ,[args*] ...)
	 (format "(Ecall Nothing ~a ~a)" (htok tok) (hlist args*))]
	[(timed-call ,time ,tok ,[args*] ...)
	 (format "(Ecall (Just ~a) ~a ~a)" time (htok tok) (hlist args*))]

	[(soc-finished) "Esocfinished"]
	[(soc-return ,[body]) (format "(Esocreturn ~a)" body)]

	[(relay) "(Erelay Nothing)"]

	[(,prim ,[rand*] ...)
	 (guard (regiment-primitive? prim))
	 (format "(Primapp ~a ~a" (hprim prim) (hlist rand*))]

	[,other "UNMATCHED_SCHEMETOKSTUF"]
	[,other (error 'haskellize-tokmac:process-expr "unmatched expr: ~s" other)]
	))

    (define (process-tokbind tokbind)
      (match tokbind
	[(,tok ,args ,body ,body* ...)
	 (format "(~a, ~a, ~a)" 
		 (htok tok) 
		 (hlist (map hid args)) 
		 (if (not (null? body*))
		     (hbegin (map process-expr (cons body body*)))
		     (process-expr body))
		 )]))

    (define (process-tokname tokname)
      (symbol->string tokname))
		
    (lambda (prog)
      (match prog
	;; Accept the program with or without the "language" wrapper:
	[(program ,contents ...)
	 (haskellize-tokmac `(no-lang '(program ,contents ...)))]
        [(,lang '(program (bindings ,[process-constbind -> cbinds] ...)
				(socpgm (bindings ,[process-constbind -> socbinds] ...)
					,[process-expr -> socstmts] ...)
				(nodepgm (tokens ,[process-tokbind -> nodetoks] ...)
					 (startup ,[process-tokname -> starttoks] ...))))
	 `(haskellize-tokmac-lang
	   ,(format 
	     "(Pgm {~n  consts = ~a,~n  socconsts=~a,~n  socpgm=~a,~n  nodetoks=~a,  startup=~a~n})" 
	     (hlist cbinds)
	     (hlist socbinds)
	     (hlist socstmts)
	     (hlist nodetoks)
	     (hlist starttoks)))]))))
  
#!eof
	 `(Pgm (ConstBindings ,cbinds)
	       (SocPgm (ConstBindings ,socbinds)
		       ,socstmts)
	       (NodePgm ,nodetoks ,starttoks)


	   `(cleanup-token-machine-lang
	     '(program (bindings ,nodebinds ...)
		       (socpgm (bindings ,socbinds ...) 
			       ,socstmts ...)
		       (nodepgm (tokens ,nodup-binds ...)
				(startup ,starttoks ...))))
	   )]
	[,other (error 'cleanup-token-machine "bad input: ~s" prog)]))


