;; [2004.08.06]

;; This outputs a token machine in Haskell concrete syntax.

;; Uses the pretty-printer.
;; Requires case-sensitivity.

(define hash-symbol
  (lambda (s)
    (let* ([ls (string->list (symbol->string s))]
	   [nums (reverse (map char->integer ls))]
	   [sum (apply +
		       (map (lambda (x exp)		    
			      (* x (^ 256 exp)))
			    nums (iota (length nums))))])
      (remainder sum (^ 2 16)))))

(define haskellize-tokmac
  (let ()

    (define (hlist ls)
      (apply string-append
	     `("[" ,@(insert-between ", " (map (lambda (x) (format "~a" x)) ls))
	       "]")))

    (define (hprim p)
      (case p
;	[(rmap) "Pamap"]  [(rfold) "Pafold"]

	[(+) "Pplus"]  [(-) "Pminus"]
	[(*) "Pmult"]  [(/) "Pdiv"]
	[(<) "Pless"]  [(>) "Pgreater"]
	[(<=) "Pleq"]  [(>=) "Pgeq"]

	[else (let ([name (strip-illegal (symbol->string p))])
		(if (equal? name "")
		    (error 'haskellize-tokmac
			   "prim ~a didn't have a coherent haskellized name" p))
		(string-append "P" name))]))

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
    
    (define process-expr 
      (lambda (expr)
      (match expr
	[(quote ,const) 
	 (cond 
	  [(integer? const) (format "(Econst ~a)" const)]
	  ;; For now we're lamely expressing
	  [(symbol? const) (format "(Econst ~a)" (hash-symbol const))]
	  [(boolean? const) (format "(Econst ~a)" (if const 1 0))]
	  [else (error 'haskellize-tokmac:process-expr
		       "cannot handle this type of constant presently: ~s" const)])]
	[,var (guard (symbol? var)) (format "(Evar ~a)" (hid var))]

	[(begin ,[x]) x]
	[(begin ,[x] ,y ...) 
	 (let ([rest (process-expr `(begin ,y ...))])
;	   (disp "BUILDING ESEQ" x rest)
	   (format "(Eseq ~a ~a)" x rest))]
	
	[(if ,[test] ,[conseq] ,[altern])
	 (format "(Eif ~a ~a ~a)" test conseq altern)]

	;; Both of these take timing arguments in the Haskell AST:
	[(emit ,tok ,[args*] ...)
	 (format "(Eemit Nothing ~a ~a)" (htok tok) (hlist args*))]
	[(call ,tok ,[args*] ...)
	 (format "(Ecall Nothing ~a ~a)" (htok tok) (hlist args*))]
	[(timed-call ,time ,tok ,[args*] ...)
	 (format "(Ecall (Just ~a) ~a ~a)" time (htok tok) (hlist args*))]
	[(activate ,tok ,[args*] ...)
	 (format "(Eactivate ~a ~a)" (htok tok) (hlist args*))]

	[(flood ,tok) (format "(Eflood ~a)" (htok tok))]
	[(elect-leader ,tok) (format "(Eelectleader ~a)" (htok tok))]

	[(relay) "(Erelay Nothing)"]

	[(return ,[expr]            ;; Value
		 (to ,memb)         ;; To
		 (via ,parent)      ;; Via
		 (seed ,[seed_val]) ;; With seed
		 (aggr ,rator_tok)) ;; Aggregator 
	 (format "(Ereturn ~a ~a ~a ~a ~a)"
		 expr (htok memb) (htok parent) seed_val (htok rator_tok))	
	 ]

	[(soc-finished) "Esocfinished"]
	[(soc-return ,[body]) (format "(Esocreturn ~a)" body)]

	;; Assuming that it's synchronous!!
	[(local-sense) "(Esense)"]
	
	[(,prim ,[rand*] ...)
	 (guard (token-machine-primitive? prim))
	 (format "(Eprimapp ~a ~a)" (hprim prim) (hlist rand*))]

	[(,prim ,[rand*] ...)
	 (guard (regiment-primitive? prim))
	 (format "(Eprimapp ~a ~a)" (hprim prim) (hlist rand*))]

	[(let* ([,lhs* ,[rhs*]] ...) ,[body])
	 (format "(Elet ~a ~a)" 
		 (hlist (map (lambda (lhs rhs)
			       (format "(~a, ~a)" (hid lhs) rhs))
			     lhs* rhs*))
		 body)]
	 
;	[,other "UNMATCHED_SCHEMETOKSTUF"]
	[,other (error 'haskellize-tokmac:process-expr "unmatched expr: ~s" other)]
	)))

    (define process-tokbind 
      (lambda  (tokbind)
      (match tokbind
	[(,tok ,args ,body ,body* ...)
	 (format "(~a, ~a, ~a)" 
		 (htok tok) 
		 (hlist (map hid args)) 
		 (if (not (null? body*))
		     (hbegin (map process-expr (cons body body*)))
		     (process-expr body))
		 )])))
		
    (lambda (prog)
      (match prog
	;; Accept the program with or without the "language" wrapper:
	[(program ,contents ...)
	 (haskellize-tokmac `(no-lang '(program ,contents ...)))]
        [(,lang '(program (bindings ,[process-constbind -> cbinds] ...)
				(socpgm (bindings ,[process-constbind -> socbinds] ...)
					,[process-expr -> socstmts] ...)
				(nodepgm (tokens ,[process-tokbind -> nodetoks] ...)
					 (startup ,[htok -> starttoks] ...))))
	 `(haskellize-tokmac-language
	   ,(format 
	     "(Pgm {~n  consts = ~a,~n  socconsts=~a,~n  socpgm=~a,~n  nodetoks=~a,~n  startup=~a~n})" 
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

