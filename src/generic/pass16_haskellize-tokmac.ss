
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
	[else (string-append "P" (symbol->string p))]

    (define (process-const cbind)
      (match cbind
	[(,id (quote ,const))
	 (format "(\"~a\", ~a)" id const)]))
    
    (define (process-statement stmt)
      (match stmt 
	[(emit ,tok ,args* ...)
	 (format "(Eemit )")]
	[(,prim ,rand* ...)
	 (guard (regiment-primitive? prim))
	 (format "(Primapp ~a ~a" (haskellize-prim prim) (haskel)
	 ]
	))


    (define (process-tokbind tokbind)
      0)

    (define (process-tokname tokname)
      (symbol->string tokname))
		
    (lambda (prog)
      (match prog
        [(,lang '(program (bindings ,[process-const -> cbinds] ...)
				(socpgm (bindings ,[process-const -> socbinds] ...)
					,[process-statement -> socstmts] ...)
				(nodepgm (tokens ,[process-tokbind -> nodetoks] ...)
					 (startup ,[process-tokname -> starttoks] ...))))
	 (disp "cbinds" cbinds)

	 (format "(Pgm {~n  consts = ~a,~n  socconsts=~a,~n  socpgm=~a,~n  nodetoks=~a,  startup=~a~n})" 
		 (hlist cbinds)
		 (hlist socbinds)
		 (hlist socstmts)
		 (hlist nodetoks)
		 (hlist starttoks))]))))

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


