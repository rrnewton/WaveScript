
;; [2004.08.06]
;; This outputs a token machine in Haskell concrete syntax.

;; Uses the pretty-printer.
;; Requires case-sensitivity.


(define gen-haskell-token-machine
  (let ()

    (define (process-consts cbinds)
      "[]")
    
    (define (process-statements stmts)
      "[]")

    (define (process-tokbinds tokbinds)
      "[]")

    (define (process-tokname tokname)
      (symbol->string tokname))

    (lambda (prog)
      (match prog
        [(,lang '(program (bindings ,[process-consts -> cbinds] ...)
				(socpgm (bindings ,[process-consts -> socbinds] ...)
					,[process-statements -> socstmts] ...)
				(nodepgm (tokens ,[process-tokbinds -> nodetoks] ...)
					 (startup ,[process-tokname -> starttoks] ...))))
	 (format "(Pgm { consts = ~s,~n  socconsts=~s,~n socpgm=~s,~n nodetoks=~s, startup=~s~n})" 
		 cbinds 
		 socbinds 
		 socstmts
		 nodetoks
		 starttoks)]))))

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


