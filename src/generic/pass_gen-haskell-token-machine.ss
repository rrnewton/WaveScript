
;; [2004.08.06]
;; This outputs a token machine in Haskell concrete syntax.

;; Uses the pretty-printer.
;; Requires case-sensitivity.



(define gen-haskell-token-machine
  (let ()

    (lambda (prog)
      (match prog
        [(,lang '(program (bindings ,[process-consts -> cbinds] ...)
				(socpgm (bindings ,[process-consts -> socbinds] ...)
					,[process-statements -> socstmts] ...)
				(nodepgm (tokens ,[process-tokbinds -> nodetoks] ...)
					 (startup ,[process-tokname -> starttoks] ...))))

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


