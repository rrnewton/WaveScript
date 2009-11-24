;; [2004.08.06]

;; This outputs a token machine in Haskell concrete syntax.

;; Needs some unit tests though...

;; Uses the pretty-printer.
;; Requires case-sensitivity.


;; TODO: Add some unit tests.

(define hash-symbol
  (lambda (s)
    (let* ([ls (string->list (symbol->string s))]
	   [nums (reverse (map char->integer ls))]
	   [sum (apply +
		       (map (lambda (x exp)		    
			      (* x (expt 256 exp)))
			    nums (iota (length nums))))])
      (remainder sum (expt 2 16)))))

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
	[(=) "Peq"]

	[else (let ([name (strip-illegal (symbol->string p))])
		(if (equal? name "")
		    (error 'haskellize-tokmac
			   "prim ~a didn't have a coherent haskellized name" p))
		(string-append "P" name))]))

    ;; [2004.10.28] - changing this to a list rather than just two.
    (define hbegin
      (lambda (expr*)
	(format "(Eseq ~a)" (hlist expr*))))
    
    (define (htok t) (format "(Token \"~a\")" t))
    (define (hid id) (format "(Id \"~a\")" id))

    (define (process-constbind cbind)
      (match cbind
	[(,[hid -> id] ,[process-expr -> rhs])
	 (format "(~a, ~a)" id rhs)]))
    
    (define process-expr 
     (lambda (expr)
      (match expr
	[,c (guard (simple-constant? c)) (process-expr `(quote ,c))]
	[(quote ,const)
	 (cond 
	  [(integer? const) (format "(Econst ~a)" const)]
	  ;; For now we're lamely expressing
	  [(symbol? const) (format "(Econst ~a)" (hash-symbol const))]
	  [(boolean? const) (format "(Econst ~a)" (if const 1 0))]
	  ;[(string? const) (format "(Econst ~a)" (if const 1 0))
	  [else (error 'haskellize-tokmac:process-expr
		       "cannot handle this type of constant presently: ~s" const)])]
	[,var (guard (symbol? var)) (format "(Evar ~a)" (hid var))]

	[(set! ,v ,[e]) "SET_NOTDONE"]
	[(ext-ref ,[tok] ,v) "EXTREF_NOTDONE"]
	[(ext-set! ,[tok] ,v ,[x]) "EXTSET_NOTDONE"]
	[(tok ,t ,[ind])
	 ;; FIXME
	 (htok t)]
	
	[(begin ,[x]) x]
        [(begin ,[xs] ...)  (format "(Eseq ~a)" (hlist xs))]
	
	[(if ,[test] ,[conseq] ,[altern])
	 (format "(Eif ~a ~a ~a)" test conseq altern)]

	;; Both of these take timing arguments in the Haskell AST:
	;; FIXME: FINISH
	[(call (tok ,tok ,[ind]) ,[args*] ...)
	 ;(guard (memq call '(bcast subcall direct-subcall call activate call-fast)))
	 (format "(Ecall Local ~a ~a)" (htok tok) (hlist args*))]
	[(timed-call ,time (tok ,tok ,[ind]) ,[args*] ...)
	 (format "(Ecall (Timed ~a) ~a ~a)" time (htok tok) (hlist args*))]

        ;; This is a primitive, but handled special.
        ;; User better use double slashes.
        [(dbg ,s ,[args] ...) 
         (let ((str (match s
		      [(quote ,st) st]
		      [,st st]))) 
           (format "(Edbg \"~a\" ~a)" str (hlist args)))]

        [(leds ,what ,which)
         (format "(Eled ~a ~a)"
            (case what [(on) "On"] [(off) "Off"] [(toggle) "Toggle"] [else "ERR"])
            (case which [(red) "Red"] [(green) "Green"] [(yellow) "Yellow"] [else "ERR"])
         )]

	[(soc-finished) "Esocfinished"]
	[(soc-return ,[body]) (format "(Esocreturn ~a)" body)]

	;; Assuming that it's synchronous!!
	[(sync-sense) "(Esense)"]
	
	[(,prim ,[rand*] ...)
	 (guard (token-machine-primitive? prim))
	 (format "(Eprimapp ~a ~a)" (hprim prim) (hlist rand*))]
#;
	[(,prim ,[rand*] ...)
	 (guard (regiment-primitive? prim))
	 (format "(Eprimapp ~a ~a)" (hprim prim) (hlist rand*))]

	[(let ([,lhs ,[rhs]]) ,[body])
	 (format "(Elet ~a ~a)" 
		 (format "(~a, ~a)" (hid lhs) rhs)
		 body)]
	 
;	[,other "UNMATCHED_SCHEMETOKSTUF"]
	[,other (error 'haskellize-tokmac:process-expr "unmatched expr: ~s" other)]
	)))

    (define process-tokbind 
      (lambda  (tokbind)
	(mvlet ([(tok id args stored bindings body) (destructure-tokbind tokbind)])
	  (format "(~a, ~a, ~a)" 
		  (htok tok) 
		  (hlist (map hid args))
		  (process-expr body)
		 ))))
  
    (lambda (prog)
      (match prog
	;; Accept the program with or without the "language" wrapper:
        [(,lang '(program (bindings ,[process-constbind -> cbinds] ...)
		   (nodepgm (tokens ,nodetoks ...))))

	 (let ((soc-tok (assq 'SOC-start nodetoks))
	       (other-toks (filter (lambda (t) (not (eq? (car t) 'SOC-start))) nodetoks)))

	   (mvlet ([(_ _1 _2 _3 _4 socbod) (destructure-tokbind soc-tok)])
	     `(haskellize-tokmac-language
	       ,(format 
		 "(Pgm {~n  consts = ~a,~n  socconsts=[],~n  socpgm=~a,~n  nodetoks=~a,~n  startup=[Token \"node-start\"]~n})~n"	     
		 (hlist cbinds)    
		 (hlist (list (process-expr socbod)))
		 (hlist (map process-tokbind other-toks))))))]))))
  

(define-testing these-tests
  `(
    ))


(define-testing test-this (default-unit-tester
		    "30: Haskellize-Tokmac: to convert token machine to haskell-style external representation."
		    these-tests))

;(define testhaskellize test-this)
;(define testshaskellize these-tests)
(define test30 test-this)
(define tests30 these-tests)
(define test-haskellize-tokmac test-this)
(define tests-haskellize-tokmac these-tests)




; #!eof
; 	 `(Pgm (ConstBindings ,cbinds)
; 	       (SocPgm (ConstBindings ,socbinds)
; 		       ,socstmts)
; 	       (NodePgm ,nodetoks ,starttoks)


; 	   `(cleanup-token-machine-lang
; 	     '(program (bindings ,nodebinds ...)
; 		       (socpgm (bindings ,socbinds ...) 
; 			       ,socstmts ...)
; 		       (nodepgm (tokens ,nodup-binds ...)
; 				(startup ,starttoks ...))))
; 	   )]
; 	[,other (error 'cleanup-token-machine "bad input: ~s" prog)]))


