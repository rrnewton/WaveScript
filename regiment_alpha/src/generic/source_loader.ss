;;;; .title source_loader.ss 

;;;; [2005.11.11] This module loads regiment source from a ".rs" file
;;;; and desugars it according to a few simple conventions that I've
;;;; followed.  It can also load token machine ".tm" files.

; [2006.02.27] Refactored to use new common module syntax.

(module source_loader mzscheme 
  (require "../plt/iu-match.ss"
           (lib "include.ss")
           "../plt/constants.ss"
           (all-except "../plt/helpers.ss" test-this these-tests)
           (all-except "../plt/regiment_helpers.ss" test-this these-tests)
           (all-except "../plt/simulator_alpha.ss" test-this these-tests id)
           )
  (provide     	
   read-regiment-source-file
   load-regiment reg:load
   test_sourceloader
   ) ;; End provide

  (chezimports ;constants
               (except helpers           test-this these-tests)
	       (except regiment_helpers  test-this these-tests)
	       (except simulator_alpha   test-this these-tests)
	       )
; ================================================================================


;; Read the file from disk, desugar the concrete syntax appropriately.
;; .returns Two values: desugared program, and parameters.
(define read-regiment-source-file
  (lambda (fn)
    (define (desugar-token e)      
      (match e
	[((,name . ,subtok) (,arg ...) ,bods ...)
	 `[,name ,subtok (,arg ...) ,bods ...]]
	[(,name (,arg ...) ,bods ...)
	 `[,name (,arg ...) ,bods ...]]))
    (define (desugar exps)
      (let loop ((ls exps))
	(match ls
	  ;[() (error 'read-regiment-source-file "file has no return expression.")]
	  [() ()]

	  ;; Macro expansion:
	  ;==================================================
	  ;; First of all, we allow some load-time evaluation for syntax preprocessing:
	  [((quasiquote ,form) ,rest ...)
	   ;; Just evaluat it and loop:
	   (desugar (cons (eval `(quasiquote ,form)) rest))]

	  ;; Single expression file:
	  ;==================================================
	  ;; These first forms are the "single construct" style:
	  [((,lang '(program ,stuff ...)))
	   (car ls)]
	  [((tokens ,tok* ...))
	   (car ls)]

	  ;; Multiple expression file:
	  ;==================================================
	  ;; These two are the "multiple construct" style, in which
	  ;; many seperate "define" or "token" clauses are allowed.
	  [((token ,stuff ...) . ,rest)
	   (match (desugar rest)
	     [() `(tokens ,(desugar-token stuff))]
	     [(tokens . ,others)
	      `(tokens ,(desugar-token stuff) ,@others)])]
	  [((define ,x* ,y*) ... ,main)
	   `(letrec (,(map desugar-define x* y*) ...) ,(desugar-pattern-matching main))]

	  [(,other ,rest ...)
	   (error 'read-regiment-source-file
		  "invalid expression in definition context: ~s" `(,other ,rest ...))])))

    (match (file->slist fn)
      [((parameters ,p ...) ,exps ...)
       (values (desugar exps) p)]
      [(,prog ...) 
       (values (desugar prog) ())])))

;; This loads (e.g. reads, compiles, and simulates) a regiment program:  <br><br>
;;
;; [2005.11.17] Currently redundant with code in regiment.ss:            <br>
;; [2006.02.15] NOTE: This currently passes the flags to BOTH the       
;; compiler and the simulator.  They both follow the bad practice of
;; ignoring flags they don't understand.
;;
;; [2006.03.02] To make the argument conventions even more confusing,
;; I just added the option to include a simworld argument before the file-name.
(define (load-regiment . args)
  (match args    
    [(,world ,fn . ,opts) (guard (simworld? world) (string? fn))
     ;; We pass the world by putting it in the global parameter and
     ;; telling the simulator to use the existing "stale" simworld.
     (simalpha-current-simworld world)
     (apply load-regiment fn 'use-stale-world opts)]
    [(,fn . ,opts) (guard (string? fn))
     ;; Flags are things like 'verbose, params are '[sim-timeout 1000]
     ;; Flags get passed to run-compiler and compile-simulate-alpha.
     (let ([flags (filter (lambda (x) (not (list? x))) opts)]
	     [userparams (filter list? opts)])
	 (mvlet ([(prog codeparams passes)
		  (let ([type (string->symbol (extract-file-extension fn))])
		    (mvlet (((prg params) (read-regiment-source-file fn)))
		      (case type
			[(rs) (values prg params (pass-names))]
			[(tm) (values prg params 
				      (list-remove-before 'cleanup-token-machine (pass-names)))]
			[(sim alpha) (values prg params ())]
			[else (error 'load-regiment "can't handle file with this extension: ~s" fn)]
			)))])
	   
	   ;; User params override those set in the code:
	   (let ((params (append userparams (filter (lambda (pr) (not (assq (car pr) userparams))) codeparams))))
	     ;; Set all the params before running things:
	     (for-each eval params) ;; [2005.12.02] Changing this so the params stick after the run.  Better for re-running.
	     (let ((result ;(with-evaled-params params
					;  (lambda ()
		    (parameterize ([pass-names passes])
		      (apply run-simulator-alpha

			     ;; [2005.11.28] Changing this to let run-simulator-alpha apply compile-simulate-alpha for us.
			     (apply (top-level-value 'run-compiler) prog flags)
					;			       (apply compile-simulate-alpha
					;				      (apply run-compiler prog flags)
					;				      (append flags params))

			     flags
					;'srand (current-time)
			     ))))
	       (print-stats)
	       result))))]
    [,other (error 'load-regiment "bad arguments: ~s\n" other)]))

(define reg:load load-regiment) ;; shorthand


; ================================================================================
;;; Some syntactic sugar.

;; Desugar define statements found within files into letrec statements.
(define (desugar-define lhs rhs)  
  (match lhs
    [,s (guard (symbol? s))
	`[,s ,(desugar-pattern-matching rhs)]]
    [(,f ,x* ...) (guard (symbol? f))
     `[,f ,(desugar-pattern-matching `(lambda ,x* ,rhs))]]
    [,_  (error 'source_loader:desugar-define
		"invalid define expression: ~a" `(define ,lhs ,rhs))]))

;; Desugar pattern matching within lambda's, letrecs, and "match" statements. <br>
;; TODO: This should really not go in the source_loader.
(define desugar-pattern-matching 
  (let ([break-pattern 
	 (lambda (pat)
	   (match pat
	     [,s (guard (symbol? s)) 
		 (values s '())]
	     [#(,[pv* binds*] ...)
	      (let ([v (unique-name 'pattmp)]
		    [len (length pv*)])
		(let ([newbinds 
		       `([,pv* (tupref ,(iota len) ,(make-list len len) ,(make-list len v))] ...)])		  
		  (values v
			  `( ,@(filter (lambda (b) (not (eq? (car b) '_))) newbinds)
			     ,@(apply append binds*)
			     ))))]
	   ))])
  (lambda (expr)
  (let loop ([expr expr] [env '()] [subst '()])
  (match expr 
    [,c (guard (constant? c)) c]
    [(quote ,d) `(quote ,d)]
    [,var (guard (symbol? var))
	  (let ((entry (assq var subst)))
	    (if entry (cadr entry) var))]          
    [(if ,[test] ,[conseq] ,[altern])
     (guard (not (assq 'if env)))
     `(if ,test ,conseq ,altern)]
    ;; Pre type checking!!
    [(lambda (,[break-pattern -> formal* binds* ] ...) ,expr)
     (guard (not (assq 'lambda env)))     
     (let ([bod (loop expr
		 (append formal* env)
		 ;(append (apply append subst*) subst)
		 subst
		 )]
	   [binds (apply append binds*)])
       (if (null? binds)
	   `(lambda (,formal* ...)  ,bod)
	   `(lambda (,formal* ...)  (letrec (,binds ...) ,bod))))]
    [(letrec ((,[break-pattern -> lhs* binds*] ,[rhs*]) ...) ,[bod])
     (guard (not (assq 'letrec env)))
     
     `(letrec ( [,lhs* ,rhs*] ...
		,@(apply append binds*)
	       )
	,bod)]
    ;; Only handles one-armed matches right now:
;    [(match ,[x] [ ,[break-pattern -> var* binds*]  ,[rhs*] ] ...)
    [(match ,[x] (,[break-pattern -> var binds] ,[rhs]))
     `(letrec ([,var ,x]
	       ,binds ...)
	  ,rhs)]    
    [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
    )))))


; ================================================================================

(define these-tests
  `(["Run a basic test of the pattern match expander."
     (reunique-names (,desugar-pattern-matching '(lambda (#(foo #(bar baz)) x) foo)))
     (lambda (pattmp x)
       (letrec ([foo (tupref 0 2 pattmp)]
		[pattmp_1 (tupref 1 2 pattmp)]
		[bar (tupref 0 2 pattmp_1)]
		[baz (tupref 1 2 pattmp_1)])
	 foo))]

    [(,desugar-pattern-matching '(match 3 [x x]))
     (letrec ([x 3]) x)]

    ))

(define test-this (default-unit-tester "source_loader.ss: For reading regiment source files." these-tests))
(define test_sourceloader test-this) ;; Unit test entry point.

) ;; End Module.


#|	      


      (when (equal? type "rs")
	(runloop 'compile (list fn))
	(set! fn out_file))) 

	     (printf "Running simulation from file: ~a\n" fn)
	     (let ((result
		    ;; Be careful to watch for parameterization:	     
		    (mvlet (([prog params] (read-regiment-source-file fn)))
		      (with-evaled-params params
					  (lambda () 
					    (apply run-simulator-alpha prog opts))))))
	       ;; Print simalpha stats:
	       (print-stats)
	       (if plot (gnuplot result))
	       (if simrepl (new-cafe))
	       result))





		    (parameterize ([pass-names
				 (cond
				  [(equal? type "rs") (pass-names)]
				  [(equal? type "tm") (list-remove-before 'cleanup-token-machine
									  (pass-names))]
				  [else (error 'regiment "unknown input file extension: ~s" type)]
				  )])
		      (set! out_file (string-append (remove-file-extension fn) extension))
		      ;; Don't overwrite one of our own input files!
		      (if (member out_file filenames) (set! out (string-append "out." extension)))

		      (printf "~n  Writing token machine to: ~s ~n" out_file)
		      (mvlet ([(prog params) (read-regiment-source-file fn)])
		       
			(delete-file out_file)
			(let ((comped 
			       (if makesimcode 
				   ;; This bakes the parameters right into the sim code:
				   (apply compile-simulate-alpha
					  (apply run-compiler prog opts)
					  params)
				   (apply run-compiler prog opts))))			  
			  (parameterize ([print-graph #t] [print-level #f] [print-length #f])
			    (with-output-to-file out_file
			      (lambda ()
				;; Otherwise we need to propogate the params to the output file:
				(if (not makesimcode)
				    (pretty-print `(parameters ,@params)))
				(pretty-print comped))))))



|#
