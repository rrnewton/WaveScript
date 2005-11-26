;; source_loader.ss 

;; [2005.11.11] This module loads regiment source from a ".rs" file
;; and desugars it according to a few simple conventions that I've
;; followed.

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
	  [() (error 'read-regiment-source-file "file has no return expression.")]
	  ;; First of all, we allow some load-time evaluation for syntax preprocessing:
	  [((quasiquote ,form) ,rest ...)
	   ;; Just evaluat it and loop:
	   (desugar (cons (eval `(quasiquote ,form)) rest))]

	  ;; These first forms are the "single construct" style:
	  [((,lang '(program ,stuff ...)))
	   (car ls)]
	  [((tokens ,tok* ...))
	   (car ls)]

	  ;; These two are the "multiple construct" style, in which
	  ;; many seperate "define" or "token" clauses are allowed.
	  [((token ,stuff* ...) ...)
	   `(tokens ,@(map desugar-token stuff*))]
	  [((define ,x* ,y*) ... ,main)
	   `(letrec ((,x* ,y*) ...) ,main)]
	  ;[,retexp retexp]
	  [(,other ,rest ...)
	   (error 'read-regiment-source-file
		  "invalid expression in definition context: ~s" other)])))

    (match (file->slist fn)
      [((parameters ,p ...) ,exps ...)
       (values (desugar exps) p)]
      [(,prog ...) 
       (values (desugar prog) ())])))

;; This loads (e.g. reads, compiles, and simulates) a regiment program:
;; [2005.11.17] Currently redundant with code in regiment.ss:
(define load-regiment
  (lambda (fn . opts)
    ;; Flags are things like 'verbose, params are '[sim-timeout 1000]
    ;; Flags get passed to run-compiler and compile-simulate-alpha.
    (let ((flags (filter symbol? opts))
	  (userparams (filter list? opts)))
    (mvlet ([(prog codeparams passes)
	     (let ([type (string->symbol (extract-file-extension fn))])
	       (mvlet (((prg params) (read-regiment-source-file fn)))
		 (case type
		   [(rs) (values prg params pass-names)]
		   [(tm) (values prg params 
				 (list-remove-before 'cleanup-token-machine pass-names))]
		   [(sim alpha) (values prg params ())]
		   [else (error 'load-regiment "can't handle file with this extension: ~s" fn)]
		   )))])
      
      ;; User params override those set in the code:
      (let ((params (append userparams (filter (lambda (pr) (not (assq (car pr) userparams))) codeparams))))
      ;; Set all the params before running things:
      (let ((result (with-evaled-params params
			  (lambda ()
			    (fluid-let ([pass-names passes])
			      (run-simulator-alpha
			       (apply compile-simulate-alpha
				      (apply run-compiler prog flags)
				      (append flags params))
			       'srand (current-time)
			       ))))))
	(print-stats)
	result))))))

#|	      


      (when (equal? type "rs")
	(runloop 'compile (list fn))
	(set! fn out_file)))

	     (printf "Running simulation from file: ~a\n" fn)
	     (let ((result
		    ;; Be careful to watch for parameterization:	     
		    (mvlet (([prog params] (read-regiment-source-file fn)))
					;(inspect params)
		      (with-evaled-params params
					  (lambda () 
					    (apply run-simulator-alpha prog opts))))))
	       ;; Print simalpha stats:
	       (print-stats)
	       (if plot (gnuplot result))
	       (if simrepl (new-cafe))
	       result))





		    (fluid-let ([pass-names
				 (cond
				  [(equal? type "rs") pass-names]
				  [(equal? type "tm") (list-remove-before 'cleanup-token-machine
									  pass-names)]
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