;(load (string-append (getenv "HOME") "/scheme/chez/full_chez.ss"))
(if (file-exists? "compiler_chez.ss")
    (if (file-exists? "compiler_chez.so")
	(load "compiler_chez.so")
	(load "compiler_chez.ss"))
    (parameterize ([current-directory "~/RegionStreams/compiler/src"])
      (if (file-exists? "~/RegionStreams/compiler/src/compiler_chez.so")
	(load "~/RegionStreams/compiler/src/compiler_chez.so")
	(load "~/RegionStreams/compiler/src/compiler_chez.ss"))))

;; ======================================================================


(define (print-help)
  (printf "Regiment system, version ~s~n" (regiment-version))
  (printf "Usage: regiment command [options] ~n")
  (printf "~n")
  (printf "Commands: ~n")
  (printf "  help          prints this message~n")
  (printf "  compile  (c)  compile Regiment to token machines~n")
  (printf "  simulate (s)  simulate a token machine file~n")
  (printf "~n")
  (printf "General Options:  ~n")
  (printf "  -v   verbose compilation/simulation, includes warnings~n")
  (printf "~n")
  (printf "Compiler Options: ~n")
  (printf "  -l0  stop compilation just before deglobalize ~n")
  (printf "  -l1  compile barely to tokens (just after deglobalize) ~n")
  (printf "  -l2  compile to just before haskellize-tokmac ~n")
  (printf "  -l3  output haskell style external format ~n")
  (printf "~n")
  (printf "Simulator Options: ~n")
  (printf "  -timeout <n>  timeout after n clock ticks ~n")
  )
	   
(define main 
  (lambda args
    (disp "Main called w ARGS: " args)
    (if (null? args)
	(begin (print-help) (exit 0)))
;    (printf "regimentc: compile regiment programs!~n")
    (let ([opts '()]
	  [extension ".sim"])
      (letrec ([loop 
	      (lambda (args)
		(match args
		    [() '()]

		    [(-v ,rest ...) 
		     (set! opts (cons 'verbose opts))
		     (regiment-verbose #t)
		     (loop rest)
		     ]
		    [(.h ,rest ...) (print-help) (exit 0)]

		    [(-l0 ,rest ...) (set! opts (cons 'almost-tokens opts)) 
                                     (set! extension ".sexp") (loop rest)]
		    [(-l1 ,rest ...) (set! opts (cons 'barely-tokens opts)) 
		                     (set! extension ".tm")  (loop rest)]
		    [(-l2 ,rest ...) (set! opts (cons 'almost-haskell opts)) 
                                     (set! extension ".sim") (loop rest)]
		    [(-l3 ,rest ...) (set! opts (cons 'haskell-tokens opts))
                                     (set! extension ".tmh") (loop rest)]

		    [(-timeout ,n ,rest ...)
		     (let ((n (read (open-input-string (format "~a" n)))))
		     (set! opts (cons 'timeout (cons n opts))))]

		    ;; otherwise a file to compile that we add to the list
		    [(,fn ,rest ...)
		     ;(regiment-compile-file fn)
		     (cons (symbol->string fn) (loop rest))]

		    [,_ (error "Bad command line arguments to regimentc: ~a~n" args)]
		    ))])

      ;; I keep disjoint options for the two modes so I use the same option-processor (loop)
      (let ([mode (string->symbol (car args))]
	    [filenames (loop (map string->symbol (cdr args)))])

	(disp "MODE: " mode "OPTS: " opts)
	
	(case mode
	  [(c)
	    (if (null? filenames)
		(begin
		  (printf "No input file.  Type top-level Regiment expression.~n")
		  (printf "> ")(flush-output-port)
		  (let* ([expr (read)])
		    (printf "~n Using default output file: out.tm...~n")		
		    (apply run-compiler expr "out.tm" opts)))
		(for-each (lambda (fn)
			    (let ([type (extract-file-extension fn)])
			      (fluid-let ([pass-names
					   (cond
					    [(equal? type "rs") pass-names]
					    [(equal? type "tm") (list-remove-before 'cleanup-token-machine
										    pass-names)]
					    [else (error 'regiment "unknown input file extension: ~s" type)]
					    )])
				(let ([out (string-append (remove-file-extension fn) extension)])
				  (if (member out filenames)
				      (set! out (string-append "out." extension)))
				  (printf "~n  Writing token machine to: ~s ~n" out)			  
				  (apply run-compiler (car (file->slist fn)) out opts)))))
			  filenames))]
	  [(s) ;; simulate
	   (let ((fn (if (null? filenames)
			 "out.sim"
			 (car filenames))))
	     (printf "Running simulation from file: ~a\n" fn)
	     (apply run-simulator-alpha (car (file->slist fn)) opts))
	   ]
	  ))))))
  


;; ======================================================================
(suppress-greeting #t)
(scheme-start main)
(when (top-level-bound? 'command-line-arguments)
      (apply main (command-line-arguments))
      (disp "SCRIPT FINISHED" (scheme-script) (command-line-arguments)))
