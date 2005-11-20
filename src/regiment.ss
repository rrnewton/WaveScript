;#! /usr/bin/scheme --script
;#!/usr/bin/chez --script

;; Regiment.ss
;; This file is a script that drives the regiment compiler/simulator.


;(load (string-append (getenv "HOME") "/scheme/chez/full_chez.ss"))

(define start-dir (current-directory))

(printf "STARTING: ~s\n" start-dir)

;; If the compiled version is there, use that:
(if (file-exists? "compiler_chez.ss")
    (if (file-exists? "compiler_chez.so")
	(load "compiler_chez.so")
	(load "compiler_chez.ss"))
    (parameterize ();[current-directory "~/cur"])
      (if (file-exists? (string-append (getenv "REGIMENTD") (format "/src/build/~a/compiler_chez.so" (machine-type))))
	(load (string-append (getenv "REGIMENTD") (format "/src/build/~a/compiler_chez.so" (machine-type))))
	(load (string-append (getenv "REGIMENTD") (format "/src/compiler_chez.ss"))))))

; =======================================================================


(define (print-help)
  (printf "Regiment system, version ~s~n" (regiment-version))
  (printf "Usage: regiment command [options] ~n")
  (printf "~n")
  (printf "Commands: ~n")
  (printf "  help          prints this message~n")
  (printf "  compile  (c)  compile Regiment source (.rs) to token machines~n")
  (printf "  simulate (s)  simulate a token machine or simulator file~n")
  (printf "  interact (i)  start up Scheme REPL with Regiment loaded~n")
  (printf "  test     (t)  run all regiment tests~n")
  (printf "~n")
  (printf "General Options:  ~n")
  (printf "  -v   verbose compilation/simulation, includes warnings~n")
  (printf "~n")
  (printf "Compiler Options: ~n")
  (printf "  -l0  stop compilation just before deglobalize          (.sexp)~n")
  (printf "  -l1  compile barely to tokens (just after deglobalize) (.tm)~n")
  (printf "  -l2  compile to just before haskellize-tokmac          (.sim)~n")
  (printf "  -l3  output haskell style external format (deprecated) (.tmh)~n")
  (printf "  -l4  output generated simulator-alpha code             (.sim.alpha)~n")
  (printf "  -l5  output generated NesC/Tossim code                 (.sim.nesc) ~n")
  (printf "~n")
  (printf "Simulator Options: ~n")
  (printf "  -timeout <n>  timeout after n clock ticks\n")
  (printf "  -plot         when simulation finishes, gnuplot output\n")
  (printf "  -repl         when simulation finishes, run interactive REPL\n")
  )


(define (with-evaled-params params th)
  (let loop ((ls params))
    (if (null? ls) 
	(begin 
	  ;(disp "PARAMED TIME: " (sim-timeout))
	  ;(inspect sim-timeout)
	  (newline)
	  (th))
	(parameterize ([(eval (caar ls)) (eval (cadar ls))])
	  (printf "Setting parameter: ~a ~a\n" (caar ls) (cadar ls))
	  (loop (cdr ls))))))

(define main 
  (lambda args    
    (define makesimcode #f)
    (define outfile #f)
    (define plot #f)
    (define simrepl #f)
    (disp "Main called w ARGS: " args)
    (when (null? args) (print-help) (exit 0))
    
;    (printf "regimentc: compile regiment programs!~n")
    (let ([opts '()]
	  [extension ".sim"])
      ;; Loop goes through the arguments, processing them accordingly:
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

		    [(-plot ,rest ...) (set! plot #t) (loop rest)]
		    [(-repl ,rest ...) (set! simrepl #t) (loop rest)]

		    [(-l0 ,rest ...) (set! opts (cons 'almost-tokens opts))		     
                                     (set! extension ".sexp") (loop rest)]
		    [(-l1 ,rest ...) (set! opts (cons 'barely-tokens opts)) 
		                     (set! extension ".tm")  (loop rest)]
		    [(-l2 ,rest ...) (set! opts (cons 'almost-haskell opts)) 
                                     (set! extension ".sim") (loop rest)]
		    [(-l3 ,rest ...) (set! opts (cons 'haskell-tokens opts))
                                     (set! extension ".tmh") (loop rest)]

		    [(-l4 ,rest ...) 
		     (set! makesimcode #t)
		     (set! opts (cons 'almost-haskell opts))
		     (set! extension ".sim.alpha") (loop rest)]

		    [(-l5 ,rest ...)
		     (set! pass-names 
			   (snoc 'emit-nesc (snoc 'flatten-tokmac
			     (remq 'flatten-tokmac (remq 'emit-nesc pass-names)))))
		     ;(set! makesimcode #t)
		     (set! extension ".sim.nesc") (loop rest)]

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
	(let ([symargs (map string->symbol args)])
	  (let runloop ([mode (car symargs)] [filenames (loop (cdr symargs))])
	(case mode
	  [(t test)
	   (test-units)
	   ;(test-everything)
	   ]
	  [(c compile)
	   (if (null? filenames)
	       (begin
		 (printf "No input file.  Type top-level Regiment expression.~n")
		 (printf "> ")(flush-output-port)
		 (let* ([expr (read)])
		   (printf "~n Using default output file: out.tm...~n")		
		   (apply run-compiler expr "out.tm" opts)))

	       (begin 
					;(disp "MODE: " mode "OPTS: " opts)
		 (if (> (length filenames) 1)
		     (error 'regiment_command_line_compiler "can't handle more than one filename at a time currently: ~s" filenames))
		 
		 (let* ([fn (car filenames)]
			[type (extract-file-extension fn)])
		   ;; Do a little sanity checking between our options and our input file:
		    (cond
		     [(equal? type "tm") (if (or (memq '-l0 symargs) (memq '-l1 symargs))
					     (error 'regiment_command_line_compiler
						    "Cannot use -l0/-l1 with an input that's already a TM!: ~s" fn))]
		     [(equal? type "rs") (void)]
		     [else (error 'regiment_command_line_compiler "invalid input file extension: ~s" fn)])
		    ;; ----------------------------------------
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
		      ))))]

	  [(s simulate)
	   (let ((fn (if (null? filenames)
			 "out.sim"
			 (car filenames))))
	     ;; If it's not simulatable yet, we compile it just for convenience:
	     (let ([type (extract-file-extension fn)])
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
	       result))]

	  [(i interact) (new-cafe)]

	  )))))))
  


; =======================================================================
(suppress-greeting #t)
(scheme-start main)
(when (top-level-bound? 'command-line-arguments)
      (apply main (command-line-arguments))
      (disp "SCRIPT FINISHED" (scheme-script) (command-line-arguments)))
