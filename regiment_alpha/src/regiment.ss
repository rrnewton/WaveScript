;#! /usr/bin/scheme --script
;#!/usr/bin/chez --script

;; Regiment.ss
;; This file is a script that drives the regiment compiler/simulator.


;(load (string-append (getenv "HOME") "/scheme/chez/full_chez.ss"))

(define start-dir (current-directory))
;(printf "STARTING: ~s\n" start-dir)

(define regiment-origin "unknown")
(define stderr
  (let ((buffer-size 1))
    (let ((p (make-output-port 2 (make-string buffer-size))))
      (set-port-output-size! p (- buffer-size 1))
      p)))

;; If the compiled version is there, use that:
(parameterize ([current-directory (string-append (getenv "REGIMENTD") "/src/")])
  (if (file-exists? (format "./build/~a/compiler_chez.so" (machine-type)))
      (begin 
	(set! regiment-origin "compiled .so")
	(load (format "./build/~a/compiler_chez.so" (machine-type))))
      (begin (fprintf stderr "Loading Regiment from source...\n")
	     (set! regiment-origin "source")
	     (load "./compiler_chez.ss"))))

; =======================================================================

(define (print-help)
  (printf "Regiment system, version ~s (loaded from ~a)\n" (regiment-version) regiment-origin)
  (printf "Usage: regiment command [options] ~n")
  (printf "~n")
  (printf "Commands: ~n")
  (printf "  help          prints this message~n")
  (printf "  compile  (c)  compile Regiment source (.rs) to token machines~n")
  (printf "  simulate (s)  simulate a token machine or simulator file~n")
  (printf "  interact (i)  start up Scheme REPL with Regiment loaded~n")
  (printf "  test     (t)  run all regiment tests~n")
  (printf "  printlog (p)  print the contents of a log file~n")
  (printf "~n")
  (printf "General Options:  ~n")
  (printf "  -v   verbose compilation/simulation, includes warnings~n")
  (printf "~n")
  (printf "Compiler Options: ~n")
  (printf "  -lt  type check only, print typed program to stdout           ~n")
  (printf "  -ltt type check only, print *only* top level types to stdout  ~n")
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

(define (print-types-and-exit prog . opts)
  (define verbose? (memq 'verbose opts))
  (printf ";; Regiment program with infered types: \n")
  ;; Run just the verify regiment pass, it will associate types:
  (parameterize ([print-vector-length #f])
  (match (verify-regiment `(lang '(program ,prog)))
    [(,lang '(program ,p ,t))
     (match p
       [(letrec ([,id* ,t* ,rhs*] ...) ,bod)
	(for-each (lambda (id t rhs)
		    (if verbose?
			(begin (pretty-print `(define ,id : ,t ,rhs))(newline))
			(printf "~a : ~a\n" (pad-width 30 id) t)))
	  id* t* rhs*)
	(if verbose? (pretty-print bod))]
       [,p (pretty-print p)])
     ;(printf "\n;; Regiment program return type: ~a\n" t)
     (printf "  : ~a\n" t)
     (exit)]
    [,other (error 'print-types-and-exit "bad output from verify-regiment: ~s" other)])))

(define main 
  (lambda args    
    (define makesimcode #f)
    (define outfile #f)
    (define plot #f)
    (define simrepl #f)
;    (disp "Main called w ARGS: " args)
    (when (null? args) (print-help) (regiment-exit 0))
    
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
		    [(.h ,rest ...) (print-help) (regiment-exit 0)]

		    [(-plot ,rest ...) (set! plot #t) (loop rest)]
		    [(-repl ,rest ...) (set! simrepl #t) (loop rest)]

		    [(-lt ,rest ...) (set! opts (cons 'type-only-verbose opts)) (loop rest)]
		    [(-ltt ,rest ...) (set! opts (cons 'type-only opts)) (loop rest)]

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
	  ;; Unit Test mode:
	  [(t test)
	   (test-units)
	   ;(test-everything)
	   ]
	  ;; Compile mode:
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
		      (mvlet ([(prog params) 
			       (parameterize ([current-directory start-dir])
				 (read-regiment-source-file fn))])

			;; If we're in type-check mode we print types and exit here:
			(if (memq 'type-only-verbose opts)
			    (print-types-and-exit prog 'verbose))
			(if (memq 'type-only opts)
			    (print-types-and-exit prog))
			    

			(printf "~n  Writing token machine to: ~s ~n" out_file)
			(delete-file out_file)
			(let ((comped 
			       (if makesimcode 
				   ;; This bakes the parameters right into the sim code:
				   (apply compile-simulate-alpha
					  (apply run-compiler prog opts)
					  params)
				   (apply run-compiler prog opts))))			  
			  (parameterize ([print-graph #t] 
					 [print-level #f] 
					 [print-length #f]
					 [pretty-maximum-lines #f])
			    (with-output-to-file out_file
			      (lambda ()
				;; Otherwise we need to propogate the params to the output file:
				(if (not makesimcode)
				    (pretty-print `(parameters ,@params)))
				(pretty-print comped))))))
		      ))))]
	  ;; Simulation mode (also may invoke compiler):
	  [(s simulate)
	   (let ((fn (if (null? filenames)
			 "out.sim"
			 (car filenames))))
	     ;; If it's not simulatable yet, we switch to compile-mode and compiler it first:
	     (let ([type (extract-file-extension fn)])
	       (if (equal? type "rs")
		   (begin (runloop 'compile (list fn))
			  (set! fn out_file))
		   (if (equal? type "tm")
		       (begin (runloop 'compile (list fn))
			      (set! fn out_file)))))

	     (printf "Running simulation from file: ~a\n" fn)
	     (let ((result
		    ;; Be careful to watch for parameterization:	     
		    (mvlet (([prog params] (read-regiment-source-file fn)))
					;(inspect params)
		      (with-evaled-params params
					  (lambda () 
					    (apply run-simulator-alpha prog 
						   'srand (current-time)
						   opts))))))
	       ;; Print simalpha stats:
	       (print-stats)
	       (if plot (gnuplot result))
	       (if simrepl (new-cafe))
	       result))]

	  ;; Interactive mode.  A Scheme REPL.
	  [(i interact)
	   (for-each (lambda (arg)
		       (and (not (equal? arg "i"))
			    (not (equal? arg "interact"))
			    (load arg)))
	     args)
	   (new-cafe)]

	  ;; Printing SExp log files.
	  [(p printlog)	   
	   (match (filter (lambda (arg)
			    (and (not (equal? arg "p"))
				 (not (equal? arg "printlog"))))
		    args)
	     [() (if (file-exists? "__temp.log") (reg:printlog "__temp.log")
		     (if (file-exists? "__temp.log.gz") (reg:printlog "__temp.log.gz")
			 (error 'regiment:printlog "no log file supplied or found")))]
	     [(,file) (reg:printlog file)]
	     [,else  (error 'regiment:printlog "only can print one logfile at a time")])]

	  )))))))
  
(define (reg:printlog file)
  (let ((stream (reg:read-log file 'stream)))
    (let loop ((s (reg:read-log file 'stream)))
      (unless (null? s)
	(display (log-line->human-readable 0 (stream-car s) ()))
	(loop (stream-cdr s))))))

(define (regiment-exit code)
  ;; In case we're building a heap, we set this before we exit.
  ;(disp "SETTING HEAP: " regiment-origin (top-level-value 'regiment-origin))
  (set! regiment-origin "saved heap")
  ;(disp "HEAP SET: " regiment-origin (top-level-value 'regiment-origin))
  (exit code))

; =======================================================================
(suppress-greeting #t)
;; If we're running from heap, we need to set the scheme-start:
;; Set the invocation directory to whatever the current directory is:
(scheme-start (lambda args (set! start-dir (cd)) 
		      ;(random-seed (current-time))
		      (apply main args)))
;(random-seed (current-time))

;; If we're running from source, we invoke main right here:
;; Shave off the first argument, it just carries the working directory:
;(apply main (cdr (command-line-arguments)))
(if (null? (command-line-arguments))
    (error 'regiment.ss "script must take at least one argument.  First argument should be working directory."))
(apply main (cdr (command-line-arguments)))
