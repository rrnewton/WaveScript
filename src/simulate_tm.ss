#! /bin/sh
#|
exec mred -qr "$0" ${1+"$@"}
|#


(load/use-compiled "compiler_plt.ss")

(define (print-help)
  (printf "Regiment simulator, PLT compiled (version ~s) ~n" (regiment-version))
  (printf "Usage: simulate_tm [options] <file>.tm ~n")
  (printf "~n")
  (printf "Options: ~n")
  (printf "  -s0  Simplest simulator model.  Circular radio, no loss/failure. (default)~n")
  (printf "  ~n")
  (printf "  -l0  Turn off event logging~n")
  (printf "  -l1  Turn on event logging~n")
  (printf "  -l2  Highly verbose logging of simulator events~n")
  (printf "  ~n")
  (printf "  -log <file>  Log simulation info to a particular file, otherwise stdout.~n")
  (printf "~n")
  )


(define (main . args)
  (if (null? args)
      (begin (print-help) (exit 0))      
      
      (let ([opts '()])
	(simulation-logger (current-output-port))
	(letrec ([loop 
	      (lambda (args)
		(match args
		       [() '()]
		       
		       [(-v ,rest ...) 
			(regiment-verbose #t)
			(loop rest)]

		       [(-log ,[symbol->string -> file] ,rest ...)
			(printf "Logging simulation output to: ~s~n" file)
			(if (file-exists? file) (delete-file file))
			(simulation-logger (open-output-file file))
			(loop rest)]

		       [(-l0 ,rest ...)
			(simulation-logger #f)
			(loop rest)]

		       [(-l1 ,rest ...)
			(if (not (simulation-logger))
			    (simulation-logger (current-output-port)))
			(loop rest)]

		       [(-s0 ,rest ...) 		     
			(loop rest)]

		    ;; otherwise a file to compile that we add to the list
		    [(,fn ,rest ...)
		     ;(regiment-compile-file fn)
		     (cons (symbol->string fn) (loop rest))]

		    [,_ (error "Bad command line arguments to regimentc: ~a~n" args)]
		    ))])

	  (let ([filenames (loop (map string->symbol args))])	    
	    (if (null? filenames)
		(begin
		  (printf "No tokem machine input file.~n")
		  (exit 1)))
	    (if (not (null? (cdr filenames)))
		(begin
		  (printf "More than one filename provided: ~s~n" filenames)
		  (exit 1)))

	    (let* ([expr (if (list? (car filenames))
			     (car filenames)
			     (read (open-input-file (car filenames))))])
	      (printf "Token machine read: ~n")
	      (pretty-print expr)
	      (newline)
		
		(init-graphics)
		(close-graphics)
		(cleanse-world)
					;(graphical-repl)		
		(init-world)
		(init-graphics)
		(cleanse-world)
	      (let ([csn (compile-simulate-nought expr)])
		(printf "Simulator engines compiled... ~n")
		
		(let ([res (graphical-simulation (build-simulation csn) 4.0)])
		  (printf "~nSimulator result: ~n")
		  (display res)(newline)
		  ;(pretty-print res))
		  )
		;(exit)
                )))))))

;(printf "Args: ~s~n" (vector->list (current-command-line-arguments)))
(apply main (vector->list (current-command-line-arguments)))
