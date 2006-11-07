#! /bin/sh
#| 
exec regiment i --script "$0" ${1+"$@"};
|#

;;;; .title Run one of the test programs to generate data.

;; Uncomment this only to use Check Syntax in PLT on this file, not to run it:
#;
(begin 
  (require "../../src/plt/iu-match.ss")
  (require "../../src/generic/constants.ss")
;  (require "../../src/plt/chez_compat.ss")
  (require "../../src/plt/helpers.ss")
  (require "../../src/generic/source_loader.ss")  
  (require "../../src/generic/simulator_alpha.ss")
  (require "../../src/generic/simulator_alpha_datatypes.ss")
;  (require "../../src/generic/firelightning_sim.ss")
  )

;;;; .title Analyze Dead-simple fire monitoring vs. groundtruth.

;;;; TODO: POSSIBLE OTHER CONTROLS: <gradient-mode> <retry-delay> <max-retries>

;;;;<br>  Usage: ./<script-name> <mode> <options>
;;;;<br>  Options: 
;;;;<br>     -l <logfile>
;;;;<br>     -o <resultsfile>
;;;;<br>     -param <n>   Set the value of 'varied-param'.  In this case it changes the local temp threshold.
;;;;<br>     -noise <n>   Set the level of heat-noise-magnitude, this is stddev in heat readings

(define curlogfile (format "./deadsimple.log.gz"))
;(define resultsfile 'uninit)
(define inputfile 'uninit)
(define worldseed #f)

(define (main)
     ;; Run the simulation.
     ;; TODO: If we can, run it on a separate thread.  It's only executed for its
     ;; writes to the logfile.     
     (let ([devnull (open-output-file "/dev/null" 'append)]
	   [real-console (console-output-port)])
       (printf "Running Simulation in Regiment... (progress will show as vtimes)\n")
       (parameterize ([current-output-port devnull]
					;[console-output-port devnull]
		      )
	 (if worldseed 
	     (begin (printf "Animating prerolled world.\n")
		    (animate-world! worldseed)))

	 (progress-dots 
	  (lambda ()
	    (apply load-regiment 
		   ;; If we have a stored world, use it:
		   (append (if worldseed (list worldseed))
			   (list ;(++ (REGIMENTD) "/demos/firelightning/deadsimple_alarm.rs")
			         ;"prog1.ss"
			         inputfile
				 `[simulation-logger ,curlogfile]
				 ;;'[sim-timeout 500000]
				 'verbose
				 ))))
	  75000000 ;536870911
	  (lambda () 
	    (if (simalpha-current-simworld)
		(fprintf real-console " ~a" (simworld-vtime (simalpha-current-simworld)))
		(display #\. real-console))
	    (flush-output-port real-console)))
 
	 (fprintf real-console "\nSimulation finished.\n")
	 (close-output-port devnull)
	 (collect 4))))


;; Body of the script:

(define (thescript script-args)
(let ([run-modes '()])
(let loop ((x (map string->symbol script-args)))
  (match x
    [() (main)]

    [(-param ,n ,rest ...)
     (printf "Setting varied-param to ~a\n" n)
     (varied-param (string->number (symbol->string n))) (loop rest)]
    [(-noise ,n ,rest ...) 
     (printf "Setting temperature-noise to stddev ~a\n" n)
     (set! heat-noise-magnitude (string->number (symbol->string n))) (loop rest)]

    [(-l ,file ,rest ...) (set! curlogfile (symbol->string file)) (loop rest)]
    [(-f ,file ,rest ...) (set! inputfile (symbol->string file)) (loop rest)]

;    [(-o ,file ,rest ...) (set! resultsfile (symbol->string file)) (loop rest)]    
    [(-w ,file ,rest ...) (set! worldseed (car (file->slist (symbol->string file)))) (loop rest)]

    ;; Actions:
    [(,action ,rest ...) (guard (memq action '(run analyze)))
     (set! run-modes (snoc action run-modes)) (loop rest)]
    [,other (error 'run_querysim "bad arguments: ~s\n" other)]))))


(printf "Cmd args: ~s\n" (command-line-arguments))
(thescript (command-line-arguments))
