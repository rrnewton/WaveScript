
;; [2004.06.03]
;; This has been separated from the generic version because chez and
;; plt use their own respective hash tables.

;; Like other chez files this is loaded from the src/ directory, not
;; the src/chez/ directory.

;; We are loaded from the root directory, not the chez subdirectory.
(include "../generic/sim/simulator_nought.examples.ss")
(include "../generic/sim/simulator_nought.ss")
	
;; This version chops up the simulation computation into little bits.
;; The return value is a promise that keeps doing a little computation
;; and returning a new promise.  Eventually it will return:
;;   'All_Threads_Returned  OR  'Threads_Timed_Out
(define run-simulation-stream 
  (generate-simulator

   ;; THREADRUNNER:
   (lambda args
     (disp "stream version of run sim")
     (let ();[return-buffer '()])
       ;; Here we override the 'soc-return binding from generate-simulator.
       ;; We collect the answers in our stream.

       ;; [2004.06.18] This fluid-let is insufficient.  Whenever we
       ;; run the engine we need to fall back inside the dynamic
       ;; context... How can that be done??  For now I give up and set! it.
       ;; And hope it stays set!ed!
       ;[set-top-level-value! 'soc-return 
       ;(lambda (x) (disp "RETURNING VIA SOC (FIRST ONE)") 
       ;(set! return-buffer (cons x return-buffer)))]
       ;; -- I think this is really outdated -- [2004.10.24]
       
       (let loop ((eng (apply run-flat-threads-engine args)))
	 ;; This returns a *stream* by means of returning promises:
	 (delay 
	   ;; Have to fluid let this every time, because the dynamic
	   ;; extent is chopped up in time thanks to engines.
	   (fluid-let (;[soc-return
		       ;(lambda (x) (disp "RETURNING VIA SOC") 
		       ;	(set! return-buffer (cons x return-buffer)))])
		       ) ;; ^^ I believe this is really outdated, now
			 ;; we pass soc-ret as an arg to the simulated
			 ;; object... [2004.10.24]a
	     (eng 1000		  
		       (lambda (rem val) 
			 (disp "Simulation-Stream-Engine success" rem val)
			 val)
		       (lambda (nexteng)
			 ;(let ((temp return-buffer))
			 ;(set! return-buffer '()) ;;<TODO> Use semaphore.
			 ;(dotted-append temp (loop nexteng))
			 (loop nexteng)
			 ;)
			 )))))))
   ;; SIMCORE:
   generic-text-simulator-core ))


;; [2004.06.22]  Just a little function to dump the topology to a file.
(define (save-topology name)
  (let ((f (open-output-file name 'replace)))
    (pretty-print graph f)
    (close-output-port f)))

(define (load-topology name)  
  (let ((f (open-input-file name)))
    (set! graph (read f))
    (set! object-graph (make-object-graph graph))
    (set! all-objs (map car object-graph))
    (close-input-port f)))



;----------------
;; [2005.03.27] - Just moved this because I'm switching over to simulator_alpha.

(define text-repl  (repl-builder void void run-compiler run-simulation-stream))
(define precomp-repl (repl-builder 
		      void  ;; Startup
		      void  ;; Cleanse		      
		      (lambda (x) ;; Compiler
			(parameterize ([pass-list '(cleanup-token-machine)])
			  (match x
				 [(precomp ,exp) `(unknown-lang (quote ,exp))]
				 [,other (run-compiler other)])))		      
		      run-simulation-stream)) ;; Runner

(define pretoken-repl 
  (repl-builder
   void void
   compile-almost-to-tokens
   (lambda (x . timeout)
     (printf "Running compiled program: ~n")
     (parameterize ([print-length 3] [print-level 2] [pretty-initial-indent 10])
		   (pretty-print x))
     (parameterize ([print-length 10] [print-level 2])
       (let ([result (eval x)])
	 (printf "~n Returned: ~s~n~n" result)
	 (if (list? result)
	     (printf "Length: ~s~n" (length result)))
	 result))
     )))

;; ???????
(define precomp-graphical-repl (repl-builder 
		      void  ;; Startup
		      void  ;; Cleanse
		      (lambda (x) x) ;; Compiler
		      run-simulation-stream))

