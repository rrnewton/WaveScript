
;; [2004.06.03]
;; This has been separated from the generic version because chez and
;; plt use their own respective hash tables.

;; Like other chez files this is loaded from the src/ directory, not
;; the src/chez/ directory.

;; Lame directory switching
(cd "generic")
(include "simulator_nought.ss")
(cd "..")


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
