
;; [2004.06.03]
;; This has been separated from the generic version because chez and
;; plt use their own respective hash tables.

;; Like other chez files this is loaded from the src/ directory, not
;; the src/chez/ directory.

(define (make-default-hash-table) (make-hash-table 50))

(define hashtab-get (hash-inquirer eq?))
(define hashtab-set! (hash-associator eq?))

;; Lame directory switching
(cd "generic")
(include "simulator_nought.ss")
(cd "..")

;; <TODO> This buffer stuff should really use a safe fifo..
;; This version returns a stream of answers rather than a list all at once.
(define run-simulation-stream
  (generate-simulator
   (lambda args
     (disp "stream version of run sim")
     (let ([return-buffer '()])
       ;; Here we override the 'soc-return binding from generate-simulator.
       ;; We collect the answers in our stream.

       ;; [2004.06.18] This fluid-let is insufficient.  Whenever we
       ;; run the engine we need to fall back inside the dynamic
       ;; context... How can that be done??  For now I give up and set! it.
       ;; And hope it stays set!ed!
       [set-top-level-value! 'soc-return 
			     (lambda (x) (disp "RETURNING VIA SOC (FIRST ONE)") 
				     (set! return-buffer (cons x return-buffer)))]
       
       (let loop ((eng (apply run-flat-threads-engine args)))
	 (delay 
	   ;; Have to fluid let this every time, because the dynamic
	   ;; extent is chopped up in time thanks to engines.
	   (fluid-let ([soc-return
			(lambda (x) (disp "RETURNING VIA SOC") 
				(set! return-buffer (cons x return-buffer)))])
	     (eng 1000
		       (lambda (rem val) 
			 (disp "engine success" rem val)
			 return-buffer
;			 (if (or (pair? val) (null? val))  val
;			     (begin (printf "Warning: value returned by run-flat-threads-engine is: ~s~n" val)
;				    (list val)))
			 )
		       (lambda (nexteng)
			 (let ((temp return-buffer))
			   (set! return-buffer '()) ;;<TODO> Use semaphore.
			   (dotted-append temp (loop nexteng))))))))))
   generic-text-simulator-core ))
