#! /bin/bash
#|
exec regiment i --script "$0"
|#

(printf "Running script to measure reference counts.\n")(flush-output-port)
(printf "================================================================================\n")

(regiment-quiet #t)

;(define all-files `("demos/wavescope/demo1c_timer.ws" ))
;(define all-files `("demos/wavescope/demo3l_moreunions.ws" ))
(include "./all-files.ss")


(for-each 
    (lambda (file)
      (define path (** (WAVESCRIPTD) "/" file))
      (define (hook jumpout)
	(lambda (prog)
	  (define (local-rc? xp)
	    (and (pair? xp)
		 (memq (car xp) '(incr-local-refcount decr-local-refcount))
		 (caddr xp)))
	  (define (heap-rc? xp)
	    (and (pair? xp)
		 (memq (car xp) '(incr-heap-refcount decr-heap-refcount))
		 (caddr xp)))
	  (define (rc? x) (or (local-rc? x) (heap-rc? x)))
	  (define (in-a-row? hits)
	    (let loop ([ls hits] [last #f])
	      (cond 
	       [(null? ls) #f]
	       [(and (car ls) (eq? (car ls) last)) #t]
	       [else (loop (cdr ls) (car ls))])))
	  (define local-cnt 0)
	  (define heap-cnt 0)
	  (define either-cnt 0)
	  ;; Look at opportunities for refcount canceling.
	  (blaze-path-to 
	       prog
	       (lambda (xp)
		 (match xp
		   [(begin ,hits ...)
		    ;(> (length (filter id hits)) 1) 		    
		    (when (in-a-row? (map local-rc? hits)) (set! local-cnt  (add1 local-cnt)))
		    (when (in-a-row? (map heap-rc?  hits)) (set! heap-cnt   (add1 heap-cnt)))
		    (when (in-a-row? (map rc?       hits)) (set! either-cnt (add1 either-cnt)))
		   
		    #f;(in-a-row? (map rc?       hits))
		    ]
		   [,_ #f])))

	  (printf "Total opportunities for simple refcount elimination: local/heap/either \t~s\t~s\t~s\n" 
		  local-cnt heap-cnt either-cnt)
	  (let ([local (length (deep-all-matches 
				(lambda (sym) (eq-any? sym 'incr-local-refcount 'decr-local-refcount))
				prog))]
		[heap (length (deep-all-matches 
				      (lambda (sym) (eq-any? sym 'incr-heap-refcount 'decr-heap-refcount))
				      prog))])
	    (printf "Total refcounts: ~a ~s ~s ~s ~s\n" (basename path) (+ local heap) local heap 
		    ;(count-nodes prog)
		    (stream-wiring-ast-size prog)
		    )
	    )
	  (jumpout #f)))
      (printf "  Processing: ~s\n" path)      
      (current-directory (dirname path))
      (call/cc (lambda (jumpout)
		 (ws-compiler-hooks 
		  `([insert-refcounts ,(hook jumpout)]))
		 (wscomp path '() 'wsc2)
		 ))
      (printf "============================================================\n")
      )
  all-files)

(exit 0)
