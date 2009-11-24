#!r6rs

;;;; NOTE: This file is a bit of a mess.  This is the file in which I
;;;; put messy shorthands and entrypoints that I'm using (usually
;;;; temporarily.) -Ryan 

(library (ws shortcuts)
  (export t   
	  )
  (import (except (rnrs (6)) error) (main_r6rs) (main))

; =============================================================
;;; Shorthands.  
;;; These are just for my convenient usage in interactive invocation of the compiler.

 (define-id-syntax t (time (test-units)))

#;
(IFCHEZ
 (define-id-syntax t  
   (let ([start 
	  (begin (collect (collect-maximum-generation))
		 (statistics))]
	 [startobs (oblist)])
     (and 
      (time (test-units))
      (collect (collect-maximum-generation))
      (let ([end (statistics)]
	    [endobs (oblist)])
	(let ([before (- (sstats-bytes start) (sstats-gc-bytes start))]
	      [after (- (sstats-bytes end) (sstats-gc-bytes end))])
	  (printf "\nAfter a thorough collection:\n")
	  (printf "   ~:d additional allocated bytes are left over (~:d before ~:d after).\n"
		  (- after before) before after)
	  (let ([len1 (length startobs)]
		[len2 (length endobs)]
		[diff (difference endobs startobs)])
	    (printf "   ~s syms in oblist before, ~s syms after, diff ~s\n" len1 len2 (- len2 len1))
	    (parameterize ([print-length 20])
	      (printf "Difference: ~s\n" diff))
	    (printf "  Difference with top-level bindings: ~s\n\n" (filter top-level-bound? diff))
	    ))))))
 (define-id-syntax t (time (test-units)))
)

#|
(define-id-syntax mem  ;; shorthand
  (begin (collect (collect-maximum-generation))
	 (let ([stats (statistics)])
	   (printf "Approx dynamic mem usage: ~:d\n" (- (sstats-bytes stats) (sstats-gc-bytes stats))))))

(define-id-syntax cur  ;; shorthand, current test
  (begin (cd "~/wavescript/src/demos/wavescope/")
	 (browse-stream (wsint "demo6_sync.ws"))))
|#



; =============================================================


) ;; End module
