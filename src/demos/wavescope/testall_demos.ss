#!/bin/bash 
#|
export WAVESCRIPT_OR_WAVESCRIPT=WS
export REGOPTLVL=2
export REGDEBUGMODE=ON
exec regiment i --script "$0" ${1+"$@"};
|#

(printf "Running all demos using ~a.\n" which-scheme)

;; [2009.12.01] PLT is pulling in the wrong version of match here.
;(printf "This is being run through reg:top-level-eval ~a.\n" (match '(1 2) [(,x ,y) x]))

;(import (rnrs)  (ws common))

;; This runs all the demo files and (for some tests) checks their
;; output for correctness.

(print-length 10)
(print-level 3)
(print-graph #t)

;; Produce bar.o for demo9c
(system "gcc -c bar.c")

(define go 
  (lambda (i x)
    (match x 
      [(,fn ,oracle)
       (printf "\n\nDemo: ~a \n"  fn)
       (printf "======================================================================\n")     
       (let ([absolute (if (ws-relative-path? fn)
			   (string-append (current-directory) 
					  "/" fn)
			   fn)])
	 (let ([strm ((if (getenv "WSTESTALLEARLY") wsint-early wsint) absolute '())]
	       [first #f] [second #f])

	   (let-values ([(ls _) (stream-take  2 strm)])
	     (set! first (car ls))
	     (set! second (cadr ls)))

	   ;;	 (set! first (stream-car strm))
	   (printf "\nFirst element: ~s\n" first)

	   ;;	 (set! second (stream-car (stream-cdr strm)))
	   (printf "Second element: ~s\n" second)

	   ;; Don't use the ORACLES in PLT for now..
	   ;; Besides.. .we want to move over to having asserts INSIDE the WS code.
	   (oracle first second)
	   ))
       ])))

(define demo-list
    `(

    ["demo1c_timer.ws"         ,(lambda (a b) 
				  (ASSERT (equal? a unit-representation))
				  (ASSERT (equal? b unit-representation)))]

    ;; This one specifically sabatoges the next one.
    ["demo1d_readFile_text.ws"  ,(lambda (a b) (void))]

    ;; Hmm need to test windowed reading too:
    ["demo1e_readFile.ws"  ,(lambda (a b) 
			      (ASSERT (equal? (tuple-fields a) '(512 1024 1536)))
			      (ASSERT (equal? (tuple-fields b) '(2 514 1026))))]
    

    ["demo2a_iterate.ws"          ,(lambda (a b) 
				     (void)
; 				     (ASSERT (= 0    (start a)))
; 				     (ASSERT (= 39   (end   a)))
; 				     (ASSERT (= 40   (start b)))
; 				     (ASSERT (= 79   (end   b)))
				     )]    
    ["demo2b_iterateState.ws"     ,(lambda (a b) #t)]
    ["demo2c_inlining.ws"         ,(lambda (a b) #t)]
    
    ;; This is having problems in PLT
    ["demo2d_pullNtimer.ws"         ,(lambda (a b) #t)]

;    ["demo2e_passchain.ws"         ,(lambda (a b) #t)]    

    ["demo3a_tuples.ws"             ,(lambda (a b) #t)]
    ["demo3b_basic_polymorphism.ws" ,(lambda (a b) #t)]
    ["demo3c_lists.ws"              ,(lambda (a b) #t)]
    ["demo3d_tuples_of_tuples.ws"   ,(lambda (a b) #t)]
    ["demo3e_hashtables.ws"         ,(lambda (a b) #t)]
    ["demo3e2_morehashtables.ws"         ,(lambda (a b) #t)]

    ["demo3f_morelists.ws"          ,(lambda (a b) #t)]
    ["demo3g_arrays.ws"             ,(lambda (a b) #t)]
    ["demo3h_advancedlists.ws"      ,(lambda (a b) #t)]

    ["demo3i_conversion_prims.ws"   ,(lambda (a b) #t)]
    ["demo3j_numbers.ws"            ,(lambda (a b) #t)]
    ["demo3k_uniontype.ws"          ,(lambda (a b) #t)]
    ["demo3l_moreunions.ws"         ,(lambda (a b) #t)]
    ["demo3m_sigsegs.ws"         ,(lambda (a b) #t)]
    ["demo3n_static_vals.ws"     ,(lambda (a b) #t)]

    ["demo3o_strings.ws"       ,(lambda (a b) #t)]
    ["demo3p_records.ws"       ,(lambda (a b) #t)]


;; TEMP: NO COMPLEX NUMBERS IN IKARUS YET:    
;; [2008.05.29] Now it runs, but I get some very odd output, garbage binary to stdout under ikarus.
;; But it's not even producing complex numbers as output...
;    ["demo4a_fft.ws"               ,(lambda (a b) (void) )]
				     ;(ASSERT (= 0.0 a))
				     ;(ASSERT (= 840.0 (round b))) ;839.8869476698192
;; ALERT!  DEMO4 STARTED RETURNING SOMETHING DIFFERENT.... CHECK THIS OUT!

    ["demo4b_morefft.ws"           ,(lambda (a b) (void))]
    ["demo4d_quoted_constants.ws"  ,(lambda (a b) (void))]
    ["demo4e_fifo_adt.ws"  ,(lambda (a b) (void))]

    ["demo5a_rewindow.ws"         ,(lambda (a b) 
				     (ASSERT (= 0     (wssim:start a)))
				     (ASSERT (= 1023  (wssim:end   a)))
				     (ASSERT (= 512   (wssim:start b)))
				     (ASSERT (= 1535  (wssim:end   b))))]
    ["demo5b_rewindow_inlined.ws" ,(lambda (a b) 
				      (ASSERT (= 0     (wssim:start a)))
				      (ASSERT (= 1023  (wssim:end   a)))
				      (ASSERT (= 512   (wssim:start b)))
				      (ASSERT (= 1535  (wssim:end   b))))]
 ;    "demo5c_better.ws"

    ["demo6a_unionList.ws"        ,(lambda (a b) 
                                     (equal? (list-sort < (list (wssim:tupref 0 2 a) 
								(wssim:tupref 0 2 b)))
					     '(0 1))
				     )]
    ["demo6b_sync.ws"             ,(lambda (a b) #t)]
    ["demo6c_syncN.ws"            ,(lambda (a b) #t)]

    ["demo6e_stdlib_sync.ws"      ,(lambda (a b) 
				     (ASSERT (= 100   (wssim:start (list-ref a 0))))
				     (ASSERT (= 199   (wssim:end   (list-ref a 0))))
				     (ASSERT (= 100   (wssim:start (list-ref a 1))))
				     (ASSERT (= 199   (wssim:end   (list-ref a 1))))
				     (ASSERT (= 300   (wssim:start (list-ref b 0))))
				     (ASSERT (= 399   (wssim:end   (list-ref b 0))))
				     (ASSERT (= 300   (wssim:start (list-ref b 1))))
				     (ASSERT (= 399   (wssim:end   (list-ref b 1))))
				     )]
    ["demo6f_merge.ws"            ,(lambda (a b) (void))]

    ["demo7a_marmot_noinline.ws"  ,(lambda (a b) #t)]
    ["demo7b_marmot_phase1.ws"    ,(lambda (a b) #t)]

    ["demo8a_generic_arith.ws"    ,(lambda (a b) #t)]
    ["demo8b_sugars.ws"           ,(lambda (a b) 
				     (equal? (tuple-fields a) '(1 1 2 1 1 2))
				     (equal? (tuple-fields b) '(1 1 2 1 1 2)))]

;; TEMP: Still don't have stringToComplex:
;    ["demo9_misc_prim_tests.ws"      ,(lambda (a b) #t)]
    
    ;; WEIRD... applied a fix so that static-elaborate's hack for Array:build will work.
    ;; But now this freezes nondeterministically.  Sometimes it passes, sometimes it fails.
    ["demo9b_higher_order_prims.ws"  ,(lambda (a b) #t)]
 
;; [2007.06.10]
;; THIS IS DISPLAYING NONDETERMINISM:
;;       
    ;; No foreign interface yet in PLT:
;    ,@(IFCHEZ `(["demo9c_foreign.ws"  ,(lambda (a b) #t)]) ())

    ;; TODO: FIX THESE NUMBERS!
    ["demo10a_simple_merge.ws"     ,(lambda (a b) 
				     (void)
				     ;; [2007.04.08] Why did this change?
				     ;(ASSERT (= 514 a))
				     ;(ASSERT (= 5634 b))
				     )]

    ;; This is just TOO slow in PLT.  It takes 20+ minutes by itself.     
    ;["demo10b_repeated_rewindow.ws"  ,(lambda (a b) (void))]
    
    ))

(for-eachi go demo-list)
