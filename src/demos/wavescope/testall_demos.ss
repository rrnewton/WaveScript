#!/bin/sh 
#|
exec regiment i --script "$0" ${1+"$@"};
|#


;; This runs all the demo files and (for some tests) checks their
;; output for correctness.


(print-length 10)
(print-level 3)
(print-graph #t)

(define (go i x)
  (match x 
    [(,fn ,oracle)
     (printf "\n\nDemo: ~a \n"  fn)
     (printf "======================================================================\n")
     (let ([strm (wsint fn)] [first #f] [second #f])
       (set! first (stream-car strm))
       ;(set! first (strm))
       (printf "\nFirst element: ~s\n" first)
       (set! second (stream-car (stream-cdr strm)))
       ;(set! second (strm))
       (printf "Second element: ~s\n" second)
       (ASSERT (oracle first second)))
     ]))

(for-eachi go 
  `(["demo0_audio.ws"             ,(lambda (a b) 
				     (import wavescript_sim_library_NEW)
				     (ASSERT (= 0    (start a)))
				     (ASSERT (= 4095 (end   a)))
				     (ASSERT (= 4096 (start b)))
				     (ASSERT (= 8191 (end   b)))
				     )]
    ["demo1_audiofile.ws"         ,(lambda (a b) 
				     (import wavescript_sim_library_NEW)
				     (ASSERT (= 0  (start a)))
				     (ASSERT (= 9  (end   a)))
				     (ASSERT (= 10 (start b)))
				     (ASSERT (= 19 (end   b)))
				     (and 
;; FIX THE INT16 READING HERE!!
#;				      
				      (equal? a 
					      [make-sigseg 0 9 #(256 512 768 1024 1280 1536 1792 2048 2304 2560) nulltimebase])
				      
					  ))]

    ["demo1b_dataFile.ws"         ,(lambda (a b) 
				     (ASSERT (equal? a #(1 1.0)))
				     (ASSERT (equal? b #(2 2.0))))]


    ["demo2a_iterate.ws"          ,(lambda (a b) 
				     (import wavescript_sim_library_NEW)
				     (ASSERT (= 0    (start a)))
				     (ASSERT (= 39   (end   a)))
				     (ASSERT (= 40   (start b)))
				     (ASSERT (= 79   (end   b)))
				     )]
    ["demo2b_iterateState.ws"     ,(lambda (a b) #t)]

    ["demo3a_tuples.ws"           ,(lambda (a b) #t)]
    ["demo3b_inlining.ws"         ,(lambda (a b) #t)]
    ["demo3c_lists.ws"            ,(lambda (a b) #t)]
    ["demo3d_tuples_of_tuples.ws" ,(lambda (a b) #t)]

    ["demo4_fft.ws"               ,(lambda (a b) 
				     (ASSERT (= 0.0 a))
				     (ASSERT (= 840.0 (round b))) ;839.8869476698192
				     )]

    ["demo5a_rewindow.ws"         ,(lambda (a b) #t)]
    ["demo5b_rewindow_inlined.ws" ,(lambda (a b) #t)]
;    "demo5c_better.ws"

    ["demo6a_unionList.ws"        ,(lambda (a b) #t)]
    ["demo6b_sync.ws"             ,(lambda (a b) #t)]
    ["demo6c_syncN.ws"            ,(lambda (a b) #t)]

    ["demo7a_marmot_noinline.ws"  ,(lambda (a b) #t)]
    ["demo7b_marmot_phase1.ws"    ,(lambda (a b) #t)]

    ["demo11_simple_merge.ws"     ,(lambda (a b) #t)]
    ))
