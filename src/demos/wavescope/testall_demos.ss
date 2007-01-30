#!/bin/sh 
#|
exec regiment.opt i --script "$0" ${1+"$@"};
|#

;; This doesn't verify much, but it does verify that all the demos,
;; parse type check, run and produce *something*.

(print-length 10)
(print-level 3)
(print-graph #t)

(define (go i fn)
  (printf "\n\nDemo: ~a \n"  fn)
  (printf "======================================================================\n")
  (let ([strm (wsint fn)])
    (printf "\nFirst element: ~s\n" (stream-car strm))
    (printf "Second element: ~s\n" (stream-car (stream-cdr strm))))   
  )

(for-eachi go 
  '("demo0_audio.ws"
    "demo1_audiofile.ws"

    "demo2a_iterate.ws"
    "demo2b_iterateState.ws"

    "demo3a_tuples.ws"
    "demo3b_inlining.ws"
    "demo3c_lists.ws"
    "demo3d_tuples_of_tuples.ws"

    "demo4_fft.ws"

    "demo5a_rewindow.ws"
    "demo5b_rewindow_inlined.ws"
;    "demo5c_better.ws"

    "demo6a_sync.ws"
    "demo6b_syncN.ws"

    "demo7a_marmot_noinline.ws"
    "demo7b_marmot_phase1.ws"

    "demo11_simple_merge.ws"
    ))
