#!/bin/sh 
#|
exec regiment i --script "$0" ${1+"$@"};
|#

;; This doesn't verify much, but it does verify that all the demos,
;; parse type check, run and produce *something*.

(print-length 10)
(print-level 3)
(print-graph #t)

(define (go i fn)
  (printf "\nDemo: ~a \n"  fn)
  (printf "======================================================================\n")
  (printf "\nFirst element: ~s\n" (stream-car (wsint fn)))
  )

(for-eachi go 
  '("demo0_audio.ws"
    "demo1_audiofile.ws"
    "demo2_iterate.ws"
    "demo2b_iterateState.ws"
    "demo3_fft.ws"
    "demo4_rewindow.ws"
    "demo5_inline.ws"
;    "demo5b_better.ws"
    "demo6_sync.ws"
    "demo7_syncN.ws"
    "demo8_marmot_noinline.ws"
    "demo9_marmot.ws"
    ))

