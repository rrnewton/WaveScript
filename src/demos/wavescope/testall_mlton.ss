#!/usr/bin/mzscheme -fmv-

(require (lib "pretty.ss")
         "../../../benchmarks/measure.ss" ; FIXME: should be location-independent
)


;; list of demo source files
;;
(define demos
  '("demo1c_timer.ws"
    "demo1d_readFile_text.ws"
    "demo1e_readFile.ws"
    
    "demo2a_iterate.ws"
    "demo2b_iterateState.ws"
    "demo2c_inlining.ws"
    ;"demo2d_pullNtimer.ws"
    ;"demo2e_passchain.ws"

    "demo3a_tuples.ws"
    "demo3c_lists.ws"
    "demo3d_tuples_of_tuples.ws"
    "demo3f_morelists.ws"
    "demo3g_arrays.ws"
    "demo3h_advancedlists.ws"
    "demo3j_numbers.ws"
    "demo3k_uniontype.ws"
    "demo3l_moreunions.ws"
    "demo3m_sigsegs.ws"

    "demo4a_fft.ws"
    ;"demo4c_quoted_constants.ws"
    
    "demo5a_rewindow.ws"
    "demo5b_rewindow_inlined.ws"

    "demo6a_unionList.ws"
    "demo6b_sync.ws"
    "demo6c_syncN.ws"
    "demo6e_stdlib_sync.ws"
    ;"demo6f_merge.ws"

    "demo7a_marmot_noinline.ws"
    "demo7b_marmot_phase1.ws"

    "demo8a_generic_arith.ws"
    "demo8b_sugars.ws"
    ;"demo8c_moresugar.ws"

    ;"demo9_misc_prim_tests.ws"
    ;"demo9b_higher_order_prims.ws"
    ;"demo9c_foreign.ws" ; FIXME: need to build in a way to run "gcc -c bar.c" first

    "demo10a_simple_merge.ws"
    ;"demo10b_repeated_rewindow.ws"
    ))


;; measure all the demos
;;
(pretty-print
 (map
   (lambda (demo)
     (measure-wavescript-program `([ws-filename . ,demo]
                                   [num-tuples . 100]
                                   [backend . mlton])))
   demos)
)
