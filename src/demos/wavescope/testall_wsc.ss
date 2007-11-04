#!/usr/bin/mzscheme -fmv-

(require (lib "pretty.ss")
         "../../../benchmarks/measure.ss" ; FIXME: should be location-independent
)


;; list of demo source files
;;
(define demos
  '(((ws-filename . "demo1c_timer.ws")          (num-tuples . 10))
    ((ws-filename . "demo1d_readFile_text.ws"))
    ((ws-filename . "demo1e_readFile.ws"))
    
    ((ws-filename . "demo2a_iterate.ws"))
    ((ws-filename . "demo2b_iterateState.ws"))
    ((ws-filename . "demo2c_inlining.ws"))
    ;"demo2d_pullNtimer.ws"
    ;"demo2e_passchain.ws"

    ((ws-filename . "demo3a_tuples.ws"))
    ((ws-filename . "demo3c_lists.ws"))
    ((ws-filename . "demo3d_tuples_of_tuples.ws"))
    ((ws-filename . "demo3f_morelists.ws"))
    ((ws-filename . "demo3g_arrays.ws"))
    ((ws-filename . "demo3h_advancedlists.ws"))
    ((ws-filename . "demo3j_numbers.ws"))
    ((ws-filename . "demo3k_uniontype.ws"))
    ((ws-filename . "demo3l_moreunions.ws"))
    ((ws-filename . "demo3m_sigsegs.ws"))

    ((ws-filename . "demo4a_fft.ws"))
    ;"demo4c_quoted_constants.ws"
    
    ((ws-filename . "demo5a_rewindow.ws"))
    ((ws-filename . "demo5b_rewindow_inlined.ws"))

    ((ws-filename . "demo6a_unionList.ws"))
    ((ws-filename . "demo6b_sync.ws"))
    ((ws-filename . "demo6c_syncN.ws"))
    ((ws-filename . "demo6e_stdlib_sync.ws"))
    ;"demo6f_merge.ws"

    ((ws-filename . "demo7a_marmot_noinline.ws"))
    ((ws-filename . "demo7b_marmot_phase1.ws"))

    ((ws-filename . "demo8a_generic_arith.ws"))
    ((ws-filename . "demo8b_sugars.ws"))
    ;"demo8c_moresugar.ws"

    ;"demo9_misc_prim_tests.ws"
    ;"demo9b_higher_order_prims.ws"
    ;"demo9c_foreign.ws" ; FIXME: need to build in a way to run "gcc -c bar.c" first

    ((ws-filename . "demo10a_simple_merge.ws"))
    ;"demo10b_repeated_rewindow.ws"
    ))


;; measure all the demos
;;
(pretty-print
 (map
   (lambda (demo) (measure-wavescript-program `((backend . c++) ,@demo)))
   demos)
)
