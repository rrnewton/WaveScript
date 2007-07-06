
;; Ryan Newton
;; Started 2004.03.26
;; Here is the main file for my compiler.
;=======================================================================

#cs ;; Case Sensitivity

(module main_plt mzscheme 
;; [2004.06.16] This should have been done already, but just for good measure...
(define current_interpreter 'mzscheme)
(define simulator-batch-mode #f)

#;
(fprintf stderr "Regiment: Loading ~a compiler in chezscheme~a...\n"
	 (let ([ws #f]  [reg #f])
	   (IFWAVESCOPE (set! ws #t) (set! reg #t))
	   (cond 
	    [(and ws reg) "ws+reg"]
	    [ws   "ws"]
	    [reg "reg"]))
	 (if (top-level-bound? 'regiment-origin)
	     (format " (from ~a)" regiment-origin)    
	     "(LOADED VIA UNKNOWN METHOD!?)"
	     ))

 
(require (lib "include.ss")
         (all-except "generic/util/helpers.ss" id rec)
         (all-except "generic/compiler_components/regiment_helpers.ss")         
	 "plt/hashtab.ss"
	 "plt/chez_compat.ss"
	 "generic/util/hash.ss"
         (all-except "generic/grammars/grammar_checker.ss" these-tests test-this))

(require-for-syntax "plt/identifier-syntax.ss")

(require "generic/constants.ss"
         "plt/iu-match.ss"
          ;; [2004.12.06] I think I had this working:
         "plt/critical_section.ss")

;; Set parameter:
(REGIMENTD (cond             
             [(getenv "REGIMENTD") (getenv "REGIMENTD")]
             [(directory-exists? "~/wavescript") "~/wavescript/"]
             [(directory-exists? "~/WaveScope/src/wavescript") "~/WaveScope/src/wavescript/"]
             [(directory-exists? "~/regiment_alpha") "~/regiment_alpha/"]
             [else 
              (string-append (path->string (current-directory)) "../")              
               ]))

;; Set Misc PLT parameters:
;(print-graph #t )
;(print-gensym #f)
;(print-level 20))
;(print-length 80)
;(print-vector-length #f)
;(pretty-maximum-lines 700)
(print-struct #f)


(require 


;; Include these at top-level for the system tests:
(all-except "generic/util/streams.ss" these-tests test-this)
(all-except "generic/compiler_components/prim_defs.ss" these-tests test-this)


(all-except "generic/compiler_components/hm_type_inference.ss" these-tests test-this)

;(all-except "plt/desugar-pattern-matching.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/desugar-pattern-matching.ss" these-tests test-this)

(all-except "generic/passes/normalize_source/verify-regiment.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/resolve-varrefs.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/ws-label-mutable.ss")
(all-except "generic/passes/normalize_source/typecheck.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/desugar-misc.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/eta-primitives.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/rename-vars.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/remove-unquoted-constant.ss" these-tests test-this)

(all-except "generic/passes/static_elaborate/static-elaborate.ss" these-tests test-this)
(all-except "generic/passes/static_elaborate/verify-elaborated.ss" these-tests test-this)
(all-except "generic/passes/static_elaborate/degeneralize-arithmetic.ss" these-tests test-this)

(all-except "generic/passes/normalize_query/reduce-primitives.ss" these-tests test-this)

(all-except "generic/passes/wavescope_bkend/merge-iterates.ss" these-tests test-this)
;(all-except "generic/passes/wavescope_bkend/purify-iterate.ss" these-tests test-this)
(all-except "generic/passes/wavescope_bkend/nominalize-types.ss" these-tests test-this)
(all-except "generic/passes/wavescope_bkend/convert-sums-to-tuples.ss")
(all-except "generic/passes/wavescope_bkend/reify-certain-types.ss")
(all-except "generic/passes/wavescope_bkend/type-annotate-misc.ss" these-tests test-this)

"generic/passes/wavescope_bkend/convert-sums-to-tuples.ss"
(all-except "generic/passes/wavescope_bkend/flatten-iterate-spine.ss" these-tests test-this)

(all-except "generic/passes/wavescope_bkend/anihilate-higher-order.ss" these-tests test-this)

;; These are miscellaneous small passes used by wavescript:
(all-except "generic/passes/small-ws-passes.ss" these-tests test-this)
(all-except "generic/passes/wavescope_bkend/explicit-stream-wiring.ss" these-tests test-this)
(all-except "generic/passes/wavescope_bkend/emit-c.ss" these-tests test-this)
(all-except "generic/passes/ocaml_bkend/emit-caml.ss" these-tests test-this)
(all-except "generic/passes/mlton_bkend/emit-mlton.ss" these-tests test-this)

(all-except "generic/passes/normalize_query/remove-complex-constant.ss" these-tests test-this)
; pass07_verify-stage2.ss
(all-except "generic/passes/normalize_query/uncover-free.ss" these-tests test-this)
(all-except "generic/passes/normalize_query/lift-letrec.ss" these-tests test-this)
(all-except "generic/passes/normalize_query/lift-letrec-body.ss" these-tests test-this)
(all-except "generic/passes/normalize_query/remove-complex-opera.ss" these-tests test-this)
(all-except "generic/passes/normalize_query/ws-remove-letrec.ss") 
(all-except "generic/passes/normalize_query/remove-lazy-letrec.ss" these-tests test-this) 
(all-except "generic/passes/normalize_query/verify-core.ss" these-tests test-this)

(all-except "generic/passes/normalize_query/ws-remove-letrec.ss") 
(all-except "generic/passes/normalize_query/ws-remove-complex-opera.ss") 
(all-except "generic/passes/normalize_query/ws-lift-let.ss") 

) 

(IFWAVESCOPE (begin) (require
(all-except "generic/passes/analyze_query/classify-names.ss" these-tests test-this)
(all-except "generic/passes/analyze_query/add-heartbeats.ss" these-tests test-this)
(all-except "generic/passes/analyze_query/add-control-flow.ss" these-tests test-this)
(all-except "generic/passes/analyze_query/add-places.ss" these-tests test-this)
(all-except "generic/passes/analyze_query/analyze-places.ss" these-tests test-this)
(all-except "generic/passes/analyze_query/add-data-flow.ss" these-tests test-this)
(all-except "generic/passes/analyze_query/resolve-fold-trees.ss" these-tests test-this)

(all-except "generic/passes/deglobalize/deglobalize.ss" these-tests test-this)
(all-except "generic/passes/deglobalize/deglobalize2.ss" these-tests test-this)

(all-except "generic/passes/tokmac_bkend/cleanup-token-machine.ss" these-tests test-this)
(all-except "generic/passes/tokmac_bkend/desugar-macros.ss" these-tests test-this)
(all-except "generic/passes/tokmac_bkend/find-emittoks.ss" these-tests test-this)
(all-except "generic/passes/tokmac_bkend/desugar-gradients.ss" these-tests test-this)
(all-except "generic/passes/tokmac_bkend/desugar-let-stored.ss" these-tests test-this)
(all-except "generic/passes/tokmac_bkend/rename-stored.ss" these-tests test-this)

;;;(all-except "plt/pass09_separate-graph" these-tests test-this)
;;;(all-except "plt/pass18_add-routing.ss" these-tests test-this
;;(all-except "plt/pass24_analyze-calls.ss" these-tests test-this)
;;(all-except "plt/pass25_inline.ss" these-tests test-this)
;;(all-except "plt/pass26_prune-returns.ss" these-tests test-this)

(all-except "generic/passes/tokmac_bkend/cps-tokmac.ss" these-tests test-this)
(all-except "generic/passes/tokmac_bkend/sever-cont-state.ss" these-tests test-this)
(all-except "generic/passes/tokmac_bkend/closure-convert.ss" these-tests test-this)
            "generic/passes/tokmac_bkend/inline-tokens.ss"

(all-except "generic/passes/nesc_bkend/flatten-tokmac.ss" these-tests test-this)
(all-except "generic/passes/nesc_bkend/emit-nesc.ss" these-tests test-this)         
))



(IF_GRAPHICS
 (require
;;;; (all-except "plt/simulator_nought.ss" these-tests test-this)
;; (all-except "plt/simulator_nought_graphics.ss" these-tests test-this wrap-def-simulate)
  (all-except "plt/graphics_stub.ss" these-test test-this)))

;; Import these for the benefit of the unit tests below:
(IFWAVESCOPE (begin)  
  (require "generic/sim/simulator_alpha_datatypes.ss"))

;(require (all-except "generic/sim/wavescript_sim_library_push.ss" these-tests test-this))
(require (all-except "generic/langs/lang_wavescript.ss" these-tests test-this))
(require (all-except "generic/compiler_components/source_loader.ss" these-tests test-this))

(IFWAVESCOPE (begin) (begin
  (require (all-except "generic/compiler_components/logfiles.ss" these-tests test-this))
  (require "generic/sim/alpha_lib.ss")
  (require (all-except "generic/sim/simulator_alpha.ss" these-tests test-this))
))

(include "generic/testing/tests.ss")

;============================================================
;; INLINE THE MAIN COMPILER CODE

;; Bring these into top-level for some of the mini-passes defined in main.ss.
(require (all-except "generic/passes/pass-mechanism.ss" these-tests test-this))
(require (all-except "generic/compiler_components/reg_core_generic_traverse.ss" these-tests test-this) )

(require "generic/compiler_components/c_generator.ss")
(include "main.ss")

;============================================================

(define error-handler error-display-handler)

;; Bring these into the top-level for our REPL convenience.
(include "generic/shortcuts.ss")
(require "generic/util/reg_macros.ss")

  
;; [2007.01.23] For now we exit on error unconditionally.
;; [2007.05.17] Making this optional again.  The command line interface should turn it
(when (and (top-level-bound? 'REGIMENT-BATCH-MODE)
	   (top-level-value 'REGIMENT-BATCH-MODE))
  (uncaught-exception-handler
   (lambda (exn)
     (printf "ERROR:\n   ~a\n\nException: ~s\n" (exn-message exn) exn)
     (exit 1))))

;; This could get verbose... ideally we'd like to export all of it:
;; THIS IS REALLY REDUNDANT... I WISH THERE WERE SOME WAY AROUND THIS:
(provide (all-defined) 
	 (all-from "plt/chez_compat.ss")
	 (all-from "generic/constants.ss")
	 (all-from "plt/iu-match.ss")
	 (all-from "generic/util/helpers.ss")
	 (all-from "generic/compiler_components/source_loader.ss")
	 (all-from  "generic/util/streams.ss")
	 (all-from "generic/passes/normalize_source/typecheck.ss")
	 (all-from "generic/grammars/grammar_checker.ss")
	 (all-from "generic/compiler_components/regiment_helpers.ss")
	 (all-from "generic/compiler_components/hm_type_inference.ss")
         (all-from "generic/passes/pass-mechanism.ss")
	 (all-from "generic/compiler_components/reg_core_generic_traverse.ss")
	 (all-from "plt/hashtab.ss")	 	 
	 (all-from "generic/util/hash.ss")
	 (all-from "generic/langs/lang_wavescript.ss")

	 (all-from "generic/compiler_components/prim_defs.ss" )
	 (all-from "generic/compiler_components/hm_type_inference.ss" )
	 (all-from "generic/passes/normalize_source/desugar-pattern-matching.ss" )
	 (all-from "generic/passes/normalize_source/verify-regiment.ss" )
	 (all-from "generic/passes/normalize_source/resolve-varrefs.ss" )
	 (all-from "generic/passes/normalize_source/ws-label-mutable.ss")
	 (all-from "generic/passes/normalize_source/typecheck.ss" )
	 (all-from "generic/passes/normalize_source/desugar-misc.ss" )
	 (all-from "generic/passes/normalize_source/eta-primitives.ss" )
	 (all-from "generic/passes/normalize_source/rename-vars.ss" )
	 (all-from "generic/passes/normalize_source/remove-unquoted-constant.ss" )
	 (all-from "generic/passes/static_elaborate/static-elaborate.ss" )
	 (all-from "generic/passes/static_elaborate/verify-elaborated.ss" )
	 (all-from "generic/passes/static_elaborate/degeneralize-arithmetic.ss" )
	 (all-from "generic/passes/normalize_query/reduce-primitives.ss" )
	 (all-from "generic/passes/wavescope_bkend/merge-iterates.ss" )
	 (all-from "generic/passes/wavescope_bkend/nominalize-types.ss" )
	 (all-from "generic/passes/wavescope_bkend/type-annotate-misc.ss" )
	 (all-from "generic/passes/wavescope_bkend/flatten-iterate-spine.ss" )
	 (all-from "generic/passes/wavescope_bkend/anihilate-higher-order.ss" )
	 (all-from "generic/passes/small-ws-passes.ss" )
	 (all-from "generic/passes/wavescope_bkend/explicit-stream-wiring.ss" )
	 (all-from "generic/passes/wavescope_bkend/emit-c.ss" )
	 (all-from "generic/passes/normalize_query/remove-complex-constant.ss" )
	 (all-from "generic/passes/normalize_query/uncover-free.ss" )
	 (all-from "generic/passes/normalize_query/lift-letrec.ss" )
	 (all-from "generic/passes/normalize_query/lift-letrec-body.ss" )
	 (all-from "generic/passes/normalize_query/remove-complex-opera.ss" )
;	 (all-from "generic/passes/normalize_query/ws-remove-letrec.ss") 
	 (all-from "generic/passes/normalize_query/remove-lazy-letrec.ss" ) 
	 (all-from "generic/passes/normalize_query/verify-core.ss" )
	 (all-from "generic/passes/normalize_query/ws-lift-let.ss") 


;	 (all-from )
;	 (all-from )
;	 (all-from )
;	 (all-from )
	 )

(IFWAVESCOPE 
  (void)
  (provide 
   (all-from "generic/compiler_components/logfiles.ss")
   (all-from "generic/sim/alpha_lib.ss")
   (all-from "generic/sim/simulator_alpha.ss")

   (all-from "generic/passes/tokmac_bkend/cleanup-token-machine.ss" )
   (all-from "generic/passes/tokmac_bkend/desugar-macros.ss" )
   (all-from "generic/passes/tokmac_bkend/find-emittoks.ss" )
   (all-from "generic/passes/tokmac_bkend/desugar-gradients.ss" )
   (all-from "generic/passes/tokmac_bkend/desugar-let-stored.ss" )
   (all-from "generic/passes/tokmac_bkend/rename-stored.ss" )   

   (all-from "generic/passes/tokmac_bkend/cps-tokmac.ss" )
   (all-from "generic/passes/tokmac_bkend/sever-cont-state.ss" )
   (all-from "generic/passes/tokmac_bkend/closure-convert.ss" )
   (all-from "generic/passes/tokmac_bkend/inline-tokens.ss")

   (all-from "generic/passes/nesc_bkend/flatten-tokmac.ss" )
   (all-from "generic/passes/nesc_bkend/emit-nesc.ss" )         

   (all-from "generic/passes/analyze_query/add-data-flow.ss" )

   (all-from "generic/passes/deglobalize/deglobalize.ss" )
   (all-from "generic/passes/deglobalize/deglobalize2.ss" )

   (all-from "generic/passes/analyze_query/classify-names.ss" )
   (all-from "generic/passes/analyze_query/add-heartbeats.ss" )
   (all-from "generic/passes/analyze_query/add-control-flow.ss" )
   (all-from "generic/passes/analyze_query/add-places.ss" )
   (all-from "generic/passes/analyze_query/analyze-places.ss" )
   (all-from "generic/passes/analyze_query/add-data-flow.ss" )
   (all-from "generic/passes/analyze_query/resolve-fold-trees.ss" )

;  (all-from  )   
;  (all-from  )   
;  (all-from  )   
;  (all-from  )   
   ))

  
;(require main_plt)(current-directory "demos/wavescope")
;(browse-stream (wsint "demo4_fft.ws"))
;(browse-stream (wsint "demo9b_higher_order_prims.ws"))
;(browse-stream (wsint "demo1d_dataFile_binary.ws"))
#;
(begin 
  (current-directory "~/wavescript/apps/marmot")
  (wsint "run_first_phase.ws"))
#;
(begin 
  (current-directory "~/wavescript/demos/wavescope")
  (wsmlton "demo1c_timer.ws"))


  
) ; End module
