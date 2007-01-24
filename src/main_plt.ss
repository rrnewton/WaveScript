
;; Ryan Newton
;; Started 2004.03.26
;; Here is the main file for my compiler.
;=======================================================================

#cs ;; Case Sensitivity

;; [2004.06.16] This should have been done already, but just for good measure...
(define current_interpreter 'mzscheme)
(define simulator-batch-mode #f)

;(module main_plt mzscheme
 
(require (lib "include.ss")
         (all-except "generic/util/helpers.ss" id rec)
         (all-except "generic/compiler_components/regiment_helpers.ss")         
	 "plt/hashtab.ss"
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

(require 

(all-except "generic/compiler_components/hm_type_inference.ss" these-tests test-this)

;(all-except "plt/desugar-pattern-matching.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/desugar-pattern-matching.ss" these-tests test-this)

(all-except "generic/passes/normalize_source/verify-regiment.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/typecheck.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/desugar-misc.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/eta-primitives.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/rename-vars.ss" these-tests test-this)
(all-except "generic/passes/normalize_source/remove-unquoted-constant.ss" these-tests test-this)

(all-except "generic/passes/static_elaborate/static-elaborate.ss" these-tests test-this)
(all-except "generic/passes/static_elaborate/verify-elaborated.ss" these-tests test-this)

(all-except "generic/passes/normalize_query/reduce-primitives.ss" these-tests test-this)

(all-except "generic/passes/wavescope_bkend/merge-iterates.ss" these-tests test-this)
;(all-except "generic/passes/wavescope_bkend/purify-iterate.ss" these-tests test-this)
(all-except "generic/passes/wavescope_bkend/nominalize-types.ss" these-tests test-this)
(all-except "generic/passes/wavescope_bkend/type-annotate-misc.ss" these-tests test-this)
(all-except "generic/passes/wavescope_bkend/flatten-iterate-spine.ss" these-tests test-this)

(all-except "generic/passes/normalize_query/remove-complex-constant.ss" these-tests test-this)
; pass07_verify-stage2.ss
(all-except "generic/passes/normalize_query/uncover-free.ss" these-tests test-this)
(all-except "generic/passes/normalize_query/lift-letrec.ss" these-tests test-this)
(all-except "generic/passes/normalize_query/lift-letrec-body.ss" these-tests test-this)
(all-except "generic/passes/normalize_query/remove-complex-opera.ss" these-tests test-this)
(all-except "generic/passes/normalize_query/remove-lazy-letrec.ss" these-tests test-this) 
(all-except "generic/passes/normalize_query/verify-core.ss" these-tests test-this)

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
)


(IF_GRAPHICS
 (require
;;;; (all-except "plt/simulator_nought.ss" these-tests test-this)
;; (all-except "plt/simulator_nought_graphics.ss" these-tests test-this wrap-def-simulate)
  (all-except "plt/graphics_stub.ss" these-test test-this)))

;; Import these for the benefit of the unit tests below:
(require "generic/sim/simulator_alpha_datatypes.ss")
(require "generic/sim/alpha_lib.ss")
(require (all-except "generic/sim/simulator_alpha.ss" these-tests test-this))

;(require (all-except "generic/sim/wavescript_sim_library.ss" these-tests test-this))
(require (all-except "generic/langs/lang_wavescript.ss" these-tests test-this))

(require (all-except "generic/compiler_components/source_loader.ss" these-tests test-this))
(require (all-except "generic/compiler_components/logfiles.ss" these-tests test-this))

(include "generic/testing/tests_noclosure.ss")
(include "generic/testing/tests.ss")

;============================================================
;; INLINE THE MAIN COMPILER CODE

;; Bring these into top-level for some of the mini-passes defined in main.ss.
(require (all-except "generic/passes/pass-mechanism.ss" these-tests test-this))
(require (all-except "generic/compiler_components/reg_core_generic_traverse.ss" these-tests test-this) )

(include "main.ss")
;============================================================

(define error-handler error-display-handler)
(load/use-compiled "generic/testing/driver.ss")

;; Bring these into the top-level for our REPL convenience.
(include "generic/shortcuts.ss")
(require "generic/util/reg_macros.ss")



;; [2007.01.23] For now we exit on error:
(current-exception-handler
 (lambda (exn)
   (printf "ERROR:\n   ~a\n\nException: ~s\n" (exn-message exn) exn)
   (exit 1)))

















#|

;; Can't get langs to work.  Just abandon evaluation:
(game-eval (lambda args 'unspecified))
(host-eval (lambda args 'unspecified))

;(require "plt/language-mechanism.ss") ;; This blows up if we try to require it up top!
;(load/use-compiled "generic/lang00.ss")
;(load/use-compiled "generic/lang05.ss")

(load/use-compiled "generic/util/repl.ss")

(define-syntax lazy-letrec
  (syntax-rules ()
    [(_ ([lhs rhs] ...) body ...)        
     (letrec ([lhs rhs] ...) body ...)]))


(define (load_loader)
  (current-directory (build-path "plt"))
  (let ([f (load-extension 
	    (build-path "compiled" "native"  (system-library-subpath) "_loader.so"))])
  (printf "Got loader: ~s~n" f)
  (let-values ([(th r) (f #t)])
	      (printf "Loaded, ~s  ~s~n" th r)
	      (th))))

;(begin (display ) (newline) (exit))

;(begin (init-graphics) (cleanse-world) (graphical-repl))
;(define (start) (begin (init-graphics) (cleanse-world) (graphical-repl))) ;; shorthand

;(t)

;(ra '(tokens (node-start () (soc-return 3))))


(define (t)
  (ra
   '


(cps-tokmac-lang
  '(program
     (bindings (result_1 '34234324324))
     (nodepgm
       (tokens
         (returnhandler_6
           retid
           (destid flag val toind viaind)
           (stored (acc_9 ()))
           (if (= flag '222)
               (let ([oldacc_5 acc_9])
                 (begin (set! acc_9 '())
                        (let ([parentpointer_7
                               (ext-ref
                                 (tok global-tree viaind)
                                 storedgparent_13)])
                          (if (not parentpointer_7)
                              (dbg '"ERROR: fell off the via tree.")
                              (if (= '0 parentpointer_7)
                                  (call (tok SOC-return-handler toind)
                                        (cons val oldacc_5))
                                  (bcast
                                    (tok returnhandler_6 retid)
                                    parentpointer_7
                                    '333
                                    (cons val oldacc_5)
                                    '0
                                    '0))))
                        #0=(void)))
               (if (not (if (= destid '0) '#t (= destid (my-id))))
                   (void)
                   (set! acc_9 (cons val acc_9)))))
         (global-tree
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (storedgparent_13 #1='#f)
             (storedgorigin_12 #2='#f)
             (storedghopcount_11 #3='#f)
             (storedgversion_10 #4='#f))
           (if (if (not storedghopcount_11)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version storedgversion_10)
                           (if (= g_version storedgversion_10)
                               (< g_hopcount storedghopcount_11)
                               '#f)
                           '#f)
                       '#f))
               (begin (bcast
                        (tok global-tree subtok_ind)
                        (my-id)
                        g_origin
                        (+ '1 g_hopcount)
                        g_version)
                      (if (not (= g_hopcount '0))
                          (begin (set! storedgparent_13 g_parent)
                                 (set! storedgorigin_12 g_origin)
                                 (set! storedghopcount_11 g_hopcount)
                                 (set! storedgversion_10 g_version)
                                 #0#)
                          (void))
                      #0#)
               (void)))
         (spread-global
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (storedgparent_19 #1#)
             (storedgorigin_18 #2#)
             (storedghopcount_17 #3#)
             (storedgversion_16 #4#)
             (ver_15 (void))
             (storedliftoption_14 '#f))
           (if (if (not storedghopcount_17)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version storedgversion_16)
                           (if (= g_version storedgversion_16)
                               (< g_hopcount storedghopcount_17)
                               '#f)
                           '#f)
                       '#f))
               (begin (if storedliftoption_14
                          (void)
                          (begin (set! storedliftoption_14 '#t)
                                 (set! ver_15 '0)
                                 #0#))
                      (ext-set! (tok global-tree 0) storedgparent_13 '0)
                      (set! ver_15 (+ '1 ver_15))
                      (bcast (tok global-tree '0) (my-id) '1 ver_15)
                      (timed-call
                        1000
                        (tok spread-global 0)
                        '#f
                        '#f
                        '0
                        '#f)
                      (if (not (= g_hopcount '0))
                          (begin (set! storedgparent_19 g_parent)
                                 (set! storedgorigin_18 g_origin)
                                 (set! storedghopcount_17 g_hopcount)
                                 (set! storedgversion_16 g_version)
                                 #0#)
                          (void))
                      #0#)
               (void)))
         (node-start subtok_ind () (stored) (void))
         (SOC-start
           subtok_ind
           ()
           (stored)
           (begin (void)
                  (let ([aggrID_3 (+ (* '1000 '0) '0)])
                    (call (tok returnhandler_6 aggrID_3)
                          (my-id)
                          '222
                          result_1
                          '0
                          '0))
                  (soc-finished)
                  'multiple-bindings-for-token
                  #0#))))))


   
   ))








(define foob 
  (emit-nesc 
   (flatten-tokmac 
    (cleanup-token-machine 
     '(tokens (SOC-start () (printf "woot\n")))))))


;(test-units)
|#

