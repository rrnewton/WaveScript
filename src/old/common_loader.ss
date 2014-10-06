
;;;; 
;;;; .author Ryan Newton

;;;; [2007.12.27] This loads the bulk of the Regiment/WaveScript
;;;; source code.  It's shared between the different backends.
;;;; Hopefully this is the only place you'll find a long list of all
;;;; the source filenames in the project.


(common:load-source "generic/util/helpers.ss") 

;; These provide some more utility code related to threads:
(cond-expand
 [(and chez threads)
  (printf "Configuring for multithreaded execution.\n")
  (common:load-source "chez/threaded_utils.ss")
  ;; Threads don't get initialized until we run the compiler.
  (import threaded_utils)]
 ;; Otherwise provide a dummy implementation of "par":
 [else 
  (define par list)
  (define parmv values)
  (define par-list (lambda (th*) (map (lambda (th) (th)) th*)))
  (define par-map map)
  (define (init-par cpus) (void))
  (define (par-status) (void))
  (define (par-reset!) (void))
  (define (shutdown-par) (void))])

;; Not using these currently:
;(common:load-source "generic/util/imperative_streams.ss") ;(import (except imperative_streams test-this these-tests))


;; Include these at top-level for the system tests:
(common:load-source "generic/util/streams.ss")

;; Lists all the Regiment primitives and their types:
(common:load-source "generic/compiler_components/prim_defs.ss")

(common:load-source "generic/compiler_components/wavescript_helpers.ss")

;; [2007.04.30] The "type?" predicate is currently used in grammars.ss
;(cond-expand [(chez (eval-when (compile eval load) (compile-profile #t)))] [else (void)])
(common:load-source "generic/compiler_components/type_environments.ss")
;(cond-expand [(chez (eval-when (compile eval load) (compile-profile #f)))] [else (void)])


;; Michael's tools for manipulating expression annotations:
(common:load-source "generic/compiler_components/annotations.ss")

(common:load-source "generic/grammars/grammar_checker.ss") 

(cond-expand
 [(or chez larceny) 
  (common:load-source "generic/util/tsort.ss")]
 [plt (void)])

(common:load-source "generic/compiler_components/c_generator.ss") 

(common:load-source "generic/util/scheme_fft.ss") ;; FFT from the chez users guide

(common:load-source "generic/util/slib_fft.ss")   ;; FFT from slib.
(common:load-source "generic/util/fft.ss") 

(IFWAVESCOPE (begin)	     
  (common:load-source "generic/sim/simulator_alpha_datatypes.ss"))

;; Load this before the simulator.
(IF_GRAPHICS
    (begin
      ;; Only for swl1.0+ .  Gives us define-class, etc.
      (import swl:oop) 
      (import swl:generics) 
      (import (except swl:macros mvlet))
      (import swl:option)
      (import swl:threads)

      (todo:common:load-source "chez/basic_graphics.ss")
      (todo:common:load-source "chez/graphics_stub.ss")
      (import basic_graphics)
      (import graphics_stub))
    ;; Otherwise, throw in some stubs that are invoked by the generated code:
    (begin ;; [2006.03.01] Nixing these.  Instead we should be disciplined about not generating any such calls.
           ;(define-syntax draw-mark (syntax-rules () [(_ x ...) (begin x ... 'nogui-stub)]))
	   ;(define-syntax  make-rgb (syntax-rules () [(_ x ...) (begin x ... 'nogui-stub)]))
	   ))


;; Don't use these yet from WS:
(IFWAVESCOPE
 (begin)
 (begin 
   (common:load-source "generic/compiler_components/logfiles.ss") 

   (common:load-source "generic/sim/alpha_lib.ss") 

   (common:load-source "generic/sim/alpha_lib_scheduler_simple.ss")
   ;(common:load-source "generic/alpha_lib_scheduler.ss")

   (common:load-source "generic/sim/simulator_alpha.ss") 
   ;; This experiment is done with:
   ;(common:load-source "generic/sim/firelightning_sim.ss")
   ;; Didn't get this working in all backends, moot now:
   (cond-expand [chez (include "generic/passes/nesc_bkend/tossim.ss")] [else (void)])
   ))



(IFWAVESCOPE (begin) 
   ;; This is used by the subsequent passes that process TML:
  (common:load-source "generic/compiler_components/tml_generic_traverse.ss"))

(common:load-source "generic/compiler_components/reg_core_generic_traverse.ss") 

;; Type inference is used by verify-wavescript, below.
;(cond-expand [(chez (eval-when (compile eval load) (compile-profile #t)))] [else (void)])
(common:load-source "generic/compiler_components/hm_type_inference.ss") 
;(cond-expand [(chez (eval-when (compile eval load) (compile-profile #f)))] [else (void)])

(common:load-source "generic/passes/pass-mechanism_basic.ss") 

(common:load-source "generic/passes/pass-mechanism.ss") 


;(trace syntax->list)
;(common:load-source "generic/passes/pass-mechanism.ss") 
(common:load-source "generic/passes/graphviz.ss") 

;; Load this pass early because it's used in a couple places.
(IFWAVESCOPE (begin)  
  (common:load-source "generic/passes/tokmac_bkend/cleanup-token-machine.ss"))

;(define prim_random #%random) ;; Lame hack to get around slib's messed up random.
;(define (random-real) (#%random 1.0)) ;; Lame hack to get around slib's messed up random.
(cond-expand [chez (include "generic/langs/language-mechanism.ss")] [else])

(common:load-source "generic/langs/lang_wavescript.ss")

(cond-expand
 [plt     (common:load-source "depends/matpak.ss")]
 [larceny (common:load-source "../depends/matpak.ss")]
 [chez    (common:load-source "../../depends/matpak.ss")])

;(todo:common:load-source "generic/sim/wavescript_sim_library.ss")      ;; TODO: remove
;(todo:common:load-source "generic/sim/wavescript_sim_library_NEW.ss")  ;; TODO: remove
;; Don't want to import this one into top-level:
(cond-expand [chez (include "generic/sim/wavescript_sim_library_push.ss")]  [(or larceny plt)])

(cond-expand [(or chez plt)
	      (common:load-source "generic/testing/lang_wavescript_tests.ss")]
	     [larceny])


(cond-expand
 [chez 
  (include "generic/langs/lang00.ss")
  (include "generic/langs/lang06_uncover-free.ss")
  (include "generic/langs/lang07_lift-letrec.ss")

  (IFWAVESCOPE (begin)
    (begin 
      (include "generic/langs/lang11_classify-names.ss")
      (include "generic/langs/lang12_heartbeats.ss")
      (include "generic/langs/lang13_control-flow.ss")
      (include "generic/langs/lang14_places.ss")

      (include "generic/langs/lang20_deglobalize.ss") 

      (include "generic/scrap/lang30_haskellize-tokmac.ss") 
      (include "generic/langs/lang32_emit-nesc.ss")))
  ] [else])

(if VERBOSE-LOAD (printf "  Midway through, doing passes...\n"))


(common:load-source "generic/passes/normalize_source/verify-wavescript.ss")          
(common:load-source "generic/passes/normalize_source/typecheck.ss")                
(common:load-source "generic/passes/normalize_source/desugar-pattern-matching.ss") 


;; We dump the heap in larceny to grab tho load at an intermediate point:
;;================================================================================;;
;(call/cc (lambda (topk) (dump-heap "larc.heap" (lambda args (topk))) (repl) (exit)))
(cond-expand [larceny (dump-heap "larc.heap" (lambda args (load "newloads.ss") 
						     ;(repl)
						     (exit)))]
	     [else])
