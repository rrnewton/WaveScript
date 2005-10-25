;; compiler_chez.ss
;; Loads the regiment compiler in Chez Scheme.

;; NOTE: This file uses (include ...) rather than (load ...) 
;; This basically inlines all the code in question into this file at compile time.
;; Thus, making a compiled copy of this file makes a compiled copy of the whole system.
;; HOWEVER: I broke this rule for things that depend on whether or not SWL is loaded.
;; TODO FIXME: I can also make this happen at compile time, via macros.

;; ======================================================================

(if (not (top-level-bound? 'default-break-handler))
    (define-top-level-value 'default-break-handler (break-handler)))
(break-handler (lambda args 
		 (apply default-break-handler args) 
		 (if (null? args) (void) (car args))))

;; The regiment compiler expects case-sensitive treatment of symbols:
;; (But hopefully it should work either way, as long as its consistent.
(eval-when (compile load eval) 
	   (case-sensitive #t)
	   (source-directories '("." "~/cur" "~/cur/chez" "~/cur/generic"))
	   (optimize-level 0);2) 
	   ;; Currently [2005.10.20] optimize levels result in these times on unit tests:
	   ;; 1: 29046 ms elapsed cpu time, including 9314 ms collecting
	   ;; 2: 29365 ms elapsed cpu time, including 7988 ms collecting
	   ;; 3: 25488 ms elapsed cpu time, including 8571 ms collecting
	   ;; 3 with no debug mode! 13993 ms elapsed cpu time, including 3844 ms collecting	   
	   ;; Wow, down to 3.251 seconds on my 10second network average-value test.
	   )
(print-graph #t)
(print-gensym #f)

(define-syntax IF_GRAPHICS
  (lambda (x)
    (syntax-case x ()
		 [(_ E1 E2)
		  ;; The swl script sets this variable:
		  ;; When we build through SWL, we link in the SWL code.  Otherwise not.
		  (if (getenv "SWL_ROOT")
		      #'E1
		      #'E2)]
		 [(_ E1)
		  #'(IF_GRAPHICS E1 (void))])))

;; This makes our modules work properly in newer versions of Chez:
(eval-when (compile load eval)
	   (if (top-level-bound? 'internal-defines-as-letrec*)
	       (internal-defines-as-letrec* #t)))

;; TEMP
(define current_interpreter 'chezscheme)

(printf "Loading compiler in chezscheme..~n")
(IF_GRAPHICS (printf "(Linking GUI code using SWL.)\n")
	(printf "(No GUI available.)\n"))
(flush-output-port)

(include "chez/match.ss")

;(include "generic/constants.ss")

;; [2004.06.04] MOVED THIS DEFINIITON to helpers.ss
;; Uncomment this to remove debugging code and possibly make the
;; system run faster.
;;(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (void)]))
;;(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (begin expr ...)]))

;; [2005.09.27] Disabling this, don't need it without simulator_nought
;(define random #%random)
;;;; [2004.06.28] Moving this here, hope that works:
;(include "../depends/slib/chez.init")  ;; Freeing myself of slib [2005.10.19]
;(require 'tsort) ;; for the simulator: 

(include "generic/constants.ss")

;; This in turn includes "../generic/helpers.ss" so we gotta load it from its dir.
;; I used symbolic links to fix this up so it refers to "generic/helpers.ss", 
;; therefore we don't need to change directories.
;(eval-when (compile eval) (cd "chez"))
(include "chez/hash.ss") (import hash) ;; TEMPORARY
(include "chez/helpers.ss") (import (except helpers test-this these-tests))
(include "chez/tsort.ss") (import (except topsort-module test-this these-tests))

;; This is a trick to deal with mutual recursion in the modules:
;; FIXME: Doesn't work right now:
;(define test-tsort (let () (import topsort-module) (test-this)))  


;(eval-when (compile eval) (cd ".."))

(include "chez/simulator_alpha_datatypes.ss") (import simulator_alpha_datatypes)
(include "chez/alpha_lib_scheduler_simple.ss") ;(import alpha_lib_scheduler_simple)
;(include "generic/alpha_lib_scheduler.ss")
(include "chez/simulator_alpha.ss") (import simulator_alpha)
(include "chez/alpha_lib.ss") ;(import alpha_lib)


(include "generic/grammar_checker.ss")

;(define prim_random #%random) ;; Lame hack to get around slib's messed up random.
(define (random-real) (#%random 1.0)) ;; Lame hack to get around slib's messed up random.
(include "generic/language-mechanism.ss")

(include "generic/lang00.ss")

(include "generic/lang06_uncover-free.ss")
(include "generic/lang07_lift-letrec.ss")

(include "generic/lang11_classify-names.ss")
(include "generic/lang12_heartbeats.ss")
(include "generic/lang13_control-flow.ss")
(include "generic/lang14_places.ss")

(include "generic/lang20_deglobalize.ss") 
(include "generic/lang30_haskellize-tokmac.ss") 

(include "generic/pass00_verify-regiment.ss")
(include "generic/pass01_eta-primitives.ss")
(include "generic/pass02_rename-vars.ss")
(include "generic/pass03_remove-unquoted-constant.ss")
(include "generic/pass04_static-elaborate.ss")
(include "generic/pass05_reduce-primitives.ss")

(include "generic/pass06_remove-complex-constant.ss")
; pass07_verify-stage2.ss
(include "generic/pass08_uncover-free.ss")
(include "generic/pass09_lift-letrec.ss")
(include "generic/pass10_lift-letrec-body.ss")
(include "generic/pass11_remove-complex-opera.ss")
(include "generic/pass12_verify-core.ss")
(include "generic/pass13_classify-names.ss")
;(include "generic/pass09_separate-graph")

(include "generic/pass14_add-heartbeats.ss")
(include "generic/pass15_add-control-flow.ss")
(include "generic/pass16_add-places.ss")
(include "generic/pass17_analyze-places.ss")
;(include "generic/pass18_add-routing.ss")

(include "chez/pass20_deglobalize.ss") (import deglobalize)

;; This is used by the subsequent passes that process TML:
(include "generic/tml_generic_traverse.ss")

(include "generic/pass21_cleanup-token-machine.ss")
;(include "generic/pass22_desugar-soc-return.ss")
;; TODO: Merge with pass22, besides this isn't really 26 anyway!
(include "generic/pass22_desugar-macros.ss")
;(include "generic/pass26_desugar-macros.ss")

(include "generic/pass23_desugar-gradients.ss")
(include "generic/pass24_desugar-let-stored.ss")
(include "generic/pass25_rename-stored.ss")


;(include "generic/pass24_analyze-calls.ss")
;(include "generic/pass25_inline.ss")
;(include "generic/pass26_prune-returns.ss")
(include "generic/pass27_cps-tokmac.ss")
(include "generic/pass28_closure-convert.ss")

;;(include "generic/pass29_verify-token-machine.ss")
(include "generic/pass30_haskellize-tokmac.ss")

;(load "../depends/slib/chez.init")
;(require 'tsort) ;; for the simulator: 

;; Basic parallel computation (engines):
(IF_GRAPHICS
    (load "chez/swl_flat_threads.ss")
    (load "chez/flat_threads.ss"))

;; LAME:
;(if (top-level-bound? 'SWL-ACTIVE) (eval '(import flat_threads)))

;; Load this before the simulator.
(IF_GRAPHICS
    (begin
      (load "chez/basic_graphics.ss")
      (load "chez/graphics_stub.ss")
      (eval '(import basic_graphics))
      (eval '(import graphics_stub)))
    (eval '(begin (define draw-mark (lambda args (void)))
		  (define rgb (lambda args (void))))))

;; Basic simulator for the nodal language:
;(load "chez/simulator_nought.ss")
(module simulator_nought
	(;run-simulation
	 ;run-simulation-stream
	 ;compile-simulate-nought 
	 ;build-simulation
	 ;;;process-statement-nought
	 ;init-world
	 ;cleanse-world
	 ;testsim
	 ;testssim
	 )
;	(include "chez/simulator_nought.ss")
)
;(import simulator_nought)



;; If we're in SWL then load the GRAPHICS portion:
;; Disabled temporarily!:
#; 
(when (top-level-bound? 'SWL-ACTIVE)
      (load "chez/demo_display.ss")
      (load "chez/simulator_nought_graphics.ss"))

;(trace  explode-primitive process-expr process-letrec)

(include "compiler.ss")

;; Driver depends on 'pass-names being defined.
(include "generic/driver.ss")
;  (game-eval (lambda args 'unspecified))
  (game-eval eval)
  (host-eval (lambda args 'unspecified))
(include "generic/tests_noclosure.ss")
(include "generic/tests.ss")

;; Load the repl which depends on the whole compiler and simulator.
(include "generic/repl.ss")

;; DISABLED TEMPORARILY:
#;
(if (top-level-bound? 'SWL-ACTIVE)
    (begin
      (eval '(import basic_graphics))
      (eval '(import graphics_stub))
      (load "chez/simulator_nought_graphics.ss")

      (let ([grepl-init (lambda () 
			  (init-world)
			  (init-graphics))]
	    [grepl-cleanse 
		      ;; Inbetween evaluations, reset colors.
		      (lambda ()
			(for-each
			 (lambda (simob)
			   (if (simobject-gobj simob)
			       (set-fill-color! (simobject-gobj simob) 
						Starting-Node-Color)))
			 all-objs)
			(cleanse-world))])
	(define-top-level-value 'graphical-repl
	  (repl-builder grepl-init 
			grepl-cleanse
			run-compiler
			graphical-simulation))
	(define-top-level-value 'precomp-graphical-repl
	  (repl-builder grepl-init 
			grepl-cleanse
			(lambda (x) x)
			graphical-simulation)))

      (define-top-level-value 'grepl graphical-repl)
      ))


(define (g) (eval (cadadr testssim)))
(pretty-maximum-lines 2000)


;(r '(letrec ((x (rmap sense world)) [y world] [z (lambda (n) (+ (- n 3) n))]) x))

;; Open this up so we can read the global counters:
(import simulator_alpha_datatypes)
