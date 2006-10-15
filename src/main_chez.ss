;; main_chez.ss
;; Loads the regiment compiler in Chez Scheme.

;; NOTE: This file uses (include ...) rather than (load ...) 
;; This basically inlines all the code in question into this file at compile time.
;; Thus, making a compiled copy of this file makes a compiled copy of the whole system.
;; HOWEVER: I broke this rule for things that depend on whether or not SWL is loaded.
;; TODO FIXME: I can also make this happen at compile time, via macros.

; =======================================================================

;; Wipe *all* previous bindings before coming RELOADING the system.
;; [2006.02.28] Without this we get killed by the fact that we redefine "module".
(if (top-level-bound? 'REGIMENTD) (eval '(import scheme)))

;;; Compile-time configuration.
;;;
;;; This runs extra-early, when the file is compiled.         <br>
;;; It sets up the location of the Regiment tree.             <br>
;;;
;;; The Regiment compiler expects case-sensitive treatment of symbols:
;;; (But hopefully it should work either way, as long as its consistent.
(eval-when (compile load eval) 
  (include "config.ss")
  
  (compile-profile #t)

	   (case-sensitive #t)

	   ;; For now let's just error if we have no dir:
;	   (if (not (getenv "REGIMENTD"))
;	       (error 'regiment "environment variable REGIMENTD was not set"))
	   
	   ;; This is a bit weird, ultimately the global param
	   ;; REGIMENTD is the thing to look at, but we have some
	   ;; issues with the order of evaluation/loading/binding
	   ;; here, so first we bind this:
	   (define-syntax default-regimentd
	     (syntax-rules ()
	       [(_) (if (getenv "REGIMENTD") (getenv "REGIMENTD") (current-directory))]))

	   (source-directories (list 
				     "."
				     (string-append (default-regimentd) "/src/generic")
				     (string-append (default-regimentd) "/src")
				     (string-append (default-regimentd) "/src/chez")
				     ))
	   
	   ;(optimize-level 0) ;0/1/2/3)
	   ;; Currently [2005.10.20] optimize levels result in these times on unit tests:
	   ;; 1: 29046 ms elapsed cpu time, including 9314 ms collecting
	   ;; 2: 29365 ms elapsed cpu time, including 7988 ms collecting
	   ;; 3: 25488 ms elapsed cpu time, including 8571 ms collecting
	   ;; 3 with no debug mode! 13993 ms elapsed cpu time, including 3844 ms collecting	   
	   

	   ;; This makes our modules work properly in newer versions of Chez:
	   ;; Otherwise we get unorder definitions, which breaks certain Regiment code.
	   (if (top-level-bound? 'internal-defines-as-letrec*)
	       (internal-defines-as-letrec* #t))

           (define current_interpreter 'chezscheme)           
	   (define-top-level-value 'simulator-batch-mode #f)


	   (case REGOPTLVL
	     [(0)
	      ;; Disable source optimization altogether.
	      (optimize-level 0)
	      (run-cp0 (lambda (cp0 x) x))]
	     [(2) (optimize-level 2)]
	     [(3)
	      (printf "Configuring compiler for \n")
	      ;; This configuration is for running extended simulation-experiments only:
	      ;; REMEMBER to also disable IFDEBUG in constants.ss
	      (define-top-level-value 'simulator-batch-mode #t)
	      (optimize-level 3)
	      (compile-compressed #f)
	      (generate-inspector-information #f)
	      ;; Messing with this didn't seem to help performance.
	      #;
	      (run-cp0
	       (lambda (cp0 x)
		 (parameterize ([cp0-effort-limit 50000]  ;; per-call inline effort, 200 default
				[cp0-score-limit  500]   ;; per-call expansion allowance, 20 default
				[cp0-outer-unroll-limit 3]) ;; inline recursive?
		   (let ((x (cp0 x)))
		     (parameterize (
				    (cp0-effort-limit 0)
				    )
		       (cp0 x))))))]
	     [else (error 'regiment-compiler "bad setting for REGOPTLVL: ~s" REGOPTLVL)])
	   )

;======================================================================
;;; Setup stuff.

;; Set some Chez scheme parameters.
(print-graph #t )
(print-gensym #f)
;(print-level 8)
(print-level #f) ;20)
(print-length #f) ;80)
(print-vector-length #f)
;(pretty-maximum-lines 700)
(pretty-maximum-lines #f)

;; Storing and then modifying the default break handler.
;; This is just a little hack to let us break "on" a value and then continue.
#|(if (not (top-level-bound? 'default-break-handler))
    (define-top-level-value 'default-break-handler (break-handler)))
(break-handler (lambda args 
		 (apply default-break-handler args)
		 (if (null? args) (void) (car args))))
|#
;; [2006.08.28] Nixing that hack.  It's much better to just have our own inspect function:
(define (inspect/continue x) (inspect x) x)

;; This forces the output to standard error in spite the Scheme
;; parameters console-output-port and current-output-port.
(define stderr  
  (let ((buffer-size 1))
    (let ((p (make-output-port 2 (make-string buffer-size))))
      (set-port-output-size! p (- buffer-size 1))
      p)))

;======================================================================

(fprintf stderr "Regiment: Loading compiler in chezscheme~a...\n"
	 (if (top-level-bound? 'regiment-origin)
	     (format " (from ~a)" regiment-origin)
	     ""))

;======================================================================
;;; Begin loading files.

;(define start-dir (eval-when (compile load eval) (cd)))
(eval-when (compile load eval) 
  (cd (string-append (default-regimentd) "/src/chez")))

(include "match.ss")      ;; Pattern matcher, dependency.
(include "regmodule.ss")  ;; Common module syntax.
;; After this point, everything must use chez:module for native chez modules.
;; 'module' will become my chez/plt portable regiment modules.
(import reg:module)


 ;; Load this first.  Widely visible constants/parameters.
(include "chez_constants.ss")

;; Because I run from command line, it helps to enter the inspector after an error.
(unless (top-level-bound? 'default-error-handler)
  (define-top-level-value 'default-error-handler (error-handler)))
(error-handler
 (lambda (who msg . args)
   (call/cc (lambda (k) 	     
	      (parameterize ([error-handler default-error-handler]
			     [current-directory (string-append (REGIMENTD) "/src/generic")])
		(fprintf (console-output-port)
			 "~%Error~a: ~a.~%"
			 (if who (format " in ~s" who) "")
			 (parameterize ([print-level 3] [print-length 6])
			   (apply format msg args)))
		(fprintf (console-output-port)
			 "Entering debugger to inspect current continuation, type 'q' to exit.\n")
		;; Set the current directory so that the inspector can find line numbers:
		(inspect k))))))

(IF_GRAPHICS (fprintf stderr "(Linking GUI code using SWL.)\n")
	     (fprintf stderr "(No GUI available.)\n"))
(flush-output-port stderr)

(include "../generic/reg_macros.ss") (import reg_macros)

;; A global parameter that is the equivalent of the eponymous
;; environment var.  Now that constants.ss is loaded we can set this. <br>
;; This uses the kinder behavior -- try the current directory.
;; (However, that's a bit irrelevent if an error was already signaled above.)
(REGIMENTD (default-regimentd))

(include "hash.ss") (import hashfun) ;; TEMPORARY
(include "hashtab.ss") (import hashtab)
(include "helpers.ss") (import (except helpers test-this these-tests))

;; These provide some more utility code related to threads:
(IF_THREADS (begin (include "threaded_utils.ss") (import threaded_utils)))

;; Lists all the Regiment primitives and their types:
(include "../generic/prim_defs.ss") (import prim_defs)
(include "../generic/grammar_checker.ss") (import grammar_checker)
(include "../generic/regiment_helpers.ss") (import (except regiment_helpers test-this these-tests))
(include "tsort.ss") (import (except topsort-module test-this these-tests))
(include "pregexp.ss") (import pregexp_module)

(include "../generic/c_generator.ss") (import c_generator)
(include "../generic/scheme_fft.ss")
(include "../generic/fft.ss") (import fft)

;======================================================================
;; [2005.11.16] This is a nasty dependency, but I had to write the "sleep" function in C:
;; This tries to dynamically load the shared object the first time the function is called:
(define (sleep t)
  (IF_GRAPHICS
   (thread-sleep t) ;; This uses the SWL thread library.  (Not real OS threads.)
   (begin
    ;(printf "Dynamically loading usleep from shared library...\n")(flush-output-port)
     (parameterize ((current-directory (string-append (REGIMENTD) "/src/")))
       (if (not (file-exists? (format "build/~a/usleep_libc.so" (machine-type))))
	   ;; This even resorts to calling make to build the sleep object!!
	   ;; This is kinda silly, and will cause a bunch of text to dump to stdout/err.
	   (system "(cd C; make usleep_libc)"))
       (if (file-exists? (format "build/~a/usleep_libc.so" (machine-type)))
					;(parameterize ((current-directory (format "chez/usleep/~a" (machine-type))))
	   (load (format "build/~a/usleep_libc.so" (machine-type)))
	   ;; If that build failed and we still don't have it we have to throw an error:
	   (define-top-level-value 'sleep (lambda args (error 'sleep "function not loaded from C shared object file.")))))
     (sleep t))))
;; Only make a system call once to sync us with the outside world.
(define current-time (seconds-since-1970))
;======================================================================

(include "simulator_alpha_datatypes.ss") (import simulator_alpha_datatypes)

;; Load this before the simulator.
(IF_GRAPHICS
    (begin
      (include "basic_graphics.ss")
      (include "graphics_stub.ss")
      (import basic_graphics)
      (import graphics_stub))
    ;; Otherwise, throw in some stubs that are invoked by the generated code:
    (begin ;; [2006.03.01] Nixing these.  Instead we should be disciplined about not generating any such calls.
           ;(define-syntax draw-mark (syntax-rules () [(_ x ...) (begin x ... 'nogui-stub)]))
	   ;(define-syntax  make-rgb (syntax-rules () [(_ x ...) (begin x ... 'nogui-stub)]))
	   ))

(include "../generic/logfiles.ss") (import logfiles)

(include "alpha_lib.ss") 
(import alpha_lib) ;; [2005.11.03] FIXME Temporary, reactivating this... shouldn't need to be on.
(include "alpha_lib_scheduler_simple.ss") ;(import alpha_lib_scheduler_simple)
;(include "../generic/alpha_lib_scheduler.ss")

(include "simulator_alpha.ss") 
(import simulator_alpha)
(include "../generic/firelightning_sim.ss")
(include "../generic/tossim.ss")

;(include "../reg_grammar.ss")

;; Type inference is used by verify-regiment, below.
(include "../generic/hm_type_inference.ss") (import hm_type_inference)
;(include "../generic/prim_defs_OLD.ss")
;(import prim_defs_OLD) ;; TEMP

;; This is used by the subsequent passes that process TML:
(include "../generic/tml_generic_traverse.ss") (import tml_generic_traverse)
(include "../generic/reg_core_generic_traverse.ss") (import reg_core_generic_traverse)

;(define prim_random #%random) ;; Lame hack to get around slib's messed up random.
;(define (random-real) (#%random 1.0)) ;; Lame hack to get around slib's messed up random.
(include "../generic/language-mechanism.ss")
(include "../generic/lang_wavescript.ss")

(include "../generic/lang00.ss")

(include "../generic/lang06_uncover-free.ss")
(include "../generic/lang07_lift-letrec.ss")

(include "../generic/lang11_classify-names.ss")
(include "../generic/lang12_heartbeats.ss")
(include "../generic/lang13_control-flow.ss")
(include "../generic/lang14_places.ss")

(include "../generic/lang20_deglobalize.ss") 

(include "../generic/lang30_haskellize-tokmac.ss") 
(include "../generic/lang32_emit-nesc.ss")

(include "../generic/pass00_verify-regiment.ss")
(include "../generic/pass000_desugar-pattern-matching.ss") (import pass000_desugar-pattern-matching)
(include "../generic/source_loader.ss") (import source_loader) ;; For loading regiment sources.


(include "../generic/pass01_eta-primitives.ss")
(include "../generic/pass02_rename-vars.ss")
(include "../generic/pass03_remove-unquoted-constant.ss")
(include "../generic/pass04_static-elaborate.ss") (import pass04_static-elaborate)
(include "../generic/pass05_reduce-primitives.ss")

(include "../generic/pass_merge-iterates.ss") (import pass_merge-iterates)

(include "../generic/pass06_remove-complex-constant.ss")
; pass07_verify-stage2.ss
(include "../generic/pass08_uncover-free.ss")
(include "../generic/pass09_lift-letrec.ss")
(include "../generic/pass10_lift-letrec-body.ss")
(include "../generic/pass11_remove-complex-opera.ss")
(include "../generic/pass12_verify-core.ss")
(include "../generic/pass13_classify-names.ss")
;(include "../generic/pass09_separate-graph")

(include "../generic/pass14_add-heartbeats.ss")
(include "../generic/pass15_add-control-flow.ss")
(include "../generic/pass16_add-places.ss")
(include "../generic/pass17_analyze-places.ss")

(include "../generic/pass17_resolve-fold-trees.ss") (import pass17_resolve-fold-trees)
;(include "../generic/pass18_add-routing.ss")

(include "../generic/pass20_deglobalize.ss") 
(import pass20_deglobalize)

(include "../generic/pass20_deglobalize2.ss")
(import pass20_deglobalize2)
(include "../generic/pass20b_tmgen.ss")

;; Uses delazy-bindings:
(include "../generic/pass17_add-data-flow.ss")      (import pass17_add-data-flow)

(include "../generic/pass21_cleanup-token-machine.ss")
;(include "../generic/pass22_desugar-soc-return.ss")
;; TODO: Merge with pass22, besides this isn't really 26 anyway!
(include "../generic/pass22_desugar-macros.ss")
;(include "../generic/pass26_desugar-macros.ss")

(include "../generic/pass23a_find-emittoks.ss")
;(include "../generic/pass23_desugar-gradients_shared.ss")  ;; "header" file
(include "../generic/pass23_desugar-gradients.ss")
(include "../generic/pass23_desugar-gradients_verbose.ss")
(include "../generic/pass23_desugar-gradients_simple.ss")
(include "../generic/pass23_desugar-gradients_ETX.ss")

(include "../generic/pass24_desugar-let-stored.ss")
(include "../generic/pass25_rename-stored.ss")


;(include "../generic/pass24_analyze-calls.ss")
;(include "../generic/pass25_inline.ss")
;(include "../generic/pass26_prune-returns.ss")
(include "../generic/pass26_cps-tokmac.ss")
(include "../generic/pass27_sever-cont-state.ss")
;; (include "../generic/pass27.2_add-kclosure.ss")
(include "../generic/pass28_closure-convert.ss")

(include "../generic/pass29_inline-tokens.ss") (import pass29_inline-tokens)
(include "../generic/pass30_haskellize-tokmac.ss")

(include "../generic/pass31_flatten-tokmac.ss")
(include "../generic/pass32_emit-nesc.ss")

;; [2006.08.27] Now for the passes in the WaveScript branch:
(include "../generic/wavescript_emit-c.ss") (import wavescript_emit-c)
(include "../generic/wavescript_nominalize-types.ss") (import wavescript_nominalize-types)


;(load "../depends/slib/chez.init")
;(require 'tsort) ;; for the simulator: 

;; Basic parallel computation (using engines):
;; [2006.02.28] These were used by simulator_nought, but are not used currently.
(IF_GRAPHICS
    (load "swl_flat_threads.ss") ;; uses swl threads instead
    (load "flat_threads.ss"))

;; LAME:
;(if (top-level-bound? 'SWL-ACTIVE) (eval '(import flat_threads)))


;; Basic simulator for the nodal language:
;(load "chez/simulator_nought.ss")
(chez:module simulator_nought
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
      (load "demo_display.ss")
      (load "chez/simulator_nought_graphics.ss"))

;(trace  explode-primitive process-expr process-letrec)


(eval-when (compile load eval) (cd ".."))
(include "main.ss")
(eval-when (compile load eval) (cd "chez"))

;; [2006.04.18] This testing system isn't doing much for us currently
;; other than exercising our static elaborator.
;; TODO: Fold it into the unit tests.
;;
;; Driver depends on 'pass-list being defined.
(include "../generic/driver.ss")
;  (game-eval (lambda args 'unspecified))
  (game-eval eval)
  (host-eval (lambda args 'unspecified))
(include "../generic/tests_noclosure.ss")
(include "../generic/tests.ss")

;; [2006.04.18] This is pretty out of date as well:
;; Load the repl which depends on the whole compiler and simulator.
(include "../generic/repl.ss")

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


;(r '(letrec ((x (rmap sense world)) [y world] [z (lambda (n) (+ (- n 3) n))]) x))

;; Open this up so we can read the global counters:
(import simulator_alpha_datatypes)


;; After loading is finished switch to the src/ directory:
;; But if you do this, remember to switch back to "chez/" when using the interactive inspector:
;(eval-when (compile load eval)
;  (cd start-dir)
(cd (string-append (REGIMENTD) "/src"))
;  )


;; This is an (attempted) hack for maintaining clean seperation between repeated loads.
;;
;; [2006.03.27] This doesn't work yet, repeated loads still allocate more memory. 
;; Chez garbage collects compiled code, right?
;; Ahh, Chez doesn't collect space used by environments.
;; So I WOULD need that part that tries to wipe all the bindings.
(define wipe ;; shorthand
  (let ([diff difference])
    (lambda () 
  (time 
   (begin 
     (collect (collect-maximum-generation))

     (let ([stats (statistics)]
	   [loader bl2])
       (printf "Pre-wipe mem usage: ~:d\n" (- (sstats-bytes stats) (sstats-gc-bytes stats))))
     (eval '(import scheme))

     ;; This is a simple version:
     (for-each (lambda (s)
		 (when (and (top-level-bound? s) 
			    (not (memq s '(wipe bl2 bln __UTILSDIR __utilsdir)))) ;; Don't kill my little loading mechanism
		   ;(printf "Killing ~s\n" s)
		   (set-top-level-value! s #f)))
       (diff (environment-symbols (interaction-environment))
	     (environment-symbols (scheme-environment))))

#;
     (let ([newenv (copy-environment ;(interaction-environment)
				     (scheme-environment) 
				     #t
				     (cons 'b (environment-symbols (scheme-environment))))])
       ;; First we anihilate our existing environment.  Possibly unnecessary.
       ;; This doesn't work because I keep running into read-only
       ;; bindings, even if I avoid the (import scheme) above....
       (let ([tlb? top-level-bound?]
	     [stlv! set-top-level-value!]
	     [evl eval])
	 (for-each (lambda (s)
		     (when (tlb? s)
		       (printf "Killing ~s\n" s)
		       ;; Not mutable for some reason:
		       (stlv! s #f)
		       ;(evl `(set! ,s #f))
		       ))
	   (environment-symbols (interaction-environment))))
       (interaction-environment  newenv))
     
     (collect (collect-maximum-generation))
     (let ([stats (statistics)]) 
       (printf "Post-wipe mem usage: ~:d\n" (- (sstats-bytes stats) (sstats-gc-bytes stats))))

     ))
  )))

(define-id-syntax bw  (begin (wipe) b)) ;; shorthand


#|
(import reg:module)
;(include "fft_test.ss")
(optimize-level 2)
(load "fft_test.ss")
(import fft_test)
;(testfft-bigger)
(testfft-more)
|#
