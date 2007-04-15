;;;; main_chez.ss
;;;; Loads the regiment compiler in Chez Scheme.

;;;; NOTE: This file uses (include ...) rather than (load ...) 
;;;; This basically inlines all the code in question into this file at compile time.
;;;; Thus, making a compiled copy of this file makes a compiled copy of the whole system.
;;;; HOWEVER: I broke this rule for things that depend on whether or not SWL is loaded.
;;;; TODO FIXME: I can also make this happen at compile time, via macros.



; =======================================================================

;; Wipe *all* previous bindings before coming RELOADING the system.
;; [2006.02.28] Without this we get killed by the fact that we redefine "module".
;; This is *only* relevant if repeatedly loading regiment into a single REPL.
(when (top-level-bound? 'REGIMENTD) 
  (printf "WIPING previous bindings before reloading Regiment system.\n")
  (eval '(import scheme)))


;;; Compile-time configuration.
;;;
;;; This runs extra-early, when the file is compiled.         <br>
;;; It sets up the location of the Regiment tree.             <br>
;;;
;;; The Regiment compiler expects case-sensitive treatment of symbols:
;;; (But hopefully it should work either way, as long as its consistent.
(eval-when (compile load eval)

  ;; We load this at compile time to figure out some of our
  ;; compilation options:
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

	   (source-directories (#%list 
				;"."
;				(string-append (default-regimentd) "/src/chez")
;				(string-append (default-regimentd) "/src/generic")
				(string-append (default-regimentd) "/src")
				;"."
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

	   (define reg:set_opt_lvl!
	     (lambda ()
	       (case (REGOPTLVL)
	       [(0)
		;; Disable source optimization altogether.
		(optimize-level 0)
		(run-cp0 (lambda (cp0 x) x))]
	       [(2) (optimize-level 2)]
	       [(3)
		(printf "Configuring compiler for full optimize mode. (UNSAFE)\n")
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
	       [else (error 'regiment-compiler "bad setting for REGOPTLVL: ~s" REGOPTLVL)])))

	   (reg:set_opt_lvl!) ;; According to $REGOPTLVL
	   
	   )

;; Trying to set the svn rev when the code is *compiled*:
(define-syntax bind-svn-revision
  (lambda (x)
    (syntax-case x ()
      [(_)
       (let ()
	 (define (system-to-str str)
	   (let* ([pr (process str)]
		  [in (car pr)]
		  [out (cadr pr)]
		  [id  (caddr pr)])
	     (let ((p (open-output-string)))
	       (let loop ((c (read-char in)))
		 (if (eof-object? c)	  
		     (begin 
		       (close-input-port in)
		       (close-output-port out)
		       (get-output-string p))
		     (begin (display c p)
			    (loop (read-char in))))))))
	 (if (eq? (machine-type) 'i3nt)
	     -9999
	     (and (zero? (system "which svn &> /dev/null"))
		  (parameterize ([current-directory (string-append (default-regimentd) "/src")])
		    ;(printf"<<<<<<<<<<<READING SVN REV>>>>>>>>>>>>\n")
		    (let ([rev (read (open-input-string (system-to-str "svn info | grep Revision | sed s/Revision://")))])
		      (with-syntax ([revis (datum->syntax-object #'_ rev)])
			#'(define-top-level-value 'svn-revision revis))
		      )
		    ))))])))

(bind-svn-revision)

#;
(eval-when (compile eval)
  (define-syntax svn-revision
    (syntax-case ()
      [(_)
       (let ()
	 (define (system-to-str str)
	   (let* ([pr (process str)]
		  [in (car pr)]
		  [out (cadr pr)]
		  [id  (caddr pr)])
	     (let ((p (open-output-string)))
	       (let loop ((c (read-char in)))
		 (if (eof-object? c)	  
		     (begin 
		       (close-input-port in)
		       (close-output-port out)
		       (get-output-string p))
		     (begin (display c p)
			    (loop (read-char in))))))))
	 (inspect 'woot)
	 (if (eq? (machine-type) 'i3nt)
	     -9999
	     (and (zero? (system "which svn > /dev/null"))
		  (parameterize ([current-directory (default-regimentd)])
		    (printf"<<<<<<<<<<<READING SVN REV>>>>>>>>>>>>\n")
		    (read (open-input-string (system-to-str "svn info | grep Revision | sed s/Revision://")))
		    ))))])))


#;
(eval-when (compile eval)
  (define svn-revision
    (let ()
      (define (system-to-str str)
	(let* ([pr (process str)]
	       [in (car pr)]
	       [out (cadr pr)]
	       [id  (caddr pr)])
	  (let ((p (open-output-string)))
	    (let loop ((c (read-char in)))
	      (if (eof-object? c)	  
		  (begin 
		    (close-input-port in)
		    (close-output-port out)
		    (get-output-string p))
		  (begin (display c p)
			 (loop (read-char in))))))))
      (if (eq? (machine-type) 'i3nt)
	  -9999
	  (and (zero? (system "which svn > /dev/null"))
	       (parameterize ([current-directory (default-regimentd)])
		 (printf"<<<<<<<<<<<READING SVN REV>>>>>>>>>>>>\n")
		 (read (open-input-string (system-to-str "svn info | grep Revision | sed s/Revision://")))
		 ))))))

;======================================================================
;;; Setup stuff.

;; [2006.11.24] This is just a temporary thing so I can watch how fast things load.
(define VERBOSE-LOAD #f)

;  (source-directories (list "./"))
;(cd (car (source-directories)))
;  (inspect (list (source-directories) (cd)))

;(include "./config.ss") ;; Need to do this for windows... haven't got a good system yet.
(include "config.ss")

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

(fprintf stderr "Regiment: Loading ~a compiler in chezscheme~a...\n"
	 (let ([ws #f]  [reg #f])
	   (IFWAVESCOPE (set! ws #t) (set! reg #t))
	   (cond 
	    [(and ws reg) "ws+reg"]
	    [ws   "ws"]
	    [reg "reg"]))
	 (if (top-level-bound? 'regiment-origin)
	     (format " (from ~a)" regiment-origin)    
	     ;; [2007.04.15]:
	     ;; HACK, unknown origin must be boot file.
	     " (from .boot file)"
	     ))

;======================================================================
;;; Begin loading files.

;(define start-dir (eval-when (compile load eval) (cd)))
(eval-when (compile load eval) 
  (current-directory (string-append (default-regimentd) "/src/chez")))


(include "chez/match.ss")      ;; Pattern matcher, dependency.
(include "chez/rn-match.ss")      ;; My version of the pattern matcher.

;; To completely DISABLE my new prototype matcher, do this:
;(alias rn-match iu-match)
;;
;; Currently, just having rn-match in the type-checker bloats the
;; compiled .so from 2.85 mb to 3.1 mb.  This will only get worse.  
;; The question is whether we see enough of an improvement in runtime
;; to justify it.

;; Import the IU matcher globally:
(import iu-match)

;; After this point, everything must use chez:module for native chez modules.
;; 'module' will become my chez/plt portable regiment modules.
(eval-when (load eval)
  (include "chez/regmodule.ss")  ;; Common module syntax.
  (import reg:module)  
  )
 
 ;; Load this first.  Widely visible constants/parameters.
(include "chez/chez_constants.ss")

(if VERBOSE-LOAD (printf "  Starting load...\n"))

;; Because I run from command line, it helps to enter the inspector after an error.
(unless (top-level-bound? 'default-error-handler)
  (define-top-level-value 'default-error-handler (error-handler)))

(error-handler
 (lambda (who msg . args)
   (call/cc (lambda (k) 	     
	      (parameterize ([error-handler default-error-handler]
			     ;[current-directory (string-append (REGIMENTD) "/src/generic")]
			     )
		(fprintf (console-output-port)
			 "~%Error~a: ~a.~%"
			 (if who (format " in ~s" who) "")
			 (parameterize ([print-level 3] [print-length 6])
			   (apply format msg args)))

		(when (and (top-level-bound? 'REGIMENT-BATCH-MODE)
			   (top-level-value 'REGIMENT-BATCH-MODE))
		  (exit 1)) ;; Should only do this if we're running as a script.

		(fprintf (console-output-port)
			 "Entering debugger to inspect current continuation, type 'q' to exit.\n")		
		;; Set the current directory so that the inspector can find line numbers:
		(inspect k))))
   ))

(IF_GRAPHICS (fprintf stderr "(Linking GUI code using SWL.)\n")
	     (fprintf stderr "(No GUI available.)\n"))
(flush-output-port stderr)

(include "generic/util/reg_macros.ss") (import reg_macros)

;; A global parameter that is the equivalent of the eponymous
;; environment var.  Now that constants.ss is loaded we can set this. <br>
;; This uses the kinder behavior -- try the current directory.
;; (However, that's a bit irrelevent if an error was already signaled above.)
(REGIMENTD (default-regimentd))

(include "generic/util/hash.ss") (import hash) ;; TEMPORARY, used for "hash" function from slib.
;; Including full slib hash tables also... nesed equal?-based hashtabs sometime.
(include "generic/util/slib_hashtab.ss") (import (add-prefix slib_hashtab slib:))
(include "chez/hashtab.ss")      (import hashtab)
(include "generic/util/helpers.ss") (import (except helpers test-this these-tests))
(include "generic/util/streams.ss") (import (except streams test-this these-tests))
;; Not using these currently:
;(include "generic/util/imperative_streams.ss") ;(import (except imperative_streams test-this these-tests))

;; These provide some more utility code related to threads:
(IF_THREADS 
 (begin (printf "Configuring for multithreaded execution.\n")
	(include "chez/threaded_utils.ss") 
	(import threaded_utils)
	(init-par 2) ;; TODO: PUT IN CORRECT NUMBER OF CPUS!
	)
 ;; Otherwise provide a dummy implementation of "par":
 (begin (define par list)
	(define par-list (lambda (th*) (map (lambda (th) (th)) th*)))
	(define par-map map)
	)
 )

;; Lists all the Regiment primitives and their types:
(include "generic/compiler_components/prim_defs.ss") (import prim_defs)

(include "generic/grammars/grammar_checker.ss") (import grammar_checker)
(include "generic/compiler_components/regiment_helpers.ss") (import (except regiment_helpers test-this these-tests))
(include "generic/util/tsort.ss") ;(import (except tsort test-this these-tests))
(include "chez/pregexp.ss") (import pregexp_module)

(include "generic/compiler_components/c_generator.ss") (import c_generator)
;(include "generic/util/scheme_fft.ss")
(include "generic/util/slib_fft.ss")
(include "generic/util/fft.ss") (import fft)

(IFWAVESCOPE (begin)	     
  (begin (include "generic/sim/simulator_alpha_datatypes.ss") (import simulator_alpha_datatypes)))

;; Load this before the simulator.
(IF_GRAPHICS
    (begin
      ;; Only for swl1.0+.  Gives us define-class, etc.
      (import swl:oop) 
      (import swl:generics) 
      (import (except swl:macros mvlet))
      (import swl:option)
      (import swl:threads)

      (include "chez/basic_graphics.ss")
      (include "chez/graphics_stub.ss")
      (import basic_graphics)
      (import graphics_stub))
    ;; Otherwise, throw in some stubs that are invoked by the generated code:
    (begin ;; [2006.03.01] Nixing these.  Instead we should be disciplined about not generating any such calls.
           ;(define-syntax draw-mark (syntax-rules () [(_ x ...) (begin x ... 'nogui-stub)]))
	   ;(define-syntax  make-rgb (syntax-rules () [(_ x ...) (begin x ... 'nogui-stub)]))
	   ))


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

;; [2007.03.13] Haven't been using this recently, disabling it due to Windows incompatibility:
;; Only make a system call once to sync us with the outside world.
;(define current-time (seconds-since-1970))
;======================================================================



;; Don't use these yet from WS:
(IFWAVESCOPE
 (begin)
 (begin 
   (include "generic/compiler_components/logfiles.ss") (import logfiles)

   (include "generic/sim/alpha_lib.ss") 
   (import alpha_lib) ;; [2005.11.03] FIXME Temporary, reactivating this... shouldn't need to be on.

   (include "generic/sim/alpha_lib_scheduler_simple.ss") ;(import alpha_lib_scheduler_simple)
					;(include "generic/alpha_lib_scheduler.ss")

   (include "generic/sim/simulator_alpha.ss") (import simulator_alpha)
   (include "generic/sim/firelightning_sim.ss")
   (include "generic/passes/nesc_bkend/tossim.ss")
   ))

;(include "../reg_grammar.ss")

;; Type inference is used by verify-regiment, below.
(include "generic/compiler_components/hm_type_inference.ss") (import hm_type_inference)
;(include "generic/prim_defs_OLD.ss")
;(import prim_defs_OLD) ;; TEMP

;; This is used by the subsequent passes that process TML:
(IFWAVESCOPE (begin) 
   (begin (include "generic/compiler_components/tml_generic_traverse.ss")
	  (import tml_generic_traverse)))
(include "generic/compiler_components/reg_core_generic_traverse.ss") (import reg_core_generic_traverse)
(include "generic/passes/pass-mechanism.ss") (import pass-mechanism)

;; Load this pass early because it's used in a couple places.
(IFWAVESCOPE (begin)  
  (begin (include "generic/passes/tokmac_bkend/cleanup-token-machine.ss") (import cleanup-token-machine)))

;(define prim_random #%random) ;; Lame hack to get around slib's messed up random.
;(define (random-real) (#%random 1.0)) ;; Lame hack to get around slib's messed up random.
(include "generic/langs/language-mechanism.ss")

(include "generic/langs/lang_wavescript.ss")
(include "../../depends/matpak.ss") (import matpak)
;(include "generic/sim/wavescript_sim_library.ss")      ;; TODO: remove
;(include "generic/sim/wavescript_sim_library_NEW.ss")  ;; TODO: remove
(include "generic/sim/wavescript_sim_library_push.ss")
(include "generic/testing/lang_wavescript_tests.ss")

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

(if VERBOSE-LOAD (printf "  Midway through, doing passes...\n"))

(include "generic/passes/normalize_source/verify-regiment.ss")          (import verify-regiment)
(include "generic/passes/normalize_source/typecheck.ss")                (import typecheck)
(include "generic/passes/normalize_source/desugar-pattern-matching.ss") (import desugar-pattern-matching)

;;  For loading regiment source.  Depends on desugar-pattern-matching:
(include "generic/compiler_components/source_loader.ss") (import source_loader) 

(include "generic/passes/normalize_source/resolve-varrefs.ss") (import resolve-varrefs)
(include "generic/passes/normalize_source/ws-label-mutable.ss") (import ws-label-mutable)
(include "generic/passes/normalize_source/rename-vars.ss") (import rename-vars)
(include "generic/passes/normalize_source/eta-primitives.ss") (import eta-primitives)
(include "generic/passes/normalize_source/desugar-misc.ss") (import desugar-misc)
(include "generic/passes/normalize_source/remove-unquoted-constant.ss") (import remove-unquoted-constant)

(include "generic/passes/static_elaborate/static-elaborate.ss")  (import static-elaborate)
(include "generic/passes/static_elaborate/degeneralize-arithmetic.ss")  (import degeneralize-arithmetic)
(include "generic/passes/static_elaborate/verify-elaborated.ss") (import verify-elaborated)

(include "generic/passes/normalize_query/reduce-primitives.ss") (import reduce-primitives)

(include "generic/passes/wavescope_bkend/merge-iterates.ss") (import merge-iterates)
(include "generic/passes/wavescope_bkend/purify-iterate.ss") (import purify-iterate)
(include "generic/passes/wavescope_bkend/flatten-iterate-spine.ss") (import flatten-iterate-spine)

(include "generic/passes/wavescope_bkend/anihilate-higher-order.ss") (import anihilate-higher-order)

(include "generic/passes/normalize_query/remove-complex-constant.ss") (import remove-complex-constant)
; pass07_verify-stage2.ss
(include "generic/passes/normalize_query/uncover-free.ss") (import uncover-free)

(include "generic/passes/normalize_query/lift-letrec.ss")          (import lift-letrec)
(include "generic/passes/normalize_query/lift-letrec-body.ss")     (import lift-letrec-body)
(include "generic/passes/normalize_query/remove-complex-opera.ss") (import remove-complex-opera)
(include "generic/passes/normalize_query/ws-remove-letrec.ss") (import ws-remove-letrec)
(include "generic/passes/normalize_query/ws-remove-complex-opera.ss") (import ws-remove-complex-opera)
(include "generic/passes/normalize_query/ws-lift-let.ss") (import ws-lift-let)
(include "generic/passes/normalize_query/remove-lazy-letrec.ss")   (import remove-lazy-letrec)
(include "generic/passes/normalize_query/verify-core.ss")          (import verify-core)

(IFWAVESCOPE (begin)
  (begin
    (include "generic/passes/analyze_query/classify-names.ss")   (import classify-names)
    (include "generic/passes/analyze_query/add-heartbeats.ss")   (import add-heartbeats)
    (include "generic/passes/analyze_query/add-control-flow.ss") (import add-control-flow)
    (include "generic/passes/analyze_query/add-places.ss")       (import add-places)
    (include "generic/passes/analyze_query/analyze-places.ss")   (import analyze-places)

    (include "generic/passes/analyze_query/resolve-fold-trees.ss") (import resolve-fold-trees)
					;(include "generic/pass18_add-routing.ss")

    (include "generic/passes/deglobalize/deglobalize.ss") (import deglobalize)

    (include "generic/passes/deglobalize/deglobalize2.ss") (import deglobalize2)
    (include "generic/passes/deglobalize/deglobalize2_tmgen.ss")

    ;; Uses delazy-bindings:
    (include "generic/passes/analyze_query/add-data-flow.ss")      (import add-data-flow)

					;(include "generic/pass22_desugar-soc-return.ss")
    ;; TODO: Merge with pass22, besides this isn't really 26 anyway!
    (include "generic/passes/tokmac_bkend/desugar-macros.ss")        (import desugar-macros)
    (include "generic/passes/tokmac_bkend/find-emittoks.ss")         (import find-emittoks)
    (include "generic/passes/tokmac_bkend/desugar-gradients.ss")     (import desugar-gradients)
					;(include "generic/passes/tokmac_bkend/desugar-gradients_verbose.ss")
					;(include "generic/passes/tokmac_bkend/desugar-gradients_simple.ss")
					;(include "generic/passes/tokmac_bkend/desugar-gradients_ETX.ss")

    (include "generic/passes/tokmac_bkend/desugar-let-stored.ss") (import desugar-let-stored)
    (include "generic/passes/tokmac_bkend/rename-stored.ss")      (import rename-stored)

					;(include "generic/pass24_analyze-calls.ss")
					;(include "generic/pass25_inline.ss")
					;(include "generic/pass26_prune-returns.ss")
    (include "generic/passes/tokmac_bkend/cps-tokmac.ss")       (import cps-tokmac) 
    (include "generic/passes/tokmac_bkend/sever-cont-state.ss") (import sever-cont-state)
    ;; (include "generic/pass27.2_add-kclosure.ss")
    (include "generic/passes/tokmac_bkend/closure-convert.ss")  (import closure-convert)
    (include "generic/passes/tokmac_bkend/inline-tokens.ss")    (import inline-tokens)

    ;; This is out of use, but not deleted yet:
    (include "generic/scrap/pass30_haskellize-tokmac.ss")

    (include "generic/passes/nesc_bkend/flatten-tokmac.ss")     (import flatten-tokmac)
    (include "generic/passes/nesc_bkend/emit-nesc.ss")          (import emit-nesc)
    ))


;; These are miscellaneous small passes used by wavescript:
(include "generic/passes/small-ws-passes.ss")             (import small-ws-passes)

;; [2006.08.27] Now for the passes in the WaveScript branch:
(include "generic/passes/wavescope_bkend/type-annotate-misc.ss") (import type-annotate-misc)
(include "generic/passes/wavescope_bkend/nominalize-types.ss") (import nominalize-types)
(include "generic/passes/wavescope_bkend/emit-c.ss")           (import emit-c)

(include "generic/passes/wavescope_bkend/explicit-stream-wiring.ss") (import explicit-stream-wiring)
(include "generic/passes/ocaml_bkend/emit-caml.ss")           (import emit-caml)


;(inspect (emit-caml-wsquery caml-example))

;(load "../depends/slib/chez.init")
;(require 'tsort) ;; for the simulator: 

;; Basic parallel computation (using engines):
;; [2006.02.28] These were used by simulator_nought, but are not used currently.
(IF_GRAPHICS
    (load "chez/swl_flat_threads.ss") ;; uses swl threads instead
    (load "chez/flat_threads.ss"))

;; LAME:
;(if (top-level-bound? 'SWL-ACTIVE) (eval '(import flat_threads)))

;; If we're in SWL then load the GRAPHICS portion:
;; Disabled temporarily!:
#; 
(when (top-level-bound? 'SWL-ACTIVE)
      (load "demo_display.ss")
      (load "chez/simulator_nought_graphics.ss"))

(if VERBOSE-LOAD (printf "  Now for main file.\n"))

;;; NOW INCLUDE THE (GENERIC) MAIN FILE:
;===============================================================================
(include "main.ss")

(if VERBOSE-LOAD (printf "  Finished init routines in main file..\n"))

;;; TESTING FILES:
;===============================================================================

;; [2006.04.18] This testing system isn't doing much for us currently
;; other than exercising our static elaborator.
;; TODO: Fold it into the unit tests.
;;
;; Driver depends on 'pass-list being defined.
(include "generic/testing/driver.ss")
;  (game-eval (lambda args 'unspecified))
  (game-eval eval)
  (host-eval (lambda args 'unspecified))
(include "generic/testing/tests_noclosure.ss")
(include "generic/testing/tests.ss")



;; [2006.04.18] This is pretty out of date as well:
;; Load the repl which depends on the whole compiler and simulator.
(IFWAVESCOPE (begin) (include "generic/scrap/repl.ss"))

(include "generic/shortcuts.ss")


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


;; Open this up so we can read the global counters:
(IFWAVESCOPE (begin) (import simulator_alpha_datatypes))

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

     (let ([stats (statistics)])
       (printf "Pre-wipe mem usage: ~:d\n" (- (sstats-bytes stats) (sstats-gc-bytes stats))))
     (eval '(import scheme))

     ;; This is a simple version:
     (for-each (lambda (s)
		 (when (and (top-level-bound? s) 
			    (not (memq s '(wipe 
					   ;; Don't kill my little loading mechanism:
					   ;bl2 bln __UTILSDIR __utilsdir
					   ))))
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

;; This wipes bindings and reloads.
(define (reload) 
  ;(define main (++ (REGIMENTD) "/src/main_chez.ss"))
  (current-directory (REGIMENTD))
  (current-directory "src")
  ;(wipe) 
  ;(load main)
  (load "main_chez.ss")
  )

(if VERBOSE-LOAD (printf "  Finished loading... \n"))








#|
(define caml-example 
'(type-annotate-misc-language
  '(program
       (let ([tmp_1 (Stream Int)
		    (dataFile '"fake.txt" '"text" '44000 '0)])
	 tmp_1)
     (Stream Int))))

(define caml-example2
  (parameterize ([compiler-invocation-mode 'wavescript-simulator])
    (run-ws-compiler 
     (ws-postprocess
      '((define s1 (app audioFile "./countup.raw" 40 0 44000))
	(define s2
	  (iterate
	   (lambda (w ___VIRTQUEUE___)
	     (begin (emit ___VIRTQUEUE___ w) ___VIRTQUEUE___))
	   s1))
	(<- BASE s2))))))

(define caml-example3
  (parameterize ([compiler-invocation-mode 'wavescript-simulator])
    (run-ws-compiler 
     (ws-postprocess
      '((<-
	 BASE
	 (iterate
	  (lambda (#0() ___VIRTQUEUE___)
	    (begin (emit ___VIRTQUEUE___ 39) ___VIRTQUEUE___))
	  (app timer 30.0))))
      ))))

(newline)(display (text->string (emit-caml-wsquery (explicit-stream-wiring caml-example3))))(newline)
|#
