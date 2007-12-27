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

;;; TEMP TEMP TEMP 
;  (compile-profile #t)
  ;; Note: this also bloats the code size. 

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
	       [else (error 'regiment-compiler "bad setting for REGOPTLVL: <~s>" (REGOPTLVL))])))

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
			#'(define-top-level-value 'svn-revision (let ([x 'revis]) (if (number? x) x #f))))
		      )
		    ))))])))

(bind-svn-revision)


;======================================================================
;;; Setup stuff.

;; [2006.11.24] This is just a temporary thing so I can watch how fast things load.
(define VERBOSE-LOAD #f)

;(include "./config.ss") ;; Need to do this for windows... haven't got a good system yet.
(include "config.ss")

;; Set some Chez scheme parameters.
(print-graph #t )
(print-gensym #f)
;(print-level 8)
(print-level #f) ;20)
(print-length #f) ;80)
(print-vector-length #t)
;(pretty-maximum-lines 700)
(pretty-maximum-lines #f)
(pretty-line-length 150)
(pretty-standard-indent 0)

;; Storing and then modifying the default break handler.
;; This is just a little hack to let us break "on" a value and then continue.
;; ...<removed>...
;; [2006.08.28] Nixing that hack.  It's much better to just have our own inspect function:
(define (inspect/continue x) (inspect x) x)

;; Debugging, find out where a spurious inspect is!!
(define (debug-inspect x) (warning 'inspect "inspector called! ~s" x) (call/cc #%inspect))
#;
(begin 
  (optimize-level 0)
  (define-top-level-value 'inspect debug-inspect)
  (define-top-level-value 'inspect/continue debug-inspect))

;; Because I run from command line, it helps to enter the inspector after an error.
;; First let's backup the existing error-handler:
(unless (top-level-bound? 'default-error-handler)
  (define-top-level-value 'default-error-handler (error-handler)))

(define inspector-error-handler
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
		(inspect k))))))

(error-handler inspector-error-handler)

;; A Chez hack to see all the source locations in a continuation.
(define (continuation->sourcelocs k)
    (let loop ([depth 0] [ob (inspect/object k)])
      (when (> (ob 'depth) 1)
	(call-with-values (lambda () (ob 'source-path))
	  (lambda args
	    (when (= (length args) 3) ;; Success
	      (apply printf "~a: File: ~a, line ~a char ~a\n" depth args)
	      )))
	(loop (add1 depth) (ob 'link)))))
(define k->files continuation->sourcelocs)


;; This forces the output to standard error in spite the Scheme
;; parameters console-output-port and current-output-port.
(define stderr  
  (let ((buffer-size 1))
    (let ((p (make-output-port 2 (make-string buffer-size))))
      (set-port-output-size! p (- buffer-size 1))
      p)))



;; This is an (attempted) hack for maintaining clean seperation between repeated loads.
;;
;; [2006.03.27] This doesn't work yet, repeated loads still allocate more memory. 
;; Chez garbage collects compiled code, right?
;; Ahh, Chez doesn't collect space used by environments.
;; So I WOULD need that part that tries to wipe all the bindings.
(define (wipe) ;; shorthand
  (define set-difference
    (lambda (set1 set2)
      (let loop ([set1 set1] [set2 set2])
	(cond
	 ((null? set1) '())
	 ((memq (car set1) set2) (loop (cdr set1) set2))
	 (else (cons (car set1)  (loop (cdr set1) set2)))))))
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
       (set-difference (environment-symbols (interaction-environment))
		       (environment-symbols (scheme-environment))))
     (collect (collect-maximum-generation))
     (let ([stats (statistics)]) 
       (printf "Post-wipe mem usage: ~:d\n" (- (sstats-bytes stats) (sstats-gc-bytes stats)))))))

;; This wipes bindings and reloads.
(define (reload) 
  ;(define main (** (REGIMENTD) "/src/main_chez.ss"))
  (current-directory (REGIMENTD))
  (current-directory "src")
  ;(wipe) 
  ;(load main)
  (load "main_chez.ss")
  )



;======================================================================
;;; Print a banner message.

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

;======================================================================
;;; Begin loading files.  First some setup stuff.

(eval-when (compile load eval) 
  (define-top-level-value 'pre-load-directory (current-directory))
  (current-directory (string-append (default-regimentd) "/src/chez")))

(include "chez/match.ss")      ;; Pattern matcher, dependency.
(include "chez/rn-match.ss")      ;; My version of the pattern matcher.

;; To completely DISABLE my new prototype matcher, uncomment this:
;; This may be useful for debugging, the iu-matcher gives better source locations.
;(alias rn-match-bak rn-match) (alias rn-match iu-match) ;; TEMPTOGGLE
;;
;; [2007.04.19] Currently, just having rn-match in the type-checker
;; plus the static elaborator bloats the code size a noticable amount.

;; Import the IU matcher globally:
(import iu-match)
;(import rn-match) ;; Can't yet use rn-match globally.

;; After this point, everything must use chez:module for native chez modules.
;; 'module' will become my chez/plt portable regiment modules.
(eval-when (load eval)
  (include "chez/regmodule.ss")  ;; Common module syntax.
  (import reg:module)  
  )

;======================================================================
;;; Now begin loading Regiment/WaveScript proper. 


 ;; Load this first.  Widely visible constants/parameters.
(include "chez/chez_constants.ss")

;; A global parameter that is the equivalent of the eponymous
;; environment var.  Now that constants.ss is loaded we can set this. <br>
;; This uses the kinder behavior -- try the current directory.
;; (However, that's a bit irrelevent if an error was already signaled above.)
(REGIMENTD (default-regimentd))

(if VERBOSE-LOAD (printf "  Starting load...\n"))

(IF_GRAPHICS (fprintf stderr "(Linking GUI code using SWL.)\n")
	     (fprintf stderr "(No GUI available.)\n"))
(flush-output-port stderr)

(define (basename pathstr)
  ;; Everything after the last "#/"
  (define file (let loop ([ls (reverse! (string->list pathstr))])
		 (cond 
		  [(null? ls) ()]
		  [(eq? (car ls) #\/) ()]
		  [else (cons (car ls) (loop (cdr ls)))])))
  (list->string (reverse! (cdr (or (memq #\. file) (cons #\. file))))))

(alias todo:common:load-source include)

(define-syntax (common:load-source x)
  (syntax-case x () 
   [(_ file) 
    ;; Make sure we put the *real* symbol include in there.  
    ;; Having problems with this for some reason:
    (with-syntax ([incl (datum->syntax-object #'_ 'include)]
		  [modname (datum->syntax-object #'_
			     (string->symbol (basename (datum file))))])
      #'(begin (incl file)
	       (import modname)))
    ])
  )

#;
(define-syntax common:load-source
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (let ([p (open-input-file fn)])
          (let f ([x (read p)])
            (if (eof-object? x)
                (begin (close-input-port p) '())
                (cons (datum->syntax-object k x)
                      (f (read p))))))))
    (syntax-case x ()
      [(k filename)
       (let ([fn (datum filename)])
         (with-syntax ([(exp ...) (read-file fn #'k)])
           #'(begin exp ...)))])))

;;====================================================================================================;;
;;====================================================================================================;;
;;====================================================================================================;;
;; This loads the bulk of the source files.


(begin 

(todo:common:load-source "generic/util/reg_macros.ss") (import reg_macros)


(todo:common:load-source "generic/util/hash.ss") (import hash) ;; TEMPORARY, used for "hash" function from slib.
;; Including full slib hash tables also... nesed equal?-based hashtabs sometime.
(todo:common:load-source "generic/util/slib_hashtab.ss") (import (add-prefix slib_hashtab slib:))
(todo:common:load-source "chez/hashtab.ss")              (import hashtab) ;; eq? based.
(todo:common:load-source "generic/util/helpers.ss") (import (except helpers test-this these-tests))



;; These provide some more utility code related to threads:
(cond-expand
 [(and chez threads)
  (printf "Configuring for multithreaded execution.\n")
  (todo:common:load-source "chez/threaded_utils.ss")
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


(todo:common:load-source "generic/util/streams.ss") (import (except streams test-this these-tests))

;; Not using these currently:
;(common:load-source "generic/util/imperative_streams.ss") ;(import (except imperative_streams test-this these-tests))



                                   (include "common_loader.ss")






(import prim_defs)

(printf "PRIMDEFS LOADED\n")(flush-output-port)

;; Lists all the Regiment primitives and their types:
(todo:common:load-source "generic/compiler_components/prim_defs.ss") (import prim_defs)
(todo:common:load-source "generic/compiler_components/regiment_helpers.ss") (import (except regiment_helpers test-this these-tests))

;; [2007.04.30] The "type?" predicate is currently used in grammars.ss
(todo:common:load-source "generic/compiler_components/type_environments.ss") (import type_environments)

(todo:common:load-source "generic/compiler_components/annotations.ss") (import annotations)

(todo:common:load-source "generic/grammars/grammar_checker.ss") (import grammar_checker)
(todo:common:load-source "generic/util/tsort.ss") ;(import (except tsort test-this these-tests))
(todo:common:load-source "chez/pregexp.ss") (import pregexp_module)

(todo:common:load-source "generic/compiler_components/c_generator.ss") (import c_generator)
(todo:common:load-source "generic/util/scheme_fft.ss") ;; FFT from the chez users guide
(todo:common:load-source "generic/util/slib_fft.ss")   ;; FFT from slib.
(todo:common:load-source "generic/util/fft.ss") (import fft)

(IFWAVESCOPE (begin)	     
  (begin (todo:common:load-source "generic/sim/simulator_alpha_datatypes.ss") (import simulator_alpha_datatypes)))

;; Load this before the simulator.
(IF_GRAPHICS
    (begin
      ;; Only for swl1.0+.  Gives us define-class, etc.
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
   (todo:common:load-source "generic/compiler_components/logfiles.ss") (import logfiles)

   (todo:common:load-source "generic/sim/alpha_lib.ss") 
   (import alpha_lib) ;; [2005.11.03] FIXME Temporary, reactivating this... shouldn't need to be on.

   (todo:common:load-source "generic/sim/alpha_lib_scheduler_simple.ss") ;(import alpha_lib_scheduler_simple)
					;(todo:common:load-source "generic/alpha_lib_scheduler.ss")

   (todo:common:load-source "generic/sim/simulator_alpha.ss") (import simulator_alpha)
   (todo:common:load-source "generic/sim/firelightning_sim.ss")
   (todo:common:load-source "generic/passes/nesc_bkend/tossim.ss")
   ))

(IFWAVESCOPE (begin) 
   ;; This is used by the subsequent passes that process TML:
   (begin (todo:common:load-source "generic/compiler_components/tml_generic_traverse.ss")
	  (import tml_generic_traverse)))
(todo:common:load-source "generic/compiler_components/reg_core_generic_traverse.ss") (import reg_core_generic_traverse)

;; Type inference is used by verify-regiment, below.
(todo:common:load-source "generic/compiler_components/hm_type_inference.ss") (import hm_type_inference)

(todo:common:load-source "generic/passes/pass-mechanism_basic.ss") (import pass-mechanism_basic)
(todo:common:load-source "generic/passes/pass-mechanism.ss") (import pass-mechanism)
(todo:common:load-source "generic/passes/graphviz.ss") (import graphviz)


;; Load this pass early because it's used in a couple places.
(IFWAVESCOPE (begin)  
  (begin (todo:common:load-source "generic/passes/tokmac_bkend/cleanup-token-machine.ss") (import cleanup-token-machine)))

;(define prim_random #%random) ;; Lame hack to get around slib's messed up random.
;(define (random-real) (#%random 1.0)) ;; Lame hack to get around slib's messed up random.
(todo:common:load-source "generic/langs/language-mechanism.ss")

(todo:common:load-source "generic/langs/lang_wavescript.ss")(import lang_wavescript)
(todo:common:load-source "../../depends/matpak.ss") (import matpak)
;(todo:common:load-source "generic/sim/wavescript_sim_library.ss")      ;; TODO: remove
;(todo:common:load-source "generic/sim/wavescript_sim_library_NEW.ss")  ;; TODO: remove
(todo:common:load-source "generic/sim/wavescript_sim_library_push.ss")
(todo:common:load-source "generic/testing/lang_wavescript_tests.ss") (import lang_wavescript_tests)

(todo:common:load-source "generic/langs/lang00.ss")

(todo:common:load-source "generic/langs/lang06_uncover-free.ss")
(todo:common:load-source "generic/langs/lang07_lift-letrec.ss")

(IFWAVESCOPE (begin)
  (begin 
    (todo:common:load-source "generic/langs/lang11_classify-names.ss")
    (todo:common:load-source "generic/langs/lang12_heartbeats.ss")
    (todo:common:load-source "generic/langs/lang13_control-flow.ss")
    (todo:common:load-source "generic/langs/lang14_places.ss")

    (todo:common:load-source "generic/langs/lang20_deglobalize.ss") 

    (todo:common:load-source "generic/scrap/lang30_haskellize-tokmac.ss") 
    (todo:common:load-source "generic/langs/lang32_emit-nesc.ss")))

(if VERBOSE-LOAD (printf "  Midway through, doing passes...\n"))

(todo:common:load-source "generic/passes/normalize_source/verify-regiment.ss")          (import verify-regiment)
(todo:common:load-source "generic/passes/normalize_source/typecheck.ss")                (import typecheck)
(todo:common:load-source "generic/passes/normalize_source/desugar-pattern-matching.ss") (import desugar-pattern-matching)

;;  For loading regiment source.  Depends on desugar-pattern-matching:
(todo:common:load-source "generic/compiler_components/source_loader.ss") (import source_loader) 


(todo:common:load-source "generic/passes/optimizations/smoosh-together.ss") (import smoosh-together)
(todo:common:load-source "generic/passes/optimizations/rewrite_opts.ss") (import rewrite_opts)
(todo:common:load-source "generic/passes/optimizations/data_reps.ss")


(todo:common:load-source "generic/passes/normalize_source/resolve-varrefs.ss") (import resolve-varrefs)
(todo:common:load-source "generic/passes/normalize_source/ws-label-mutable.ss") (import ws-label-mutable)
(todo:common:load-source "generic/passes/normalize_source/rename-vars.ss") (import rename-vars)
(todo:common:load-source "generic/passes/normalize_source/eta-primitives.ss") (import eta-primitives)
(todo:common:load-source "generic/passes/normalize_source/desugar-misc.ss") (import desugar-misc)
(todo:common:load-source "generic/passes/normalize_source/remove-unquoted-constant.ss") (import remove-unquoted-constant)

;(eval-when (compile eval load) (compile-profile #t))
(todo:common:load-source "generic/passes/static_elaborate/static-elaborate.ss")  (import static-elaborate)
(todo:common:load-source "generic/passes/normalize_query/reduce-primitives.ss") (import reduce-primitives)
(todo:common:load-source "generic/passes/normalize_query/ws-remove-letrec.ss") (import ws-remove-letrec)
(todo:common:load-source "generic/passes/static_elaborate/interpret-meta.ss")  (import interpret-meta)
;(eval-when (compile eval load) (compile-profile #f))

(todo:common:load-source "generic/passes/static_elaborate/degeneralize-arithmetic.ss")  (import degeneralize-arithmetic)
(todo:common:load-source "generic/passes/static_elaborate/split-union-types.ss")  (import split-union-types)
(todo:common:load-source "generic/passes/static_elaborate/verify-elaborated.ss") (import verify-elaborated)


(todo:common:load-source "generic/passes/optimizations/merge-iterates.ss") (import merge-iterates)
(todo:common:load-source "generic/passes/optimizations/simple-merge-iterates.ss") (import simple-merge-iterates)
(todo:common:load-source "generic/passes/wavescope_bkend/purify-iterate.ss") (import purify-iterate)
(todo:common:load-source "generic/passes/wavescope_bkend/flatten-iterate-spine.ss") (import flatten-iterate-spine)

(todo:common:load-source "generic/passes/wavescope_bkend/anihilate-higher-order.ss") (import anihilate-higher-order)

(todo:common:load-source "generic/passes/normalize_query/remove-complex-constant.ss") (import remove-complex-constant)
; pass07_verify-stage2.ss
(todo:common:load-source "generic/passes/normalize_query/uncover-free.ss") (import uncover-free)

(todo:common:load-source "generic/passes/normalize_query/lift-letrec.ss")          (import lift-letrec)
(todo:common:load-source "generic/passes/normalize_query/lift-letrec-body.ss")     (import lift-letrec-body)
(todo:common:load-source "generic/passes/normalize_query/remove-complex-opera.ss") (import remove-complex-opera)



;(eval-when (compile eval load) (compile-profile #t))
(todo:common:load-source "generic/passes/normalize_query/ws-remove-complex-opera.ss") (import ws-remove-complex-opera)
;(eval-when (compile eval load) (compile-profile #f))

(todo:common:load-source "generic/passes/normalize_query/ws-lift-let.ss") (import ws-lift-let)
(todo:common:load-source "generic/passes/normalize_query/ws-normalize-context.ss") (import ws-normalize-context)
(todo:common:load-source "generic/passes/normalize_query/remove-lazy-letrec.ss")   (import remove-lazy-letrec)
(todo:common:load-source "generic/passes/normalize_query/verify-core.ss")          (import verify-core)

(IFWAVESCOPE (begin)
  (begin
    (todo:common:load-source "generic/passes/analyze_query/classify-names.ss")   (import classify-names)
    (todo:common:load-source "generic/passes/analyze_query/add-heartbeats.ss")   (import add-heartbeats)
    (todo:common:load-source "generic/passes/analyze_query/add-control-flow.ss") (import add-control-flow)
    (todo:common:load-source "generic/passes/analyze_query/add-places.ss")       (import add-places)
    (todo:common:load-source "generic/passes/analyze_query/analyze-places.ss")   (import analyze-places)

    (todo:common:load-source "generic/passes/analyze_query/resolve-fold-trees.ss") (import resolve-fold-trees)
					;(todo:common:load-source "generic/pass18_add-routing.ss")

    (todo:common:load-source "generic/passes/deglobalize/deglobalize.ss") (import deglobalize)

    (todo:common:load-source "generic/passes/deglobalize/deglobalize2.ss") (import deglobalize2)
    (todo:common:load-source "generic/passes/deglobalize/deglobalize2_tmgen.ss")

    ;; Uses delazy-bindings:
    (todo:common:load-source "generic/passes/analyze_query/add-data-flow.ss")      (import add-data-flow)

					;(todo:common:load-source "generic/pass22_desugar-soc-return.ss")
    ;; TODO: Merge with pass22, besides this isn't really 26 anyway!
    (todo:common:load-source "generic/passes/tokmac_bkend/desugar-macros.ss")        (import desugar-macros)
    (todo:common:load-source "generic/passes/tokmac_bkend/find-emittoks.ss")         (import find-emittoks)
    (todo:common:load-source "generic/passes/tokmac_bkend/desugar-gradients.ss")     (import desugar-gradients)
					;(todo:common:load-source "generic/passes/tokmac_bkend/desugar-gradients_verbose.ss")
					;(todo:common:load-source "generic/passes/tokmac_bkend/desugar-gradients_simple.ss")
					;(todo:common:load-source "generic/passes/tokmac_bkend/desugar-gradients_ETX.ss")

    (todo:common:load-source "generic/passes/tokmac_bkend/desugar-let-stored.ss") (import desugar-let-stored)
    (todo:common:load-source "generic/passes/tokmac_bkend/rename-stored.ss")      (import rename-stored)

					;(todo:common:load-source "generic/pass24_analyze-calls.ss")
					;(todo:common:load-source "generic/pass25_inline.ss")
					;(todo:common:load-source "generic/pass26_prune-returns.ss")
    (todo:common:load-source "generic/passes/tokmac_bkend/cps-tokmac.ss")       (import cps-tokmac) 
    (todo:common:load-source "generic/passes/tokmac_bkend/sever-cont-state.ss") (import sever-cont-state)
    ;; (todo:common:load-source "generic/pass27.2_add-kclosure.ss")
    (todo:common:load-source "generic/passes/tokmac_bkend/closure-convert.ss")  (import closure-convert)
    (todo:common:load-source "generic/passes/tokmac_bkend/inline-tokens.ss")    (import inline-tokens)

    ;; This is out of use, but not deleted yet:
    (todo:common:load-source "generic/scrap/pass30_haskellize-tokmac.ss")

    (todo:common:load-source "generic/passes/nesc_bkend/flatten-tokmac.ss")     (import flatten-tokmac)
    (todo:common:load-source "generic/passes/nesc_bkend/emit-nesc.ss")          (import emit-nesc)
    ))


;; These are miscellaneous small passes used by wavescript:
(todo:common:load-source "generic/passes/small-ws-passes.ss")             (import small-ws-passes)

;; [2006.08.27] Now for the passes in the WaveScript branch:
(todo:common:load-source "generic/passes/wavescope_bkend/reify-certain-types.ss") (import reify-certain-types)
(todo:common:load-source "generic/passes/wavescope_bkend/type-annotate-misc.ss") (import type-annotate-misc)
(todo:common:load-source "generic/passes/wavescope_bkend/convert-sums-to-tuples.ss") (import convert-sums-to-tuples)

(todo:common:load-source "generic/passes/wavescope_bkend/nominalize-types.ss") (import nominalize-types)
(todo:common:load-source "generic/passes/wavescope_bkend/emit-c.ss")           (import emit-c)
(todo:common:load-source "generic/passes/wavescope_bkend/emit-c2.ss")          (import emit-c2)

(todo:common:load-source "generic/passes/analyze_data_rates/annotate-with-data-rates.ss") (import annotate-with-data-rates)

(todo:common:load-source "generic/passes/wavescope_bkend/explicit-stream-wiring.ss") (import explicit-stream-wiring)

(eval-when (compile eval load) (compile-profile #t))
(todo:common:load-source "generic/passes/ocaml_bkend/shared-emit-ml.ss")      ;(import shared-emit-ml)
(todo:common:load-source "generic/passes/ocaml_bkend/emit-caml.ss")           (import emit-caml)
(todo:common:load-source "generic/passes/mlton_bkend/emit-mlton.ss")          (import emit-mlton)
(eval-when (compile eval load) (compile-profile #f))



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
(todo:common:load-source "main.ss")

(if VERBOSE-LOAD (printf "  Finished init routines in main file..\n"))

;;; TESTING FILES:
;===============================================================================

;; [2006.04.18] This testing system isn't doing much for us currently
;; other than exercising our static elaborator.
;; TODO: Fold it into the unit tests.
;;
;; Driver depends on 'pass-list being defined.

;; Let's not waste load time on this... it's not used currently.
(IFWAVESCOPE
    (begin)
  (begin 
    (todo:common:load-source "generic/testing/driver.ss")
    (game-eval eval)
    (host-eval (lambda args 'unspecified))
    (todo:common:load-source "generic/testing/tests_noclosure.ss")
    ))

(todo:common:load-source "generic/testing/tests.ss")

;; [2006.04.18] This is pretty out of date as well:
;; Load the repl which depends on the whole compiler and simulator.
(IFWAVESCOPE (begin) (todo:common:load-source "generic/scrap/repl.ss"))

(todo:common:load-source "generic/shortcuts.ss")

)

;; END COMMON LOADER
;;====================================================================================================;;
;;====================================================================================================;;
;;====================================================================================================;;



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


(if VERBOSE-LOAD (printf "  Finished loading... \n"))
(eval-when (compile load eval) 
  (current-directory (top-level-value 'pre-load-directory)))


