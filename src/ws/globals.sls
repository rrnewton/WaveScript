#!r6rs

;;;; .title Constants.ss -- A collection of global constants, flags, parameters, and datatype defs
;;;; .author Ryan Newton

;;;; A collection of global constants, flags, and datatype defs.<br><br>


;;;; NOTE: also see DEBUGMODE from helpers.ss.  It's a global syntax definition of interest. <br>
;;;; [2005.03.29] MOVING DEBUGMODE to this file. <br><br>

;;;; [2005.02.24] <br>
;;;; Now I'm keeping some parameters (which are actually not constant) in here as well. 
;;;; <br><br>

;;;; [2004.07.02] <br>
;;;; Hmm, I considered making this a while ago.  But I never know
;;;; whether to keep constants module-local or lift them out like this.
;;;; I'm just going to settle for an ad-hoc strategy, anything that
;;;; needs to (or might need to) be used in more than one module will
;;;; get lifted up here. <br><br>

(library (ws globals)
    (export

	 define-wavescript-parameter wavescript-parameters reg:make-parameter
         WAVESCRIPTD  
	 ;define-testing
	 IFWAVESCOPE ;; Load WS extensions or no?
	 IFDEBUG IFWINDOWS

	 ;; Chez/PLT specific (included from respective modules).
	 reg:define-struct ;;;reg:struct? reg:struct->list reg:list->struct 
	 ;IFCHEZ IF_GRAPHICS cond-expand 
	 IF_THREADS
;	 reg:include	 

	 
         ;; Syntax:
	  VERBOSE-LOAD
         DEBUGMODE UBERDEBUGMODE  DEBUGASSERT ASSERT 
         WAVESCRIPT_DEBUG HACK wavescript-emit-debug check-pass-grammars mlton-ascribe-types
	 REGOPTLVL

         SCHEDULE_DELAY         
         RADIO_DELAY ;PROCESSING_TIME ;; Not used yet

         inferencer-let-bound-poly
         inferencer-enable-LUB
	 included-var-bindings
	 default-case-symbol
         
	 compiler-invocation-mode 
	 wsc2-gc-mode wsc2-sigseg-mode
	 embedded-mode? wsc2-variant-mode? java-mode?
;	 wavescript-compile-sums-as-tuples
	 wavescript-verbosity 
	 wavescript-track-source-locations
	 wavescript-current-pass

	 wsint-tuple-limit
	 wsint-output-file
	 wsint-time-query
	 wserror-handler
	 
	 simulation-logger 
	 simulation-logger-count
	 simulation-logger-level
	 simulation-logger-human-readable
	 simulation-logger-fasl-batched
	 simulation-logger-gzip-output
	 reg:all-unit-tests reg:all-tester-names
	 reg:comment-code
         
         pass-list
         
         default-slow-pulse default-fast-pulse
         
         deglobalize-markup-returns
         
         unknown-place noplace

	 MAX_TUPLE_SIZE
         
	 TMNULL
         KINIT_FLAG KCALL_FLAG NULLK
	 DEFAULT_SUBTOK DEFAULT_SUBTOK_VAR
         MAX_SUBTOK        

	 desugar-gradients-mode 
	 etx-retry-delay 
	 etx-max-retries

         window-width window-height
         processor-screen-radius
	 set-procesor-screen-radius!
         world-xbound world-ybound radius numsimnodes SPECIAL_RETURN_TOKEN 
         BASE_ID NULL_ID	 
       	 return-window-size
         
         sim-num-nodes
	 sim-startup-stagger
	 sim-timeout

         simalpha-output-port
	 simalpha-outer-radius
	 simalpha-inner-radius
	 simalpha-placement-type
	 simalpha-max-gridlike-perturbation
	 simalpha-world-xbound
	 simalpha-world-ybound
	 simalpha-channel-model
	 simalpha-failure-model
	 simalpha-current-simworld 
	 simalpha-preset-topology
	 simalpha-realtime-mode
	 simalpha-consec-ids
	 simalpha-dbg-on
	 simalpha-zeropad-args
	 simalpha-stream-result 
	 simalpha-sense-function
	 simalpha-sense-function-constructor
	 simalpha-graphics-on
         simulator-write-sims-to-disk
	 simalpha-generate-modules
	 simalpha-label-msgcounts 
	 simalpha-label-sensorvals 
	 simalpha-pause-hook

	 suppress-main-stream-printing
         ws-print-output-port ;; For the WS simulator.
	 ws-no-prelude
	 ws-optimizations-enabled
	 ws-optimization-level
	 ws-profile-limit
	 ws-full-inline
	 ws-compiler-hooks

	 dump-graphviz-output
	 ws-alternate-return-stream
	 abort-compiler-continuation

	 varied-param
	 dummy-param

	 foreign-guardian

         make-rgb rgb?   rgb-red    rgb-green  rgb-blue

	 Default-Drawing-Color
	 Default-Window-Color 
	 Default-Window-Text-Color
	 Default-Background-Color 
	 Default-Canvas-Text-Color
	 Default-Supertext-Color  
	 Default-Subtext-Color    
	 Default-LED-Off-Color      
	 Default-Node-Color       
	 Default-Base-Border-Color
	 Default-Edge-Full-Color  
	 Default-Edge-Dead-Color  
	 Default-Line-Highlight-Color
	 Default-Mark-Color          
	 Default-Proc-Border-Color   
	 Default-Mouse-Highlight-Color 

	 default-unit-tester-retries

	 nodeid?

	 int16? int32? int64? uint8? uint16?
	 basename dirname

	 make-sigseg sigseg? sigseg-start sigseg-end sigseg-vec sigseg-timebase
	 make-tuple  tuple? (rename (safe-tuple-fields tuple-fields)) 
	 make-wsrecord wsrecord-pairs wsrecord-select wsrecord-extend wsrecord-restrict empty-wsrecord wsrecord? 
	 make-timebase timebase-num timebase?
         make-uniontype uniontype-tag uniontype-val  uniontype?
	 make-double double-val double?
	 
	 bench-stats? make-bench-stats  ;bench-stats 
	 bench-stats-bytes bench-stats-tuples bench-stats-cpu-time
	 set-bench-stats-bytes! set-bench-stats-tuples! set-bench-stats-cpu-time!

         ) ;; End export

	(import (except (rnrs (6)) error)
		(for (ws compat compat) run expand)
		;(for (ws compat compat) run expand (meta 1) (meta 2))
	 )
	
  ;; No support in my R6RS-ish port yet:
  (define-syntax IF_THREADS
    (syntax-rules (chez plt) 
      [(_ a b) b]
      [(_ a)   (void)]))

#;
  (define-syntax IF_THREADS
    (syntax-rules (chez plt) 
      [(_ a b) (cond-expand [(and chez threads) a] [else b])]
      [(_ a)   (cond-expand [(and chez threads) a] [else (void)])]))

  ;; TODO: siwtch for when we're running under windows:
  (define-syntax IFWINDOWS (syntax-rules () [(_ a) (void)]))
	
;=======================================================================

;;; Regiment parameters.

(define reg:make-parameter make-parameter)
;(define-syntax reg:make-parameter (identifier-syntax (hash-percent make-parameter)))


;; In the following manner we distinguish wavescript parameters from normal
;; parameters.  We keep a list of all the existing wavescript
;; parameters.  And it also makes it more acceptable for us to scatter
;; around the parameter definitions, because you can grep for
;; define-wavescript-parameter.
(define wavescript-parameters (reg:make-parameter '()))
(define-syntax define-wavescript-parameter
  (syntax-rules () 
    [(_ name args ...)
     (define name 
       (begin (wavescript-parameters (cons (quote name) (wavescript-parameters)))
	      (reg:make-parameter args ...)))]))


;; Parameter determining the location of the Regiment tree. <br>
;; This is set when the system loads.
(define-wavescript-parameter WAVESCRIPTD "./")


;=======================================================================;;
;; [2006.09.11] Factored some config parameters that the user will want to change.

;; [2009.12.12] Having problems with PLT 4.2.2.
;; Pasting the contents of config.ss seems to change the behavior.
;(include "config.ss")
(begin


;; This file contains configuration options that users of the
;; WaveScript compiler might want to change.

;; This file may be loaded more than once.
;; MORE (and detailed) configuration options can be found in globals.ss

;; The IFDEBUG/DEBUGMODE toggles are like a #define that toggles debug code for the whole compiler.
;; This is not a very appropriate place for this definition, but it's the most convenient
;; so that it can be seen from everywhere.
;; <br><br>
(define-syntax IFDEBUG 
  (lambda (x)
    ;===============================;
    (define DEFAULT_IS_DEBUG_MODE #f) ;; <-- CHANGE DEFAULT HERE
    ;===============================;
    (define (on)  (syntax-case x () [(_ debon deboff) #'debon]))
    (define (off) (syntax-case x () [(_ debon deboff) #'deboff]))
    (cond
     [(getenv "REGDEBUGMODE") => 
      (lambda (m) 
	(if (or (equal? m "0") (equal? m "OFF") (equal? m ""))
	     (off) (on)))]
     [else (if DEFAULT_IS_DEBUG_MODE (on) (off))])))

;; [2006.08.28] This is the "#define" for building WAVESCOPE related code.
;; When turned off, the system should build Regiment without WaveScope extensions.
(define-syntax IFWAVESCOPE
  (let ()
    ;===============================;
    (define DEFAULT_WAVESCOPE-WAVESCRIPT_MODE "WAVESCRIPT") ;; <-- CHANGE DEFAULT HERE
    ;===============================;
    (define (ws x)  (syntax-case x () [(_ ws reg) #'ws]   [(_ ws) #'ws]))
    (define (reg x) (syntax-case x () [(_ ws reg) #'reg]  [(_ ws) #'(begin)]))
    (define (set m)
      ;(printf "  ENVVAR: ~s\n" m)
      (cond 
       [(equal? m "WAVESCRIPT") ws]
       [(equal? m "WAVESCOPE")  ws]
       [(equal? m "WS")         ws]
       [(equal? m "WAVESCRIPT")  reg]
       [(equal? m "REG")       reg]
       [(equal? m "BOTH")
	(lambda (x) (syntax-case x () [(_ ws reg) #'(begin ws reg)] [(_ ws) #'ws]))]

       [else 
      ;(error 'IFWAVESCOPE "unknown value for environment var WAVESCRIPT_OR_WAVESCRIPT: ~s" m)
       (raise (make-error))
       ]
       ))
    (cond
     [(getenv "WAVESCRIPT_OR_WAVESCRIPT") => (lambda (m) (set m))]
     [else (set DEFAULT_WAVESCOPE-WAVESCRIPT_MODE)]
     ;[else (error 'IFWAVESCOPE "environment var not set")]
     )))


;; [2006.09.11] This configures the scheme compiler when loading wavescript.
;; NOTE: this controls the optimization level for the COMPILER, and
;; also for the token machine simulator or the Scheme WS backend.
;; But it does *not* control the number of optimizations applied by
;; the compiler to the generated code.  
;;
;; Look at the parameter ws-optimizations-enabled for that.
;;
;;   0 -- mode for debugging 
;;   2 -- good performance but still safe
;;   3 -- unsafe optimizations, used for long running simulations.
(define (REGOPTLVL)
  (cond
   [(getenv "REGOPTLVL") => (lambda (str) (read (open-string-input-port str)))]
   [else 2]  ;; <-- CHANGE DEFAULT HERE
   ))
;; Note that this is separate from IFDEBUG above.


;; [2006.11.24] This is just a temporary thing so I can watch how fast things load.
(define VERBOSE-LOAD #f)


) ;; END HACK

;=======================================================================;;


;=======================================================================;;
;;                       <<< DEBUG TOGGLES >>>                          ;;
;=======================================================================;;
;;; Debug Toggles


;; DEBUGMODE is just syntactic sugar on top of IFDEBUG.  It contains
;; any number of subexpressions and executes them only when IFDEBUG is activated.
(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (IFDEBUG (list expr ...) '())]))

;; This is for debug annotations that take a really long time.
;; For now it's enabled at the same time that DEBUGMODE is.
(define-syntax UBERDEBUGMODE (syntax-rules () [(_ expr ...) (IFDEBUG (list expr ...) '())]))

(define-syntax DEBUGASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) 
       #'(DEBUGMODE
	  (if expr #t 
	      (error 'DEBUGASSERT "failed: ~s" 
		     (format-syntax-nicely #'expr))))]
      ;; This form is (ASSERT integer? x) returning the value of x.
      ;; NOTE!!! It still evaluates and returns the VALUE in nondebugmode, it just skips the predictae.
      [(_ fun val) #'(IFDEBUG 
		      (let ([v val])
		       (if (fun v) v			   
			   (error 'DEBUGASSERT 
				  (format "failed: ~s\n Value which did not satisfy above predicate: ~s" 
					  (format-syntax-nicely #'fun)
					  v))))
		      val)])))

(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) #'(or expr (error 'ASSERT (format "failed: ~s" (format-syntax-nicely #'expr))))]
      ;; This form is (ASSERT integer? x) returning the value of x.
      [(_ fun val) #'(ASSERT "" fun val)]
      [(_ str fun val) 
       #'(let ([v val])
	   (if (fun v) v			   
	       (error 'ASSERT 
		      (format "failed, ~s:\n ~s\n Value which did not satisfy above predicate: ~s" 
			      str
			      (format-syntax-nicely #'fun)
			      v))))])))

;(define Regiment-Log-File "~/tmp/Regiment.log.ss")
; ; ;(define Regiment-Log-File (string-append (current-directory) "/Regiment.log.ss"))
; ; ;(define Regiment-Log-File (string-append "Regiment.log.ss"))
; ; Delete that logfile at load time:
;(if (file-exists? Regiment-Log-File)
;    (delete-file Regiment-Log-File))


;=======================================================================;;
;;; Compiler Controls.

;; This parameter determines how much the compiler should print.
;; Here's a little summary that may not stay up-to-date.
;; Note, at >=2, full types are dumped to .__types.txt
;;  4 - Getting crazy.        - *all* variable types, not realiased
;;  3 - Extra debugging info. - included def's types, not realiased
;;  2 - Extra compiler output - included def's types, still realiased 
;;  1 - Normal.
;;  0 - Silent except for high priority warnings.
;; -1 - Absolutely silent.
(define-wavescript-parameter wavescript-verbosity 1)

;; This determines whether the compiler tracks source locations in loaded files.
(define-wavescript-parameter wavescript-track-source-locations #t)

;; It's useful to have this info globally for debugging/error messages.
(define wavescript-current-pass (reg:make-parameter #f))

(define valid-invocation-modes 
  '(wavescript-simulator  ;; Scheme backends: ws and ws.early
    wavescript-compiler-caml ;; also used for mlton
    wavescript-compiler-xstream
    wavescript-compiler-c
    wavescript-compiler-tbb
    wavescript-compiler-nesc
    wavescript-compiler-java
    wavescript-compiler-javame
    wavescript-simulator ;; [2007.03.20] This is now deprecated!!
    ))

(define-wavescript-parameter compiler-invocation-mode 
  'wavescript-simulator
  (lambda (x)
    (if (memq x valid-invocation-modes) x
	(error 'compiler-invocation-mode "invalid mode: ~s" x))))

(define (embedded-mode? mode)
  (memq mode
	'(wavescript-compiler-nesc
	  wavescript-compiler-javame)))

(define (java-mode? mode)
  (memq mode
	'(wavescript-compiler-java
	  wavescript-compiler-javame)))

;; These are the compiler variants that are based on emit-c2.ss
(define (wsc2-variant-mode? mode)
  (memq mode
	'(wavescript-compiler-c
	  wavescript-compiler-tbb
	  wavescript-compiler-nesc
	  wavescript-compiler-java
	  wavescript-compiler-javame)))

;; Options are:
;;  refcount - normal
;;  deferred - deferred refcount, unfinished
;;  boehm    - conservative collector
;;  none     - simply don't collect
(define-wavescript-parameter wsc2-gc-mode 'refcount)
;; [2008.08.27] Making this boehm for now because of a known bug with
;; simple 'refcount and closed mono-functions not inlined.

;; Options are:
;;  copyalways - sigsegs are contiguous arrays
;;  seglist    - lists of segments with sharing
(define-wavescript-parameter wsc2-sigseg-mode 'copyalways)

;; This parameter controls what optimizations the compiler applies.

;; It can be set by passing a -O2 style flag, or by specifically
;; enabling optimizations with -opt <NAME>.
;(define-wavescript-parameter ws-optimizations-enabled '(rewrites))
;; TEMPTOGGLE - turning maxinline on by default:
(define-wavescript-parameter ws-optimizations-enabled '())

;; This parameter is read by various parts of the compiler.
;; It's value is 0,1,2, or 3.  -O3 enables unsafe optimizations.
(define-wavescript-parameter ws-optimization-level 2)

;; Profiling controls:
;; Used by annotate-with-data-rates: We can limit the amount of
;; profiling performed either by amount of real time elapsed, virtual
;; time elapsed, number of output elements, or not at all.
;;
;; NOTE: virttime limiting isn't implemented yet.  And currently it
;; can only stop after each output element is produced.
;; 
;; Valid values: 'none, '(time <ms>), '(virttime <ms>), or '(elements <n>)
(define-wavescript-parameter ws-profile-limit 
  ;'(elements 3)
  ;'(time 3000)
  ;`(time ,(* 21 3000))
  ;'(virttime 1000000) ;; Virtual milliseconds.
  ;'(virttime 15000) ;; Virtual milliseconds.
  '(virttime 1000) ;; Virtual milliseconds.
  )

;; Controls whether we inline *everything*, or reserve the right to
;; hold back on monomorphic, first-order functions.
(define-wavescript-parameter ws-full-inline #t)
;; [2010.11.04] How tested was this mode?

;; This parameter stores an association list binding the names of
;; passes to hooks (functions) that should run after the specified
;; pass runs.  This is useful for writing scripts that measure
;; properties of the compiler.
(define-wavescript-parameter ws-compiler-hooks '())

;; This must be set according to the backend that we're using.
;; It must be #t for the C++ backend, and it will be #f for the Caml backend.
;(define-wavescript-parameter wavescript-compile-sums-as-tuples 'unset)

;; When we use the 'print' command within a WS program, this is where that output goes.
(define-wavescript-parameter ws-print-output-port (current-output-port))

;; This suppresses loading of built-in internal*.ws files.
(define-wavescript-parameter ws-no-prelude #f)

;; Suppress the default behavior wherein the "main" stream is echo'd to stdout.
;; Should work across backends.
(define-wavescript-parameter suppress-main-stream-printing #f)

;; This parameter adds extra debug/assertion code to the generated code.
;; Currently we just set it based on whether the whole system is in debug mode.
(define-wavescript-parameter wavescript-emit-debug (IFDEBUG #t #f))

;; [2007.08.17] TEMP: TURNING OFF FOR NOW:
(define-wavescript-parameter check-pass-grammars (IFDEBUG #f #f))

;; Output type annotations on all the generated mlton code.
;; Makes the output more verbose...
(define-wavescript-parameter mlton-ascribe-types #t)

(define-wavescript-parameter dump-graphviz-output #f)

;; [2007.12.01] Allows us to return something other than 'main'
(define-wavescript-parameter ws-alternate-return-stream #f)

;; [2009.11.23] This is a continuation to abort compilation.
;; (Irrespective of which version of the compiler you were called from.)
(define-wavescript-parameter abort-compiler-continuation #f)

;; Just syntactic sugar.  This one is for the Regiment compiler.  It
;; checks the (wavescript-emit-debug) parameter, and if true, returns
;; its arguments in a list, otherwise null.
;; .returns Its arguments in list form or the null list.
(define-syntax WAVESCRIPT_DEBUG
  (syntax-rules ()
    [(_ expr ...) (if (wavescript-emit-debug) (list expr ...) '())]))


;; [2006.03.20] This enables me to explicitely label the nasty hacks in the system.
;; May want to change this in the future to give throw warnings or whatnot.
(define-syntax HACK 
  (lambda (x) 
    (syntax-case x ()
    [(_ str expr)
     (if (not (string? (syntax->datum #'str)))
	 (error 'HACK "bad syntax, should be string ~s" #'str))
     #'expr]
    [(_ expr) #'expr])))


;; This one toggles logging.  
;; It can be set to : 
;;   #t -- Turn logging on, use default log files.
;;   #f -- Turn logging off.
;;   string -- log to specified file 
;;   function -- use specified logger function
;; FIXME : Finish implementing these behaviors.
(define-wavescript-parameter simulation-logger (IFDEBUG #t #f)) ;; Set the default to #t in debug mode, #f otherwise.
;(define-wavescript-parameter simulation-logger #t) ;; Set the default to #t in debug mode, #f otherwise.

;; This sets the level at which we log messages.  All logger calls with less/eq this go through.
(define-wavescript-parameter simulation-logger-level 5)  ;; Very inclusive at first.

;; This toggles whether the file is printed in human readable form
;; (indented, etc), or in machine readable (SExp) form.
(define-wavescript-parameter simulation-logger-human-readable #f)

;; This toggles whether or not the logfile is written in plaintext
;; form (well, except for gizpping), or in "FASL" (fast loading) form,
;; which is a fast-loading way to marshal scheme data structures to a
;; binary file.  Values:
;;  #f - plaintext
;;  #t - same log output, but fasl encoded
;;  <int> - In addition to fasling, also "batch" the output into vector-chunks
;;          This greatly reduces file size and also increases reading speed.
;;  [500 is good for running large simulations.]
(define-wavescript-parameter simulation-logger-fasl-batched 500
  (lambda (x) (ASSERT (or (eq? x #f) (eq? x #t) (and (integer? x) (positive? x))))
	  x))

;; Controls whether a .log or .log.gz is produced.
(define-wavescript-parameter simulation-logger-gzip-output #t)


;; Just a counter for the simulation logger messages.  
;; If it's #f that means it's not set, but it can be 
;; set to zero at the start of a simulation.
(define simulation-logger-count (reg:make-parameter #f))

;;; Used by wsint:
;;
;; Limits the amount of output from the run:
(define-wavescript-parameter wsint-tuple-limit #f)
;; Directs the output to a file
(define-wavescript-parameter wsint-output-file #f)
;; Times how long it takes to pull the tuples from the stream.
(define-wavescript-parameter wsint-time-query #f)

(define wserror-handler (reg:make-parameter  (lambda (str) (error 'wserror str))))

;; [2006.02.22] <br>
;; This is used by various demo programs to externally control a
;; parameter that needs to be varied for testing/analysis purposes.
(define-wavescript-parameter varied-param 'unset-varied-param!!!)
;(define-wavescript-parameter varied-param (begin (fprintf (current-error-port) "varied-param: HACK LOADED") 12))

;; This parameter determines whether comments will be inserted in generated code.
;; Does not effect execution one way or the other
(define-wavescript-parameter reg:comment-code #f)

; ----------------------------------------

;; This stores the list of all passes (well, pass names) that get run by default.
(define-wavescript-parameter pass-list '())

;; This parameter accumulates all the unit tests from the system as they are defined.
(define reg:all-unit-tests (reg:make-parameter '()))
;; [2008.04.29] A hack to deal with some R6RS implementation's lazy
;; library loading.  This accumulates symbols bound by define-testing.
;; They'd better be exported from their respective libraries!!
(define reg:all-tester-names (reg:make-parameter '()))

;; This is kind of silly, but this is a paramter whose value is never
;; read.  It's used to give .rs/.tm files an excuse to execute
;; arbitrary code in the RHSs of their parameter statements.
(define dummy-param (reg:make-parameter #f))


;=======================================================================;;
;;                         Per-module constants                         ;;
;=======================================================================;;

;;; Used primarily by wavescript_sim_library
;====================================================

;; This is used to deallocate C-allocated memory.
(define foreign-guardian (reg:make-parameter #f))

;;; Used primarily by hm_type_inference.ss (and type_environments.ss)
;====================================================
  
;; This controls whether let-bound-polymorphism is allowed at all.
(define inferencer-let-bound-poly (reg:make-parameter #t))

;; If this is enabled, the type assigned to a let-bound variable is
;; lowered to the LUB of its call-site requirements, rather than the
;; most general type.
;;   This is only turned off for debugging purposes...
(define inferencer-enable-LUB (reg:make-parameter #f))

;; This is optional -- helps with printing the types.  We may want to
;; suppress printing the types for bindings imported via include.
(define included-var-bindings (reg:make-parameter '()))


;;; Used primarily by desugar-pattern-matching
;====================================================
(define default-case-symbol '__default__)  ;; Should be gensym
  
;;; Used primarily by nominalize-types:
;====================================================

(define MAX_TUPLE_SIZE 500)

;;; Used primarily by pass12_add-heartbeats:
;====================================================

;; The slow-pulse is used for region formation.
(define-wavescript-parameter default-slow-pulse (* 10 60000)) ;; MILLISECONDS

;; The fast-pulse is used for folding.
(define-wavescript-parameter default-fast-pulse 5000)  ;; MILLISECONDS

;;; Used primarily by pass14_add-places:
; (and by pass15_add-routing
;====================================================

(define unknown-place '?) ;'X?) ;; This symbol marks a place we don't know.

(define noplace '_)             ;; This symbol marks no place at all.

;;; Used primarily by pass20_deglobalize
;====================================================

;; (Debugging, simulation only) <br> 
;;   When this parameter is turned on, the emitted code returns tagged
;;   values to the base-station.  This extra meta-data generally tells
;;   the user what Regiment primitive produced the values, as well as
;;   which node-ids they came from.
(define-wavescript-parameter deglobalize-markup-returns #f)

;;; Used primarily by the pass cleanup-token-machine
;====================================================

;; This is the subtok ID that's implicitly inserted when a user omits
;; the subtokid by not using the full (tok <name> <subid>) form.
(define DEFAULT_SUBTOK 0)

;; This is the variable name that's stuck in if no binding for
;; subtokid is provided by the user for a particular token. <br><br>
;; Currently [2005.11.26], I believe capture may occur.
;; FIXME: This should be a gensym that's user-unusable.
(define DEFAULT_SUBTOK_VAR 'subtok_ind)

;(define MAX_SUBTOK 1000)  ;; We allow there to be 1000 copies of any one token.
(define MAX_SUBTOK 256) ;; On second thought let's make it an 8 bit value.


;;; Used primarily by pass desugar-gradients
;====================================================

;; This controls which gradient implementation is used. <br>
;; Valid options are                                    <br>
;;   'inlined  -- original method, build return handlers for every greturn statement.               <br>
;;   'linked   -- only a single return handler that takes extra arguments, reduces code bloat.      <br>
;;               NOTE: CURRENTLY DOESN'T WORK WITH NON-AGGREGATED GRETURNS!
;;   'etx     -- like 'dynamic, but uses an ETX metric for selecting trees, rather than hopcount.  <br>
(define-wavescript-parameter desugar-gradients-mode 'etx) ; TOGGLE FOR UNIT TESTING.

;; Currently [2006.01.25] I'm implementing a very simple retry
;; mechanism for up-sending data in ETX based gradients.  If an upward
;; bound message doesn't receive an ACK it will be resent after a
;; constant amount of time.H
(define-wavescript-parameter etx-retry-delay 50)
;; And this is the maximum number of times a retry will be made.
(define-wavescript-parameter etx-max-retries 5)

;;; Used primarily by pass cps-tokmac
;====================================================
;; This object is used as null pointer for continuations.
;; If a token handler is called with the null continuation, 
;; it need not invoke it.
(define NULLK ''NULLK)

;;; And by convert closure:
;==========================

;; Datum passed as a flag to signal continuation initialization.
(define KINIT_FLAG 'KINIT) ; 11  

;; Datum passed as a flag to signal continuation invocation.
(define KCALL_FLAG 'KCALL) ; 99  ;; 



;;; Used primarily by helpers.ss:
;====================================================

;; Controls the number of times the default unit tester will retry a failed test.
;; (Only applies to nondeterministic tests marked as 'retry'.)<br>
(define-wavescript-parameter default-unit-tester-retries 3)



;;; Used primarily by Simulator_nought.ss: (DEPRECATED)
;====================================================

;; These are the virtual coordinate bounds of the world.
;; [2005.09.25] These are constants for an out of use file:
;; Now we use wavescript-parameters for this type of thing...
(define world-xbound 60)

;; And the y-bound
(define world-ybound 60) 

(define radius 20) ;; And the comm radius.

(define numsimnodes 30) ;; And the total # processors.

(define SPECIAL_RETURN_TOKEN 'RETTT)

;; Id number for the base-station / Source-of-Control (SOC)
;; I don't want to use negative numbers. I set this high for now
;; so it should not conflict with any other ID -- it's an upper bound. <br> 
;; (Right now other ids are 1-1000)                                    <br>
(define BASE_ID 0) ;; I've been using 10000 or 0.

;; We "option lift" the ID type by having this number signify "NULL":  
;; This is because our backend is not sophisticated enough yet to have real option types.
(define NULL_ID 10001)

;; Nodeid? tells us whether a scheme object could potentially be a
;; node id.  If you use symbols or other non-integers for BASE_ID or
;; NULL_ID, you should change this binding appropriately.
(define-syntax nodeid? (syntax-rules () [(_ x) (integer? x)]))

;; In milliseconds, this is effectively the epoch size.  
;; Nodes aggregate and resend at this frequency.
(define return-window-size 500)

;; These are the height and width of the drawing surface itself.  (Not
;; the whole window as the name would imply.
; TODO FIXME: promote these to parameters and add hooks for processor screen radius.
(define window-width 700)
(define window-height 700)

;; This determines the size of a node when drawn on the screen.
;; (It has to be inexact for SWL's sake; otherwise we end up with
;; undesirable rational numbers.) <br><br>
(define-wavescript-parameter processor-screen-radius 16.0)

;; This sets the value of the previous processor-screen-radius
;; parameter based on the current number of processors and window size. <br><br>
;;
;; Note, if graphics are loaded a hook will be added to invoke this
;; whenever the num-nodes (or, ideally, the window size) cahnges.
(define (set-procesor-screen-radius!)
  ;; Compute sqrt(1/8 * area-per-node)
  (let ([newrad  (min (inexact (/ window-width 45.)) ;; Max relative size.
		      ;16 ;; Max absolute size.
		      ;; If there's not enough room, we make them smaller.
		      (sqrt (/ (inexact (* window-height window-width)) ;; Compute pixel area.M
			       (* 12 (sim-num-nodes)))))])
    (DEBUGASSERT (inexact? newrad))
    (processor-screen-radius newrad)))


  
;;; Used primarily by MULTIPLE SIMULATORS
;====================================================
; [2005.11.14] I'm segregating and renaming the parameters that are
; used by the tossim interface and simulator alpha.

(define-wavescript-parameter sim-num-nodes 30 
  (lambda (x) (if (and (integer? x) (>= x 1))
		  x (error 'sim-num-nodes "must be a positive integer"))))

;; Controls the time-out for both simulator-alpha and tossim. <br>
;; Valid values:                                              <br>
;; #f    : No time-out                                        <br>
;; Float : Time out after certain number of cpu seconds.      <br>
;; Int   : Timeout after certain number of simulator clock ticks
(define-wavescript-parameter sim-timeout 2000)

;; Number of milleseconds over which to start up the nodes.   <br>
;; [2005.11.14] FIXME: Not used yet in simulator-alpha.
(define-wavescript-parameter sim-startup-stagger 0)

;====================================================
;;; Used primarily by Simulator_alpha.ss: <br>
;;;
;;; Sim alpha also reuses some of the parameters from Sim Nought.

(define token-store-size 1000) ;; Store up to 1000 token-objs per node.

;; Just a global pointer to whatever the currently running simworld is.
(define simalpha-current-simworld 
  (reg:make-parameter #f (lambda (x) (if #t ;(or (not x) (simworld? x)) ;; Can't check this simworld? not defined yet.
				  x
				  (error 'simalpha-current-simworld 
					 "invalid val for param: ~a" x)))))

;; This parameter is a way for .rs files to set their own topologies.
;; If it's set to a filename, that topology is used rather than rolling a new one.
(define-wavescript-parameter simalpha-preset-topology #f)

;; This is the null pointer representation for Token names.  Probably just Zero.
(define TMNULL ''0)


;;; HOW DO YOU DO .form!???! FIXME
;; .form Simalpha uses parameters for the world bounds.  This is the xbound.
(define-wavescript-parameter simalpha-world-xbound 60)

;; .form And the ybound...
(define-wavescript-parameter simalpha-world-ybound 60)

;; .form????????? Comm radius: outer.
(define-wavescript-parameter simalpha-outer-radius 15)

;; Comm radius: inner.
(define-wavescript-parameter simalpha-inner-radius 10)

;; When this is set to #t, the simulator slows itself down to match real-time.
;; If it can't match real time, will have undefined behavior.
(define-wavescript-parameter simalpha-realtime-mode #f)

;; This is used by the simulator.  Values:
;;   #f: Node IDs are large and random.
;; <int>: This number serves as the *start* of nodeIDs.  I usually use
;;        zero for BASE, and I like to use 2000 or so for this.  If
;;        nodeids are too small it's easy to mix them up with other
;;        numbers floating around.
(define-wavescript-parameter simalpha-consec-ids 2000)

;; This controls where the simulator writes its output.
;; If this is false, default is stdout.  Otherwise it must be set to an output port.
(define-wavescript-parameter simalpha-output-port #f) 


;; This parameter determines node-placement strategy.  Valid settings are:
;;  'random    -- A simple random topology.
;;  'connected -- A random topology which tries to ensure that the
;;                communication graph is "connected". Bear in mind that it may
;;                introduce other biases in the distribution of placements that it
;;                 produces.  (But with an opaque channel function it cannot guarantee this.)
;;  'gridlike   -- a randomly perturbed grid.  TODO: Expose some parameters for controlling the randomness.
(define-wavescript-parameter simalpha-placement-type 'gridlike)

;; This is the fraction of a grid square (can be greater than 1) that
;; represents the maximum pertubation of a node's x or y coordinate in
;; 'gridlike' mode.
(define-wavescript-parameter simalpha-max-gridlike-perturbation 1/2)

;; [2005.10.03] Can be:
;;   'lossless -- 100% until simalpha-outer-radius, 0% beyond  
;;   'linear-disc -- 100% at simalpha-inner-radius radius, 0% past outer, linear interpolation between.
;;   'empirical -- Uses gathered data on radio connectivity (UNIMPLEMENTED)
(define-wavescript-parameter simalpha-channel-model 'lossless)

;; [2005.10.03] Can only be 'none right now.  Can implement other kinds of stopping failure at some point.
(define-wavescript-parameter simalpha-failure-model 'none)

;; Controls whether dbg print statements happen.  Can be changed dynamically.
(define-wavescript-parameter simalpha-dbg-on #t)

;; This parameter controls the feature wherein omitted trailing args to token handlers are
;; filled in as ZERO.  It may be set to:
;; #t/#f turn padding on/off
;; warning: turn padding on, but issue a warning when it is used.
(define-wavescript-parameter simalpha-zeropad-args #f) ;'warning)

;; When this parameter is turned on, the simulator returns a stream of
;; soc-return values rather than waiting for the sim to end
(define-wavescript-parameter simalpha-stream-result #f)

;; If #t the simulator will open up a GUI as it simulates (if it can).
(define-wavescript-parameter simalpha-graphics-on #t)

;; Determines whether edges are drawn.  Sometimes drawing edges (and
;; highlighting them), can be slow.
;(define-wavescript-parameter simalpha-draw-edges #t)

;; When this is #t the simulator writes all simulations to a file and loads them.  Better for debugging!
;; This controls both the "language-mechanism" and simalpha..
(define-wavescript-parameter simulator-write-sims-to-disk (IFDEBUG #t #f))


;; This parameter controls whether or not the generated (simulator)
;; code will be wrapped in "module" declarations.  This makes
;; execution faster, but has the drawback that the code will not be
;; garbage collectable.  (Top level modules cannot be garbage
;; collected in Chez Scheme.)
(define-wavescript-parameter simalpha-generate-modules 
  ;; If we're running long simulations in batch mode, we want to turn this on:
  ;; We just refer to the top-level environment for this: (dodging module-system/load-order issues)
  ;(eval 'simulator-batch-mode))
  #t)

;; This is a little feature that will print message counts to the GUI:
(define-wavescript-parameter simalpha-label-msgcounts #f)
(define-wavescript-parameter simalpha-label-sensorvals #f)

;; If this parameter is set, it must be set to a thunk which will somehow pause the scheduler main loop.
(define simalpha-pause-hook (reg:make-parameter #f))


;; This parameter is used to instantiate new instances of the sensed-world. <br> <br>
;;
;; Cannot set it right now because no sensor stubs have been defined yet here in constants.ss .  
;; When sensor stubs are defined, this shoud be set to some default value.
(define-wavescript-parameter simalpha-sense-function-constructor 'unset)
;; This parameter is bound by the simulator to the constructed sensor-functions.
;; Shouldn't be modified directly.
;; Should have type:  () -> VTime -> SensorTypeString, NodeID, Xcoord, Ycoord -> SensedValue
(define-wavescript-parameter simalpha-sense-function #f)
;(define simalpha-sense-function (reg:make-parameter #f (lambda (x) 
;						     (inspect `(sensor! . ,x))
;						     x)))

;(define pi 3.14159) ;; TODO: better accuracy wouldn't hurt.


; ======================================================================


;;; Used primarily by alpha_lib_scheduler_simple.ss
;=====================

;; Constant: amount of virtual time consumed by an action.  Nonzero to force forward progress.
;(define ACTION_LENGTH 100)  ;; Thus we ignore the "duration" field of simevts.
(define SCHEDULE_DELAY 1)
;; Vtimes:
(define RADIO_DELAY 30)  ;; Communication time
;(define PROCESSING_TIME 0)  ;; Not used yet... time to process incoming messages


;;; Used primarily by the graphics system:
; ========================================

;; This isn't a "constant" but it's a datatype def that needs to be visible everywhere.
(reg:define-struct (rgb red green blue))


;; My light background theme:
#;
(begin
  (define Default-Drawing-Color     (make-rgb 0 255 0))
  (define Default-Window-Color      (make-rgb 200 200 200))
  (define Default-Window-Text-Color (make-rgb 0 0 0)) ;; NOT USED YET
  (define Default-Background-Color  (make-rgb 215 215 255))
  (define Default-Canvas-Text-Color (make-rgb 0 0 0))
  (define Default-Supertext-Color   (make-rgb 0 0 0))
  (define Default-Subtext-Color     (make-rgb 0 100 0))
  (define Default-LED-Off-Color     (make-rgb 0 50 50))
  
  ;(define Starting-Node-Color (make <rgb> 200 10 10))
  (define Default-Node-Color           (make-rgb 130 130 130))
  (define Default-Base-Border-Color    (make-rgb 130 130 130))
;  (define Default-Edge-Color           (make-rgb 10 10 10))
  (define Default-Edge-Full-Color      (make-rgb 10 10 10))
  (define Default-Edge-Dead-Color      (make-rgb 200 200 240))
  (define Default-Line-Highlight-Color (make-rgb 0 0 200))
  (define Default-Mark-Color           (make-rgb 0 0 0))
  (define Default-Proc-Border-Color    (make-rgb 0 0 0))
  (define Default-Mouse-Highlight-Color (make-rgb 200 200 0))
  )

;; My dark background theme:
(begin
  (define Default-Drawing-Color     (make-rgb 0 255 0))
  (define Default-Window-Color      (make-rgb 150 150 150))
  (define Default-Window-Text-Color (make-rgb 0 0 0)) ;; NOT USED YET
  (define Default-Background-Color  (make-rgb 50 50 50))
  (define Default-Canvas-Text-Color (make-rgb 150 150 200))
  (define Default-Supertext-Color   (make-rgb 200 200 200))
  (define Default-Subtext-Color     (make-rgb 100 200 100))
  (define Default-LED-Off-Color     (make-rgb 0 50 50))
  
  ;(define Starting-Node-Color (make <rgb> 200 10 10))
  (define Default-Node-Color           (make-rgb 130 130 130))
  (define Default-Base-Border-Color    (make-rgb 0 255 0))
;  (define Default-Edge-Full-Color           (make-rgb 70 70 70))
  (define Default-Edge-Full-Color      (make-rgb 160 160 160))
  (define Default-Edge-Dead-Color      (make-rgb 55 55 55))
  (define Default-Line-Highlight-Color (make-rgb 0 0 200))
  (define Default-Mark-Color           (make-rgb 255 0 0))
  (define Default-Proc-Border-Color    (make-rgb 0 0 0))
  (define Default-Mouse-Highlight-Color (make-rgb 200 200 0))
  )

; ======================================================================

;; Definitions for WS Values used by interpret-meta and by wavescript_sim_library

;; Contains a start and end SEQUENCE NUMBER as well as a vector.
(reg:define-struct (sigseg start end vec timebase))

;; Tuple VALUE representation:
;; [2007.07.29] Adding this to distinguish tuples from vectors.
(reg:define-struct (tuple fields))

;; [2010.06.28] Ack, we need to be careful whenever asking for the
;; fields.  We also allow 'UNIT as a constant.  Maybe I want to phase
;; that out now that I have the above tuple struct.
(define (safe-tuple-fields t)
 (cond
   [(eq? t 'UNIT) '()]
   [(tuple? t) (tuple-fields t)]
   [else (error 'safe-tuple-fields "Not a tuple value representation: ~s" t)]))


;; [2008.09.28] This datatype is used for records at metaprog time.
;; The association list of name-value pairs must be sorted in
;; alphabetic order.  It may contain duplicate names, but extend and
;; restrict operations must respect the order of these duplicated
;; entries.
(reg:define-struct (wsrecord pairs))
(define (empty-wsrecord) (make-wsrecord '()))
(define (wsrecord-select fld rec)
  (cdr (ASSERT (assq fld (wsrecord-pairs rec)))))
(define (wsrecord-restrict name rec)
  (define (assq-remove key ls)
    (let loop ((ls ls))
      (cond
       [(null? ls) '()]
       [(eq? (caar ls) key) (cdr ls)]
       [else (cons (car ls) (loop (cdr ls)))])))
  (make-wsrecord (assq-remove name (wsrecord-pairs rec))))
;; NOTE: Actually for now we're not maintaining a sorted list:
#;
(define (wsrecord-extend name val rec )
  (define (symbol<=? s1 s2)
    (string<=? (symbol->string s1) (symbol->string s2)))
  (define pair (cons name val))
  (let loop ((ls (wsrecord-pairs rec)))
    (cond
     [(null? ls) (list pair)]
     [(symbol<=? name (caar ls)) (cons pair ls)]
     [else (cons (car ls) (loop (cdr ls)))])))
(define (wsrecord-extend name val rec)
  (make-wsrecord (cons (cons name val) (wsrecord-pairs rec))))

(reg:define-struct (uniontype tag val)) ;; Sum types

(reg:define-struct (timebase num))

;; Adding this to distinguish float and double values [2008.08.22]
(reg:define-struct (double val))

;; Used for profiling and annotating data rates.
(reg:define-struct (bench-stats bytes tuples cpu-time))


; ======================================================================

;;; Functions (not constants or macros). <br>
;;;
;;;   Everything below is not a constant or "preprocessor definition"
;;; (macro), but a function.  However, these are functions that are
;;; (1) simple and (2) must be scoped very broadly, thus justifying
;;; their inclusion in this file.


;; Indicates whether an integer can be represented in a certain number of bits:
(define (int16? c) (and (< c (expt 2 15)) (>= c (- (expt 2 15)))))
(define (int32? c) (and (< c (expt 2 31)) (>= c (- (expt 2 31)))))
(define (int64? c) (and (< c (expt 2 63)) (>= c (- (expt 2 63)))))

(define (uint8?  c) (and (< c (expt 2 8))  (>= c 0)))
(define (uint16? c) (and (< c (expt 2 16)) (>= c 0)))


;; The directory name of a path.
(define (dirname pathstr)
  ;; Everything up until the last "#/"
  (define chars (string->list pathstr))
  ;(define dir (reverse! (or (memq #\/ (reverse! chars)) '(#\.))))
  (define dir (reverse (or (memq #\/ (reverse chars)) '(#\.))))
  (list->string dir))


;; The filename, the *last* part of a path.
(define (basename pathstr)
  ;; Everything after the last "#/"
  ;(define file (let loop ([ls (reverse! (string->list pathstr))])
  (define file (let loop ([ls (reverse (string->list pathstr))])
		 (cond 
		  [(null? ls) '()]
		  [(eq? (car ls) #\/) '()]
		  [else (cons (car ls) (loop (cdr ls)))])))
  ;(list->string (reverse! (cdr (or (memq #\. file) (cons #\. file))))))
  ;(list->string (reverse-bang (cdr (or (memq #\. file) (cons #\. file))))))
  (list->string (reverse (cdr (or (memq #\. file) (cons #\. file))))))


; ======================================================================

;;; Initialization code.
;;;
;;; PLT doesn't support compressed or fasl log writing currently.  We
;;; make sure that's turned off if we're loading under PLT.
#; ;; TEMPTOGGLE
(cond-expand
 [chez]
 [larceny] ;; This should do what PLT does probably...
 [plt ;(or  larceny)
  (simulation-logger-fasl-batched #f)
  (simulation-logger-gzip-output #f)])


; ======================================================================

;(display "globals loaded\n")

) ;; End Module
;(import constants)
