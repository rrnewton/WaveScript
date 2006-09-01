
;;;; .title Constants.ss -- A collection of global constants, flags, and datatype defs
;;;; .author Ryan Newton

;;;; A collection of global constants, flags, and datatype defs.<br><br>



;;;; NOTE: [2006.03.13] Most modules are structured such that the
;;;; chez/ and plt/ versions include the generic version.  This module
;;;; is the reverse.  The generic/ version is the parent, and it
;;;; depends on chunks of functionality which are defined in the chez/
;;;; and plt/ versions respectively.


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

(module constants mzscheme
	(require 
	 (lib "include.ss")
;	 "../plt/regmodule.ss"
	 "../plt/plt_constants.ss"
	 )

	;; Many exports:
	(provide 

	 ;; These are exported here, but only PLT uses them.  
	 ;; They're stubs to make the common module syntax work.
	 chezimports chezprovide

	 ;; Chez/PLT specific (included from respective modules).
	 reg:define-struct reg:struct? reg:struct->list reg:list->struct 
	 IFCHEZ IF_GRAPHICS IF_THREADS
;	 reg:include	 

         ;; Syntax:
	 IFDEBUG
         DEBUGMODE UBERDEBUGMODE DEBUGPRINT DEBUGPRINT2 DEBUGASSERT ASSERT
         REGIMENT_DEBUG HACK
	 IFWAVESCOPE 
         ;chezprovide chezimports ;; To make the common module facility work.
         
         REGIMENTD
         SCHEDULE_DELAY         
         RADIO_DELAY ;PROCESSING_TIME ;; Not used yet
         
	 define-regiment-parameter regiment-parameters
	 regiment-verbose 
	 simulation-logger 
	 simulation-logger-count
	 simulation-logger-level
	 simulation-logger-human-readable
	 simulation-logger-fasl-batched
	 simulation-logger-gzip-output
	 reg:all-unit-tests
	 reg:comment-code
         
         pass-list
         
         default-slow-pulse default-fast-pulse
         
         deglobalize-markup-returns
         
         unknown-place noplace
         
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
	 simalpha-realtime-mode
	 simalpha-consec-ids
	 simalpha-dbg-on
	 simalpha-zeropad-args
	 simalpha-stream-result 
	 simalpha-sense-function
	 simalpha-sense-function-constructor
	 simalpha-graphics-on
         simalpha-write-sims-to-disk
	 simalpha-generate-modules
	 simalpha-label-msgcounts 
	 simalpha-label-sensorvals 
	 simalpha-pause-hook

	 varied-param
	 dummy-param

         make-rgb
         rgb-red
         rgb-green
         rgb-blue

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

         )
	
	;; Some bindings need to be explicitely exposed to syntax transformers:
	(chezprovide (REGIMENT_DEBUG regiment-emit-debug))
	
	;; Import the platform specific constants.
	(chezimports chez_constants)

	;; These stubs allow our common module syntax to work.
	(define-syntax chezimports
	  (syntax-rules ()
	    [(_ e ...) (begin)]))
	(define-syntax chezprovide
	  (syntax-rules ()
	    [(_ e ...) (begin)]))     
	
;=======================================================================

;;; Regiment parameters.

;; In the following manner we distinguish regiment parameters from normal
;; parameters.  We keep a list of all the existing regiment
;; parameters.  And it also makes it more acceptable for us to scatter
;; around the parameter definitions, because you can grep for
;; define-regiment-parameter.
(define regiment-parameters (make-parameter '()))
(define-syntax define-regiment-parameter
  (syntax-rules () 
    [(_ name args ...)
     (define name 
       (begin (regiment-parameters (cons (quote name) (regiment-parameters)))
	      (make-parameter args ...)))]))

;; Parameter determining the location of the Regiment tree. <br>
;; This is set when the system loads.
(define-regiment-parameter REGIMENTD "./")

;=======================================================================;;
;;                       <<< DEBUG TOGGLES >>>                          ;;
;=======================================================================;;
;;; Debug Toggles

;; The IFDEBUG/DEBUGMODE toggles are like a #define that toggles debug code for the whole compiler.
;; This is not a very appropriate place for this definition, but it's the most convenient
;; so that it can be seen from everywhere.
;; <br><br>
;; Uncomment one line for debug mode, the other to deactivate it.
(define-syntax IFDEBUG (syntax-rules () [(_ debon deboff) debon]))  ;; ON
;(define-syntax IFDEBUG (syntax-rules () [(_ debon deboff) deboff])) ;; OFF

;; DEBUGMODE is just syntactic sugar on top of IFDEBUG.  It contains
;; any number of subexpressions and executes them only when IFDEBUG is activated.
(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (IFDEBUG (list expr ...) ())]))

;; This is for debug annotations that take a really long time.
;; For now it's enabled at the same time that DEBUGMODE is.
(define-syntax UBERDEBUGMODE (syntax-rules () [(_ expr ...) (IFDEBUG (list expr ...) ())]))

;; DEBUGASSERT is another piece of sugar.  Asserts a boolean value if IFDEBUG is activated.
#;(define-syntax DEBUGASSERT
  (syntax-rules () 
    [(_ expr) 
     (DEBUGMODE
      (if expr #t 
	  (error 'DEBUGASSERT "failed: ~s" (quote expr))))]
    [(_ obj expr)
     (DEBUGMODE
      (if expr #t 
	  (error obj "DEBUGASSERT failed: ~s" (quote expr))))]
    ))

(define-syntax DEBUGASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) 
       #'(DEBUGMODE
	  (if expr #t 
	      (error 'DEBUGASSERT "failed: ~s" #'expr )))]
      ;; This form is (ASSERT integer? x) returning the value of x.
      [(_ fun val) #'(IFDEBUG 
		      (let ([v val])
		       (if (fun v) v			   
			   (error 'DEBUGASSERT 
				  "failed: ~s\n Value which did not satisfy above predicate: ~s" 
				  #'fun v)))
		      val)])))

(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) #'(or expr (error 'ASSERT "failed: ~s" #'expr))]
      ;; This form is (ASSERT integer? x) returning the value of x.
      [(_ fun val) #'(let ([v val])
		       (if (fun v) v			   
			   (error 'ASSERT "failed: ~s\n Value which did not satisfy above predicate: ~s" #'fun 
				  v)))]
      )))

;(define Regiment-Log-File "~/tmp/Regiment.log.ss")
; ; ;(define Regiment-Log-File (string-append (current-directory) "/Regiment.log.ss"))
; ; ;(define Regiment-Log-File (string-append "Regiment.log.ss"))
; ; Delete that logfile at load time:
;(if (file-exists? Regiment-Log-File)
;    (delete-file Regiment-Log-File))

; THESE ARE ONLY USED BY SIMULATOR_NOUGHT PRESENTLY. [2005.03.18]
; This one prints nothing at all:
;(define-syntax DEBUGPRINT (syntax-rules () [(_ expr ...) (begin expr ...)]))
(define-syntax DEBUGPRINT (syntax-rules () [(_ expr ...) (void)]))
; This is a SECOND print channel for even lamer information:
(define-syntax DEBUGPRINT2 (syntax-rules () [(_ expr ...) (void)]))            ; ON
;(define-syntax DEBUGPRINT2 (syntax-rules () [(_ expr ...) (begin expr ...)]))  ; OFF

;; This parameter determines whether the compiler should print extra (debugging related) info during compilation.
(define-regiment-parameter regiment-verbose #f)

;; This parameter adds extra debug/assertion code to the generated code.
;; Currently we just set it based on whether the whole system is in debug mode.
(define-regiment-parameter regiment-emit-debug (IFDEBUG #t #f))

;; Just syntactic sugar.  This one is for the Regiment compiler.  It
;; checks the (regiment-emit-debug) parameter, and if true, returns
;; its arguments in a list, otherwise null.
;; .returns Its arguments in list form or the null list.
(define-syntax REGIMENT_DEBUG
  (syntax-rules ()
    [(_ expr ...) (if (regiment-emit-debug) (list expr ...) ())]))

;; [2006.08.28] This is the "#define" for building WAVESCOPE related code.
;; When turned off, the system should build Regiment without WaveScope extensions.
(define-syntax IFWAVESCOPE (syntax-rules () [(_ on off) on] [(_ on) on]))  ;; ON
;(define-syntax IFWAVESCOPE (syntax-rules () [(_ on off) off] [(_ on) (void)])) ;; OFF

;(define-syntax WAVESCOPE (syntax-rules () [(_ expr ...) (IFWAVESCOPE (list expr ...) ())]))


;; [2006.03.20] This enables me to explicitely label the nasty hacks in the system.
;; May want to change this in the future to give throw warnings or whatnot.
(define-syntax (HACK x)
  (syntax-case x ()
    [(_ str expr)
     (if (not (string? (syntax-object->datum #'str)))
	 (error 'HACK "bad syntax, should be string ~s" #'str))
     #'expr]
    [(_ expr) #'expr]))


;; This one toggles logging.  
;; It can be set to : 
;;   #t -- Turn logging on, use default log files.
;;   #f -- Turn logging off.
;;   string -- log to specified file 
;;   function -- use specified logger function
;; FIXME : Finish implementing these behaviors.
(define-regiment-parameter simulation-logger (IFDEBUG #t #f)) ;; Set the default to #t in debug mode, #f otherwise.
;(define-regiment-parameter simulation-logger #t) ;; Set the default to #t in debug mode, #f otherwise.

;; This sets the level at which we log messages.  All logger calls with less/eq this go through.
(define-regiment-parameter simulation-logger-level 5)  ;; Very inclusive at first.

;; This toggles whether the file is printed in human readable form
;; (indented, etc), or in machine readable (SExp) form.
(define-regiment-parameter simulation-logger-human-readable #f)

;; This toggles whether or not the logfile is written in plaintext
;; form (well, except for gizpping), or in "FASL" (fast loading) form,
;; which is a fast-loading way to marshal scheme data structures to a
;; binary file.  Values:
;;  #f - plaintext
;;  #t - same log output, but fasl encoded
;;  <int> - In addition to fasling, also "batch" the output into vector-chunks
;;          This greatly reduces file size and also increases reading speed.
;;  [500 is good for running large simulations.]
(define-regiment-parameter simulation-logger-fasl-batched 500
  (lambda (x) (ASSERT (or (eq? x #f) (eq? x #t) (and (integer? x) (positive? x)))) 
	  x))

;; Controls whether a .log or .log.gz is produced.
(define-regiment-parameter simulation-logger-gzip-output #t)


;; Just a counter for the simulation logger messages.  
;; If it's #f that means it's not set, but it can be 
;; set to zero at the start of a simulation.
(define simulation-logger-count (make-parameter #f))


;; [2006.02.22] <br>
;; This is used by various demo programs to externally control a
;; parameter that needs to be varied for testing/analysis purposes.
(define-regiment-parameter varied-param 20)

;; This parameter determines whether comments will be inserted in generated code.
;; Does not effect execution one way or the other
(define-regiment-parameter reg:comment-code #f)

; ----------------------------------------

;; This stores the list of all passes (well, pass names) that get run by default.
(define-regiment-parameter pass-list '())

;; This parameter accumulates all the unit tests from the system as they are defined.
(define reg:all-unit-tests (make-parameter '()))

;; This is kind of silly, but this is a paramter whose value is never
;; read.  It's used to give .rs/.tm files an excuse to execute
;; arbitrary code in the RHSs of their parameter statements.
(define dummy-param (make-parameter #f))

;=======================================================================;;
;;                         Per-module constants                         ;;
;=======================================================================;;

;;; Used primarily by pass12_add-heartbeats:
;====================================================

;; The slow-pulse is used for region formation.
(define-regiment-parameter default-slow-pulse 60000) ;; MILLISECONDS

;; The fast-pulse is used for folding.
(define-regiment-parameter default-fast-pulse 5000)  ;; MILLISECONDS

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
(define-regiment-parameter deglobalize-markup-returns #f)

;;; Used primarily by pass21_cleanup-token-machine
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
(define-regiment-parameter desugar-gradients-mode 'etx) ; TOGGLE FOR UNIT TESTING.

;; Currently [2006.01.25] I'm implementing a very simple retry
;; mechanism for up-sending data in ETX based gradients.  If an upward
;; bound message doesn't receive an ACK it will be resent after a
;; constant amount of time.H
(define-regiment-parameter etx-retry-delay 50)
;; And this is the maximum number of times a retry will be made.
(define-regiment-parameter etx-max-retries 3)

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
(define-regiment-parameter default-unit-tester-retries 3)


;;; Used primarily by Simulator_nought.ss: (DEPRECATED)
;====================================================

;; These are the virtual coordinate bounds of the world.
;; [2005.09.25] These are constants for an out of use file:
;; Now we use regiment-parameters for this type of thing...
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
(define-regiment-parameter processor-screen-radius 16.0)

;; This sets the value of the previous processor-screen-radius
;; parameter based on the current number of processors and window size. <br><br>
;;
;; Note, if graphics are loaded a hook will be added to invoke this
;; whenever the num-nodes (or, ideally, the window size) cahnges.
(define (set-procesor-screen-radius!)
  ;; Compute sqrt(1/8 * area-per-node)
  (let ([newrad  (min (exact->inexact (/ window-width 45.)) ;; Max relative size.
		      ;16 ;; Max absolute size.
		      ;; If there's not enough room, we make them smaller.
		      (sqrt (/ (exact->inexact (* window-height window-width)) ;; Compute pixel area.M
			       (* 12 (sim-num-nodes)))))])
    (DEBUGASSERT (inexact? newrad))
    (processor-screen-radius newrad)))
  
;;; Used primarily by MULTIPLE SIMULATORS
;====================================================
; [2005.11.14] I'm segregating and renaming the parameters that are
; used by the tossim interface and simulator alpha.

(define-regiment-parameter sim-num-nodes 30 
  (lambda (x) (if (and (integer? x) (>= x 1))
		  x (error 'sim-num-nodes "must be a positive integer"))))

;; Controls the time-out for both simulator-alpha and tossim. <br>
;; Valid values:                                              <br>
;; #f    : No time-out                                        <br>
;; Float : Time out after certain number of cpu seconds.      <br>
;; Int   : Timeout after certain number of simulator clock ticks
(define-regiment-parameter sim-timeout 2000)

;; Number of milleseconds over which to start up the nodes.   <br>
;; [2005.11.14] FIXME: Not used yet in simulator-alpha.
(define-regiment-parameter sim-startup-stagger 0)

;====================================================
;;; Used primarily by Simulator_alpha.ss: <br>
;;;
;;; Sim alpha also reuses some of the parameters from Sim Nought.

(define token-store-size 1000) ;; Store up to 1000 token-objs per node.

;; Just a global pointer to whatever the currently running simworld is.
(define simalpha-current-simworld 
  (make-parameter #f (lambda (x) (if #t ;(or (not x) (simworld? x)) ;; Can't check this simworld? not defined yet.
				  x
				  (error 'simalpha-current-simworld 
					 "invalid val for param: ~a" x)))))

;; This is the null pointer representation for Token names.  Probably just Zero.
(define TMNULL ''0)


;;; HOW DO YOU DO .form!???! FIXME
;; .form Simalpha uses parameters for the world bounds.  This is the xbound.
(define-regiment-parameter simalpha-world-xbound 60)

;; .form And the ybound...
(define-regiment-parameter simalpha-world-ybound 60)

;; .form????????? Comm radius: outer.
(define-regiment-parameter simalpha-outer-radius 15)

;; Comm radius: inner.
(define-regiment-parameter simalpha-inner-radius 10)

;; When this is set to #t, the simulator slows itself down to match real-time.
;; If it can't match real time, will have undefined behavior.
(define-regiment-parameter simalpha-realtime-mode #f)

;; This is used by the simulator, 
;; if true then the node ids are small consecutive numbers rather than
;; large random ones.
(define-regiment-parameter simalpha-consec-ids #t)

;; This controls where the simulator writes its output.
;; If this is false, default is stdout.  Otherwise it must be set to an output port.
(define-regiment-parameter simalpha-output-port #f) 


;; This parameter determines node-placement strategy.  Valid settings are:
;;  'random    -- A simple random topology.
;;  'connected -- A random topology which tries to ensure that the
;;                communication graph is "connected". Bear in mind that it may
;;                introduce other biases in the distribution of placements that it
;;                 produces.  (But with an opaque channel function it cannot guarantee this.)
;;  'gridlike   -- a randomly perturbed grid.  TODO: Expose some parameters for controlling the randomness.
(define-regiment-parameter simalpha-placement-type 'gridlike)

;; This is the fraction of a grid square (can be greater than 1) that
;; represents the maximum pertubation of a node's x or y coordinate in
;; 'gridlike' mode.
(define-regiment-parameter simalpha-max-gridlike-perturbation 1/2)

;; [2005.10.03] Can be:
;;   'lossless -- 100% until simalpha-outer-radius, 0% beyond  
;;   'linear-disc -- 100% at simalpha-inner-radius radius, 0% past outer, linear interpolation between.
;;   'empirical -- Uses gathered data on radio connectivity (UNIMPLEMENTED)
(define-regiment-parameter simalpha-channel-model 'lossless)

;; [2005.10.03] Can only be 'none right now.  Can implement other kinds of stopping failure at some point.
(define-regiment-parameter simalpha-failure-model 'none)

;; Controls whether dbg print statements happen.  Can be changed dynamically.
(define-regiment-parameter simalpha-dbg-on #t)

;; This parameter controls the feature wherein omitted trailing args to token handlers are
;; filled in as ZERO.  It may be set to:
;; #t/#f turn padding on/off
;; warning: turn padding on, but issue a warning when it is used.
(define-regiment-parameter simalpha-zeropad-args #f) ;'warning)

;; When this parameter is turned on, the simulator returns a stream of
;; soc-return values rather than waiting for the sim to end
(define-regiment-parameter simalpha-stream-result #f)

;; If #t the simulator will open up a GUI as it simulates (if it can).
(define-regiment-parameter simalpha-graphics-on #t)

;; Determines whether edges are drawn.  Sometimes drawing edges (and
;; highlighting them), can be slow.
;(define-regiment-parameter simalpha-draw-edges #t)

;; When this is #t the simulator writes all simulations to disk and loads them.  Better for debugging!
(define-regiment-parameter simalpha-write-sims-to-disk (IFDEBUG #t #f))

;; This parameter controls whether or not the generated (simulator)
;; code will be wrapped in "module" declarations.  This makes
;; execution faster, but has the drawback that the code will not be
;; garbage collectable.  (Top level modules cannot be garbage
;; collected in Chez Scheme.)
(define-regiment-parameter simalpha-generate-modules 
  ;; If we're running long simulations in batch mode, we want to turn this on:
  ;; We just refer to the top-level environment for this: (dodging module-system/load-order issues)
  ;(eval 'simulator-batch-mode))
  #t)

;; This is a little feature that will print message counts to the GUI:
(define-regiment-parameter simalpha-label-msgcounts #f)
(define-regiment-parameter simalpha-label-sensorvals #f)

;; If this parameter is set, it must be set to a thunk which will somehow pause the scheduler main loop.
(define simalpha-pause-hook (make-parameter #f))


;; This parameter is used to instantiate new instances of the sensed-world. <br> <br>
;;
;; Cannot set it right now because no sensor stubs have been defined yet here in constants.ss .  
;; When sensor stubs are defined, this shoud be set to some default value.
(define-regiment-parameter simalpha-sense-function-constructor 'unset)
;; This parameter on the hand is bound by the simulator to the constructed sensor-functions.
(define-regiment-parameter simalpha-sense-function #f)
;(define simalpha-sense-function (make-parameter #f (lambda (x) 
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
#;(begin
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

;;; Functions (not constants or macros). <br>
;;;
;;;   Everything below is not a constant or "preprocessor definition"
;;; (macro), but a function.  However, these are functions that are
;;; (1) simple and (2) must be scoped very broadly, thus justifying
;;; their inclusion in this file.


; ======================================================================

;;; Initialization code.
;;;
;;; PLT doesn't support compressed or fasl log writing currently.  We
;;; make sure that's turned off if we're loading under PLT.

(IFCHEZ (void)
 (begin 
   (simulation-logger-fasl-batched #f)
   (simulation-logger-gzip-output #f)))

; ======================================================================

) ;; End Module
;(import constants)
