

;; [2004.07.02]
;; Hmm, I considered making this a while ago.  But I never know
;; whether to keep constants module-local or lift them out like this.
;; I'm just going to settle for an ad-hoc strategy, anything that
;; needs to (or might need to) be used in more than one module will
;; get lifted up here.

;; [2005.02.24]
;; Now I'm keeping some parameters (which are actually not constant) in here as well.

;; NOTE: also see DEBUGMODE from helpers.ss.  It's a global syntax definition of interest.
;; [2005.03.29] MOVING DEBUGMODE to this file.
;;======================================================================

;; DEBUGMODE toggles is like a #define that toggles debug code for the whole compiler.
;; This is not a very appropriate place for this definition, but it's the most convenient
;; so that it can be seen from everywhere.
;; Uncomment one line for debug mode, the other to deactivate it.
(define-syntax IFDEBUG (syntax-rules () [(_ debon deboff) debon]))  ;; ON
;(define-syntax IFDEBUG (syntax-rules () [(_ debon deboff) deboff])) ;; OFF

(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (IFDEBUG (list expr ...) ())]))
(define-syntax DEBUGASSERT
  (syntax-rules () 
    [(_ expr ...) 
     (DEBUGMODE
      (if (and expr ...) #t 
	  (error 'DEBUGASSERT "failed: ~s" (quote (and expr ...)))))]))

;(define Regiment-Log-File "~/tmp/Regiment.log.ss")
;;;(define Regiment-Log-File (string-append (current-directory) "/Regiment.log.ss"))
;;;(define Regiment-Log-File (string-append "Regiment.log.ss"))
;; Delete that logfile at load time:
;(if (file-exists? Regiment-Log-File)
;    (delete-file Regiment-Log-File))


;; THESE ARE ONLY USED BY SIMULATOR_NOUGHT PRESENTLY. [2005.03.18]
;; This one prints nothing at all:
(define-syntax DEBUGPRINT 
  (syntax-rules ()
;    [(_ expr ...) (begin expr ...)]))
    [(_ expr ...) (void)]))
;; This is a SECOND print channel for even lamer information:
(define-syntax DEBUGPRINT2
  (syntax-rules ()
;    [(_ expr ...) (begin expr ...)]))
    [(_ expr ...) (void)]))

;; Just a shorthand:
(define-syntax REGIMENT_DEBUG
  (syntax-rules ()
    [(_ expr ...) (if (regiment-emit-debug) (list expr ...) ())]))


;;======================================================================

;; Used primarily by pass12_add-heartbeats:
;;===================================================
;; The slow-pulse is used for region formation.
(define slow-pulse 1000) ;; MILLISECONDS
;; The fast-pulse is used for folding.
(define fast-pulse 100)  ;; MILLISECONDS

;; Used primarily by pass14_add-places:
;; (and by pass15_add-routing
;;===================================================
(define unknown-place '?) ;'X?)
(define noplace '_)

;; Used primarily by pass21_cleanup-token-machine
;;===================================================
(define DEFAULT_SUBTOK 0)
(define DEFAULT_SUBTOK_VAR 'subtok_ind)
(define MAX_SUBTOK 1000)  ;; We allow there to be 1000 copies of any one token.


;; Used primarily by pass cps-tokmac
;;===================================================
;; This object is used as null pointer for continuations.
;; If a token handler is called with the null continuation, 
;; it need not invoke it.
(define NULLK ''NULLK)

;; And by convert closure:
(define KINIT_FLAG 'KINIT) ; 11
(define KCALL_FLAG 'KCALL) ; 99 


;;======================================================================

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

;; This parameter determines whether the compiler should print extra (debugging related) info during compilation.
(define-regiment-parameter regiment-verbose #f)

;; This parameter adds extra debug/assertion code to the generated code.
;; Currently we just set it based on whether the whole system is in debug mode.
(define-regiment-parameter regiment-emit-debug (IFDEBUG #t #f))

;; This one toggles logging.  
;; It can be set to : 
;;   #t -- Turn logging on, use default log files.
;;   #f -- Turn logging off.
;;   string -- log to specified file 
;;   function -- use specified logger function
;; FIXME : Finish implementing these behaviors.
(define-regiment-parameter simulation-logger (IFDEBUG #t #f)) ;; Set the default to #t in debug mode, #f otherwise.
;; This sets the level at which we log messages.  All logger calls with less/eq this go through.
(define-regiment-parameter simulation-logger-level 5)  ;; Very inclusive at first.

;; Just a counter for the simulation logger messages.  
;; If it's #f that means it's not set, but it can be 
;; set to zero at the start of a simulation.
(define-regiment-parameter simulation-logger-count #f)

;; This parameter accumulates all the unit tests from the system as they are defined.
(define-regiment-parameter reg:all-unit-tests '())

;; This parameter 
(define-regiment-parameter reg:comment-code #f)

;; Used primarily by helpers.ss:
;;===================================================

;; If retry is enabled, let's retry three times:
(define-regiment-parameter default-unit-tester-retries 3)


;; Used primarily by Simulator_nought.ss:
;;===================================================
;; These are the virtual coordinate bounds of the world.
;; [2005.09.25] These are constants for an out of use file:
;; Now we use regiment-parameters for this type of thing...
(define world-xbound 60)
(define world-ybound 60)
(define radius 20) ;; And the comm radius.
(define numsimnodes 30) ;; And the total # processors.

(define SPECIAL_RETURN_TOKEN 'RETTT)

;; Id number for the base-station / Source-of-Control (SOC)
;; I don't want to use negative numbers. I set this high for now
;; so it should not conflict with any other ID -- it's an upper bound.  
;; (Right now other ids are 1-1000)
(define BASE_ID 10000)
;; We "option lift" the ID type by having this number signify "NULL":  
;; This is because our backend is not sophisticated enough yet to have real option types.
(define NULL_ID 0)

;; In milliseconds, this is effectively the epoch size.  
;; Nodes aggregate and resend at this frequency.
(define return-window-size 500)


;; Used primarily by Simulator_alpha.ss:
;;===================================================
;; Sim alpha also reuses some of the parameters from Sim nought.

(define token-store-size 1000) ;; Store up to 1000 token-objs per node.

;; Just a global pointer to whatever the currently running simworld is.
(define simalpha-current-simworld 
  (make-parameter #f (lambda (x) (if #t ;(or (not x) (simworld? x)) 
				  x
				     (error 'simalpha-current-simworld 
					    "invalid val for param: ~a" x)))))

(define-regiment-parameter simalpha-num-nodes 30)
(define-regiment-parameter simalpha-world-xbound 60)
(define-regiment-parameter simalpha-world-ybound 60)
;; Comm radius's:
(define-regiment-parameter simalpha-outer-radius 15)
(define-regiment-parameter simalpha-inner-radius 10)

;; Valid values:
;; #f    : No time-out
;; Float : Time out after certain number of cpu seconds.
;; Int   : Timeout after certain number of simulator clock ticks
(define-regiment-parameter simalpha-timeout 10.0)

;; This is used by the simulator, 
;; if true then the node ids are small consecutive numbers rather than
;; large random ones.
(define-regiment-parameter simalpha-consec-ids #t)

(define-regiment-parameter simalpha-output-port #f) ;; If this is false, default is stdout.


;; This parameter determines node-placement strategy.  Valid settings are:
;;  'random    -- A simple random topology.
;;  'connected -- A random topology which tries to ensure that the
;;                communication graph is "connected". Bear in mind that it may
;;                introduce other biases in the distribution of placements that it
;;                 produces.  (But with an opaque channel function it cannot guarantee this.)
;;  'gridlike   -- a randomly perturbed grid.  TODO: Expose some parameters for controlling the randomness.
(define-regiment-parameter simalpha-placement-type 'gridlike)

;; [2005.10.03] Can be:
;;   'lossless -- 100% until simalpha-outer-radius, 0% beyond  
;;   'linear-disc -- 100% at simalpha-inner-radius radius, 0% past outer, linear interpolation between.
;;   'empirical -- Uses gathered data on radio connectivity (UNIMPLEMENTED)
(define-regiment-parameter simalpha-channel-model 'lossless)

;; [2005.10.03] Can only be 'none right now.  Can implement other kinds of stopping failure at some point.
(define-regiment-parameter simalpha-failure-model 'none)

;; This is a read-only parameter set by the system.  
(define simalpha-connectivity-function (make-parameter 'uninitialized (lambda (x) x)))


(define-regiment-parameter simalpha-dbg-on #f)      ;; dbg print statements
(define-regiment-parameter simalpha-padding-warning #f) ;; warning when omitted args are zeroed/padded

;; When this parameter is turned on, the simulator returns a stream of
;; soc-return values rather than waiting for the sim to end

(define-regiment-parameter simalpha-stream-result #f)


;; This globally defined functions decides the sensor values.
;; Here's a version that makes the sensor reading the distance from the origin:
(define (sense-dist-from-origin id x y t)
  (sqrt (+ (expt x 2) (expt y 2))))

(define (sense-sine-wave id x y t)
  ;(printf "(sensing ~a ~a ~a ~a) " id x y t)
  ;(exact->inexact
   (inexact->exact 
    (floor
     (+ 127.5 (* 127.5 (sin (* t (/ 3.14 1000))))))))

;; This one changes amplitude across space:
(define (sense-spatial-sine-wave id x y t)
  ;(printf "(sensing ~a ~a ~a ~a) " id x y t)
  ;(exact->inexact
   (inexact->exact 
    (floor
     (let ((waveamp (+ 127.5 (* 127.5 (sin (* t (/ 3.14 1000))))))
	   (distorigin (sqrt (+ (* x x) (* y y))))
	   (maxdist (sqrt (+ (expt world-xbound 2) (expt world-ybound 2)))))
       (* waveamp (/ distorigin maxdist))))))

;; TODO: add noise to this, store state per ID: curry inputs:
(define (sense-noisy-rising id x y t)
  (/ t 100.))

(define (sense-random-1to100 id x y t)
  (add1 (reg:random-int 100)))

#;
(define (sense-fast-sine-wave id x y t)
  (printf "(sensing ~a ~a ~a ~a) " id x y t)
  (inexact->exact 
   (floor
    (+ 127.5 (* 127.5 (sin (* t (/ 3.14 1000))))))))

(define-regiment-parameter simalpha-sense-function 
  sense-spatial-sine-wave)
;  sense-sine-wave)


;; Used primarily by alpha_lib_scheduler_simple.ss
;;====================

;; Constant: amount of virtual time consumed by an action.  Nonzero to force forward progress.
;(define ACTION_LENGTH 100)  ;; Thus we ignore the "duration" field of simevts.
(define SCHEDULE_DELAY 1)
;; Vtimes:
(define RADIO_DELAY 30)  ;; Communication time
;(define PROCESSING_TIME 0)  ;; Not used yet... time to process incoming messages



