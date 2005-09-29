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

;; [2005.09.29] Moved this here because we need to access it from a variety of places.
;; And well, after all it is a *constant* (shouldn't change during compilation, except 
;; for unit testing and stuff which uses fluid-let against this binding).
(define pass-names
  '(verify-regiment
    eta-primitives
    rename-var
    remove-unquoted-constant                        ;;  5
    
    static-elaborate
    
    reduce-primitives    
    remove-complex-constant                         ;;  7
    uncover-free                                    ;; 14
    ;    convert-closure                                 ;; 15
    lift-letrec                                     ;; 16
    lift-letrec-body                                ;; 22
    remove-complex-opera*
    verify-core
    classify-names
    add-heartbeats
    add-control-flow
    add-places
    ;    add-routing
    analyze-places
    deglobalize
    
    cleanup-token-machine    

    desugar-gradients
    cleanup-token-machine   ;; Rerun to expand out some stuff.
    
    ;    analyze-tokmac-recursion
    ;    inline-tokmac

;    desugar-let-stored
;    rename-stored

;; Temporarily I am disabling these ..
    cps-tokmac
    closure-convert

    cleanup-token-machine ;; Trying this.. [2005.09.27]

    ;; moving these after closure-convert.  WHY? Can't remember atm [2005.09.27]
;; [2005.09.27] OH.  I moved them because I didn't want cps to split references to 
;; a let-stored variable across two tokens.  (That gets messy, one has to use ext-ref.)
    desugar-let-stored
    rename-stored

    ;    verify-token-machine
    ;    haskellize-tokmac 
    ))


;;======================================================================
;; This is not a very appropriate place for this definition, but it's the most convenient
;; so that it can be had from.
;;;;(define-syntax DEBUGMODE (syntax-rules () [(_ expr) expr]))
;;;;;;(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (begin expr ...)]))
;; ON
(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (list expr ...)]))
;; OFF
;(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) ()]))


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

;; Used primarily by pass21_cleanup_tokmac
;;===================================================
(define DEFAULT_SUBTOK 0)
(define DEFAULT_SUBTOK_VAR 'subtok_ind)
(define MAX_SUBTOK 1000)  ;; We allow there to be 1000 copies of any one token.

;;======================================================================

(define regiment-parameters (make-parameter '()))
(define-syntax define-regiment-parameter
  (syntax-rules () 
    [(_ name args ...)
     (define name 
       (begin (regiment-parameters (cons (quote name) (regiment-parameters)))
	      (make-parameter args ...)))]))

;; This parameter determines whether the compiler should output extra debugging info.
(define-regiment-parameter regiment-verbose #f)

;; This one optionally provides a target for logging simulation data.
(define-regiment-parameter simulation-logger #f)
;; This sets the level at which we log messages.  All logger calls with less/eq this go through.
(define-regiment-parameter simulation-logger-level 5)  ;; Very inclusive at first.

;; Just a counter for the simulation logger messages.  
;; If it's #f that means it's not set, but it can be 
;; set to zero at the start of a simulation.
(define-regiment-parameter simulation-logger-count #f)

(define-regiment-parameter regiment-consec-ids #t)



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

;; Vtimes:
(define RADIO_DELAY 10)  ;; Communication time
;(define PROCESSING_TIME 0)  ;; Not used yet... time to process incoming messages

(define-regiment-parameter simalpha-num-nodes 30)
(define-regiment-parameter simalpha-output-port #f) ;; If this is false, default is stdout.

(define-regiment-parameter simalpha-dbg-on #f)      ;; dbg print statements
(define-regiment-parameter simalpha-padding-warning #f) ;; warning when omitted args are zeroed/padded


;; Used primarily by alpha_lib_scheduler_simple.ss
;;====================

;; Constant: amount of virtual time consumed by an action.  Nonzero to force forward progress.
;(define ACTION_LENGTH 100)  ;; Thus we ignore the "duration" field of simevts.
(define SCHEDULE_DELAY 1)



