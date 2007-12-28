
;; TODO: is_scheduled etc...

;; TODO: REHIDE the scheduler state if we don't need it!!

;;;; .title Simulator Alpha: A second Token Machine simulator.
;;;; .author Ryan Newton [2005.02.25]


;;;;<br> Related files include: (as of [2006.03.01]) 
;;;;<br>   simulator_alpha_datatypes.ss  -- data type definitions             
;;;;<br>   alpha_lib.ss                  -- "run time" library for the running simulator. 
;;;;<br>   alpha_lib_scheduler_simple.ss -- Basic action scheduler            
;;;;<br>   alpha_lib_scheduler.ss        -- NOT USED right now [2005.09.27]   
;;;;<br>   simalpha_rollworld.ss         -- Constructing random topologies    
;;;;<br>   simalpha_ui.ss                -- Functions for printing/displaying sim state 
;;;;<br>   simulator_nought.examples.ss  -- Some sample programs              
;;;;<br>                                   (for old sim, but still applicable)
;;;;<br>  

;===============================================================================

;;;; This will be a second attempt simulator.  (The first was simulator_nought.)
;;;; It will support only core tml (no gradients). <br><br>
;;;;
;;;; It will have a single thread of control and a queue of simulator
;;;; events sorted by virtual clock times. <br><br>
;;;;
;;;; Later, it may serve as a place to test scheduling algorithms so
;;;; that we can actually implement the atomic action model (with
;;;; action-abort). <br><br>
;;;;
;;;; 


; NOTE: Right now all calls go through the dyndispatch table.

;===============================================================================
; Changes:

; [2005.10.03] Implemented token-deschedule

; [2005.10.18] Factored out datatype defs and global params (and even
; some helper functions) to a separate files.

; [2006.03.01] Factored again, removing UI functions and
; world-construction.  Now only the front-end invocation procedure and
; the tm-to-sim code-conversion should be in this file.

;===============================================================================

(module simulator_alpha mzscheme

  (require 
   ;(lib "compat.ss")
   (lib "include.ss")
   (lib "pretty.ss")
   (prefix srfi1. (lib "1.ss" "srfi")) ; make-list
   
   "../../plt/iu-match.ss"       
   "../../plt/hashtab.ss"   
   "../compiler_components/prim_defs.ss"
   "../compiler_components/logfiles.ss"

   (all-except "simulator_alpha_datatypes.ss")
   (all-except "../constants.ss" test-this these-tests)
   (all-except "../util/helpers.ss" id flush-output-port test-this these-tests)
   (all-except "../util/tsort.ss" test-this these-tests)      
   (all-except "../compiler_components/regiment_helpers.ss" id flush-output-port test-this these-tests)
   (all-except "../passes/tokmac_bkend/cleanup-token-machine.ss" test-this these-tests)
   
   (all-except "alpha_lib.ss" test-this these-tests)
   (all-except "alpha_lib_scheduler_simple.ss")
   )

  (provide 

   run-simulator-alpha rerun-simulator-alpha clean-simworld!
   compile-simulate-alpha csa ; shorthand
   test-this these-tests
   
   simalpha-total-messages
   print-stats print-node-stats 
   print-connectivity plot-connectivity
   fresh-simulation
   simalpha-draw-world	 
   
   freeze-world
   animate-world!
   
   testalpha 

   )

  (chezimports ;scheme
	       chez_constants
	       logfiles
	       (except tsort test-this these-tests)
	       regiment_helpers
	       ;;simulator_alpha_datatypes
	       alpha_lib_scheduler_simple)

  (IFCHEZ
   ;; [2005.11.05] This fills in the implementation-specific casing for the generated code:
   ;; Could just be identity function, but wrapping in a module should give better performance.
   (define (build-genned-code-module node-code)
     (if (simalpha-generate-modules)
	 `(let ()
	    (chez:module genned-code (node-code) 
	      (import scheme)      (import constants)
	      (import logfiles)    (import simulator_alpha_datatypes)
	      (import alpha_lib)   (import alpha_lib_scheduler_simple)
	      ,node-code)
	    (import genned-code)
	    (set-top-level-value! 'node-code node-code)
	    )
	 node-code))
   ;; This PLT version uses require to make sure the support code is loaded.
   (define (build-genned-code-module node-code)
     `(begin 	       
        (current-directory (string-append (REGIMENTD) "/src/"))
        (module _genned_node_code mzscheme
	       ;(provide node-code)	       
	       (require "generic/constants.ss")
	       (require "generic/compiler_components/logfiles.ss")
	       (require "generic/util/hash.ss")
	       (require "plt/hashtab.ss")
	       (require (all-except "generic/util/helpers.ss" test-this these-tests))
	       (require (all-except "generic/sim/simulator_alpha_datatypes.ss" test-this these-tests))
	       (require (all-except "generic/sim/alpha_lib.ss" test-this these-tests))
	       (require "generic/sim/alpha_lib_scheduler_simple.ss")
	       ;; Bind at top level:
	       (let ()
		 ,node-code
		 (eval `(define node-code ,node-code)))
	       )
	     (require _genned_node_code))))

  ;; We are loaded from the root directory, not the chez subdirectory.
  ;;(include "generic/simulator_nought.examples.ss")
;  (include (** (REGIMENTD) "/src/generic/simalpha_ui.ss"))
;  (include (** (REGIMENTD) "/src/generic/simalpha_rollworld.ss"))
;  (include (** (REGIMENTD) "/src/generic/simulator_alpha.ss"))

  
(IFCHEZ
 (begin 
   (include "generic/sim/simalpha_ui.ss")
   (include "generic/sim/simalpha_rollworld.ss"))
 (begin
   (include "simalpha_ui.ss")
   (include "simalpha_rollworld.ss")))

;(reg:include "generic" "simalpha_ui.ss")
;(reg:include "generic" "simalpha_rollworld.ss")


;======================================================================
;;; Helper functions.

;; Simple free variables function:
(define (simalpha-free-vars expr)
  (let loop ((env ()) (expr expr))
    (match expr	 
	   [,var (guard (symbol? var)) (if (memq var env) '() (list var))]   
	   [(quote ,x) '()]
 	   [(,prim ,rand* ...) (regiment-primitive? prim)
	    (let ((frees (map (lambda (x) (loop env x)) rand*)))
	      (apply append frees))]
	   [(lambda (,formals) ,expr)
	    (loop (append formals env) expr)]
	   [,else (error 'simalpha-free-vars "not simple expression: ~s" expr)])))


;; Here's a helper to check the invariants on a msg-object
#|(define (valid-msg-object? mo)
  (and (msg-object? mo)
       (let ([token  (msg-object-token  mo)]
	     [timestamp (msg-object-timestamp mo)]
	     [origin (msg-object-origin mo)]
	     [parent (msg-object-from mo)]
	     [count  (msg-object-count  mo)]
	     [args   (msg-object-args   mo)])	
	 (or 
	  #;(and (return-msg? mo) ;; Things are different for return-messages.
		  (list? count) (andmap integer? count)
		  (list? timestamp) (andmap integer? timestamp)
		  (or (not parent) (simobject? parent))
		  (or (not origin) (simobject? origin))
		  (list? args)		  
		  )
	     (and (token-name? token)
		  (or (not timestamp) (integer? timestamp))
		  (or (not parent) (simobject? parent))
		  (or (not origin) (simobject? origin))
		  (integer? count)
		  (list? args)))))
|#


;----------------------------------------

(define (base-station? x)
  (cond 
   [(simobject? x) (= BASE_ID (node-id (simobject-node x)))]
   [(node? x)      (= BASE_ID (node-id x))]
   [else (error base-station? "bad input: ~s" x)]))

(define (id x) x)
  
(define (dotted-append ls ob)
  (let loop ((ls ls))
    (if (null? ls) ob
	(cons (car ls) (loop (cdr ls))))))

;; Increment a top-level binding.  Totally dynamic.
(define (incr-top-level! v)
  (set-top-level-value! v (add1 (top-level-value v))))

;; TODO, returns all the nodes in the graph that are connected to the
;; given simobject.  Gonna use this for unit testing oracles.
(define (all-connected simob sim)
  (graph-get-connected-component simob (simworld-object-graph sim)))

                         
; =======================================================================

;;; Compiling for simulation.

;; Subroutine of compile-simulate-alpha below.
(define (process-statement current-handler-name tokbinds stored)

  (define massage-str 
    ;; Turns a %d using string into a ~a using string.
    (lambda (s)
      (let ((newstr (string-copy s)))
	(let loop ((i (- (string-length s) 2)))
	  (cond
	   [(< i 0) (void)]
	   [(and (eq? (string-ref s i) #\%)
		 (eq? (string-ref s (add1 i)) #\d))
	    (string-set! newstr i #\~)
	    (string-set! newstr (add1 i) #\a)
	    (loop (sub1 i))]
					;				 [(and (eq? (string-ref s i) #\\)
					;				       (eq? (string-ref s (add1 i)) #\n))
					;				  (string-set! newstr i #\~)
					;				  (loop (sub1 i))]
	   [else (loop (sub1 i))]))
	newstr)))
  
  ;; This is a substitution list, used for simple renaming of primitives:
  ;; Right now I'm tightening up my numeric ops, getting it more ready for static typing. -[2005.10.20] 
  ;; TODO: Make this PLT compatible.
  (define prim-substs
    '(;; [2007.01.28] Should phase these plain versions out:
      [+ fx+] [- fx-] [* fx*] [/ fx/]
      [_+_ fx+] [_-_ fx-] [*_ fx*] [/_ fx/]
      [_+. fl+] [_-. fl-] [*. fl*] [/. fl/]
      [_+: cfl+] [_-: cfl-] [*: cfl*] [/: cfl/]
      [^ expt] [^_ expt] [^. expt] [^: expt]
      [int->float fixnum->flonum]
      [float->int flonum->fixnum]
      [random reg:random-int]
      [List:length length]
      [length vector-length]
      [fold foldl]

      ))
  
  (let ([allstored (apply append (map cadr stored))])
    (letrec ([find-which-stored
	      (lambda (v)
		(let loop ([ls stored])
		  (let ((pos (list-find-position v (cadr (car ls)))))
		    (if pos (values (caar ls) pos)
			(loop (cdr ls))))))]
    
	     [process-expr 
	      (lambda (expr)
		;; This is a little wider than the allowable grammar to allow
		;; me to do test cases:
		(match expr		       
		  [(quote ,const) `(quote ,const)]		  
		  [,num (guard (number? num)) num]

		  [(BLACKBOX ,[stuff]) 
		   ;; This finally gets popped open here at the simulator.
		   stuff]

		  ;; Token references return pairs which index the token store hash table.
		  [(tok ,t ,[e]) `(make-simtok ',t ,e)]

		  ;; This is sketchy: we just fail silently and return #f if there is no token there.
		  ;; FIXME TODO: We should seriously have some sort of better error handling convention.
		  ;; FIXME TODO: Should I use fresh var bindings for the exttokobj bindings below???
		  [(ext-ref (tok ,tokname ,[subtok]) ,x)
		   (guard (and (symbol? x) (memq x allstored)))
		   ;; The stored names should be unique at this point!  So this should be ok:
		   (mvlet ([(which-tok pos) (find-which-stored x)])
		     (DEBUGMODE
		      (if (not (eq? which-tok tokname))
			  (error 'simulator_alpha:process-statement 
				 "bad ext-ref: (ext-ref (~s . ~s) ~s)" tokname subtok x)))
		     `(let ([exttokobj (retrieve-token the-store (make-simtok ',tokname ,subtok))])
			,(format "Ext-ref of (tok ~s ~s) variable ~s" tokname subtok x)
			(if exttokobj
			    (,(string->symbol (format "~a-~a" tokname x)) exttokobj)
			    #f)))]
		  [(ext-set! (tok ,tokname ,[subtok]) ,x ,[e])
		   (guard (and (symbol? x) (memq x allstored)))		   
		   (mvlet ([(which-tok pos) (find-which-stored x)])
			  (DEBUGMODE
			   (if (not (eq? which-tok tokname))
			       (error 'compile-simulate-alpha:process-statement 
				      "bad ext-set to: (ext-ref (~s . ~s) ~s)" tokname subtok x)))
			  `(let ([exttokobj (retrieve-token the-store (make-simtok ',tokname ,subtok))])
			     ,(format "Ext-set! of (tok ~s ~s) variable ~s" tokname subtok x)
			     (if exttokobj
				 (,(string->symbol (format "set-~a-~a!" tokname x)) exttokobj ,e)
				 (warning 'ext-set! "var ~s: token not present: ~s" ',x `(,tokname . subtok))
				 )))]
		  
		  ;; [2006.02.16] Shouldn't really be using this right now.  Look into getting rid of:
		  [(ext-ref ,[e] ,n)  (guard (integer? n))
		   ;; [2006.01.12] Ack, I went to the trouble of making my token 
		   ;; objects use records rather than simply being vectors.  But 
		   ;; now I want to reference them by index!  
		   ;; Oh well, this gets ugly, supporting with an addition to the alpha_lib.

		   `(let ([exttokobj (retrieve-token the-store ,e)])
		      (if exttokobj (tokobj-ref exttokobj ,n) #f))]

		  [(ext-set! ,[e] ,n ,[v])  (guard (integer? n))
		   `(tokobj-set! ,e ,n ,v)]

		  [(ext-ref ,foo ...)
		   (error 'compile-simulate-alpha:process-statement
			  "Ext-refs are only allowed for known tokens at the moment: ~s" `(ext-ref ,foo ...))]
		  [(ext-set! ,foo ...)
		   (error 'compile-simulate-alpha:process-statement
			  "Ext-set!s are only allowed for known tokens at the moment: ~s" `(ext-set! ,foo ...))]		  

		  ;; Local tokstore-reference:
		  [,x (guard (and (symbol? x) (memq x allstored)))
                    (mvlet ([(which-tok pos) (find-which-stored x)])
                           (if (not (eq? which-tok current-handler-name))
                               (error 'compile-simulate-alpha:process-statement 
				      "bad local stored-ref: ~s actually belongs to ~s not ~s" 
				      x which-tok current-handler-name))
                           ;; 'tokobj' is already bound to our token object
			   `(begin 
			     ,(format "Local Stored Ref of variable: ~s" x)
			     (,(string->symbol (format "~a-~a" which-tok x)) tokobj)))]

		  [,x (guard (or (symbol? x) (simple-constant? x))) x]
		  [(quote ,x) `(quote ,x)]

		  [(begin) '(void)]
		  [(begin ,[x] ...) `(begin ,x ...)]

		  ;; Subcall may or may-not have been eliminated by CPS at this point:
		  ;; TODO: Implement subcall directly:
		  [(,subcall ,[rator] ,[rand*] ...)		  
		   (guard (memq subcall '(subcall direct-subcall)))
		   ;; Call the handler directly
		   `(begin "Simulator subcall code" 
			   ((simobject-meta-handler this)
			    (bare-msg-object ,rator
					     (list ,@rand*) current-vtime)
			    current-vtime))]
		  [(return ,[x]) x]

		  ;; [2006.10.27]
		  ;; This is yet another thing that probably shouldn't be allowed at the TM level:
		  ;; It maps a token-handler over a list with subcall (essentially).
		  [(tokmap ,[rator] ,[rand])
		   `(let ([rat ,rator]
			  [rnd ,rand])
		      (let ([f (lambda (x) 
				 ((simobject-meta-handler this)
				  ;; HACK: CHEATING AND PUTTING IN GRADIENT ARGS:
				  (bare-msg-object rat (list 0 0 'nongrad-invoke 0 x) current-vtime)
				  current-vtime))])			
					;(inspect (cons 'map rnd))
					;(inspect (cons 'mapresult (map f rnd)))
			(map f rnd)))]
		  ;; And this:
		  [(fold ,[rator] ,[zero] ,[rand])
		   `(let ([rat ,rator]
			  [zer ,zero]
			  [rnd ,rand])
		      (let ([f (lambda (stt x)				 
				 ((simobject-meta-handler this)
				  ;; HACK: CHEATING AND PUTTING IN GRADIENT ARGS:
					;          (g_parent g_origin g_hopcount g_version a_31 b_30)
				  (bare-msg-object rat (list 0 0 'nongrad-invoke 0 stt x) current-vtime)
				  current-vtime))])
			;(inspect (cons 'fold rnd))
			(foldl f zer rnd)))]

		  [(List:append ,[a] ,[b]) `(append ,a ,b)]
		  
		  ;; NOTE! These rands ARE NOT simple.
		  [(call ,[rator] ,[rand*] ...)
		   (let ((rands (unique-name 'rands)))
                     `(let ((,rands (list ,@rand*)))
			;; Make sure those ^^ are all done evaluating before we mess with the msg buf:
			(set-simobject-local-msg-buf! this
                           (cons (make-simevt #f ;; No scheduled time, ASAP
                                              (bare-msg-object ,rator ,rands current-vtime))
                                 (simobject-local-msg-buf this)))))]

		  
		  ;; [2005.10.31] This is my first hack at high-priority scheduling for subcalls:
		  [(call-fast ,[rator] ,[rand*] ...)
		   (let ((rands (unique-name 'rands)))
                     `(let ((,rands (list ,@rand*)))
			;; Make sure those ^^ are all done evaluating before we mess with the msg buf:
			(set-simobject-timed-token-buf! this
                           (cons (make-simevt (- current-vtime 1) ;; Scheduled time is yesterday!
                                              (bare-msg-object ,rator ,rands current-vtime))
                                 (simobject-timed-token-buf this)))))]
		     
		  [(bcast ,[rator] ,[rand*] ...)		   
		   `(begin 
		      ;(simalpha-total-messages (add1 (simalpha-total-messages)))
		      (set-simobject-outgoing-msg-buf! this
  		        (cons (make-simevt #f ;; No scheduled time, ASAP
					   (bare-msg-object ,rator (list ,@rand*) current-vtime))
			      (simobject-outgoing-msg-buf this))))]

		  [(ucast ,[dest] ,[rator] ,[rand*] ...)
		   (let ([tmp (unique-name 'tmpmsgob)])
		     `(let ([,tmp (bare-msg-object ,rator (list ,@rand*) current-vtime)])
			;; Set the "to" field.
			(set-msg-object-to! ,tmp ,dest)
			(set-simobject-outgoing-msg-buf! this
  		           (cons (make-simevt #f ,tmp) ;; No scheduled time, ASAP 
				 (simobject-outgoing-msg-buf this)))))]

		  ;; This could potentially be desugared by an earlier pass and made 
		  ;; split-phase by the CPS algorithm.  But currently I'mjust 
		  ;; implementing it directly. [2006.01.25]
		  [(ucast-wack ,[dest] ,[rator] ,[rand*] ...)
		   (let ([tmp (unique-name 'tmpmsgob)]
			 [dst (unique-name 'dest)]
			 [conn (unique-name 'conn)])
		     ;; Check the link quality, does this message go through?
		     `(let* ([,dst ,dest]
			     [,conn ,(process-expr `(linkqual-to ,dst))]) ;(get-connectivity ,(process-expr '(my-id)) ,dst)])
			;(printf "Ucast Wack, conn: ~a\n" ,conn)
			(if (< (random 100) ,conn)
			    (let ([,tmp (bare-msg-object ,rator (list ,@rand*) current-vtime)])
			      ;(printf "Got ACK! ~a\n" ,tmp)
			      ;; Set the "to" field.
			      (set-msg-object-to! ,tmp ,dst)
			      (set-simobject-outgoing-msg-buf! this
			         (cons (make-simevt #f ,tmp) ;; No scheduled time, ASAP 
				       (simobject-outgoing-msg-buf this)))
			      #t)
			    #f)))]

;; These are desugared before now.
;		  [(activate ,rator ,rand* ...)
;		   (build-activate-call `(quote ,rator) rand*)]

		  [(timed-call ,delay ,[rator] ,[rand*] ...)
		   ;; Delay is in milleseconds.
		  `(set-simobject-timed-token-buf!
		    this (cons (make-simevt (+ ,delay current-vtime)
					    (bare-msg-object ,rator (list ,@rand*) current-vtime))
			       (simobject-timed-token-buf this)))]

		  ;; If it's in the hash table, it's present:
		  ;; This is static wrt to token name for the moment:
;		  [(token-present? (tok ,t ,n)) (guard (number? n)) 
;		   `(if (hashtab-get the-store (make-simtok ',t ,n)) #t #f)]  ;; Needs not be separate case. [2005.10.03]
		  [(token-present? ,[e])
		   `(if (retrieve-token the-store ,e) #t #f)]

		  [(evict ,[e]) `(evict-token the-store ,e)]
		  [(evict-all ,[e]) `(evict-all-tokens the-store ,e)]

		  [(token-scheduled? ,[tok]) ;; INEFFICIENT
		   ;; queue is list containing (simevt . simob) pairs.
		   ;; We go through the current queue looking for a scheduled event that matches this token.
		   ;; NOTE: This is the queue of *all* events, we also must make sure it happens on this *node*.
		   `(begin		      
		      #;
		      (disp "SCANNING QUEUE: " (length (simworld-scheduler-queue (simobject-worldptr this)))
			    "and locals: "     (length (simobject-local-msg-buf this))
			    (map msg-object-token 
				 (map simevt-msgobj
				      (simobject-local-msg-buf this))))
		      
		      ;; FIXME: 
		      ;; PROFILING INDICATES THAT THIS IS VERY SLOW:

		      (or ; First, check the schedulers queue:
		       ;; (TODO FIXME, CHECK: THIS MIGHT NOT EVEN BE NECESSARY:)
		       (let loop ((queue (simworld-scheduler-queue (simobject-worldptr this))))
			 (if (null? queue) #f 
			     (let ((simtok (msg-object-token (simevt-msgobj (caar queue)))))
			       (or (and (eq? this (cdar queue)) ;; Is it an event on this node.
					(simtok-equal? ,tok simtok)
					(begin 	       
					  (if (regiment-verbose)
					  (DEBUGMODE (printf "Wow! we actually found the answer to ~s\n"
							     "token-scheduled? in the scheduler-queue!")))
					  #t)
					) ;; If so is it the token in question?
				   (loop (cdr queue))))))
		       ;; Second, check the msg bufs:
		       (let loop ((locals (append 
					   (simobject-local-msg-buf this)
					   (simobject-timed-token-buf this)
					   )))
			 (if (null? locals) #f
			     (let ((simtok (msg-object-token (simevt-msgobj (car locals)))))
			       (or (simtok-equal? ,tok simtok)
				   (loop (cdr locals)))))))
		       )]
		  [(token-deschedule ,[tok]) ;; INEFFICIENT
		   ;; queue is list containing (simevt . simob) pairs.
		   ;; We go through the current queue looking for a scheduled event that matches this token.
		   ;; NOTE: This is the queue of *all* events, we also must make sure it happens on this *node*.
		   `(let* ([thistok ,tok]
			   [notthistok (lambda (x) 
					 (let ((tok (cond 
						     [(and (pair? x) (simevt? (car x)))
						      (msg-object-token (simevt-msgobj (car x)))]
						     [(simevt? x)
						      (msg-object-token (simevt-msgobj x))]
						     [else (error 'token-deschedule-generated-code "code error.")])))
					;(not (and (eq? (simtok-name thistok) (simtok-name tok))
					;	  (eq? (simtok-subid thistok) (simtok-subid tok))))
					   (not (simtok-equal? tok thistok))))]
			   [world (simobject-worldptr this)])
		      ;; Clear the actual simulation queue.
		      (set-simworld-scheduler-queue! world (filter notthistok (simworld-scheduler-queue world)))
		      (set-simobject-local-msg-buf! this (filter notthistok (simobject-local-msg-buf this)))
		      (set-simobject-timed-token-buf! this (filter notthistok (simobject-timed-token-buf this)))

		       )]



		  ;; Local Stored variable mutation:
		  [(set! ,v ,[rhs]) (guard (memq v allstored))
		   (mvlet ([(which-tok pos) (find-which-stored v)])
			  (DEBUGMODE
			   (if (not (eq? which-tok current-handler-name))
			       (error 'compile-simulate-alpha:process-statement 
				      "(set!) bad local stored-ref: ~s actually belongs to ~s not ~s" 
				      v which-tok current-handler-name)))
			  `(begin 
			     ,(format "Local Stored Set! of variable: ~s" v)
			     (,(string->symbol (format "set-~a-~a!" which-tok v)) tokobj ,rhs)))]

		  [(set! ,v ,[rhs])  `(set! ,v ,rhs)]

		  [(if ,[test] ,[conseq] ,[altern])
		   `(if ,test ,conseq ,altern)]

		  ; =========================================
		  ;; Now we generate specific code for many of the primitives

		  [(,senseprim)
		   (guard (memq senseprim '(async-sense sync-sense)))
		   (process-expr `(,senseprim '"default"))]
		  [(,senseprim (quote ,type))
		   (guard (memq senseprim '(async-sense sync-sense)))
		   (unless (string? type) (error 'senseprim "invalid sensor type specifier: ~s" type))
		   ;; Feed time, then type, id, x, y to sensor function:
		   `(((simalpha-sense-function) ,(process-expr '(my-clock)))
		     ',(string->symbol type)       ;; sensor-type
		     ,(process-expr '(my-id))      ;; node-id
		     (car ,(process-expr '(loc)))  ;; x-coord
		     (cadr ,(process-expr '(loc))) ;; y-coord
		     )]

		  [(my-id) '(node-id (simobject-node this))]
		  [(my-clock)  'current-vtime]

		  ;; FIXME: These are temporarily symmetric.
		  [(,linkqual ,[idexp])
		   (guard (memq linkqual '(linkqual-to linkqual-from)))
		   `(let* ([idnum ,idexp]
			   [nbr (hashtab-get (simworld-obj-hash (simalpha-current-simworld))
					     idnum)])
		      (if (not nbr) (error 'linkqual "Cannot estimate link qual, bad neighbor ID: ~a" idnum))
		      ((simworld-connectivity-function (simalpha-current-simworld))
		       (node-pos (simobject-node this))
		       (node-pos (simobject-node nbr))))]

		  [(rgb ,[r] ,[g] ,[b]) `(make-rgb ,r ,g ,b)]

		  ;; [2005.05.07] Shouldn't run into this now:
#;		  [(soc-return ,x)
		   (process-expr `(return ,x 
					  (to (tok SOC-return-handler 0) )
					  (via (tok global-tree 0))
					  (seed '#f)
					  (aggr #f)))]
;		   (process-expr `(call (tok SOC-return-handler 0) ,x))]
		 
		  ;[(soc-return-finished ,x) 		  

		;; [2005.10.02] Inserting magical soc-returns so that
		;; we can (kind-of) simulate code where they haven't
		;; been desugared.
		[(soc-return ,[x]) `(simulator-soc-return ,x)]

		[(token->subid ,[e]) `(simtok-subid ,e)]

		[(loc) '(sim-loc)]
		[(locdiff ,[l1] ,[l2]) `(sim-locdiff ,l1 ,l2)]
		
		[(light-node ,r ,g ,b) `(sim-light-up ,r ,g ,b)]
		[(draw-mark ,[loc]) `(sim-draw-mark ,loc)]
		[(draw-mark ,[loc] ,[col]) `(sim-draw-mark ,loc ,col)]
		[(leds ,which ,what) `(sim-leds ',which ',what)]
		[(highlight-edge ,[args] ...) `(sim-highlight-edge ,args ...)]

		; =================================================================================
		;; Printing functions
		[(dbg (quote ,str) ,[args] ...)
		 ;; TODO FIX ME: would be nice to print properly
		 (let ()
;		     (disp "MANGLED" (massage-str str))
		     (let ((massaged (massage-str str)))
		       `(begin (if (simalpha-dbg-on)
				   (begin (display (format ,massaged ,@args) (console-output-port))
					  (newline (console-output-port))))
			       ;; Send a copy to the logger as well:
			       (logger 1 (simworld-vtime (simalpha-current-simworld)) '_ 
				       'DBG `[msg ,(apply format massaged args)])
			       )))]
		[(printf (quote ,str) ,[args] ...)
		 (let ((massaged (massage-str str)))
		   `(sim-print ,massaged ,@args))]
		[(setlabel (quote ,str) ,[args] ...)
		 `(sim-setlabel (format ,str ,@args))]
		; =================================================================================


		;; Any prim apps that didn't get caught above are equivalent to the normal Scheme versions:
		[(,prim ,[rand*] ...)
		 (guard (or (token-machine-primitive? prim)
			    (basic-primitive? prim)))
;		 (printf "Prim : ~s\n" prim)
		 (let ((entry (assq prim prim-substs)))
		   (if entry
		       `(,(cadr entry) ,rand* ...)
		       `(,prim ,rand* ...)))]

		;; We're being REAL lenient in what we allow in token machines being simulated:
		[(,lettype ((,lhs ,[rhs]) ...) ,[bods] ...)
		 (guard (memq lettype '(let let*)))
		 `(,lettype ((,lhs ,rhs)  ...) ,bods ...)]
		;; We're letting them get away with other primitives because
		;; we're being lenient, as mentioned above.
		[(app ,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
		
		;; Supporting the output of cps-tokmac also:
		[(kcall ,[rator]  ,[rand*] ...)
		 (let ((tmp (unique-name 'tmprator))
		       (tmp2 (unique-name 'tmprands)))
		   `(let ((,tmp ,rator)
			  (,tmp2 (list ,@rand*)))
		      ;(printf "Kcall ~s ~s ..\n" ,tmp ,tmp2)
		      (if (eq? ,tmp ,NULLK)
			  "kcall fizzles."
			  (apply ,tmp ,tmp2))))]
		[(lambda (,v ...) ,[bod]) `(lambda (,v ...) ;(printf "cont invoked...\n")
						   ,bod)]
		
		[(app ,rator ,[rand*] ...)
		 ;; This is an arbitrary scheme application. Where would these come from?
		 ;; Don't want to be too lenient tho:
		   (guard (not (token-machine-primitive? rator))
			  (not (memq rator '(emit bcast call timed-call activate relay return call-fast))))
		   (warning 'simulator_alpha.process-expr
			    "arbitrary rator applied: ~s" rator)
		   `(,(process-expr rator) ,rand* ...)]
		
		[,otherwise (error 'simulator_alpha.process-expr 
				   "don't know what to do with this: ~s" otherwise)])
	   )])
    (lambda (stmt) (process-expr stmt)))))


;; Subroutine of compile-simulate-alpha below.
(define (process-binds binds)
;  (disp "processbinds" binds expr)
  (let* ([graph (map (lambda (bind)
		       (cons (car bind)
			     (simalpha-free-vars (cadr bind))))
		     binds)]
	 [flat (reverse (topological-sort graph ))];eq?))]
	 [newbinds 
	  (map (lambda (sym) 
		 (let ([bind (assq sym binds)])
		   (if bind bind
		       (error 'simulator_alpha:process-binds
			      "no entry for sym '~s' in constant binds ~s" sym binds))))
	       flat)])
    (if (cyclic? graph)
	(error 'process-binds "for now can't take a cyclic graph: ~s" newbinds))
    newbinds))
	 

;; Every token handler, once converted for the simulator, has a signature: <br>&nbsp;
;; 
;;   simobject, vtime, subtoken-index -> args -> ()                        <br>&nbsp;
;;   (mutates tokstore, outgoing-msg-buf, local-msg-buf, timed-tokens)     <br>
;;
;; The result of running a handler is to mutate certain fields of the
;; simobject.  The handler *does not* directly interface with the
;; scheduler.  It merely changes the token store and accumulates local,
;; remote, and timed messages.                                             <br>
;;
;; This takes token bindings, returns compiled bindings along with an
;; association list of stored-vars.
;; .parameter tbinds Token bindings (source code)
;; .returns   2 values: compiled-binds, ((tokname storedV ...) ...)
(define (process-tokbinds tbinds)
  (let* ([allstored
          (map (lambda (bind)
		 (mvlet ([(tok id args stored bindings body) (destructure-tokbind bind)])
			(list tok (map car stored))))
               tbinds)]
			 
         [binds 
          (map
           (lambda (tbind)
	     (mvlet ([(tok id args stored bindings body) (destructure-tokbind tbind)])
		      `[,tok 
			(lambda (current-vtime ,id . vals) ;world)

			  ;; Set the global "this" for the below dynamic extent.
			  (parameterize ((current-simobject this))
			  (let ,(map list args (make-list (length args) ''sim-alpha-uninitialized))

			    (let ([numvals (length vals)])
			      ;; Issue a single warning if we're padding arguments:
 			      (if (< numvals ,(length args))
				  (case (simalpha-zeropad-args) 
				    [(#t) (void)]
				    [(warning)
				      (warning 'simulator-alpha "executing handler <~s.~s> padding args ~s with zero." 
					       ',tok ,id (list-tail ',args numvals))]
				    [(#f) (error 'simulator-alpha "executing handler <~s.~s> padding args ~s with zero.\nSupplied args were:\n~s\n"
						 ',tok ,id (list-tail ',args numvals) vals)]))
 			      (if (> numvals ,(length args))
 				  (error 'simulator-alpha "executing ~s, got too many args: ~s for inputs ~s" 
					 ',tok vals ',args))
			      
			      ,@(map (lambda (arg)
				       `(if (null? vals)
					    (set! ,arg 0)
					    (begin (set! ,arg (car vals))
						   (set! vals (cdr vals)))))
				  args)
			      "Done initializing arguments."

			    (let* ([the-store (simobject-token-store this)]
				   [simtok-obj (make-simtok ',tok ,id)]
				   [old-outgoing (simobject-outgoing-msg-buf this)]
				   [old-local    (simobject-local-msg-buf this)])
			      (DEBUGMODE 
			       ;; Check invariants on the store:
			       (check-store the-store))

			      "Is there already an allocated token object?:"
			      ;; Requires equal? based hash table:
			      (let ([tokobj (retrieve-token the-store simtok-obj)])
				(if (not tokobj)				   
				    (begin "If not, then we allocate that token object..."
					   " setting the invoke counter to zero."
					   (set! tokobj (,(string->symbol (format "make-~a" tok))
							 ;0 ;; invoke-counter
							 ,@(map cadr stored)))
					   (add-token the-store simtok-obj tokobj)
					   ))
				(set-simobject-outgoing-msg-buf! this '())
				(set-simobject-local-msg-buf! this '())
				;; Timed-token-buf need not be reversed, because it is ordered by vtime.
				(let ((this-handler-retval ,((process-statement tok tbinds allstored) body)))
				  ;; We must reverse the outgoing order because of how they were added:
				  (set-simobject-outgoing-msg-buf! this 
	   		  	    (append (reverse (simobject-outgoing-msg-buf this)) old-outgoing))
				  (set-simobject-local-msg-buf! this 
                                    (append (simobject-local-msg-buf this) ;(reverse (simobject-local-msg-buf this)) 
					    old-local))
				  this-handler-retval)
				))))))
                         ]))
	  tbinds)])
    (printf "~n;  Converted program for Simulator:~n")
    (printf "Allstored was:~n") (pretty-print allstored)
    (printf "<-------------------------------------------------------------------->~n")
    ;(pretty-print binds)

    (values binds allstored)))

; =======================================================================


;; This is the procedure for compiling a TML program into a simulator program. <br><br>
;; The resulting binding for node-code is a function that takes the "this" simobject.  
;; And returns a "meta-token-handler", which has type: <br>&nbsp;&nbsp;
;;   (msg-object, vtime -> ()) <br>
;; This meta-handler is a dispatcher for all of the individual token
;; handlers.  It dispatches based on the token-name contained in the msg-object.
;; 
;; .returns A code module containing a definition for "node-code".
(define (compile-simulate-alpha prog . extraparams)
  ;; .parameter prog A Token-Machine either with or without the (foo-lang ...) wrapper.
  ;; .parameter extraparams A list of bindings to "parameterize" when the simulation runs.

  (let ((prog (match prog 
		     [(tokens ,t ...) `(program (bindings) (nodepgm (tokens ,t ...)))]
		     [(program ,_ ...) prog]
		     [(,input-lang '(program ,stuff ...)) `(program ,stuff ...)])))
    (match prog
      [(program (bindings ,nodebinds ...)
		(nodepgm (tokens ,nodetoks ...) ;(startup ,starttoks ...)
			 ))
       
       ;; Here we hack in some special handlers that are assumed by TML:
       ;; These handlers are processed by process-tokbinds below, they
       ;; are thus part of the input grammar rather than the output grammar.
       (set! nodetoks
	     ;; An extra handler for doing SOC-return's...
	     `([SOC-return-handler subtokid (socrethandlerval) (stored)
		       (if (= ',BASE_ID (my-id))
			   (soc-return socrethandlerval)
			   (error 'SOC-return-handler
				  "ran on non-base node! id: ~s"
				  (my-id)))]
	       ;; The actual sense function:
	       ;; This is flexible and does not assume that cps-conversion
	       ;; has taken place.  It optionally takes a continuation argument:
	       [SenseTok subtokid (k_maybe) (stored)
			 (if (eq? '0 k_maybe)
			     (sync-sense)
			     ;; If we have run cps-tokmac without convert closure
			     ;; then it will be an actual procedure:
			     (if (procedure? k_maybe)
				 (kcall k_maybe (sync-sense))
				 (call k_maybe ',KCALL_FLAG (sync-sense))
				 ))]
	       ,@nodetoks))

       ;; Here we mutate the node-start to also call SOC-start.
       (printf "TOKS: ~s\n" (map car nodetoks))
       (set! nodetoks
	     (cons 
	      (match (assq 'node-start nodetoks)
		     [(node-start ,id () (stored ,s ...) ,body)
		      ;; CALL SOC START EXPLICITELY FROM NODE_START: (AFTER NODE-START!)
		      `(node-start ,id () (stored ,@s) 
			 (begin ,body
			    (if (= (my-id) ',BASE_ID) (call (tok SOC-start 0))  (void))))]
		     [,other (error 'compile-simulate-alpha "bad node-start! ~s" other)])
	      (assq-remove-all 'node-start nodetoks)))
       
       (mvlet ([(tbinds allstored) (process-tokbinds nodetoks)])

	      ;; Here we hack an extra handler into tbinds:
; 	      (set! tbinds
; 		    (cons `[SOC-return-handler
; 			    (lambda (current-vtime subtok_ind x)
; 			      (if (eq? ,BASE_ID (node-id (simobject-node this)))
; 				  (simulator-soc-return x)				  
; 				  (error 'SOC-return-handler
; 					 "ran on non-base node! id: ~s"
; 					 (node-id (simobject-node this)))))]
; 			  tbinds))
	 (let ((node-code
	 `(define (node-code this)	    

	   ;; First define datatype definitions for the tokens:
	   ,@(let ((alltoks (list-rem-dups (map car allstored))))
	       (append 
		(map (lambda (t)		
		       `(reg:define-struct (,t ;invoke-counter 
					    ,@(cadr (assq t allstored)))))
		  alltoks)
		; Also include generic index-based getter/setter: [2006.01.12]
		`((define (tokobj-ref obj ind)
		   (cond
		    ,@(map (lambda (rec-entry)
			     (let ([rec (car rec-entry)]
				   [fields (cadr rec-entry)])
			       `[(,(symbol-append rec '?) obj)
				 (case ind
				   ,@(map (lambda (i field)
					    `[(,i) (,(symbol-append rec '- field) obj)])
				       (iota (length fields))
				       fields)				   
				   [else (error 'tokobj "bad index ~a for record type ~a" ind ',rec)])]))
			;; Here as an optimization we put the bigger objectsfirst:  
			;; (Most importantly toks with no stored vars go last!)
			(map cdr
			  (let ([labeled (map cons (map (lambda (ls) (length (cadr ls))) allstored) allstored)])
			    (sort (lambda (l1 l2) (> (car l1) (car l2)))
				  labeled)))
			)
		    [else (error 'tokobj-ref "this was not a token object: ~a" obj)]
		    )))))

	   ,@(DEBUGMODE '(if (not (simobject? this)) (error 'node-code "'this' was not a simobject, instead: \n~a" this)))

	   ;; Set the global parameter that library code needs to access "this".
	   (parameterize ((current-simobject this)
			  ,@extraparams)
	   ;; Now we have subtoks:
;	   (reg:define-struct (tokstore ,@(apply append (map cadr allstored))))
	   ;; Need to update the sensing machinery...
	   (let (
		 #;
		 [sync-sense (lambda (t)
				((current-sense-function)
				 (node-id (simobject-node this))
				 (car (node-pos (simobject-node this)))
				 (cadr (node-pos (simobject-node this)))
				 current-vtime
				 ))]
		 )
	      (let* ,(process-binds nodebinds)
		(letrec ,tbinds		  
		 ;; Within this body, toks are bound, we return a list of start actions
		  ;"Initialize our simobject message buffers"

		  ;; This is the table through which dynamic token references can be invoked:
		  (let ([dyndispatch_table (make-default-hash-table)])
		    (begin (void) ,@(map (lambda (tokname)
					   `(hashtab-set! dyndispatch_table ',tokname ,tokname))
					 (map car tbinds)))
		    
		    ;; Return the real meta-handler
		     (lambda (msgob current-vtime)
		       ;; Here we set the parameters again because this is a different dynamic context:
		       (parameterize (,@extraparams)

		       (mvlet ([(name subtok)
				(let ((tok (msg-object-token msgob)))
				  (values (simtok-name tok)
					  (simtok-subid tok)))])
			      (let ([handler (hashtab-get dyndispatch_table name)])
				
;				(fprintf (current-error-port) "Dyndispatch: ~s in table: " name)
;				(hashtab-for-each (lambda (name _) (fprintf (current-error-port) "~s " name)) dyndispatch_table)
;				(newline (current-error-port))

				(if (not handler)
				    (error 'node-code
					   "dyndispatch: no handler for token name: ~s in table: ~n~s" name dyndispatch_table))
				;; Invoke:
				(apply handler current-vtime subtok 
				       (msg-object-args msgob))
				;; That returns nothing but fills up the simobjects buffers.
				))))
		 ))))))))
	   ;; Final return value of compile-simulate-alpha:
	   (build-genned-code-module node-code)
	   )
	 )]
      [,otherwise (error 'compile-simulate-alpha
			 "unmatched input program: ~s" prog)])))


;================================================================================
;;; Invoking the simulator.

;; This is a module-local parameter to the currently running sim program. -[2005.11.25]
(define simalpha-current-nodeprog (reg:make-parameter #f (lambda (x) x)))

;; This simply runs the simulator again on whatever the last simulation was.
;; .param args Can optionally contain 'use-stale-world
(define (rerun-simulator-alpha . args)
  (if (simalpha-current-nodeprog)
      (apply start-alpha-sim (simalpha-current-nodeprog) 'simple args)
      (error 'start-alpha-sim "cannot rerun from last simulated program, this is the first time!")))

;; This takes a token machine, rolls a new world, and runs it.  <br><br>
;;
;; [2005.11.11] Modifying this to have an option of not going to disk
;; for the simulation programs.  This should increase performance when
;; I'm just running unit tests and not debugging.

;; This requires pass21_cleanup-token-machine.ss as well as helpers.ss
;; This handles writing the generated code to a file and loading it back.
;; FLAGS:
;; 'numnodes int -- Set the number of nodes in the world to int.
;; 'outport prt  -- Set the printed output of the simulation to port prt.
;; 'srand int    -- Seed the random number generator with int.
(define run-simulator-alpha
  (lambda args
    (define THEPROG 'uninitialized)
    ;; First this loop runs, to get the token machine ready; stores it in THEPROG.
    ;; It makes a tail-call to read-params to finish up and do the actual invocation.
    (define prep-token-machine
	    (lambda args
	      (match args
		     ;; This is a weak requirement... 
		     ;; We consider the first arg to represent a token machine if it is a *list*.
		     [(,tm ,rest ...) (guard (list? tm))
		      (define (run-compiled)
			(if (simulator-write-sims-to-disk)
			    (let ((out (open-output-file "_genned_node_code.ss" 'replace)))
			      (parameterize ([print-level #f]
					     [pretty-maximum-lines #f]
					     [pretty-line-length 150]
					     [print-graph #t])
				;(write tm out)
				(fprintf out ";; Pre-generated code written out to this file: \n")
				(pretty-print tm out) ; [2005.11.28] why was this commented just now?
				(newline out)
				(close-output-port out)))
			    (set! THEPROG tm)))
		      (match tm ;; What kind of form is the token machine in?
			; Already compiled for the simulator?
			[(define (node-code this) ,e)    (run-compiled) (read-params rest)]
			[(begin (module ,_ ...) ,__ ...) (run-compiled) (read-params rest)]
			[(module ,_ ...)                 (run-compiled) (read-params rest)]

			; Convenience: if it's a (tokens ...) form it hasn't been cleaned up yet.
			[((tokens ,tok* ...) ,rest ...) ; So clean it up now:
			 (apply prep-token-machine (cleanup-token-machine `(tokens ,tok* ...)) rest)]

			; Otherwise, compile that token machine!
			[,tm			     
			 (let ((cleaned tm )) ; (cleanup-token-machine tm)))
			   (let ([comped (compile-simulate-alpha cleaned)])
			     (if (simulator-write-sims-to-disk)
				 (let ((out (open-output-file "_genned_node_code.ss" 'replace)))
					;			    (printf "Ouputting token machine to file: _genned_node_code.ss~n")
				   (parameterize ([print-level #f]
						  [pretty-maximum-lines #f]
						  [pretty-line-length 150]
						  [print-graph #f])
				     (pretty-print comped out)
				     (newline out)
				     (newline out)
				     (display ";; Above is compiled program for this token machine: " out)
				     (newline out)
				     (display "#;\n" out)
				     (pretty-print tm out)
				     (newline out))
				   (close-output-port out))
				 (set! THEPROG comped))
			     (read-params rest)
			     ))])]
		     [(,rest ...) 
		      ;; [2005.11.11]:
		      ;(error 'run-simulator-alpha "umm, shouldn't reach here I don't think...")
		      ; [2005.11.25] Allowing this again, if there are no args we just run on the already compiled sim.
		      (read-params rest)]
		     )))
    (define flags ())
    ;; Next this loop runs.
    ;; This deals with the many options that run-simulator-alpha can handle:
    (define read-params
	     (lambda (params) 
	       ;; Flags that can be passed through to start-sim-alpha
	       (define valid-pass-through-flags '(use-stale-world))
	       (match params
;		      [,x (guard (disp "read params" params) #f) 3]

		      ;; When we get to the end of the params we start it up:
		      [() 
		       (if (simulator-write-sims-to-disk)
                           ;; In this case we assume the simulation is already written to disk:
			   ;; Loading this binds "node-code" at top-level:
			   (IFCHEZ
                            (parameterize ([source-directories '(".")])
                              (load "_genned_node_code.ss"))
                            (load "_genned_node_code.ss"))
			   (eval THEPROG))
                       ;; We have to do this because of the module system:
                       (let ((node-code (top-level-value 'node-code)))
                         ;(disp "NODE CODE:" node-code) ;" eq: " (eq? node-code (eval 'node-code)))
                         ;(printf "Node code loaded from file.~n")
                         ;(if (not node-code)  (error 'run-simulator-alpha "node-code not defined!"))
			 ;; Cache this in a global parameter:
			 (simalpha-current-nodeprog node-code)

			   (apply start-alpha-sim node-code 'simple flags))]
		      [(timeout ,n . ,rest)
		       (parameterize ((sim-timeout n))
			 (read-params rest))]
		      [(numnodes ,n . ,rest)
		       (if (not (integer? n))
			   (error 'run-simulator-alpha
				  "'numnodes switch should be followed by an integer, not: ~s" n))
		       (parameterize ([sim-num-nodes n])
			 (read-params rest))]
		      [(outport ,p . ,rest)
		       (if (not (output-port? p))
			   (error 'run-simulator-alpha
				  "'outport switch should be followed by a port object, not: ~s" p))
		       (parameterize ([simalpha-output-port p])
			 (read-params rest))]
		      [(srand ,n . ,rest)
;		       (if (not (integer? n))
;			   (error 'run-simulator-alpha
;				  "'srand switch should be followed by an integer, not: ~s" n))
		       (printf "Setting up random number generator for simulator.  Srand: ~s\n" n)
		       ;q(logger "## Setting up random number generator for simulator.  Srand: ~s\n" n)
		       ;; TODO: Should just do this as a parameter.
		       (let ([stored-state #f])
			 (dynamic-wind
			     (lambda () (set! stored-state (reg:get-random-state)) (reg:set-random-state! n))
			     (lambda () (read-params rest))
			     (lambda () (reg:set-random-state! stored-state) (set! stored-state #f))))
		       ]
		      ;; Pass these flags on to start-alpha-sim
		      [(,pass-through . ,rest)
		       (guard (memq pass-through valid-pass-through-flags))
		       (set! flags (cons pass-through flags))
		       (read-params rest)]
		      ;; These flags are recognized, but ignored.
		      [(,ignored . ,rest)
		       (guard (memq ignored '(verbose)))
		       (read-params rest)]
		       
		      [,other (warning 'run-simulator-alpha "contains unrecognized parameters: ~s" other)]
;		      [,other (error 'run-simulator-alpha "unrecognized parameters: ~s" other)]
		      )))
    ;; Reset global message counter:
    (clean-simalpha-counters!)

    (apply prep-token-machine args)))

;  ================================================================================
;; [2005.09.29] Moved from alpha_lib.ss
;; This just sets up the sim and the logger and invokes one of the scheduler/execution engines.
;; I've written two different engines at different levels of time-modeling complexity.
;; It is called by run-simulator-alpha, which is the front-end that users should use.
;;     <br><br>
;; This procedure is not exposed in the API, it's internal.
(define start-alpha-sim 
  (lambda (node-code-fun . flags)
    (define logfile (if (simulation-logger-gzip-output)
			"__temp.log.gz"
			"__temp.log"))
    (define (open-opts f) 
      (if (equal? (extract-file-extension f) "gz") 
	  ;; Note: replacing logfiles is dangerous when we're running experiments.
	  '(replace compressed) ; exclusive?
	  'replace))
    
    ;; Only allow accepted flags:
    (ASSERT (list-subset? flags '(simple use-stale-world)))

    ;; Now to really run it, but first we instantiate the sensor-stubs for the program to read:
    (printf "Starting alpha-sim, initializing sensor-stub...\n")
    (parameterize ([simalpha-sense-function ((simalpha-sense-function-constructor))]
		   ;; FOR NOW: only log if we're in debugmode [2005.10.17]
		   [simulation-logger (match (simulation-logger)
					[#f #f]
					[#t (open-output-file logfile (open-opts logfile))]
					[,s (guard (string? s)) 
					    (open-output-file s (open-opts s))]
					;; [2006.03.20] For now if we don't understand the existing logger, we 
					;; just assume #t start a new one.
					[,other (open-output-file logfile (open-opts logfile))]
					#;
					[,other (error 'start-alpha-sim 
						       "unsupported simulation-logger: ~a\n" other)])]
		   [simulation-logger-count (IFDEBUG 0 #f)])

;    (IFDEBUG (inspect (simulation-logger)) ())

      (when (simulation-logger)
	(regiment-print-params ";; " (simulation-logger))
	(newline (simulation-logger)))
    
    (let* (
	   [simple-scheduler (memq 'simple flags)]
	   ;; With flags out of the way, the argument remaining is the stopping time:
	   [stopping-time? 
	    (let ([stop-time (sim-timeout)])
	      (disp "STOP TIME" stop-time)
	      (if (not stop-time)
		  (lambda (t) #f)
		  (if (inexact? stop-time)
		      ;; It's in seconds:
		      (let ([end-time (+ (* 1000 stop-time) (cpu-time))])
			(printf "Stopping after ~s seconds.~n" stop-time)
			(lambda (_) (>= (cpu-time) end-time)))
		      ;; Otherwise, vtime:
		      (begin (printf "Stopping after vtime ~s.~n" stop-time)
			     (lambda (t) (>= t stop-time))))))]
	   [sim (if (memq 'use-stale-world flags)
		    ; We reuse the stale world, but we freshen it at least:
		    (begin 
		      (fprintf (current-error-port)
			       "Starting alpha sim based on the existing \"stale\" simworld. (~s nodes)\n"
			       (length (simworld-all-objs (simalpha-current-simworld)))
			       )
		      (clean-simalpha-counters!)
		      (clean-simworld! (simalpha-current-simworld)))
		    ;; If we're not set to use the stale world, then
		    ;; we still might be set to use a topology from a file:
		    (if (simalpha-preset-topology)
			(begin 
			  (simalpha-current-simworld (read (open-input-file (simalpha-preset-topology))))
			  (animate-world! (simalpha-current-simworld))
			  (clean-simalpha-counters!)
			  ;(clean-simworld! (simalpha-current-simworld))
			  (simalpha-current-simworld)
			  )
			(fresh-simulation))
		    )])

      (DEBUGASSERT (simworld? sim))
      
      ;; Now that the new world is generated, we write its essential info to the logfile.
      ;; For now that's the nodes.
      ;; [2006.02.20] Lame.  For now I'm having problems with writing records to disk.
      (logger 0 -1 '_ 'NEWWORLD 
	      `[nodes ,(map reg:struct->list (map car (simworld-graph sim)))]
	      `[graph ,(map (lambda (entry) (map node-id entry))
			 (simworld-graph sim)
			 )]
	      )
  
    ; Here, we also leave our simworld behind after we finish for anyone who wants to look at it.
    (simalpha-current-simworld sim)
    (soc-return-buffer '()) ; Same for the return buffer.
  
    ; If there are no graphics available, we turn it off:
    (parameterize ([simalpha-graphics-on
  		  (IF_GRAPHICS (simalpha-graphics-on) #f)])
  			       
    ; Draw the world, if necessary.
    (if (simalpha-graphics-on) 
	(if (not (memq 'use-stale-world flags))
	    (simalpha-draw-world sim)))
    
    
    (parameterize (;[soc-return-buffer '()]
  		 ;; This is not used by simalpha itself, but anyone else who wants to peek:
  		 ;[simalpha-current-simworld sim]
		 )
    (let/cc exitk	
    (parameterize ([escape-alpha-sim exitk])
                (printf "Running simulator alpha (~s version) (logfile ~s) (optlvl ~a) (debug ~s)\n" 
  			(if simple-scheduler 'simple 'full)
  			logfile 
			(IFCHEZ (optimize-level) "NA")
			(eval '(IFDEBUG #t #f)))
		(fprintf (current-error-port)
			 "Running simulator alpha (~s version) (logfile ~s) (optlvl ~a) (debug ~s)\n" 
  			(if simple-scheduler 'simple 'full)
  			logfile (IFCHEZ (optimize-level) "NA") (eval '(IFDEBUG #t #f)))
  		(DEBUGMODE (display " with Debug-Mode enabled"))
  		(printf ".~n")
  		(printf "<-------------------------------------------------------------------->~n")
  
  	;; DEBUG DEBUG DEBUG
;  	(DEBUGMODE
;  	 (global-graph (simworld-graph sim)))
  
  	;(printf "Starting!  Local: ~s~n" (map simobject-local-msg-buf (simworld-all-objs sim)))
  	;; Redirect output to the designated place:
  	(let ((old-output-port (current-output-port)))
  	  (parameterize ([current-output-port
  			  (if (simalpha-output-port)
  			      (begin (printf "~n!!!  Redirecting output to port: ~s  !!!~n" (simalpha-output-port))
  				     (simalpha-output-port))
  			      (current-output-port))]
  			 ;; Just to be safe I'm resetting this here.  Don't want to freeze on any print statements! -[2005.10.26] 
  			 [print-graph #t]
  			 )

    	    (if simple-scheduler
  		(run-alpha-simple-scheduler sim node-code-fun stopping-time? old-output-port)
  		; [2005.09.30] Disabling for now, not loading the full scheduler:
  		; (run-alpha-full-scheduler sim node-code-fun stopping-time?)
  		(error 'start-alpha-sim "full scheduler not loaded")
  		)
  	  ))
  		   
      ;; Out of main loop:
	;; Now on the way out of the simulator, flush logger output and close logfile.
	(logger) ;; Logger with no arguments is a flush.
	(when (output-port? (simulation-logger)) (close-output-port (simulation-logger)))))
    ;; Out of let/cc:
    (let ((result (reverse (soc-return-buffer))))
      (printf "~nTotal globally returned values:~n ~s~n" result)
      result))
    ))))) ;; End start-alpha-sim

; =======================================================================


(define-testing these-tests
  (map (lambda (test)
	 (match test
	   [(,prog ,res) `(,prog ,res)] ;`((begin (cleanse-world) ,prog) ,res)]
	   [(,name ,prog ,res) 
	    `(,name ,prog ;(begin (cleanse-world) ,prog)
		    ,res)]))
    `(
      [3 3] 
    
#|  ;; These aren't working, what was the reason?
    [(compile-simulate-alpha
      '(program
	(bindings)
	(socpgm  (bindings) (call tok1))
	(nodepgm 
	 (tokens
	  [tok1 () (bcast tok2 '3)]
	  [tok2 (x) (bcast tok2 (+ x x))])
	 (startup))))
     '??]


    [(compile-simulate-alpha
      '(program
	(bindings)
	(nodepgm 
	 (tokens
	  [SOC-start () (stored) (void)]
	  [node-start () (stored) (call tok1)]
	  [tok1 () (stored) (bcast tok2 '3)]
	  [tok2 (x) (stored [x '99])
		(begin (set! x (+ x '1))
		       (bcast tok2 x))])
	 ;(startup)
	 )))
     '??]


    [(compile-simulate-alpha
      '(program
	(bindings)
	(nodepgm 
	 (tokens
	  [SOC-start () (stored) (display "S")]
	  [node-start () (stored) ];(begin (printf "N~s" (simobject-I-am-SOC this)) (call tok1))]
	  [tok1 () (stored) (begin (display ".") (bcast tok2 " "))]
	  [tok2 (x) (stored) (display x)]
	 ))))
     '??]
|#
   

    )))

;; Use the default unit tester from helpers.ss:
;; But this also makes sure the world is initialized before doing unit tests:
(define-testing test-this
  (let ((tester (default-unit-tester 
		  "simulator_alpha.ss: event-queue simulator for nodal language"
		  ;; Make sure that the world is clean before each test.
		  these-tests
		  tester-eq?
		  id ;wrap-def-simulate
		  )))
    (lambda args
      (apply tester args))))
(define testalpha test-this)

(define csa compile-simulate-alpha) ;; shorthand

) ;; End Module.
