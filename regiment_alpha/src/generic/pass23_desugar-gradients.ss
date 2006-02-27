;;;; .title Desugar gradients.  (pass23_desugar-gradients.ss)

;;;; This file provides a front-end that calls off to one of the
;;;; multiple gradient implementations based on the value of the
;;;; parameter 'desugar-gradient-mode'.  It also contains some code
;;;; that is shared between the different gradient implementations.
;;;;   <br><br>

;;;; By the way, currently [2006.01.18] there is some serious code
;;;; duplication between the various gradient implementations.  When a
;;;; serious change needs to be made to them, instead of maintaining
;;;; the duplicated code, most of the common functionality should be
;;;; factored out (TODO).  (For example, how to process a gemit statement is
;;;; the same in all.)


; ----------------------------------------------------------------------
;;; Constants shared by gradient implementations.

;; This is a very error prone pass, I'm optionally including a bunch of debugging print statements.
;; For now coupling it to global "REGIMENT_DEBUG"
(define-syntax DEBUG_GRADIENTS (syntax-rules () [(_ expr ...) (REGIMENT_DEBUG expr ...)]))
;(define-syntax DEBUG_GRADIENTS (syntax-rules () [(_ expr ...) (list expr ...)])) ;; ON
;(define-syntax DEBUG_GRADIENTS (syntax-rules () [(_ expr ...) ()]))              ;; OFF

;; Further, this is a shorthand for including comments in the generated code.
(define-syntax COMMENT 
  (syntax-rules ()
    [(_ str) (if (reg:comment-code) '('str) ())]))

;; These are the actual names of the gradient arguments and corresponding stored variables.  <br>
;; (Note, probably should be made unique, but we assume the *users*
;; variables have already undergone uniquefying.)
(define PARENT_ARG 'g_parent)                   ;; ditto
(define ORIGIN_ARG 'g_origin)                   ;; ditto
(define HOPCOUNT_ARG 'g_hopcount)               ;; ditto
(define VERSION_ARG 'g_version)
;; ditto
(define STORED_PARENT_ARG 'stored_g_parent)     ;; ditto
(define STORED_ORIGIN_ARG 'stored_g_origin)     ;; ditto
(define STORED_HOPCOUNT_ARG 'stored_g_hopcount) ;; ditto
(define STORED_VERSION_ARG 'stored_g_version)

;; This, when stored as parent pointer, signifies that we're at the root of the tree.
(define NO_PARENT 'atroot)

;; Value for the g_hopcount argument to indicate that it's a local (non-gradient) invocation.
(define LOCALCALL 'nongrad-invoke)

;; Call flags.  Used when invoking a gradientized token.
					;    (define RHINIT 111)
					;    (define RHLOCAL 222)
					;    (define RHREMOTE 333)
					;    (define RHSEND 444)
(define RHINIT 'rhinit)
(define RHLOCAL 'rhlocal)
(define RHREMOTE 'rhremote)
(define RHSEND 'rhsend)

;; Default return-handler timeout.  <br>
;; We can't hold buffered values forever...
(define DEFAULT_RHSEND 1000)


; ----------------------------------------------------------------------
;;; Global helpers - again, shared.

;; These are the functions that manage where the gradient args are
;; placed within the argument lists.  I'm making these global because
;; other pieces of code need to know about them (namely, the GUI).
(define (add-grad-args-to args gradargs)
  ;(append args gradargs)
  (append gradargs args)
  )
;; [2006.01.13] Also adding this for the GUI which needs to scrape these args back out:
;; NOTE!  This is very inaccurate, it could be messed up by the
;; later CPS pass that adds more args. 
(define (retrieve-grad-args lst)
  (let* ([len (length lst)]
	 [last4 (list-tail lst (- len 4))]
	 ;[last4 (list-head lst 4)]
	 [parent  (car last4)]
	 [origin  (cadr last4)]
	 [hops    (caddr last4)]
	 [version (cadddr last4)])
    (values parent origin hops version)))

; ----------------------------------------------------------------------
;;; Main procedure.

;; Front end procedure for all versions of desugar-gradients.
(define (desugar-gradients prog)
;  (inspect `(HRM ,prog ,(desugar-gradient-mode)))
  (case (desugar-gradients-mode)
    [(inlined)   (desugar-gradients_verbose prog)]
    [(linked)  (desugar-gradients_simple prog)]
    [(etx)      (desugar-gradients_ETX prog)]
    [else (error 'desugar-gradients "unknown value for param 'desugar-gradients-mode: ~a" (desugar-gradients-mode))]))

; ----------------------------------------------------------------------
;;; Tests.

(define these-tests
  `(
    
    ["Put an empty test through." 
     (desugar-gradients
      '(cleanup-token-machine-lang
	'(program
	  (bindings )
	  (nodepgm (tokens) )
	  (emittoks))))
     (desugar-gradient-lang
      '(program (bindings) (nodepgm (tokens))))]

    ["Run a random program that (was) currently causing a problem, should have same input as output."
     ,@(let ((prog '(program
			(bindings)
		      (nodepgm
			  (tokens
			      (node-start subtok_ind () (stored) (void))
			    (SOC-start
			     subtok_ind
			     ()
			     (stored)
			     (begin
			       (timed-call 100 (tok tok1 0))
			       (timed-call 200 (tok tok1 0))))
			    (tok1 subtok_ind
				  ()
				  (stored)
				  (printf '"~a \n" (my-clock)))))
		      (emittoks))))
	 `((desugar-gradients '(cleanup-token-machine-lang ',prog))
	   (desugar-gradient-lang ',(rdc prog)
)))]


    ["Run a random program that (was) currently causing a problem, should have same input as output."
     ,@(let ((prog '(program
			(bindings)
		      (nodepgm
			  (tokens
			    (node-start subtok_ind () (stored) (call (tok datafeed 99)))
			    (SOC-start subtok_ind () (stored) (void))
			    (datafeed subtok_ind () (stored) (void))))
		      (emittoks))))
	 `((desugar-gradients '(cleanup-token-machine-lang ',prog))
	   (desugar-gradient-lang ',(rdc prog))))]

    ["Make sure it gets all the gradient calls out.." 
     (let ((x (desugar-gradients
      '(foolang '(program (bindings) 
        (nodepgm
	 (tokens
	  (node-start subtok_ind () (stored) (void))
	  (SOC-start subtok_ind () (stored) (call (tok f 0)))
	  (f subtok_ind () (stored) (gemit (tok g 0) '3))
	  (g subtok_ind
	     (x)
	     (stored)
	     (if (< (gdist (tok g subtok_ind)) '3)
		 (grelay (tok g subtok_ind))
                (greturn (gdist (tok g subtok_ind))
			(to (tok h 0))
			(via (tok g 0))
			(seed '#f)
			(aggr #f))))
	  (h subtok_ind (v) (stored) (dbg '"Got val: %d\\n" v))))
	(emittoks)
	)))))
       (list (deep-assq 'gemit x)
	     (deep-assq 'grelay x)
	     (deep-assq 'ggreturn x)
	     (deep-assq 'gdist x)))
     (#f #f #f #f)]
		        
))


(define test-this (default-unit-tester
		    "23: Desugar-Gradient: convert gradient commands to plain token machine code." 
		    these-tests))

;; The usual unit tester.
(define test23 test-this)  
(define tests23 these-tests)
(define test-desugar-gradients test-this)
(define tests-desugar-gradients these-tests)

 