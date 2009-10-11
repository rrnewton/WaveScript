

;; [2009.10.10] This file establishes the connection between the
;; Scheme process that runs the compiler and performs high-level
;; management of the runitme, and the control module.

;; Presently, everything runs in the same process.  In the future, the
;; runtime, compiler, and control module could all be in separate
;; processes if desired.

(printf " --- LOADING UP COMPILER.\n")


(let* ([startd (current-directory)]
       [src-dir (string-append (getenv "REGIMENTD") "/src")]
       [src (string-append src-dir "/regiment.ss")])
  (parameterize ([current-directory src-dir]
		 [command-line `(,src ,startd "nothing")])
    (printf "INDIR ~s\n" (current-directory))
    (load src)))

;; This factors out some of the repetitive tasks in exposing Scheme functions to C.
(define-syntax define-entrypoint
  (lambda (x)
    (syntax-case x ()
	[(_ var argtype rettype expr)
	 (let ((var-entrypoint 
		(datum->syntax #'var
		 (string->symbol 
		   (string-append (symbol->string (syntax->datum #'var))
				  "-entry")))))
	 #`(begin 
	     (define var expr)
	     (define #,var-entrypoint
	       (let ([x (foreign-callable var argtype rettype)])
		 (lock-object x)
		 (foreign-callable-entry-point x)))))
	 ])))

;(pretty-print (expand '(define-entrypoint foo (int) void bar)))

;;==============================================================================
(printf " --- BRIDGING CONTROL MODULE AND RUNTIME\n")

(define-entrypoint WSQ_BeginTransaction (int) void
  (lambda (id)
    (printf ".....In WSQ_BeginTransaction ~a.....\n" id)))

;(printf "Defined wsq_begintrans ~a\n" WSQ_BeginTransaction)
;(printf "Defined wsq_begintrans entry ~a ~a\n" WSQ_BeginTransaction-entry (#%top-level-bound? 'WSQ_BeginTransaction-entry))

(define-entrypoint WSQ_EndTransaction () void
  (lambda ()
    (printf ".....In WSQ_EndTransaction....\n")
    ))

(define-entrypoint WSQ_BeginSubgraph (int) void 
  (lambda (id)
    (printf ".....In WSQ_BeginSubgraph ~a....\n" id)
    ))

(define-entrypoint WSQ_EndSubgraph () void
  (lambda ()
    (printf ".....In WSQ_EndSubgraph....\n")))

(define-entrypoint WSQ_RemSubgraph (int) void
  (lambda (id)
    (printf ".....In WSQ_RemSubgraph ~a....\n" id)))

;;==============================================================================

;; Done with initializing the runtime interface bindings.  Now return
;; control to the enclosing C program in which we are embedded.
(exit 0)
