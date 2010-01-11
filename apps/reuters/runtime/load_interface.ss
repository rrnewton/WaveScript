

;; [2009.10.10] This file establishes the connection between the
;; Scheme process that runs the compiler and performs high-level
;; management of the runitme, and the control module.

;; Presently, everything runs in the same process.  In the future, the
;; runtime, compiler, and control module could all be in separate
;; processes if desired.

(printf " <WSQ> Loading WaveScope compiler...")(flush-output-port)

(let* ([startd (current-directory)]
       [src-dir (string-append (getenv "REGIMENTD") "/src")]
       [src (string-append src-dir "/regiment.ss")])
  (parameterize ([current-directory src-dir]
		 [command-line `(,src ,startd "nothing")])
    ;(printf "INDIR ~s\n" (current-directory))
    (load src)))

(printf " finished.\n")

;; This factors out some of the repetitive tasks in exposing Scheme functions to C.
;; In addition to binding the normal scheme function, it adds another
;; symbol bound to the pointer expressed as an integer.
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

;; Next load the WSQ runtime manager.

(printf " <WSQ> Loading runtime manager...") (#%flush-output-port)
(load (string-append(getenv "REGIMENTD") "/apps/reuters/runtime/runtime_manager.ss"))
(printf "  finished.\n")

;;==============================================================================

;; Done with initializing the runtime interface bindings.  Now return
;; control to the enclosing C program in which we are embedded.
(exit 0)
