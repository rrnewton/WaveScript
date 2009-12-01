

(library (ws compat compat)

  (export 
          syntax->list getenv
 	  merge merge! sort! append! reverse! inspect define-values
	  native-inspect

 	  fluid-let parameterize reg:define-struct reg:struct?
 	  void make-parameter
 	  printf error format format-syntax-nicely
 	  define-top-level-value set-top-level-value! top-level-bound? top-level-value 
 	  reg:top-level-eval simple-eval
	  real-time cpu-time 
	  warning warning-handler display-condition
 	  print-level print-graph print-length pretty-maximum-lines pretty-line-length print-vector-length

 	  process  pretty-print get-string-available

 	  box unbox set-box! box? 
 	  promise? 
	  
 	  system make-list  repl ; native-repl
	  with-output-to-string
	  which-scheme IFCHEZ	  

	  current-directory with-output-to-port time
	  include random seed-random trace-define trace-lambda syntax-error fprintf
	  delay force gensym

	  ;nice-print-exception
	  
	  (rename (chez:call/1cc call/ec))

	  (rename (chez:debug debug))

	  ;(rename (chez:import import))

	  )
  
  (import 
	  (prefix (scheme) chez:)
	  (except (scheme) inspect warning
		  ;; [2009.03.12] Using the virtual top-level environment instead of the real one:
		  define-top-level-value set-top-level-value! top-level-bound? top-level-value 
		  error
		  )

	  ;(rnrs eval (6))
	  ;(rnrs programs)
	  ;(except (rnrs (6)) error)
#;
	  (only (scheme) 
		syntax->list getenv 
		make-list merge merge! sort! append! reverse! include
		with-output-to-string pretty-print printf parameterize
		print-level print-graph print-length pretty-maximum-lines pretty-line-length print-vector-length
		error format fluid-let void make-parameter
		define-top-level-value set-top-level-value! top-level-bound? top-level-value 
		warning  real-time cpu-time
		system process 
		box unbox set-box! box? 
		new-cafe random trace-define trace-lambda syntax-error fprintf 
		current-directory time delay force gensym
		))

  (define which-scheme 'chez)

  ;; Humorously, IFCHEZ is currently FALSE for the purpose of getting R6RS to work.
  ;(define-syntax IFCHEZ (syntax-rules ()  [(_ a b) a]))
  (define-syntax IFCHEZ (syntax-rules ()  [(_ a b) b]))

  (define native-inspect chez:inspect)

  (define format-syntax-nicely (lambda (x) x))

  ;; [2009.03.12] Using the virtual top-level environment instead of the real one:
  ;(define reg:top-level-eval chez:eval)

  ;; [2009.11.01] 'environment' is actually quite expensive. 
  (define simple-eval
    (let ([env #f])
      (lambda (xp)
	(unless env 
	  (set! env (environment '(rnrs (6)))))
	(chez:eval xp env))))
  
  ;; [2008.12.05] Check up on this one:
  (define (get-string-available inp)
    (error 'get-string-available "non-blocking IO not implemented..."))

;   (ik:include "ws/compat/common.ss")
   (include "ws/compat/reg_record.ss")
   (include "ws/compat/inspector.ss")
   (include "ws/compat/top-level-values.ss")
   (include "ws/compat/multiple-values.ss")

  (define (inspect x)    
    (parameterize ((print-graph #t))
      (generic-inspect x)))
 
;   ;; This is required to return #t for promises, but false positives are ok.
;   ;; This is too lenient, but there's no other option.
   (define promise? procedure?)

   (define implementation-specific-imports '((prefix (scheme) chez:)))
#;
   (define (default-repl-env)
     (environment '(except (rnrs (6)) error) ; (rnrs r5rs (6))
		  '(rnrs mutable-pairs (6)) '(rnrs mutable-strings (6))
		  '(main_r6rs) '(main) '(ws shortcuts)
		  '(prefix (scheme) chez:)
		  ))
   
   (define (repl)  
     (import expression-editor)
     (ee-flash-parens #t)
     ;; [2009.11.17] Can't presently figure out how to get the expression editor on.
#;
     (printf "STARTING NEW CAFE: ~s ~s ~s ~s TERM ~s interactive: ~s\n" 
	     (current-output-port) (current-input-port)
	     (standard-output-port) (standard-input-port)
	     (getenv "TERM") (interactive?)
	     )
     (with-exception-handler
       (lambda (x)
         (call/cc 
	  (lambda (k)	 
	    (nice-print-exception x k)
            (reset)
	    (repl))))
       (lambda ()
         (new-cafe reg:top-level-eval)))
     )

  (define (with-output-to-port p th)
    (parameterize ([current-output-port p])
      (th)))

  (define warning-handler 
    (make-parameter
     (lambda (who str . args)
       (printf "Warning in ~a: ~a\n" who (apply format str args)))))
  (define (warning who str . args)
    (apply (warning-handler) who str args))
#;
  (define (error who msg . args)   
    (call/cc 
     (lambda (errork)
       (newline) (printf " ** ERROR ** \n")
       ;(chez:error who (apply format msg args))
       ;(printf " In ~a: ~a" who (apply format msg args)) (newline)
       ;;(debug)
       (chez:exit 12)
       ;(inspect errork)
       ))
    )
  (define error chez:error)

  (define seed-random
    (case-lambda 
      [() (chez:random-seed (chez:time-nanosecond (chez:current-time)))]
      [(n) (chez:random-seed n)]))


  ;; Chez Scheme specific configuration.
  ;;================================================================================


  ;; A Chez hack to see all the source locations in a continuation.
  (define (continuation->sourcelocs k)
    (let loop ([depth 0] [ob (inspect/object k)])
      (when (> (ob 'depth) 1)
	(call-with-values (lambda () (ob 'source-path))
	  (lambda args
	    (if (= (length args) 3) ;; Success
	      (apply printf "  ~a: File: ~a, line ~a char ~a\n" depth args)
	      (begin 
	     	(printf "  ~a: (unknown loc) " depth )
		(ob 'print (current-output-port)))
	      )))
	(loop (add1 depth) (ob 'link)))))
  (define k->files continuation->sourcelocs)

  ; ;; Set some Chez scheme parameters.
  ; (print-graph #t )
  ; (print-gensym #f)
  ; ;(print-level 8)
  ; (print-level #f) ;20)
  ; (print-length #f) ;80)
  ; (print-vector-length #t)
  ; ;(pretty-maximum-lines 700)
  ; (pretty-maximum-lines #f)
  ; (pretty-line-length 150)
  ; (pretty-standard-indent 0)

#;
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
		;; [2008.03.30] Also reset the source-directories so Chez can find the code:
		(source-directories default-source-directories)
		;; Set the current directory so that the inspector can find line numbers:
		(inspect k))))))
#;
  (error-handler inspector-error-handler)

  (define (nice-print-exception x k)
    (printf "\n *** ERROR *** \n")        
    (if (and (irritants-condition? x)
	     (message-condition? x)
	     (error? x))
	(apply printf (condition-message x) (condition-irritants x))
	(begin (display-condition x)(newline)))
    (printf "\nBacktrace: \n")
    (continuation->sourcelocs k))

  (base-exception-handler 
   (lambda (x)
     (call/cc
      (lambda (k)
	(if (warning? x)
	    (begin (display-condition x) (newline))
	    (begin 	      
	      (nice-print-exception x k)

	      ; (printf "First entering inspector with exception object:\n")
	      ; (chez:inspect x)
	      ; (printf "Entering inspector with error continuation:\n")
	      ; (chez:inspect k)

	      ;(reset)
	      (exit 1)

	      ;(debug-message-and-continuation "test" k)
	      ;(debug)
       ))))))

   

) ;; End library

