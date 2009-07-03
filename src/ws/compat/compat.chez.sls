

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
	  warning warning-handler
 	  print-level print-graph print-length pretty-maximum-lines pretty-line-length print-vector-length

 	  process  pretty-print get-string-available

 	  box unbox set-box! box? 
 	  promise? 
	  
 	  system make-list  repl 
	  with-output-to-string
	  which-scheme IFCHEZ	  

	  current-directory with-output-to-port time
	  include random trace-define trace-lambda syntax-error fprintf
	  delay force gensym
	  
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
  (define (simple-eval xp) (eval xp '(rnrs (6))))

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

   (define (repl)
     ;(new-cafe)
     (define env (environment '(except (rnrs (6)) error) '(main_r6rs) '(main) '(prefix (scheme) chez:) '(ws shortcuts)))     
     (new-cafe 
      (lambda (x)
	(reg:top-level-eval x env)
	)))

  (define (with-output-to-port p th)
    (parameterize ([current-output-port p])
      (th)))

  (define warning-handler 
    (make-parameter
     (lambda (who str . args)
       (printf "Warning in ~a: ~a\n" who (apply format str args)))))
  (define (warning who str . args)
    (apply (warning-handler) who str args))

  (define (error who msg . args)   
    (call/cc 
     (lambda (errork)
       (newline) (printf " ** ERROR ** \n")
       (chez:error who (apply format msg args))
       ;(printf " In ~a: ~a" who (apply format msg args)) (newline)
       ;;(debug)
       ;;(chez:exit 12)e
       ;(inspect errork)
       ))
    )

) ;; End library

