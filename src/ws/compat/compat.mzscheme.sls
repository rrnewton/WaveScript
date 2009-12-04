#!r6rs

(library (ws compat compat)
  (export 
          include getenv syntax->list 
 	  random seed-random make-list
 	  merge merge! sort! append! reverse! call/ec inspect native-inspect debug define-values
 	  fluid-let parameterize reg:define-struct reg:struct?
 	  void make-parameter
 	  format printf fprintf pretty-print
	  format-syntax-nicely
 	  gensym current-directory 
	  syntax-error error
 	  define-top-level-value set-top-level-value! top-level-bound? top-level-value 
 	  reg:top-level-eval simple-eval
 	  warning warning-handler display-condition
	  real-time cpu-time time	   
 	  print-level print-graph print-length pretty-maximum-lines pretty-line-length print-vector-length
 	  with-output-to-string with-output-to-port
	  repl
 	  process get-string-available

	  box unbox set-box! box? 
	  promise? delay force

	  which-scheme IFCHEZ
	  trace-define trace-lambda

	  (rename (sys:system system))
; 	  make-rectangular
	  )
  (import (except (rnrs (6)) current-output-port error)
	  (for (rnrs eval (6)) run expand)
	  (for (only (scheme base) random getenv ;syntax->list 
		sort call/ec define-values parameterize
		void make-parameter
		format printf fprintf 
		gensym 
		time 
		file 
		current-milliseconds current-process-milliseconds
		box unbox set-box! box? 
		current-output-port
		) run expand)

	  ;(scheme srfi/19)	
;	  (scheme mzlib cml) 
	  ;(scheme mzlib/cml)
	  ;(mzlib cml)  
	  ;(srfi 19)
	  
	  ;(only (lazy promise) promise? delay force)
	  (only (scheme promise) promise? delay force)
	  
	  (prefix (scheme base) plt:)
	  (prefix (only (scheme include) include)
		  
		  plt:)
	  (only (scheme mpair) list->mlist)
	  (scheme pretty )
	  ;(scheme process)
	  (prefix (scheme system) sys:)
	  ;(mzlib process)
	  ;(only (scheme list) merge )
	  ;(only (srfi 1) ) ;make-list
          ;(scheme)
	  ;(lib "lists.ss")
	  ;(lib lists)
	  ;(scheme lists)
	  ;(mzscheme)
	  )

  (define which-scheme 'mzscheme)

#;
  (define-syntax trace-lambda
    (syntax-rules ()
      [(_ name args bod ...)
       (let ([closure (lambda args bod ...)]
	     [sym (plt:gensym)])
	 ;; Need to define a real top-level value.  Not sure if we can
	 ;; do that hack in R6RS mode.
	 (define-top-level-value sym closure)
	 (trace sym)
	 closure)]))

  (define seed-random 
    (case-lambda 
      ;[()  (plt:random-seed (time-nanosecond (current-time)))]
      ;[()  (plt:random-seed (exact (round (current-time))))]
      [() (plt:random-seed (plt:current-milliseconds))]
      [(n) (plt:random-seed n)]))

  ;; PLT needs some help printing out the line numbers of a syntax object.
  (define (format-syntax-nicely x)
    (format "Syntax ~a, line ~a in ~a" 
	    (syntax->datum x) (plt:syntax-line x) (plt:syntax-source x)))

  ;(define-syntax system   (identifier-syntax sys:system))

  ;(define-syntax sort!    (identifier-syntax sort))
  ;(define-syntax append!  (identifier-syntax append))
  ;(define-syntax reverse! (identifier-syntax reverse))
  
  ;(define-syntax real-time (identifier-syntax current-milliseconds))
  ;(define-syntax  cpu-time (identifier-syntax current-process-milliseconds))

  ;; Eta expanding to get around a present PLT bug:
  (define (reverse! ls) (reverse ls))
  (define (sort! fn ls) (sort fn ls))
  (define (append! . ls) (apply append ls))
  (define (real-time) (current-milliseconds))
  (define (cpu-time)  (current-process-milliseconds))

  (define print-level        pretty-print-depth)          ;; Same param, diff name
  (define pretty-line-length pretty-print-columns)

  ;; And these do nothing:
  (define print-length (make-parameter 0))
  (define pretty-maximum-lines (make-parameter 0))
  (define print-graph (make-parameter #f))  
  (define print-vector-length  (make-parameter #f))

  (define implementation-specific-imports 
    '(
      (prefix (scheme) plt:)
      ;; Ugh, I'm forced to bring in the whole (scheme) for the app syntax.
      ;(scheme)
      ))

   (define (default-repl-env)   
     (printf "Building default repl...\n")
     (let ([env 
	    (environment '(except (rnrs (6)) error) ; (rnrs r5rs (6))
		  '(rnrs mutable-pairs (6)) '(rnrs mutable-strings (6))
		  '(main_r6rs) '(main) '(ws shortcuts)
		  '(prefix (scheme) plt:)
		  )])
       (printf "  Env ~s\n" env)
       env))

;  (define repl read-eval-print-loop)
   (define (repl)
     (define (wrapper x) 
       (define sexp (syntax->datum x))
       (printf "Fake eval function: ~s ~s ~s ~s\n" x sexp (symbol? (car sexp)) (procedure? (car sexp)))
       ;; This is pretty weird indeed, sexp will be of the form (#%top-interaction . expr)
       (unless (and (pair? sexp)
		(symbol? (car sexp))
		;(eq? (car sexp) plt:top-interaction)
		;(eq? (car sexp) #%top-interaction)
		)
	 (error 'repl "Violated expectations of PLT reader conventions."))
       (printf "Calling reg:topeval ~s\n" (cdr sexp))

;; PLT scheme environments seem kind of broken:
;; mcar: expects argument of type <mutable-pair>; given (#%base-require r6rs/private/prelims . #<syntax>)
       ;(printf "Here's an eval ~s\n" (eval '3 (environment '(rnrs))))
       ;(printf "Here's an example environment: ~s\n" (environment '(rnrs)))

       (reg:top-level-eval (cdr sexp))       
       )
     (parameterize ((plt:current-eval wrapper))
       (printf "SPAWNING REPL\n")
       ;; Having so many problems with this:
       ;(plt:read-eval-print-loop)
       ;; Here's a simple repl
       (let loop ()
	 (printf "> ")(flush-output-port (current-output-port))
	 (let ([exp (read)])
	   (unless (eof-object? exp)
	     (reg:top-level-eval exp)
	     (loop))))
       ))
    
  (define make-list
    (case-lambda 
      [(n) (make-list n #f)]
      [(n x) (let loop ([n n])
	       (if (zero? n) '()
		   (cons x (loop (fx- n 1)))))]))

  (define-syntax syntax-error
    (syntax-rules ()
      [(kwd form msg) (syntax-violation #f msg form)]
      [(kwd msg)      (syntax-violation #f msg #f)]))


  ;; Chez's system for warnings -- same as error.
  (define (warning who str . args)
    (apply (warning-handler) who str args))
  (define warning-handler 
    (make-parameter
     (lambda (who str . args)
       (printf "Warning in ~a: ~a" who (apply format str args)))))

  (define (with-output-to-string th) 
    (let-values ([(p extract) (open-string-output-port)])
      (parameterize ([current-output-port p])
	(th)
	(extract))))
  (define (with-output-to-port p th)
    (parameterize ([current-output-port p])
      (th)))

  (define current-directory
    ;; Under PLT parameters are actually a different type than procedures.
    ;; This must be a real parameter if it is to be used with parameterize.
    (make-parameter (plt:path->string (plt:current-directory))
		    (lambda (x)
		      (plt:current-directory x)
		      (plt:path->string (plt:current-directory))))
;     (case-lambda 
;       [() (plt:path->string (plt:current-directory))]
;       [(x) (plt:current-directory x)])
    )

  (define (process str)
    (let ([ls (list->mlist (sys:process str))])
      ;; We drop to the Chez least-common-denominator:
      (list (transcoded-port (car ls) (native-transcoder))   ;; Input port
	    (transcoded-port (cadr ls) (native-transcoder))  ;; Output port
	    (caddr ls) ;; PID
	    )
      ))

#;
(define (system-to-str str)
    (match (plt:process str)
      [(,in ,out ,id ,err ,fun)
       (let ((p (open-output-string)))
         (let loop ((c (read-char in)))
           (if (and (eof-object? c)
                    (not (eq? 'running (fun 'status))))
               (begin 
                 (close-input-port in)
                 (close-output-port out)
                 (close-input-port err)
                 (get-output-string p))
               (begin (unless (eof-object? c) (display c p))
                      (loop (read-char in))))))]))

  (define (get-string-available inp)
    (error 'get-string-available "non-blocking IO not implemented yet..."))

  ;(include "ws/compat/common.ss")
  ;(include "ws/compat/inspector.ss")
  ;; TEMP: making absolute:
;   (include "/home/newton/wavescript/src/ws/compat/common.ss")
;   (include "/home/newton/wavescript/src/ws/compat/inspector.ss")
;   (include "/home/newton/wavescript/src/ws/compat/top-level-values.ss")

  ;; includes are relative to $REGIMENTD/src
  ;; Can't use this from this file or we'd run into a circular dependency.
  (define-syntax include
    (lambda (x)
      (syntax-case x ()
        [(_ fn)
	 (let ([regd (getenv "REGIMENTD")])
	   (unless regd
	     (environment '(scheme base) '(except (rnrs (6)) error)))
	   ;(display "INCLUDE RESOLVED TO: ") (display  (string-append regd "/src")) (newline)
	   #`(plt:include (file #,(datum->syntax #'_ (string-append regd "/src/" (syntax->datum #'fn)))))
	   )]
	)))

  ;; This necessitates building in-place:

  (plt:include "reg_record.ss")
  (plt:include "common.ss")
  (define (native-inspect x)
    (error 'mzscheme "doesn't have a built-in object inspector"))

  (define (debug . x)
    (error 'mzscheme "doesn't have a built-in debug mode"))

  (plt:include "inspector.ss") (define inspect generic-inspect)

;;  (plt:include "top-level-values.ss")
;; TEST TEMPTOGGLE FIXME -- seeing if including makes any difference:
(begin  
;; This defines a system of top-level "global" variables for R6RS
;; implementations which don't allow direct access to the top-level
;; environment.  

;; Be careful about relying on this default.  Generally scheme
;; implementations need a little extra somethig added to this.
#;
(define default-top-level-eval-env
    (environment 
     '(except (rnrs (6)) error) '(rnrs r5rs (6)) 
     '(rnrs mutable-pairs (6)) '(rnrs mutable-strings (6)) 
     '(main_r6rs) '(main)))

(define top-table (make-eq-hashtable 200))

(define (define-top-level-value var val) (hashtable-set! top-table var val))
(define (set-top-level-value! var val)   (hashtable-set! top-table var val))

(define special-value (lambda (x) x))
(define (top-level-bound? var) (hashtable-ref top-table var #f))
(define (top-level-value var)  
  (let ([result (hashtable-ref top-table var special-value)])
    (if (eq? result special-value)
	(error 'top-level-value "unbound: ~a" var)
	result)))

;; (Inefficient) This evaluates something inside the virtual top-level namespace. 
(define reg:top-level-eval  
  ;; [2009.11.17] IKARUS BUG: Runnig (box #f) at initialization time for the module crashes ikarus.
;  (let ([repl-env (box #f)])
  (let ([repl-env #f])

    (define default-imports
      '((except (rnrs (6)) error) ; (rnrs r5rs (6))
	(rnrs mutable-pairs (6)) (rnrs mutable-strings (6))
	(main_r6rs) (main) (ws shortcuts)))
    
    ;; Here's a little hack that transforms (define v e) expressions into
    ;; explicit top-level-value calls.
    ;; WARNING: This won't allow recursive bindings.
    ;; TODO: I could manage that with let-n-set.
    (define (eval-preprocess x)
      ;; UNLESS we do a little hack.+
      ;; We manage a virtual top-level environment ourselves:
      (if (pair? x)
	  (cond 
	   [(eq? (car x) 'define)
	    (if (pair? (cadr x))
		`(define-top-level-value ',(caadr x) (lambda ,(cdadr x) ,@(cddr x)))
		`(define-top-level-value ',(cadr x) ,(caddr x)))]
	   ;; Go inside begins:
	   ;[(eq? (car x) 'begin)    ]
	   [else x])
	  x))
    (case-lambda 
      [(exp)     
      (printf "reg:top-level-eval TRYING TO SET REPL ENV: ~s ~s\n" exp repl-env)   
       (unless repl-env
;	 (set! repl-env (apply environment (append default-imports implementation-specific-imports)))
	 (set-box! repl-env (default-repl-env))
#;
	 (set! repl-env 
	    (environment '(except (rnrs (6)) error) ; (rnrs r5rs (6))
		  '(rnrs mutable-pairs (6)) '(rnrs mutable-strings (6))
		  '(main_r6rs) '(main) '(ws shortcuts)
		  '(prefix (scheme) plt:)
		  ))
	 (printf "SET THE REPL ENV ~s\n" (unbox repl-env))
	 )
       (reg:top-level-eval exp repl-env)]

      [(exp env)      
       ;; FIXME FIXME : Redefine the existing bindings as
       ;; identifier-syntax that references the *current* value of
       ;; each binding.  However, even then there will be problems if
       ;; the code tries to define a new top-level value, and then
       ;; access it as a normal variable binding.
       (if (and (pair? exp) (eq? (car exp) 'begin))
	   ;; Go inside begins:
	   (for-each (lambda (e) (reg:top-level-eval e env))
	     (cdr exp))
	   (call-with-values (lambda () (hashtable-entries top-table))
	     (lambda (keys vals)
	       (define bound (vector-length keys))
	       (define bindsacc '())
	       (let loop ([i 0])
		 (if (fx=? i bound) (void)
		     (begin 
		       ;;(set! bindsacc (cons (list (vector-ref keys i) (vector-ref vals i)) bindsacc))
		       (set! bindsacc `((,(vector-ref keys i) ',(vector-ref vals i)) . ,bindsacc))
		       (loop (fx+ 1 i)))))
	       ;(printf "Extending with bindings: ~s\n" bindsacc)
	       ;(printf "Expr: \n")
	       ;(pretty-print (eval-preprocess exp))
	       (eval `(let ,bindsacc ,(eval-preprocess exp)) env)
	       #;
	       (for-each (lambda (e)
			   (eval `(let ,bindsacc ,e) env))
		 (eval-preprocess exp)))))
       ])))
)


  (plt:include "tracer.ss")

  (define (display-condition cond . prt)
    (error 'display-condition "TODO implement me in mzscheme"))

)
