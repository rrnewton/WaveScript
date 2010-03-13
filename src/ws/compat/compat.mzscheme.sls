#!r6rs

(library (ws compat compat)
  (export 
          plt:include getenv syntax->list 
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

	  which-scheme IFCHEZ IFPLT
	  trace-define trace-lambda

	  (rename (sys:system system))

	  ;(rename (plt:include include))
	   include

; 	  make-rectangular
          default-repl-env ;; TEMPTOGGLE -- exposing temporarily
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

	  (for (prefix (scheme base) plt:) expand run)
	  (prefix (only (scheme include) include)
		  plt:)
	  (for (only (scheme mpair) list->mlist mlist->list) expand run)

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

 (define-syntax IFPLT
    (syntax-rules ()
      [(_ a b) a]))

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

   ;; Having problems with using the native PLT repl:
;  (define repl plt:read-eval-print-loop)
   (define (repl)
     (define (cleanse-pairs x) 
       (syntax->datum (plt:datum->syntax #f x)))     

     ;; Here's a simple repl
     (let loop ()
       (printf "> ")(flush-output-port (current-output-port))
       (let ([exp (read)])
	 (unless (eof-object? exp)
	   (let ((val (reg:top-level-eval (cleanse-pairs exp))))
	     (unless (eq? val (void)) (write val))
	     (newline))
	   (loop)))))
    
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
  ;; Therefore we need to override the PLT default include.
  ;; Can't use this from this file or we'd run into a circular dependency.

;; TEMPTOGGLE [2010.03.07] SWITCHING BACK TO PLT BUILTIN INCLUDE:
  (define-syntax include
    (lambda (x)
      (syntax-case x ()
        [(form fn)
	 (let* ([regd (getenv "REGIMENTD")]
		[path (string-append regd "/src/" (syntax->datum #'fn))])

	   (printf "RESOLVED INCLUDE PATH ~s\n" path)

	   ;#`(plt:include (file #,(datum->syntax #'_ path)))
	   #`(plt:include-at/relative-to fn fn (file #,(datum->syntax #'_ path)))

	   ;(datum->syntax (car (syntax->list #'x)) (list #'plt:include `(file ,path)))
	   ;(datum->syntax #'form (list #'plt:include `(file ,path)))

	   ;(datum->syntax #'_ `(,#'plt:include (file ,path)))  ;; Matthew's suggestion

	   
	   ;; Invalid <datum> : 
	   ;(datum->syntax #'form (plt:list #'plt:include (plt:list #'file path)))

	   ;; Not a 'file' or 'lib' form:
	   ;(datum->syntax #'form (syntax->datum #`(plt:include (file #,(datum->syntax #'form path)))))

	   ;; silent fail -- no binding:
	   ;#`(plt:include-at/relative-to #,x #,x (file #,(datum->syntax #'_ path))) 
	  )])))

  ;; This necessitates building in-place:

  (plt:include "reg_record.ss")
  (plt:include "common.ss")
  (define (native-inspect x)
    (error 'mzscheme "doesn't have a built-in object inspector"))

  (define (debug . x)
    (error 'mzscheme "doesn't have a built-in debug mode"))

  (plt:include "inspector.ss") (define inspect generic-inspect)

  (plt:include "top-level-values.ss")
  (plt:include "tracer.ss")

  (define (display-condition cond . prt)
    (error 'display-condition "TODO implement me in mzscheme"))

)
