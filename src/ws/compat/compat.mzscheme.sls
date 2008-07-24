#!r6rs

(library (ws compat compat)
  (export 
          include getenv syntax->list 
 	  random make-list
 	  merge merge! sort! append! reverse! call/ec inspect define-values
 	  fluid-let parameterize reg:define-struct reg:struct?
 	  void make-parameter
 	  format printf fprintf pretty-print
	  format-syntax-nicely
 	  gensym current-directory 
	  syntax-error
 	  define-top-level-value set-top-level-value! top-level-bound? top-level-value 
 	  reg:top-level-eval simple-eval
 	  warning warning-handler real-time cpu-time time	   
 	  print-level print-graph print-length pretty-maximum-lines pretty-line-length print-vector-length
 	  with-output-to-string  repl
 	  process get-string-available

	  box unbox set-box! box? 
	  promise? delay force

	  which-scheme
	  trace-define trace-lambda

	  (rename (sys:system system))
; 	  make-rectangular
	  )
  (import (except (rnrs (6)) current-output-port)
	  (for (rnrs eval (6)) run expand)
	  (for (only (scheme base) random getenv ;syntax->list 
		sort call/ec define-values parameterize
		void make-parameter
		format printf fprintf 
		gensym 
		time read-eval-print-loop
		file 
		current-milliseconds current-process-milliseconds
		box unbox set-box! box? 
		current-output-port
		) run expand)
	  (only (lazy promise) promise? delay force)
	  
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

  (define which-scheme 'mzscheme)

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

  (define repl read-eval-print-loop)
    
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

  (define current-directory
    (case-lambda 
      [() (plt:path->string (plt:current-directory))]
      [(x) (plt:current-directory x)]))

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
	 (let ([regd 
		(getenv "REGIMENTD")
		#;(eval '(getenv "REGIMENTD") 
			   (environment '(scheme base) '(rnrs)))])
	   (unless regd
	     (error 'include "REGIMENTD must be set for include macro to work under PLT"))
	   ;(display "INCLUDE RESOLVED TO: ") (display  (string-append regd "/src")) (newline)
	   #`(plt:include (file #,(datum->syntax #'_ (string-append regd "/src/" (syntax->datum #'fn)))))
	   )]
	)))

  ;; This necessitates building in-place:
  (plt:include "common.ss")
  (plt:include "inspector.ss") (define inspect generic-inspect)
  (plt:include "top-level-values.ss")
  (plt:include "tracer.ss")


  ;(display "MZSCHHHHHHHHHHHHHHHEEEEEEEEEEEEMMMEEEEEEEE ")
  ;(display (random 300))
  ;(newline)
)
