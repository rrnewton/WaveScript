

;;;; [2008.04.25] This file implements a compatibility layer across
;;;; the different R6RS implementations.  This includes common
;;;; functions that are frequently, but not always, provided by the
;;;; host implementation.  I need to find implementation-specific ways
;;;; to load this functionality.  This particular file is the most
;;;; portable, it implements everything on top of vanilla R6RS.

(library (ws compat compat)
  (export include syntax->list getenv
	  ;fx= fx< fx> fx<= fx>= add1 sub1  
	  random
	  make-list
	  merge merge! sort! append! reverse! call/ec inspect native-inspect define-values
	  fluid-let parameterize reg:define-struct reg:struct?
	  void make-parameter
	  format printf fprintf pretty-print
	  format-syntax-nicely
	  gensym current-directory 
	  syntax-error error
	  define-top-level-value set-top-level-value! top-level-bound? top-level-value 
	  reg:top-level-eval simple-eval
	  warning warning-handler system  real-time cpu-time time
	  print-level print-graph print-length pretty-maximum-lines pretty-line-length print-vector-length
	  with-output-to-port with-output-to-string  repl
	  process get-string-available

	  box unbox set-box! box? 

	  promise? delay force

	  trace-define trace-lambda

	  ;cons* ;; HACK: larceny is missing cons*

	  which-scheme IFCHEZ
	  ;__foreign
	  )
  (import (except (rnrs (6)) error)
	  (rnrs r5rs)
	  (rnrs eval)
	  (for (primitives with-output-to-string random current-directory gensym
			   repl make-list getenv pretty-print reverse! append! sort! time
			   with-output-to-port
			   ;let-values define-values
			   ) run expand) ;; Larceny specific!!
	  ;(for (core define-values) run expand)	  
	  (prefix (primitives format system) builtin:) ; make-parameter open-output-string get-output-string
	  )

  (define which-scheme 'larceny)
  (define (format-syntax-nicely syn) syn)

  ;; From Chez user's guide:
  (define-syntax include
    (lambda (x)
      (define read-file
	(lambda (fn k)
	  ;(display "Including file from this directory: ") (display (current-directory))(newline)
	  (let ([p (open-input-file fn)])
	    (let f ([x (read p)])
	      (if (eof-object? x)
		  (begin (close-input-port p) '())
		  (cons (datum->syntax k x)
			(f (read p))))))))
      (syntax-case x ()
		   [(k filename)
		    (let ([fn 
			   ;(datum filename)
			   (syntax->datum (syntax filename))
			   ])
		      (with-syntax ([(exp ...) (read-file fn #'k)])
			#'(begin exp ...)))])))

  (define-syntax syntax-error
    (syntax-rules ()
      [(kwd form msg) (syntax-violation #f msg form)]
      [(kwd msg)      (syntax-violation #f msg #f)]))

  (include "ws/compat/common.ss")

  (reg:define-struct (boxrec contents))
  (define-syntax box      (identifier-syntax make-boxrec))
  (define-syntax set-box! (identifier-syntax set-boxrec-contents!))
  (define-syntax unbox    (identifier-syntax boxrec-contents))
  (define-syntax box?     (identifier-syntax boxrec?))
  
  ;; This is required to return #t for promises, but false positives are ok.
  ;; This is too lenient, but there's no other option.
  (define promise? procedure?)
  
  (define die error)
  (define (void) (if #f #t))

  (define call/ec call/cc)
 
  (define-syntax parameterize
    (lambda (x)
      (syntax-case x ()
		   [(_ () e1 e2 ...) (syntax (begin e1 e2 ...))]
		   [(_ ([x v] ...) e1 e2 ...)
       ;(for-all identifier? (syntax (x ...)))
       (with-syntax ([(p ...) (generate-temporaries (syntax (x ...)))]
                     [(y ...) (generate-temporaries (syntax (x ...)))])
         (syntax
           (let ([p x] ... [y v] ...)
             (let ([swap (lambda ()
                           (let ([t (p)]) (p y) (set! y t)) ...)])
               (dynamic-wind swap (lambda () e1 e2 ...) swap)))))])))

  ;; R6RS doesn't have parameters:
  ;; Larceny does but they're different:
  (define make-parameter
    (case-lambda
      [(x) (case-lambda
	     [() x]
	     [(v) (set! x v)])]
      [(x guard)
       (unless (procedure? guard)
	 (error 'make-parameter "not a procedure" guard))
       (set! x (guard x))
       (case-lambda
	 [() x]
	 [(v) (set! x (guard v))])]))

  (define (format str . args) 
    (call-with-string-output-port
     (lambda (prt)
       (apply builtin:format prt str args))))
  (define (printf str . args) 
    (apply builtin:format (current-output-port) str args))
  (define fprintf builtin:format)

  (define (native-inspect x)
    (error 'larceny "doesn't have a built-in object inspector"))
  (include "ws/compat/inspector.ss") (define inspect generic-inspect)
  (include "ws/compat/top-level-values.ss")
  (include "ws/compat/multiple-values.ss")

  (define warning-handler 
    (make-parameter
     (lambda (who str . args)
       (printf "Warning in ~a: ~a" who (apply format str args)))))
  (define (warning who str . args)
    (apply (warning-handler) who str args))

  ;; Does nothing, can't control print-level that I know of.
  (define print-level (make-parameter 0))
  (define print-length (make-parameter 0))
  (define pretty-maximum-lines (make-parameter 0))
  (define pretty-line-length   (make-parameter 80))
  (define print-graph (make-parameter #f))  
  (define print-vector-length  (make-parameter #f))

  ;(define (system x) "shell calls not implemented")
  (define (system cmd)
    (let ([ret (builtin:system cmd)])
      (if (zero? ret) #t #f)))
  (define (process x) "shell calls not implemented")

  (define (real-time) 0)
  (define (cpu-time) 0)

  (define (get-string-available inp)
    (error 'get-string-available "non-blocking IO not implemented..."))

  ;; Not implementing these yet.
;   (define-syntax trace-define 
;     (syntax-rules ()
;       [(_ pat rhs) (define pat rhs)]))
;   (define-syntax trace-lambda 
;     (syntax-rules ()
;       [(_ name pat rhs ...) (lambda pat rhs ...)]))
  
  (include "ws/compat/tracer.ss")

#;
  (define-syntax cons*
    (syntax-rules ()
      [(_ x) x]
      [(_ a b ...) (cons a (cons* b ...))]))

)