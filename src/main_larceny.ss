

;;; EXPERIMENTAL - playing around with the idea of supporting Larceny...

#|
 echo '(import (err5rs load)) (load "main_larceny.ss")' | larceny -err5rs
|#


;;  Load some basics.
(import  
 (for (rnrs base) expand run (meta 2))
 (rnrs io simple)
 (for (rnrs syntax-case) expand run (meta 2))
 (err5rs load)
 (for (rnrs r5rs) expand run) ;; backwards compat
 )

;; Import other larceny primitives:
(import (for (primitives pretty-print make-parameter time 
			 set-box! box unbox gensym getenv
			 dump-heap dump-interactive-heap
			 global-optimization
			 ) run expand))
;(import (larceny benchmarking))

;; Dummy module syntax.
(define-syntax module
  (syntax-rules (require provide chezprovide chezimports)
    [(_ name parent (require __ ...) (provide exports ...) (chezimports imp ...) exp ...)
     (module name () exp ...)]
    [(_ name parent (require __ ...) (provide exports ...) (chezprovide chezexports ...) (chezimports imp ...) exp ...)
     (module name () exp ...)]
    ;; This is a raw chez module, as opposed to a Regiment/WS module:
    [(_ name (exports ...) exp ...)  
     ;(pretty-print '(begin exp ...))
     (begin exp ...)]
    ))

;; Compatibility bindings:
(library (misc-larceny-compat)
  (export literal-identifier=? ellipsis? delay-values 
	  extend-backquote simple-eval 
	  syntax-error datum->syntax-object syntax-object->datum datum
	  andmap call/1cc void include
	  box unbox set-box!
	  )
  (import (for (rnrs base) run expand)
	  (for (rnrs io ports) expand)
	  (for (rnrs io simple) expand)
	  (rnrs r5rs)
	  (rnrs eval)
	  (for (rnrs syntax-case) run expand))

  (define datum->syntax-object datum->syntax)
  (define syntax-object->datum syntax->datum)
  (define-syntax datum
    (syntax-rules ()
      [(_ t) (syntax-object->datum (syntax t))]))

  (define-syntax syntax-error
    (syntax-rules ()
      [(kwd form msg) 
       (syntax-violation #f msg form)]))

  (define literal-identifier=?
    (lambda (x y)
      (eq? (syntax->datum x)
	   (syntax->datum y))))
	 
  (define ellipsis?
    (lambda (x)
      (and (identifier? x) (literal-identifier=? x #'(... ...)))))

  (define-syntax delay-values
    (syntax-rules ()
      ((_ e)
       (let ((vals #f))
	 (lambda ()
	   (if vals (apply values vals)
	       (call-with-values (lambda () e)
		 (lambda args (set! vals args) (apply values args))))
	   )))))

  ;; A dummy binding that uses the normal backquote
  (define-syntax extend-backquote
    (lambda (x)
      (syntax-case x () ((kwd Template Exp ...) #'(begin Exp ...)))))

  ;(define (simple-eval x) (eval x (environment '(r6rs))))
  (define (simple-eval x) (eval x (environment '(rnrs))))

  (define-syntax include
    (lambda (x)
      (define read-file
	(lambda (fn k)
	  (let ([p (open-input-file fn)])
	    (let f ([x (read p)])
	      (if (eof-object? x)
		  (begin (close-input-port p) '())
		  (cons (datum->syntax k x)
			(f (read p))))))))
      (syntax-case x ()
	 [(k filename)
	  (let ([fn (syntax->datum (syntax filename))])
	    (with-syntax ([(exp ...) (read-file fn #'k)])
	      #'(begin exp ...)))])))
  
  (define (andmap fun ls)
    (if (null? ls) #t
	(and (fun (car ls)) (andmap fun (cdr ls)))))

  (define call/1cc call/cc)
  (define-syntax void (syntax-rules () [(_) (begin)]))

  ;; Could use lists, but that would require mutable pairs:
  (define (box x) (vector x))
  (define (unbox x) (vector-ref x 0))
  (define (set-box! b x) (vector-set! b 0 x))
)
(import (for (misc-larceny-compat) run expand))

(define-syntax IFCHEZ 
  (syntax-rules ()
    [(_ a b) (void)]
    [(_ a) (void)]))

;; Expand the appropriate code for loading in Larceny:
(define-syntax cond-expand
    (lambda (syn)
      (syntax-case syn (and or not else 
			    chez plt larceny graphics threads)
      ;; Standard clauses:
      ((cond-expand) (syntax-error #'syn "Unfulfilled cond-expand"))
      ((cond-expand (else body ...))  #'(begin body ...))
      ((cond-expand ((and) body ...) more-clauses ...) #'(begin body ...))
      ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
       #'(cond-expand (req1 (cond-expand ((and req2 ...) body ...) more-clauses ...)) more-clauses ...))
      ((cond-expand ((or) body ...) more-clauses ...) #'(cond-expand more-clauses ...))
      ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
       #'(cond-expand (req1 (begin body ...)) (else (cond-expand ((or req2 ...) body ...) more-clauses ...))))
      ((cond-expand ((not req) body ...) more-clauses ...)
       #'(cond-expand (req (cond-expand more-clauses ...)) (else body ...)))

      ;; Enabled features (or potentially enabled):
      ((cond-expand (larceny body ...) more-clauses ...) #'(begin body ...))
      ;; 'threads' and 'graphics' are disabled for now under larceny.
      
      ;; Otherwise, it's disabled:
      ((cond-expand (feature-id body ...) more-clauses ...) #'(cond-expand more-clauses ...)))))


;; TEMPORARY: Let's turn this off to load a little faster.
(global-optimization #f)

;======================================================================
;;; Print a banner message.

;======================================================================
;;; Begin loading files.  First some setup stuff.

(time (load "chez/match.ss"))
;(display "TEST: ")(display (match 3 [3 9] [4 2]))(newline)

;======================================================================
;;; Now begin loading Regiment/WaveScript proper. 

(define-syntax reg:make-parameter 
   (syntax-rules ()
     [(_ x) (make-parameter 'reg:make-parameter x)]
     [(_ x g) (let ([guard g])
		(make-parameter 'reg:make-parameter (guard x) guard))]))

(import (rnrs records syntactic))

(define-syntax reg:define-struct
  (syntax-rules ()
    [(_ (name field ...))
     (begin (define-record-type name (fields field ...))
	    ;; Allows the reader to read in regiment records.  System should work without this:
	    ;(define reg:struct-dummy-val (record-reader 'name (type-descriptor name)))
	    )]))


(import (larceny compiler)) 

 ;; Load this first.  Widely visible constants/parameters.
;(time (compile-file "generic/constants.ss"))
(time (load "generic/constants.ss"))

;(time (compile-file "generic/util/reg_macros.ss"))
(time (load  "generic/util/reg_macros.ss"))


(time (load "generic/util/hash.ss")) 
(time (load "generic/util/slib_hashtab.ss"))





(dump-interactive-heap "larc.heap")
;(dump-heap "larc.heap" (lambda args (display "Starting from heap... YAY \n") (newline)))
