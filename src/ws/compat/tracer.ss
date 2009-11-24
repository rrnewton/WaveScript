

;; Simple tracing facility.

  (define-syntax trace-define 
    (syntax-rules ()
      [(_ (f . args) bod ...)
       (define f (trace-lambda f args bod ...))]
      [(_ v e)
       (define v 
	 (let ([closure e])
	   (trace-lambda args
	     (apply closure args))))]))

  ;; No support for tail calls:
  (define trace-depth (make-parameter 0))

  (define-syntax trace-lambda
    (syntax-rules (lambda)
      [(_ name args bod ...)
       (lambda x*
	 (parameterize ([trace-depth (fx+ 1 (trace-depth))])
	   (printf "~a| ~a ~s\n" (make-string (trace-depth) #\space) 'name x*)
	   (let ([result (apply (lambda args bod ...) x*)])
	     (printf "~a| ~a returned ~s\n" (make-string (trace-depth) #\space) 'name result)
	     result))
	 )]))
