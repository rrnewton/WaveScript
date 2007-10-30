;;;; [2006.11.26] PROTOTYPE.

#|

(let ([x 3]
      [y 1])
  (lambda (x vq)
    (begin 
      (set! x 3)
      (emit y vq)
      (set! y 4)
      vq
      )))


(let ([a 3]
      [b 1])
  (lambda (x vq)
    (begin 
      (set! a 3)
      (if (app foo a)
	  (emit b vq))
      (set! b x)
      vq
      )))


(lambda (x (a,b))
  ([b], (3,x))  
  )


(lambda (x (a,b))
  (let ([a1 3])
    (let ([ret (if (app foo a1)
		   [b]
		   [])])
      (let ([b1 x])
	(ret,(a1,b1))	
	))))


(lambda (x a b vq)
  (let ([a1 3])
    (let ([vq1 (if (app foo a1)
		   (add-vqueue vq b)
		   vq)])
      (let ([b1 x])
	(vq1,(a1,b1))
	))))


(lambda (x a b) ;vq
  (let ([a1 3])
    (let ([ret (if (app foo a1)
		   [b]
		   [])])
      (let ([b1 x])
	(ret,(a1,b1))	
	))))

;===============================================================================

(let ([a 3]
      [b 1])
  (lambda (x vq)
    (begin 
      (set! a 3)
      (if (app foo a)
	  (begin (emit b vq)
		 (set! a 4)))
      (set! b x)
      vq
      )))

;; Converted:
(lambda (x a b vq)       
  (let ([a1 3])
    (let ([(vq2,a3)
	   (if (app foo a1)
	       (let ([vq1 (add-vqueue vq b)])
		 (let ([a2 4])
		   (vq1,a2)))
	       (vq,a1))])
      (let ([b2 x])
	(a3,b2,(return-vqueue vq2))
	))))

;; Man, can that be optimized?  What about getting rid of tuple construction?
;; Well with tuples passed as values it's not that bad.
(lambda (x a b vq)       
  (let ([(vq2,a3)
	 (if (app foo 3)
	     ((add-vqueue vq b),4)
	     (vq,3))])
    (a3,x,(return-vqueue vq2))))
;; Even:
(lambda (x a b vq)       
  (if (app foo 3)           
      (4,x,(return-vqueue (add-vqueue vq b)))
      (3,x,(return-vqueue vq))
      ))

;; Alternatively we could just wildly substitute and hope the
;; duplication doesn't get too bad.  In this case, that would work out
;; great.


|#





;===============================================================================
;===============================================================================


(chez:module purify-iterate 
    (testit 
     ;purify-iterate 
     Expr)  

(define (extend-env env lhs* rhs*) (append (map list lhs* rhs*) env))
(define (empty-env) '(end-of-environment))
(define (env-lookup v env) (cadr (ASSERT (assq v env))))

(define (add-vqueue vq elem) `(cons ,elem ,vq))
(define (return-vqueue vq) `(reverse ,vq))


;; Ooo quadratic tree-walks:
(define get-mutable
  (core-generic-traverse
   (lambda (x fallthru)
     (match x 
       [,oth (fallthru oth)])))
  )

(define (make-tuple-pattern vars)
  (if (= 1 (length vars)) 
      (car vars)
      (list->vector vars)))
(define (make-tuple-expr exp*)
  (if (= 1 (length exp*)) 
      (car exp*)
      (cons 'tuple exp*)))

;; Possible let-binds a new partial store state.
;; Takes modified variables, 
(define possible-letST 
  (lambda (tuple-expr vars body)
    `(let ([,(make-tuple-pattern vars) ,tuple-expr])
       ,body)))

;; CLAM?

;; This processes something in effect context.
;; Returns an expression returning a tuple containing modified vars, and a "key" to that tuple.
;;
;; .param exp - expression to process
;; .param env - maps variables to their latest versions
(trace-define (process-for-effect exp env)
  (match exp
    [,var (guard (symbol? var))
	  (vector '(tuple) '())
	  ]

;; TEMP: NOT TOUCHING RHS FIXME FIXME
    [(set! ,v ,rhs)
;    [(set! ,v ,[Expr -> rhs])
     (vector rhs
	     (list v)
	     )
       ]

    )

  #;
  (core-generic-traverse
   (lambda (x fallthru)
     
     )
   ;; The original expression goes away.  Just propogate updates
   (lambda (ls k)     
     (match ls 
       [(#(,expr* ,vars*) ...)
	(vector `(tuple ,expr* ...)
		vars*)
	]))
   exp))

;; Return the virtual queue, plus the new state.
(define (integrate-return vq state-vars env)
  (make-tuple-expr 
   (list vq (make-tuple-expr 
	     (map (lambda (v) (env-lookup v env)) state-vars))))
  )

;; .param exp - expression to process
;; .param input-var - var holding iterate input
;; .param vq-var - var holding virtual queue
;; .param state-vars - all state variables for iterate
;; .param E - context for current expression (continuation)
;; .param env - maps variables to their latest versions
;; .returns 
(trace-define (thread-state exp input-var vq-var state-vars E env) 
  (core-generic-traverse
   (lambda (x fallthru)
     (match x

     ;; Get the latest version of that variable and apply the context.
     ;[,var (guard (symbol? x)) (E (cadr (ASSERT (assq var env))))]
     ;[(quote ,c) (E `(quote ,c))]
     [,var (guard (symbol? x)) (env-lookup var env)]
     [(quote ,c) `(quote ,c)]

     ;; TEMP, FIXME: loop on val.
     ;; Tail expression returns new state.
     [(begin ,val) ;; This is the VQ value.
      (integrate-return val state-vars env)
      ]
      
     ;; If we've normalized context, this is where side-effects happen!
     [(begin ,head ,rest ...)       
       (let-match ([#(,head ,vars) (process-for-effect head env)])
	 (let* (;[oldvars (map (lambda (v) (cadr (ASSERT (assq v env)))) vars)]
		[newvars (map unique-name vars)])	 
	   (possible-letST 
	    head newvars
	    (thread-state `(begin . ,rest)
			  input-var vq-var state-vars E
			  (extend-env env vars newvars))
	    )))]

#;
      [(set! ,v ,[?? -> rhs])
       (vector #()
	       (list v)
	       (list rhs)
	       )
       ]
      

#|
      ;; Here we need to wrap-up the continuation, or duplicate code.
      [(if ,test-exp ,true-exp ,false-exp)
       (possible-letk
         (CLAM E)
         (lambda (k)
           (cps test-exp
                (lambda (v)
                  `(if ,v
                       ,(cps true-exp (lambda (v) `(APk ,k ,v)))
                       ,(cps false-exp (lambda (v) `(APk ,k ,v))))))))]
      [(lambda (,x ...) ,M)
       (let ([k (generate-k)])
         (E `(lambda (,x ... ,k)
               ,(cps M (lambda (v) `(APk ,k ,v))))))]


      
      [(let ((,x ,N) ...) ,body)
       (possible-letk
         (CLAM E)
         (lambda (k)
           (cps* `(,N ...)
                 (lambda (w*)
                   `(let ,(map list `(,x ...) w*)
                      ,(cps body (lambda (v) `(APk ,k ,v))))))))]
      [(letrec ((,xs (lambda (,idss ...) ,bodys)) ...) ,letrec-body)
       (possible-letk
         (CLAM E)
         (lambda (k)
           `(letrec
              ,(map list `(,xs ...)
                    (map (lambda (ids body)
                           (let ([k (generate-k)])
                             `(LAMBDA (,ids ... ,k)
                                ,(cps body (lambda (v) `(APk ,k ,v))))))
                         idss bodys))
              ,(cps letrec-body (lambda (v) `(APk ,k ,v))))))]


      [(,prim ,N ...)
       (guard (primitive? prim))
       (cps* `(,N ...)
             (lambda (w*)
               (E `(,prim ,@w*))))]
      [(app ,M ,NS ...)
       (cps M
            (lambda (v)
              (cps* `(,NS ...)
                    (lambda (w*)
                      `(,v ,@w* ,(CLAM E))))))]

|#

;      [,oth (fallthru oth)]
       )
     )
   (lambda (ls k) (apply k ls))
   exp
   ))


(define (Expr-driver ex fallthru)
  (match ex
    [(iterate ,annot (,lett ([,lhs* ,ty* ,rhs*] ...)
		     (lambda (,x ,vq) (,tyx ,tyvq) ,[bod]))
	      ,[source])
     (guard (memq lett '(let letrec)))
     `(integrate 
       (lambda (x ,(make-tuple-pattern lhs*)) 
	 ,(thread-state bod x vq lhs* (lambda (x) x) (empty-env)))
       ,source)
     ]
    
    [,oth (fallthru oth)]
    ))
(define (Expr ex)
  (core-generic-traverse 
     Expr-driver
     (lambda (k ls) (apply k ls))
     ex))




#;
(define-pass purify-iterate
  [Expr Expr]
  )

(define (testit)  
  (Expr 
   '(iterate () (let ([st Int 3])
	       (lambda (x vq) (Int (VQueue Int))
		 (begin 
		   (set! st (+ st 1))
		   vq
		   )
		 ))
	     (audio 0 1 1))))

) ;; End module.
