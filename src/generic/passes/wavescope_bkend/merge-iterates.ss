;;;; Pass: Merge Iterates
;;;; .author RRN & MIC
;;;; 

(module merge-iterates mzscheme
  (require 
          "../../../plt/common.ss"
	  )
  (provide merge-iterates
	   test-merge-iterates	   
           )
  (chezimports )


   ;; -> [X] -> [Y] ->
   ;; -> [X(Y)] -> 
  
  (define (subst-emits body fun vq)
    (core-generic-traverse
     (lambda (expr fallthrough)  ;; driver
       (match expr
	 [(emit ,vqueue ,[x]) `(app ,fun ,x ,vq)]
	 [(iterate . ,_)
	  (error 'merge-iterates:subst-emits "shouldn't have nested iterates! ~a" expr)]
	 [,other (fallthrough other)]))
     (lambda (ls k) (apply k ls)) ;; fuser
     body))  

  (define (do-expr expr fallthrough)
    (match expr
      ;; [2006.11.24] rrn, NOTE: There seem to be some bugs in the pattern matcher 
      ;; related to (,foo ... ,last) patterns.  Avoiding them for now.

      ;; Modifying to not create free-variables in the introduced lambda abstraction.
      [(iterate (lambda (,y ,VQY) (,ty (VQueue ,outy)) ,body)
		(iterate (lambda (,x ,VQX) (,tx (VQueue ,outx)) 
				 ;; By convention the return-value is the vqueue:
				 (begin ,bodx-exprs ...)
				 )
			 ,inputstream))
       (let ([return-val (rac bodx-exprs)]
	     [exprs (rdc bodx-exprs)])
       (ASSERT (eq? return-val VQX))       
       (let ([f (unique-name 'f)])
	 (do-expr
	  `(iterate (lambda (,x ,VQX) (,tx (VQueue ,outy))
			    (letrec ([,f (,ty (VQueue ,outy) -> #())
					 (lambda (,y ,VQY) (,ty (VQueue ,outy))
						 (begin ,body (tuple)))])
			      ,(subst-emits `(begin ,@exprs ,VQX) f VQX)))
		    ,inputstream)
	  fallthrough)))]

      [(iterate (lambda ,_ ...) (iterate (lambda ,__ ...) ,___))
       (error 'merge-iterates "implementation problem, should have matched this but didn't: \n~s" 
	      `(iterate (lambda ,_ ...) (iterate (lambda ,__ ...) ,___)))]
      [,other (fallthrough other)]))

(define-pass merge-iterates [Expr do-expr])


(define these-tests 
  `(
    ["Peform basic iterate merging."
     (length (deep-assq-all 'iterate
	       (merge-iterates 
		'(foolang 
		  '(program (iterate (lambda (x vq1) (Int (VQueue Int))
					     (begin (emit vq1 (+_ x 1)) 
						    (emit vq1 (+_ x 100))
						    vq1))
				     (iterate (lambda (y vq2) (Int (VQueue Int))
						      (begin (emit vq2 (*_ y 2))
							     (emit vq2 (*_ y 3))
							     vq2))
					      SOMESTREAM))
		     T)))))
     ;; Simply verify that it reduces two iterates to one.
     1]
    ))

(define test-merge-iterates 
  (default-unit-tester "Merge-Iterates: collapse adjacent iterates"  these-tests))















#;
(define merge-iterates
  (build-compiler-pass ;; This wraps the main function with extra debugging
   'merge-iterates
   `(input)
   `(output) 
   (let ()
     (define (subst-emits body fun)
       (core-generic-traverse
	(lambda (expr fallthrough)
	  (match expr
	    [(emit ,vqueue ,x) `(app ,fun ,x)]
	    [(iterate . ,_)
	     (error 'merge-iterates:subst-emits "shouldn't have nested iterates! ~a" expr)]
	    [,other (fallthrough other)]))
	(lambda (ls k) (apply k ls))
	body))

     (define process-expr
       (lambda (expr)
	 (core-generic-traverse
	  (lambda (expr fallthrough) ;; driver
	    (match expr
	      [(iterate (lambda (,y) (,ty) 
				(letrec ([___VIRTQUEUE___ (VQueue ,outy) (virtqueue)])
				  ,body))
			(iterate (lambda (,x) (,tx) 
					 (letrec ([___VIRTQUEUE___ (VQueue ,outx) (virtqueue)])
					   ,bodx))
				 ,inputstream))
	       (let ([f (unique-name 'f)])
		 (process-expr
		  `(iterate (lambda (,x) (,tx)
				    (letrec ([___VIRTQUEUE___ (VQueue ,outy) (virtqueue)])
				      (letrec ([,f (,ty -> ,outy)
						   (lambda (,y) (,ty) ,body)])
					,(subst-emits bodx f))))
			    ,inputstream)))]
	      [,other (fallthrough other)]))
	  (lambda (ls k) (apply k ls)) ;; fuser
	  expr)))

     ;; Main pass body:
     (lambda (expr)
       (match expr
	 [(,input-language (quote (program ,body ,type)))
	  (let ([body (process-expr body)])
	    `(merge-iterates-language '(program ,body ,type)))])))))



#;
(define testresult 
  (strip-types '(letrec ([s1 (Stream (Sigseg Int)) (app audioFile
                                                 "./countup.raw"
                                                 10
                                                 0)])
       (letrec ([s2 (Stream Int) (iterate
                                       (lambda (sigseg)
                                         ((Sigseg Int))
                                         (letrec ([___VIRTQUEUE___ (VQueue
                                                                     Int) (virtqueue)])
                                           (begin
                                             (emit
                                               ___VIRTQUEUE___
                                               (seg-get
                                                 sigseg
                                                 (app start sigseg)))
                                             ___VIRTQUEUE___)))
                                       s1)])
         (letrec ([s3 (Stream Int) (iterate
                                         (lambda (x)
                                           (Int)
                                           (letrec ([___VIRTQUEUE___ (VQueue
                                                                       Int) (virtqueue)])
                                             (letrec ([f_5 (Int
                                                             ->
                                                             Int) (lambda (x)
                                                                        (Int)
                                                                        (begin
                                                                          (emit
                                                                            ___VIRTQUEUE___
                                                                            (*_ x
                                                                               2))
                                                                          ___VIRTQUEUE___))])
                                               (begin
                                                 (app f_5 (+ x 1))
                                                 ___VIRTQUEUE___))))
                                         s2)])
           s3)))))


) ;; End module
