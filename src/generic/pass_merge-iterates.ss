;;;; Pass: Merge Iterates
;;;; .author RRN & MIC
;;;; 

(module pass_merge-iterates mzscheme

  (require 
; 	   "../generic/constants.ss"
;            "../plt/iu-match.ss"
;            "../plt/hashtab.ss"
; 	   "../plt/prim_defs.ss"
;            (all-except "../plt/hm_type_inference.ss" test-this these-tests)
;            (all-except "../plt/tsort.ss" test-this these-tests)
;            (all-except "../plt/tml_generic_traverse.ss" test-this these-tests)
;            (all-except "../plt/helpers.ss" test-this these-tests)
;            (all-except "../plt/regiment_helpers.ss" test-this these-tests)
	   )
  
  (provide merge-iterates
           )

  (chezimports )


(define merge-iterates
  (build-compiler-pass ;; This wraps the main function with extra debugging
   'rename-var
   `(input)
   `(output) 
   (let ()
     (define (subst-emits body fun)
       (core-generic-traverse
	(lambda (expr fallthrough)
	  (match expr
	    [(emit ,vqueue ,x) `(app ,fun ,x)]
	    [(iterate ,_ ...)
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
	    `(,input-language '(program ,body ,type)))])))))



#;
(define testresult 
  (strip-types '(letrec ([s1 (Signal (Sigseg Integer)) (app audioFile
                                                 "./countup.raw"
                                                 10
                                                 0)])
       (letrec ([s2 (Signal Integer) (iterate
                                       (lambda (sigseg)
                                         ((Sigseg Integer))
                                         (letrec ([___VIRTQUEUE___ (VQueue
                                                                     Integer) (virtqueue)])
                                           (begin
                                             (emit
                                               ___VIRTQUEUE___
                                               (seg-get
                                                 sigseg
                                                 (app start sigseg)))
                                             ___VIRTQUEUE___)))
                                       s1)])
         (letrec ([s3 (Signal Integer) (iterate
                                         (lambda (x)
                                           (Integer)
                                           (letrec ([___VIRTQUEUE___ (VQueue
                                                                       Integer) (virtqueue)])
                                             (letrec ([f_5 (Integer
                                                             ->
                                                             Integer) (lambda (x)
                                                                        (Integer)
                                                                        (begin
                                                                          (emit
                                                                            ___VIRTQUEUE___
                                                                            (* x
                                                                               2))
                                                                          ___VIRTQUEUE___))])
                                               (begin
                                                 (app f_5 (+ x 1))
                                                 ___VIRTQUEUE___))))
                                         s2)])
           s3)))))


) ;; End module
