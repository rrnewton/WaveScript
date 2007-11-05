
;;;; This is the simpler variant of merge-iterates that precedes
;;;; Michael's "full merging" version.

(module simple-merge-iterates mzscheme
  (require 
          "../../../plt/common.ss"
	  )
  (provide simple-merge-iterates  
	   test-simple-merge-iterates)
  (chezimports)











; REV 2807: before the full-merging was checked in:
(define-pass simple-merge-iterates 
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

  ;; merge the two lists of annotations
  ;; FIXME: we will probably need something more intelligent soon!
  (define (merge-annotations annot-outer annot-inner)
    (append annot-outer annot-inner))

  (define (do-expr expr fallthrough)
    (match expr
      ;; [2006.11.24] rrn, NOTE: There seem to be some bugs in the pattern matcher 
      ;; related to (,foo ... ,last) patterns.  Avoiding them for now.

      ;; Modifying to not create free-variables in the introduced lambda abstraction.
      [(iterate ,annoty (let () (lambda (,y ,VQY) (,ty (VQueue ,outy)) ,body))
		(iterate ,annotx (let ()
				   (lambda (,x ,VQX) (,tx (VQueue ,outx))
					 ;; By convention the return-value is the vqueue:				 
					;(begin ,exprs ... ,return-val)
					 ;; rrn: loosening this up, don't require that the body's a begin:
					 ,bodx
					 ))
			 ,inputstream))
       (log-opt "OPTIMIZATION: merge-iterates: merging nested iterates\n")
       
       ;; rrn: it was good to enforce this convention, but not doing it anymore:
					;(ASSERT (eq? return-val VQX)) 
       (let ([f (unique-name 'f)])
	 (do-expr
	  `(iterate ,(merge-annotations annoty annotx)
		    (lambda (,x ,VQX) (,tx (VQueue ,outy))
			    (letrec ([,f (,ty (VQueue ,outy) -> #())
					 (lambda (,y ,VQY) (,ty (VQueue ,outy))
						 (begin ,body (tuple)))])
			      ,(subst-emits ;`(begin ,@exprs ,VQX)
				`(begin ,@bodx ,VQX)
				f VQX)))
		    ,inputstream)
	  fallthrough))]

      [(iterate ,annoty (lambda ,_ ...) (iterate ,annotx (lambda ,__ ...) ,___))
       (error 'merge-iterates "implementation problem, should have matched this but didn't: \n~s" 
	      `(iterate ,annoty (lambda ,_ ...) (iterate ,annotx (lambda ,__ ...) ,___)))]
      [,other (fallthrough other)]))

    [Expr do-expr])





;; EVEN OLDER ONE:
#;
(define simple-merge-iterates
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






;; ============================================================================================== ;;
;; TESTING:

(define-testing these-tests 
  `(
    ["Peform basic iterate merging."
     (length (deep-assq-all 'iterate
	       (simple-merge-iterates 
		'(foolang 
		  '(program (iterate (annotations) 
				     (let () 
				       (lambda (x vq1) (Int (VQueue Int))
					       (begin (emit vq1 (+_ x 1)) 
						    (emit vq1 (+_ x 100))
						    vq1)))
				     (iterate (annotations)
					      (let ()
						(lambda (y vq2) (Int (VQueue Int))
							(begin (emit vq2 (*_ y 2))
							       (emit vq2 (*_ y 3))
							       vq2)))
				      SOMESTREAM))
		     T)))))
     ;; Simply verify that it reduces two iterates to one.
     1]
    ))

(define-testing test-this  
  (default-unit-tester "Merge-Iterates: collapse adjacent iterates"  these-tests))

(define test-simple-merge-iterates test-this)




) ;; End module
