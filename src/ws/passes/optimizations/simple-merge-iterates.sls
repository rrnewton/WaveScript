#!r6rs

;;;; This is the simpler variant of merge-iterates that precedes
;;;; Michael's "full merging" version.

(library (ws passes optimizations simple-merge-iterates)
  
  (export simple-merge-iterates
           simple-merge-policy:always
	   test-simple-merge-iterates)
  (import (rnrs) (ws common)
	  (ws compiler_components annotations))


; REV 2807: before the full-merging was checked in:
;;
;; mic: this assumes that the program has been smoosh-together'd
;;
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

  ;; do full inlining of Y's body into X's emits
  (define (inline-subst-emits body fun fun-var fun-var-type)
    (core-generic-traverse
     (lambda (expr fallthrough)  ;; driver
       (match expr
         [(emit ,vqueue ,[x])
          `(let ((,fun-var ,fun-var-type ,x)) ,fun)]
         [(iterate . ,_)
          (error 'merge-iterates:inline-subst-emits "shouldn't have nested iterates! ~a" expr)]
         [,other (fallthrough other)]))
     (lambda (ls k) (apply k ls)) ;; fuser
     body))
             

  ;; merge the two lists of annotations
  ;; FIXME: we will probably need something more intelligent soon!
  #;
  (define (merge-annotations annot-outer annot-inner)
    (append annot-outer annot-inner))

  ;; stats for a merged box, ->[left]->[right]->  =>  ->[left->right]->
  (define (merge-data-rates left-stats right-stats)
    (make-bench-stats (bench-stats-bytes  right-stats)
                      (bench-stats-tuples right-stats)
                      (+ (bench-stats-cpu-time left-stats) (bench-stats-cpu-time right-stats))))

  (define (do-expr expr fallthrough)
    (match expr
      ;; [2006.11.24] rrn, NOTE: There seem to be some bugs in the pattern matcher 
      ;; related to (,foo ... ,last) patterns.  Avoiding them for now.

      ;; Modifying to not create free-variables in the introduced lambda abstraction.
      
      ;;
      ;; FIXME: this will keep the "merge-with-downstream" annotation from the upper box!!!
      ;;
      [(iterate (annotations . ,annoty) (let ,statey (lambda (,y ,VQY) (,ty (VQueue ,outy)) ,body))
		(iterate (annotations . ,annotx) (let ,statex
				   (lambda (,x ,VQX) (,tx (VQueue ,outx))
                                           ;; By convention the return-value is the vqueue:
					   ;(begin ,exprs ... ,return-val)
                                           ;; rrn: loosening this up, don't require that the body's a begin:
                                           ,bodx
                                           ))
			 ,inputstream))
       ;; don't merge indiscriminately; let an earlier pass tell us when to merge
       (guard (assq 'merge-with-downstream annoty))

       (log-opt "OPTIMIZATION: merge-iterates: merging nested iterates\n")
       
       ;; rrn: it was good to enforce this convention, but not doing it anymore:
					;(ASSERT (eq? return-val VQX)) 
       (do-expr
        `(iterate (annotations . ,(merge-annotations annoty
                                                     annotx
                                                     `([merge-with-downstream . right-only]
                                                       [data-rates manual ,merge-data-rates])))
                  (let ,(append statey statex) (lambda (,x ,VQY) (,tx (VQueue ,outy))
                                                       ,(inline-subst-emits
                                                         `(let ((,VQX (VQueue ,outy) ,VQY)) ,bodx)
                                                         `(begin ,body (tuple))
                                                         y ty)))
                  ,inputstream)
        fallthrough)]


      [(iterate ,annoty (lambda ,_ ...) (iterate ,annotx (lambda ,__ ...) ,___))
       (error 'merge-iterates "implementation problem, should have matched this but didn't: \n~s" 
	      `(iterate ,annoty (lambda ,_ ...) (iterate ,annotx (lambda ,__ ...) ,___)))]
      [,other (fallthrough other)]))


    [Expr do-expr])


;; perform the simple merge whenever possible
(define-pass simple-merge-policy:always

    ;; merge the two lists of annotations
    ;; FIXME: we will probably need something more intelligent soon!
    #;
    (define (merge-annotations annot-outer annot-inner)
      (append annot-outer annot-inner))
  
    (define (do-expr expr fallthrough)
      (match expr
        [(iterate (annotations . ,annot-up) ,f-up
                  (iterate (annotations . ,annot-down) ,f-down ,in-str))

         `(iterate (annotations . ,(merge-annotations '(annotations (merge-with-downstream))
                                                      annot-up))
                   ,f-up
                   ,(do-expr
                     `(iterate (annotations . ,annot-down) ,f-down ,in-str)
                     fallthrough))]

        
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

(define-testing test-simple-merge-iterates
  (default-unit-tester "Merge-Iterates: collapse adjacent iterates"      
  `(
    ["Peform basic iterate merging."
     (length (deep-assq-all 'iterate
	       (simple-merge-iterates 
		'(foolang 
		  '(program (iterate (annotations (merge-with-downstream)) 
				     (let () 
				       (lambda (x vq1) (Int (VQueue Int))
					       (begin (emit vq1 (_+_ x 1)) 
						    (emit vq1 (_+_ x 100))
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
    )))


) ;; End module
