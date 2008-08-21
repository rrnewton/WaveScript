#!r6rs

;; This pass flattens the stream-processing spine of the program.

;; (TODO: Should now be called flatten-stream-spine.)

(library (ws passes wavescope_bkend flatten-iterate-spine)
  (export flatten-iterate-spine)
  (import (except (rnrs (6)) error) (ws common))  

(define-pass flatten-iterate-spine
    (define (make-simple-shallow x tenv)
      (if (simple-expr? x) 
	  (values x '())	  
	  (mvlet ([(type) (recover-type x tenv)]
		  [(name) (unique-name 'tmp)])	    
	    (values name
		    `((,name ,type ,x))))))
  [Expr/Types
     (lambda (x tenv fallthru)
       ;; Coerces an expression to be simple, producing new bindings.              
       (match x

#;	 
	 [,x (guard (begin 
		      (if (pair? x) (printf "FIS: ~s\n" (car x)))
		      #f)) 00]

	 [(iterate ,annot ,fun ,[src])
	  (mvlet ([(src binds) (make-simple-shallow src tenv)])
	    ;; This had better be a symbol for now.
	    (ASSERT symbol? src)
	    
	    (if (null? binds)
		`(iterate ,annot ,fun ,src)
		`(lazy-letrec ,binds (iterate ,annot ,fun ,src)))
	    )]

	 [(zip2 ,[s1] ,[s2])
	  (mvlet ([(s1 binds1) (make-simple-shallow s1 tenv)]
		  [(s2 binds2) (make-simple-shallow s2 tenv)]
		  )
	    (define binds (append binds1 binds2))
	    (ASSERT symbol? s1)
	    (ASSERT symbol? s2)	    
	    (if (null? binds)
		`(zip2 ,s1 ,s2)
		`(lazy-letrec ,binds (zip2 ,s1 ,s2)))
	    )]

	 [(unionN ,annot ,[strms] ...)	 
	  (match (map (lambda (S) (values->list (make-simple-shallow S tenv))) strms)
	    [((,arg* ,binds*) ...)
	     (define binds (apply append binds*))
	     (DEBUGASSERT (lambda (ls) (andmap symbol? ls)) arg*)
	     (if (null? binds)
                 `(unionN ,annot . ,arg*)
                 `(lazy-letrec (,annot . ,binds) (unionN ,annot . ,arg*)))
	     ])]

	 ;; Safety net:
	 [(,missed ,_ ...) (guard (memq missed '(iterate zip2 unionN)))
	  (error 'flatten-iterate-spine "Missed construct; should have caught: ~s" `(,missed . ,_))]

	 [,other (fallthru other tenv)]
	 ))]
  )

) ; End module





#|
(flatten-iterate-spine 
 '(foolang
   '(program       
	(lazy-letrec
      ((slist_58
	(List (Stream #(Float (Sigseg Float))))
	(cons tpk1_21 (cons tpk4_51 '()))))
      (iterate
       ()
       (lazy-letrec
	((buf1_60 (List #(Float (Sigseg Float))) '())
	 (buf2_59 (List #(Float (Sigseg Float))) '()))
	(lambda (pattmp_61)
	  (#(Int #(Float (Sigseg Float))))
	  (lazy-letrec
	   ()
	   (lazy-letrec
	    ((i_62 Int (tupref 0 2 pattmp_61)))
	    (lazy-letrec
	     ((x_63
	       #(Float (Sigseg Float))
	       (tupref 1 2 pattmp_61)))
	     (lazy-letrec
	      ((VIRTQUEUE_64
		(VQueue
		 #(#(Float (Sigseg Float))
		   #(Float (Sigseg Float))))
		(virtqueue)))
	      (begin
		(if (equal? i_62 '0)
		    (set! buf1_60 (cons x_63 '()))
		    (if (equal? i_62 '1)
			(set! buf2_59 (cons x_63 '()))
			(wserror '"implementation error")))
		(if (and (equal? (List:length buf1_60) '1)
			 (equal? (List:length buf2_59) '1))
		    (begin

		      (emit
		       VIRTQUEUE_64
		       (tuple (head buf1_60) (head buf2_59)))
		      (set! buf1_60 '())
		      (set! buf2_59 '()))
		    (tuple))
		VIRTQUEUE_64)))))))
       (unionList (annotations) slist_58))) 
      SOmeTYPe))
 )
|#
