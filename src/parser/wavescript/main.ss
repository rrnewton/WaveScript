#lang scheme

(provide (all-from-out scheme) 
	 (all-from-out "../../main.sls")
	 pretty-print vector-map
	 ;stream-take browse-stream
	 values->list mlist->list mlist?
	 WSCOMPILE_MACRO
	 )

(require scheme/pretty 
	 scheme/mpair
	 "../../main.sls"
	 "../../main_r6rs.sls"
	 (only-in rnrs/base-6 vector-map)
	 )
(require (for-syntax 
	  scheme/pretty 
	  "../../main.sls"
	  "../../ws/util/iu-match.sls"
	  ;"../../ws/sim/wavescript_sim_library_push.sls"
	  "../../ws/compiler_components/hm_type_inference.sls"
	  (only-in rnrs/base-6 vector-map)))

(define-syntax WSCOMPILE_MACRO
   (lambda (x)
     (syntax-case x ()
       [(_ x)
	(let ()
	  (define (gross-hack sexp)
	    (cond 
	     [(pair? sexp) (mcons (gross-hack (car sexp))
				  (gross-hack (cdr sexp)))]
	     [(vector? sexp)
	      (vector-map gross-hack sexp)]
	     [else sexp]))
	  
	  (define (gross-unhack x)
	    (cond 
	     [(mpair? x)  (cons (gross-unhack (mcar x))
				(gross-unhack (mcdr x)))]
	     [(vector? x) (vector-map gross-unhack x)]
	     [else x]))
	  
	  (define (make-syntax ob)
	    (printf "MAKE SYNTAX ~a \n" ob)	    (exit)
	    (cond
	     [(and (mpair? ob) (eq? (mcar ob) 'src-pos))
	      (let ()
		(define vec (mcar (mcdr ob)))
	      (define body (make-syntax (mcar (mcdr (mcdr ob)))))
	      (define name  (vector-ref vec 0))
	      (define pos1  (vector-ref vec 1))
	      (define line1 (vector-ref vec 2))
	      (define off1  (vector-ref vec 3))
	      (define pos2  (vector-ref vec 4))
	      (define line2 (vector-ref vec 5))
	      (define off2  (vector-ref vec 6))
	      (when (not (pair? name))
		 (error 'src-position "The name wasn't boxed in a list ~a"
			" (which was an unfortunate hack anyway)"))
	       (printf "WOOT ~a\n" `#(,(mcar name) ,line1 ,off1 #f ,(- pos2 pos1)))
	       (datum->syntax #'x
			      body
			      `#(,(mcar name) ,line1 ,off1 #f ,(- pos2 pos1))))]

	     [(mpair? ob)   
	      (if (symbol? (mcar ob)) (printf "YAY ~a\n" (mcar ob)) (void))
	      (mcons (make-syntax (mcar ob))
		     (make-syntax (mcdr ob)))]
	     [(vector? ob) (vector-map make-syntax ob)]
	     [else ob])
	    
#;
	    (match ob

	      [(src-pos #(,name ,pos1 ,line1 ,off1 
				,pos2 ,line2 ,off2)
			,[body])
	       (when (not (pair? name))
		 (error 'src-position "The name wasn't boxed in a list ~a"
			" (which was an unfortunate hack anyway)"))
	       (printf "WOOT ~a\n" `#(,(mcar name) ,line1 ,off1 #f ,(- pos2 pos1)))
	       (datum->syntax #'x
			      body
			      `#(,(mcar name) ,line1 ,off1 #f ,(- pos2 pos1)))]
#;
	      ;; A totally unsafe traversal of the AST!!!
	      [(,[x*] ...)  
	       (printf "   LIST \n" )
	       x*]

#;
	      [#(,[x*] ...) 
	       (printf "   VECTOR \n" )
	       (list->vector x*)]
	      [,else ob]))
	  
	  (define __ (pretty-print (wsint-early-passes (gross-hack (syntax->datum #'x)) '())))

	  (define mod
	    (match (wsint-early-passes (gross-hack (syntax->datum #'x)) '())
	      [(,lang '(program ,prog ,_ ...))
	       (printf "ERGH EXPANDING... HERES PROG\n")
	       (gross-unhack (make-syntax prog))
	       ;(gross-unhack (make-syntax (strip-binding-types prog)))
	       ;(gross-unhack (strip-binding-types prog))
	       ]))

	  (printf "Got module syntax expanded... returning it.")

	  (if (file-exists? "DEBUG0.ss") 
	      (delete-file "DEBUG0.ss")
	      (void))
	  (with-output-to-file "DEBUG0.ss"
	    (lambda ()
	      (pretty-print mod)))

	  
	  ;(printf "Expanded ~a\n" mod)
	  ;(datum->syntax #'x (make-syntax mod))
	  (datum->syntax #'x mod)
	  )]
       )))


(printf "THIS IS MAIN.SS\n")


