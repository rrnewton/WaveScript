#lang scheme

(provide (all-from-out scheme) 
	 (all-from-out "../../main.sls")
	 pretty-print vector-map
	 ;stream-take browse-stream
	 values->list mlist->list mlist?
	 WSCOMPILE_MACRO
	 EXPANDING_STUFF
	 )

(require scheme/pretty 
	 scheme/mpair
	 "../../main.sls"
	 "../../main_r6rs.sls"
	 (only-in rnrs/base-6 vector-map))
(require (for-syntax 
	  scheme/pretty 
	  "../../main.sls"
	  "../../ws/util/iu-match.sls"
	  "../../ws/sim/wavescript_sim_library_push.sls"
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
	    (match ob
	      ;;    #(#0=("foo.ws") 58 6 7 106 8 2)
	      [(src-pos #(,name ,pos1 ,line1 ,off1 
				,pos2 ,line2 ,off2)
			,[body])
	       (when (not (pair? name))
		 (error 'src-position "The name wasn't boxed in a list ~a"
			" (which was an unfortunate hack anyway)"))
	       (printf "WOOT ~a\n" `#(,(car name) ,line1 ,off1 #f ,(- pos2 pos1)))
	       (datum->syntax #'x
			      body
			      `#(,(car name) ,line1 ,off1 #f ,(- pos2 pos1)))]

	      ;; A totally unsafe traversal of the AST!!!
	      [(,[x*] ...)  x*]
	      [#(,[x*] ...) (list->vector x*)]
	      [,else ob]))
	  
	  (define mod
	    (match (wsint-early-passes (gross-hack (syntax->datum #'x)) '())
	      [(,lang '(program ,prog ,_ ...))
	       (printf "ERGH EXPANDING... HERES PROG\n")
	       (gross-unhack (strip-binding-types prog))]))

	  (printf "Got module syntax expanded... returning it.")
#|
	  (if (file-exists? "DEBUG.ss") sym binding
	      (delete-file "DEBUG.ss")
	      (void))
	  (with-output-to-file "DEBUG.ss"
	    (lambda ()
	      (pretty-print mod)))
|#

	  
	  ;(printf "Expanded ~a\n" mod)
	  ;(datum->syntax #'x (make-syntax mod))
	  (datum->syntax #'x mod)
	  )]
       )))

(define-syntax EXPANDING_STUFF
  (lambda (x)
    (syntax-case x ()
		 [(op) 
		  (printf "NOW I'M IN THE EXPANDER AND ABOUT TO DO WSMACRO: Here's something ~a\n"
			  wsequal?)
		  #'99393])))

(printf "THIS IS MAIN.SS\n")


