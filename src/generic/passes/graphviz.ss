
(module graphviz mzscheme
  (require "../../plt/common.ss"
           "../compiler_components/c_generator.ss")
  (provide output-graphviz
           )

  (chezimports)


(define-pass output-graphviz
  [Program
   (lambda (prog Expr)
     (match prog 
       [(,lang '(graph (const ,_ ...)
		       (init  ,__ ...)
		       (sources ((name ,srcv*) (output-type ,st*) (code ,se*) (outgoing ,sdownstrm** ...)) ...)
		       (operators (,op* (name ,opv*) (output-type ,ot*) (code ,oe*) 
					(incoming ,oin* ...) (outgoing ,odownstrm** ...))
				  ...)
		       (sink ,base ,basetype)
		       ,meta* ...))
	(define (denumber x) (if (symbol? x) x (begin (ASSERT (list? x)) (denumber (cadr x)))))
	(define(blowup src dest*) 
	  ;; Could use deunique-name for the "label" of the node.
	  (map (lambda (dest) (format "  ~a -> ~a;\n" src (denumber dest)))
	    dest*))
	(define edges1 (map blowup srcv* sdownstrm**))
	(define edges2 (map blowup opv* odownstrm**))
	
	(define nodelabels 
	  (map (lambda (name opcode)
		      (match opcode
			[(,streamop (annotations . ,annot) . ,rest)
			 ;(guard (temp-hack-stream-primitive? streamop))
			 (list (format "  ~a [label=\"~a~a\"];\n"
				       name
				       name;(cdr (ASSERT (assq 'name annot)))
				       (let ([cpu (assq 'cpu-pin annot)]
					     [datarates (assq 'data-rates annot)])
					 (string-append
					  (if cpu       (format "\\n[cpu ~a]" (cdr cpu)) "")
					  ;; This should be improved, and should probably affect the color
					  (if datarates (format "\\n[rates ~a]" (cdr datarates)) "")
					  ))))
			 ]
			;[,_ (void)]
			)
		      )
	    (append srcv* opv*) 
	    (append se* oe*)))
	
(text->string
`("digraph Foo {
"  ,(append edges1 (reverse edges2))"
"  ,nodelabels"
}"))]))])

;;   rankdir=LR;

;; Take the output of explicit-stream-wiring and produce 







	;(define edges3 (list (format " ~a -> BASE\n" base)))	
;	(inspect (append edges1 edges2 edges3))


) ;; End module


  