
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
		       (operators (,op* (name ,opv*) (output-type ,ot*) (code ,oe*) (incoming ,oin* ...) (outgoing ,odownstrm** ...)) ...)
		       (sink ,base ,basetype)
		       ,___))
	(define (denumber x) (if (symbol? x) x (begin (ASSERT (list? x)) (denumber (cadr x)))))
	(define(blowup src dest*) 
	  ;; Could use deunique-name for the "label" of the node.
	  (map (lambda (dest) (format "  ~a -> ~a;\n" src (denumber dest)))
	    dest*))
	(define edges1 (map blowup srcv* sdownstrm**))
	(define edges2 (map blowup opv* odownstrm**))
(text->string
`("digraph Foo {
"  ,(append edges1 edges2)"
}"))]))])

;;   rankdir=LR;

;; Take the output of explicit-stream-wiring and produce 







	;(define edges3 (list (format " ~a -> BASE\n" base)))	
;	(inspect (append edges1 edges2 edges3))


) ;; End module
