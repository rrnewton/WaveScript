;;; Pass 09: separate-graph

;;; This pass takes the simple top level progam, which is basically a
;;; dataflow graph, and collects the names of its leaf values.  (The
;;; name of the root value is obvious.)  The leaf "values" are really
;;; the downward edges coming from leaf nodes.  Actually, coming only
;;; from the distributed-primitive nodes.


(define uncover-leaves
  (let ()

    (lambda (expr)
      (match expr
	     [(,input-language (quote (program ,body)))
	      (process-let body '())])
      )))


