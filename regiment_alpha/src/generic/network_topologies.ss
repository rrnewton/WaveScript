;; [2004.06.06]
;; Split this off as another file to cook up some hand-made topologies.

;; Here are some premade topologies 
(define line-graph 
  (let ((seed (map (lambda (_) (random-node)) (iota numprocs))))
    ;; Connect the graph:
    (set! seed
					;      (let ((ids (map node-id graph)))
					;	(map 
	  (map (lambda (node)
		 (cons node 
		       (filter (lambda (n) 
				 (and (not (eq? node n))
				      (< (dist (node-pos node) (node-pos n)) radius)))
				    seed)))
	       seed))
    seed))

;(define (use-line-graph th)
;  (fluid-let ((graph line-graph)
;	      (object-graph (make-object-graph line-graph))
;	      (all-objs (map car object-graph)))
;    (th))
;  )


(define two-processors
  (let ([a (random-node)]
	[b (random-node)])
    `((,a ,b)
      (,b ,a))))

