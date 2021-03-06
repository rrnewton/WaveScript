#!r6rs

;;;; Topological sort.

(library (ws util tsort) 
  (export topological-sort cyclic? test-tsort)
  (import (except (rnrs (6)) error) (ws common))

;; DAG = ([VertSource VertAdjacent ...] ...)
(define (topological-sort dag )
  (if (null? dag)
      '()
      (let* ((adj-table (make-default-hash-table))
	     (insert hashtab-set!)
	     (lookup hashtab-get)
	     (sorted '()))
	(letrec ((visit
		  (lambda (u adj-list)
		    ;; Color vertex u
		    (insert adj-table u 'colored)
		    ;; Visit uncolored vertices which u connects to
		    (for-each (lambda (v)
				(let ((val (lookup adj-table v)))
				  (if (not (eq? val 'colored))
				      (visit v (or val '())))))
			      adj-list)
		    ;; Since all vertices downstream u are visited
		    ;; by now, we can safely put u on the output list
		    (set! sorted (cons u sorted)))))
	  ;; Hash adjacency lists
	  (for-each (lambda (def)
		      (insert adj-table (car def) (cdr def)))
		    (cdr dag))
	  ;; Visit vertices
	  (visit (caar dag) (cdar dag))
	  (for-each (lambda (def)
		      (let ((val (lookup adj-table (car def))))
			(if (not (eq? val 'colored))
			    (visit (car def) (cdr def)))))
		    (cdr dag)))
	sorted)))

;; Tells whether or not a graph is cyclic.  
;; Requires canonical form where each node has exactly one entry.
;; If the graph is acyclic, return #f.
;; Otherwise, return the list of nodes participating in cycles.
;; 
;; REQUIRES:  slib tsort
(define (cyclic? g . compare)
  ;; Umm is this really bad form to perform this INSIDE the cyclic? function??
  ;; How efficient is require??  A linear search through a list of things loaded?
  ;; I'm considering defining "let-run-once"
  ;;(slib:require 'tsort)
  (let ((eq (if (null? compare) eq?
		(if (> (length compare) 1)
		    (error 'cyclic? "too many optional arguments: ~s" compare)
		    (if (procedure? (car compare))
			(car compare)
			(error 'cyclic? 
			       "optional argument must contain a comparison procedure, received : ~s"
			       (car compare)))))))
    (let ((flat (topological-sort g )));eq?)))
      (let ((cycles
	     (filter 
	      (lambda (x) x)
	      (map 
	       (lambda (entry)
		 ;; This looks at everything *after* this node in the flat ordering.
		 (let ((lst (memq (car entry) flat)))
		   (if (not lst) (error 'cyclic? "uhh, definition broken")
		       ;; This makes sure every edge is satisfied, that is, the sink 
		       ;; of the edge falls *after* us in the flat ordering.
		       (if (andmap (lambda (edge) (memq edge (cdr lst)))
				   (cdr entry))
			   #f
			   (car entry)))))
	       g))))
	(if (null? cycles)
	    #f
	    cycles)))))


(define-testing  test-tsort 
  (default-unit-tester "Topological Sort" 
    `([(',topological-sort '((shirt tie belt)
	     (tie jacket)
	     (belt jacket)
	     (watch)
	     (pants shoes belt)
	     (undershorts pants shoes)
	     (socks shoes))
            );eq?)
    unspecified]
  [(',topological-sort '((a b) (b c) (c a)
	     (d e) (e f))
           );eq?)
    unspecified]

  [(and (',cyclic? '((a b) (b a))) #t)  #t]
  [(and (',cyclic? '((a a))) #t)        #t]
  [(',cyclic? '((a b) (b c)))  #f]
  )))

) ; End module
