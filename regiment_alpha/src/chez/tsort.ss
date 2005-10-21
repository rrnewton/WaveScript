;; RRN: I lifted this from SLIB.

(module topsort-module (tsort topological-sort test-this these-tests cyclic?)

	(import scheme)

(define (tsort dag pred)
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

(define topological-sort tsort)



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
    (let ((flat (topological-sort g eq?)))
      (let ((cycles
	     (filter 
	      (lambda (x) x)
	      (map 
	       (lambda (entry)
		 (let ((lst (memq (car entry) flat)))
		   (if (not lst) (error 'cyclic? "uhh, definition broken")
		       (if (andmap (lambda (edge) (memq edge lst))
				   (cdr entry))
			   #f
			   (car entry)))))
	       g))))
	(if (null? cycles)
	    #f
	    cycles)))))



(define these-tests  
  `([(tsort '((shirt tie belt)
	     (tie jacket)
	     (belt jacket)
	     (watch)
	     (pants shoes belt)
	     (undershorts pants shoes)
	     (socks shoes))
            eq?)
    unspecified]
  [(tsort '((a b) (b c) (c a)
	     (d e) (e f))
           eq?)
    unspecified])
  )

(define test-this
  (lambda () 
    (default-unit-tester "" these-tests)))
)

;(require tsort)
;(tsort '((a b) (b c) (c a)
;	     (d e) (e f))
;           eq?)

;(define x (make-hash-table))
;(define put hash-table-put!)
;(define get hash-table-get)

