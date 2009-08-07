#! /bin/bash
#|
exec regiment.chez i --script $0 $*
exec regiment.plt i --script $0 $*
|#

;; /hubs_topology_generate.ss <N> <M>
;; This little script dumps a random topology with N nodes to stdout.

;; The topology consists of M "hubs" which are currently tree
;; topologies.  The hubs are connected by a all-to-all WAN.  Currently
;; the WAN links have less bandwidth than the LAN links within each
;; hub.

(unless (= (length (command-line)) 7)
  (error 'hubs_topology_generate 
	 "takes two arguments <N> <M>, where N is total network size and M is number of hubs."))

(define N  (string->number (rac (rdc (command-line)))))
(define M  (string->number (rac (command-line))))

;; For now we use a fixed degree for the tree
(define degree 3)

(define LAN-bw 1000)
(define LAN-lat 3)
(define WAN-bw 100)
(define WAN-lat 60)

(unique-name-counter 0)
;(random-seed (time-nanoseconds (current-time)))

(define node-names (list-build N (lambda _ (unique-name "node"))))

(printf "network ")  (for-each display (insert-between " " node-names))
(newline)

(printf "\n;; Node Specs\n")


;; The mean # of nodes in a hub:
(define nodes-per-hub (max 1 (quotient N M)))

#;
(define (hub-size n)
  (min n (max 1 (exact (floor (+ nodes-per-layer
				 (* 0.5 (gaussian) nodes-per-layer)))))))

;; Split the nodes up into a list of lists:
(define hubs
  (let ([hubs (make-vector M 1)]) 
    ;; Distribute nodes randomly to each hub (they each start with one):
    ;; First compute the counts:
    (for i = 1 to (- N M)
	 (let ([ind (random M)])
	   (vector-set! hubs ind (+ 1 (vector-ref hubs ind)))))
    (printf ";; Hub sizes: ~a\n" hubs)
    ;; Then fill them in:
    (let loop ([i 0] [ls node-names])
      (unless (= i M)
	(let ([count (vector-ref hubs i)])
	  (vector-set! hubs i (list-head ls count))
	  (loop (+ 1 i) (list-tail ls count)))))
    (vector->list hubs)))


(for-eachi (lambda (i hub)
	    (printf "\n;; Hub ~a, size ~a\n" i (length hub))
	    (for-each (lambda (nd) 
			(printf "node ~a cpu ~a\n" nd
				(+ 80 (random 40))))
	      hub)
	    ;; Build a tree topology out of the nodes:
	    
	    (let loop ([this (car hub)] 
		       [parent #f]
		       [rest (cdr hub)]
		       [numleft (length (cdr hub))])
	      ;(printf "HMMM ~a ~a\n" rest numleft)
	      (when parent
		(printf "  link ~a bw ~a lat ~a <-> ~a\n"  parent LAN-bw LAN-lat this))
	      (ASSERT (= (length rest) numleft))
	      (unless (null? rest)
		(let* ([count (min degree numleft)]
		       [next* (list-head rest count)]
		       [ration   (quotient (- numleft count) degree)]
		       [leftover (remainder (- numleft count) degree)])
		  ;; Account for next* -- shave them off:
		  (set! rest (list-tail rest count))
		  ;(printf ";; next* ~a rationing ~a degree ways with ration ~a leftover ~a\n" next* numleft ration leftover)
		  ;; Split the remaining nodes <degree> ways:
		  ;; Give the extra portion to the first one:
		  (loop (car next*) this (list-head rest (+ ration leftover)) (+ ration leftover))
		  (set! rest (list-tail rest (+ ration leftover)))
		  ;; Then do the rest of them:
		  (for-each (lambda (next)
			      (loop next this (list-head rest ration) ration)
			      (set! rest (list-tail rest ration)))
		    (cdr next*))
		  ))))
  hubs)

;; The root node of each hub:
(define roots (map car hubs))

;; Connect all the roots:

(define (map-distinct-pairs f ls)
  (let loop1 ((l1 ls))
    (if (null? l1) '()
	(let loop2 ((l2 (cdr l1)))
	  (if (null? l2) 
	      (loop1 (cdr l1))
	      (cons (f (car l1) (car l2))
		    (loop2 (cdr l2))))
	  ))))

(printf "\n;; Link all root hubs.\n")
(map-distinct-pairs 
 (lambda (rt1 rt2)
   (printf "  link ~a bw ~a lat ~a <-> ~a\n"  rt1 WAN-bw WAN-lat rt2))
 roots)

