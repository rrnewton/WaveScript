#! /bin/bash
#|
exec regiment.plt i --script $0 $*
exec regiment.chez i --script $0 $*
|#

;; ./network_generate.ss <N>
;; This little script dumps a random topology with N nodes to stdout.

(define N 
  (if (= (length (command-line)) 6)
      (string->number (rac (command-line)))
      50))


(unique-name-counter 0)
;(random-seed (time-nanoseconds (current-time)))

(define node-names (list-build N (lambda _ (unique-name "node"))))

(printf "network ")  (for-each display (insert-between " " node-names))
(newline)

(printf "\n;; Node Specs\n")
(for-each (lambda (nd)
	    (printf "node ~a cpu ~a\n" nd
		    (+ 80 (random 40))))
  node-names)

;; The mean # of nodes in a layer, the actual # will be a normal distribution
;; with stddev 50% of the mean.
;;   Default: We make the network vaguely "square":
(define nodes-per-layer (max 1 (exact (floor (sqrt N)))))

(printf "\n;; Total Nodes: ~a.  Mean nodes per layer: ~a\n" N nodes-per-layer)

;; On average, how many of the nodes in the next layer does our node connect to.
(define fraction-down-connect 0.3)

(define (layer-size n)
  (min n (max 1 (exact (floor (+ nodes-per-layer
				 (* 0.5 (gaussian) nodes-per-layer)))))))

(define init-count (layer-size N))
(define init-layer (list-head node-names init-count))

(define (list-random-subset ls n)
  (let loop ([n n] [rem ls] [len (length ls)])
    (if (zero? n) '()
	(let ((item (list-ref rem (random len))))
	  (cons item
		(loop (sub1 n) 
		      (remq item ls)
		      (sub1 len)))))))

(let loop ((i 0) (n (- N init-count)) 
	   (last-layer init-layer) 
	   (remain (list-tail node-names init-count)))
  (define count (layer-size n))
  (define this-layer (list-head remain count))
  (if (zero? n)
      (printf ";; Last layer: ~a\n" last-layer)
    (begin
      (printf "\n;; Layer ~a : ~a nodes \n" i count)
      
      ;; Build connections from the previous layer:

      (let inner ([ls this-layer] [unhit last-layer])
	(define lastlen (length last-layer))
	(define num-links 
	  (min lastlen
	       (max 1 (exact (round (* fraction-down-connect lastlen
				       (add1 (gaussian))))))))
	(define myups 
	  (list-random-subset 
	   (append unhit  ;; Prioritize making sure that everyone in the previous layer is hit at least once:
		   (list-head last-layer 
			      ;(max 0 (- num-links (length unhit)))
			      (max 0 (- lastlen (length unhit)))
			      ))
	   num-links))
	;; Link a random subset of the previous layer to us.
	(for-each (lambda (up)
		    (printf "  link ~a bw ~a lat ~a <-> ~a\n"
			    up 100 3 (car ls) ))
	  myups)
	(unless (null? (cdr ls))
	  (inner (cdr ls) (difference unhit myups))))

      (loop (add1 i) (- n count) 
	    this-layer (list-tail remain count)))))


;; Now we generate "Layers" until we run out of nodes.


;; This makes a kind of latice topology:

;(pretty-print node-names)


; network n1 n2 n3 
; node n1 cpu 55
; node n2 cpu 56
; node n3 cpu 57
; link n1 bw 210 lat 9  <-> n2
; link n2 bw 240 lat 11 <-> n3

