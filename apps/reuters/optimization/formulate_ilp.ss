#! /bin/bash
#|
exec regiment.chez i --script $0 $*
;exec regiment.plt i --script $0 $*
|#

;exec chez74 --script $0 $*

;(define )
(printf "woot2\n")


(define latencies (make-eq-hashtable))
;(define edge-bw   (make-eq-hashtable))

;; An edge is a pair (src . dst), and each edge has a bandwidth.
(define edge-bw   (make-hashtable equal-hash equal?))
(define link-bw   (make-hashtable equal-hash equal?)) ;; Same here
(define link-lat  (make-hashtable equal-hash equal?)) ;; and here

(define op-cpu     (make-eq-hashtable))
(define op-pin     (make-eq-hashtable))
(define op-query   (make-eq-hashtable))

(define node-pinned  (make-eq-hashtable))
(define node-cpu     (make-eq-hashtable))

(define (read-qopt fn)
 (filter (compose not null?)
      (file->linelists fn)))

(define raw (read-qopt "test.qopt"))

(define (filt sym)
  (map cdr 
    (filter (lambda (l) (eq? sym (car l)))
      raw)))

(define queries  
  (map car (filt 'query)))


;; Loads the meta-data into global variables and returns network graph
;; and a list of query graphs.
(define (load-globals raw)
  (define qgraphs '())
  (define netgraph '())
  ;; This maps nodes back to the queries that they are part of.
  ;(define reverse-lookup (make-eq-hashtable))
  (for-each 
      (lambda (entry)
	(void)
	(match entry
	  ;; An edge can have multiple targets (hypergraph):
	  [(edge ,src ,bw  -> ,dst* ...)
	   (for-each (lambda (dst) (hashtable-set! edge-bw (cons src dst) bw)) 
	     dst*)]

	  [(pin ,op ,node)	   
	   (when (hashtable-contains? op-pin op)
	     (error 'load-globals "Corrupt data file, cannot pin this node more than once: ~s" op))
	   (hashtable-set! op-pin op node)
	   (hashtable-set! node-pinned node
			   (set-cons:list op (hashtable-ref node-pinned node '())))]

	  [(op ,name ,cpucost)     (hashtable-set! op-cpu name cpucost)]
	  [(node ,name ,cpucap)    (hashtable-set! node-cpu name cpucap)]

	  
	  [(link ,src ,band ,latency -> ,dst)
	   (define pr (cons src dst))
	   (hashtable-set! link-bw  pr band)
	   (hashtable-set! link-lat pr latency)]

	  ;; Ignored, handled above:
	  [(query ,name ,opnames ...)
	   ;(printf "GOT QUERY ~a ~s  ~a \n" name queries (box? queries ))
	   ;(set-box! queries (cons name (unbox queries)))
	   (for-each (lambda (op) (hashtable-set! op-query op name))  opnames)
	   ;(printf "queries ~a\n" queries)
	   ]
	  ;; All the network names, not needed here (or ever really).
	  [(network ,name* ...) (void)]

	  ))
    raw)
  )


(define (cartesian-product a* b*)
  (apply append
   (map (lambda (a)
	  (map (lambda (b) (list a b)) b*))
     a*)))

;; Ok, let's see if I can actually remember my scheme.
;; This produces output that uses certain "shorthands" like MIN.
(define (generate-constraints)

  ;(define edges (filt 'edge))
  ;(define links (filt 'link))  
  (define edges (vector->list (hashtable-keys edge-bw))) ;; All edges that have been registered
  (define links (vector->list (hashtable-keys link-bw)))
  (define ops   (vector->list (hashtable-keys op-cpu))) 
  (define nodes (vector->list (hashtable-keys node-cpu)))

  (define (edgevar   n1 n2)    (symbol-append 'edge_  n1 '_ n2))
  (define (assignvar op node)  (symbol-append 'assign_op '_ node))

  (define (binary var) `((>= ,var 0) (<= ,var 1)))

  (define assignvars (map (curry apply assignvar) (cartesian-product ops nodes)))
  (define edgevars   (map (lambda (pr) (edgevar (car pr) (cdr pr))) edges))

  (ASSERT (not (null? queries)))  
 
  (cons*
     
   ;; Assignment variables: binary variables representing the op/node assignment (quadratic)
   (list
    "Constrain the sum to 1 -- only can assign op to one node:"      
    `(= 1 (+ ,@assignvars))
    "Force assignment variables to be binary:" 
    (map binary assignvars))

   (list "Force edge vars to be binary:"
	 (map binary edgevars))

   "Introduce variables for the bandwidth consumed by each query edge" 
   (map (lambda (quer) 
	  (define var (symbol-append 'SUMBW_ quer))
	  `(= ,var )
	  )
     queries)

   ""

   "Objective function: Minimize latency for each query "
   
   (map 
      (lambda (entry)	
	(match entry
	  ;; An edge can have multiple targets (hypergraph):
	  [(edge ,src ,bw  -> ,dst* ...)
	   '()]

	  [(link ,src ,band ,latency -> ,dst)
	   '()]

	  [(pin ,op ,node)            '()]
	  [(op ,name ,cpucost)        '()]
	  [(node ,name ,cpucap)       '()]
	  [(query ,name ,opnames ...) '()]
	  [(network ,name* ...)       '()]
	  ))
    raw))
  )


;;================================================================================

(pretty-print raw)
(load-globals raw)

;(pretty-print )

(pretty-print (values->list (hashtable-entries edge-bw)))

(pretty-print (values->list (hashtable-entries link-bw)))
(pretty-print (values->list (hashtable-entries link-lat)))

(newline)
(pretty-print (values->list (hashtable-entries op-cpu)))
(pretty-print (values->list (hashtable-entries node-cpu)))
(newline)
(pretty-print (values->list (hashtable-entries op-pin)))
(pretty-print (values->list (hashtable-entries node-pinned)))

(printf "queries ~a\n" queries)

(pretty-print (generate-constraints))


#;
(define (read-qopt fn)
  (define raw
    (filter (compose not null?)
      (file->linelists fn)))
  (define (filt sym)
    (map cdr 
      (filter (lambda (l) (eq? sym (car l)))
	raw)))
  (list (filt 'node)
	(filt 'query)
	(filt 'op)
	(filt 'edge)
	)
  )