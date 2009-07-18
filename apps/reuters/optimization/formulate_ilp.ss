#! /bin/bash
#|
exec regiment.chez i --script $0 $*
;exec regiment.plt i --script $0 $*
|#

;exec chez74 --script $0 $*

(define (read-qopt fn)
 (filter (compose not null?)
      (file->linelists fn)))

(define raw (read-qopt "test.qopt"))

;;==============================================================================

(define latencies (make-eq-hashtable))
;(define edge-bw   (make-eq-hashtable))

;; An edge is a pair (src . dst), and each edge has a bandwidth.
(define edge-bw   (make-hashtable equal-hash equal?))


;; This is used simply as a set to keep track of all links (without
;; duplicate entries for the reverse links).
(define links-tbl (make-hashtable equal-hash equal?))

;; This keeps track of the canonical name for each link.
(define links-canonical (make-hashtable equal-hash equal?))

;; Links are undirected.  We make entries for (a . b) and (b . a)
;; We could (should?) treat separate directions independently.
(define link-bw   (make-hashtable equal-hash equal?)) 
(define link-lat  (make-hashtable equal-hash equal?)) 

(define op-cpu     (make-eq-hashtable))
(define op-pin     (make-eq-hashtable))
(define op-query   (make-eq-hashtable))

(define node-pinned  (make-eq-hashtable))
(define node-cpu     (make-eq-hashtable))

(define (filt sym)
  (map cdr 
    (filter (lambda (l) (eq? sym (car l)))
      raw)))
(define queries  
  (map car (filt 'query)))

;;==============================================================================

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
	   (define pr1 (cons src dst))
	   (define pr2 (cons dst src))

	   (unless (hashtable-contains? links-tbl pr2)	     
	     (begin
	       (hashtable-set! links-tbl pr1 #t)
	       (hashtable-set! links-canonical pr1 pr1)
	       (hashtable-set! links-canonical pr2 pr1)
	       ))

	   (hashtable-set! link-bw  pr1 band)
	   (hashtable-set! link-bw  pr2 band)
	   (hashtable-set! link-lat pr1 latency)
	   (hashtable-set! link-lat pr2 latency)
	   ]

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

;;========================================

(define (cartesian-product a* b*)
  (apply append
   (map (lambda (a)
	  (map (lambda (b) (list a b)) b*))
     a*)))


(define (map-distinct-pairs f ls)
  (let loop1 ((l1 ls))
    (if (null? l1) '()
	(let loop2 ((l2 (cdr l1)))
	  (if (null? l2) 
	      (loop1 (cdr l1))
	      (cons (f (car l1) (car l2))
		    (loop2 (cdr l2))))
	  ))))

;; Todo: plug in a shortest path algorithm:
;; The Floyd-Warshall algorithm (dynamic programming).
;; Input a directed graph represented as a list ((src dst1 dst2 ...) ...).
;; Returns: a function that unpacks the shortest path for a src/dst pair.
(define (shortest-paths graph)
  (define N (length graph))    
  (define paths (make-vector (fx* N N) #f)) ;; A matrix encoding paths.  
  (define cost  (make-vector (fx* N N) #f)) ;; A corresponding matrix with path costs (hops).
  (define (get m x y)     (vector-ref  m (fx+ (fx* x N) y)))
  (define (set m x y val) (vector-set! m (fx+ (fx* x N) y) val))
  
  (define inf (greatest-fixnum)) ;; Positive infinity (approximately)
  
  (define indices (make-eq-hashtable N)) ;; Reencode the node names as numbers.
  (define revinds (make-eq-hashtable N)) ;; Remember the reverse mapping to decode.
  (define table   (make-vector N #f))    ;; Reencode the graph as a vector.
  (define (do-pos x y)
    (or (get cost x y)
	;; Consider posible neighbors:
	(let loop ([ls (vector-ref table x)] 
		   [bestnode #f]
		   [bestcost inf])
	  (if (null? ls)
	      ;; Note, might set the cost to "inf":
	      (begin (set paths x y bestnode)
		     (set cost  x y bestcost)
		     bestcost)
	      (let ([result (do-pos (car ls) y)])
		(if (= result inf)
		    (loop (cdr ls) bestnode bestcost)
		    (let ([thiscost (fx+ 1 result)])
		      (if (fx< thiscost bestcost)
			  (loop (cdr ls) (car ls) thiscost)
			  (begin
			    #;
			    (printf "       Had another path, not worth it: ~a ~a, I can do it for ~a through ~a\n"
				    (hashtable-ref revinds (car ls) 'huh) thiscost bestcost
				    (hashtable-ref revinds bestnode 'huh))
			    (loop (cdr ls) bestnode bestcost)
			    )))))))))


  ;; Map node names onto indices:
  ;; ASSUMES: that there is an entry for every node.
  (for-eachi (lambda (i ls)
	       (hashtable-set! indices (car ls) i)
	       (hashtable-set! revinds i (car ls)))
	     graph)

  ;; Reencode as numeric indices:
  (let ([encoded (map (lambda (ls) 
			(map (lambda (o) (ASSERT (hashtable-ref indices o #f)))
			  ls))
		   graph)])

  ;; Populate the table, and put in initial edges:
  (for-each (lambda (ls) 
	      (define hd (car ls))
	      (vector-set! table hd (cdr ls))
	      (for-each (lambda (dst) 
			  (set paths hd dst dst) ;; Connect directly
			  (set cost  hd dst 1))
		(cdr ls))
	      )
    encoded)
  
  ;; Now do a "tug" on each position, to make sure they're all computed:
  ;; Deactivate this to do it lazily:
  (if #f 
      (for x = 0 to (sub1 N)
	   (for y = 0 to (sub1 N)
		(do-pos x y))))
  
  ;(inspect paths)

  ;; Return a function that can read out the shortest path for a src/dst pair:
  (lambda (src dst) 
    (define _s (ASSERT (hashtable-ref indices src #f)))
    (define _d (ASSERT (hashtable-ref indices dst #f)))
    ;; Do a "pull" on that position:
    (do-pos _s _d)
    (let read ([n _s])
      (if (not n) #f
	  (if (fx= n _d) ;; Have we arrived?
	      (list dst)
	      (let ([tail (read (get paths n _d))])
		(if tail
		    (cons (hashtable-ref revinds n #f) tail)
		    #f))	      
	      ))))
  ))

(define f
  (shortest-paths 
  '([a b c]
    [b c]
    [c d]
    [d ]
;    [d e]
;    [e f]
;    [f g]
;    [g a]
    )))
;(pretty-print (f 'c 'b))
(printf "Computing path:\n")
(pretty-print (f 'a 'd))
(printf "Computing reverse path:\n")
(pretty-print (f 'd 'a))
(exit)


(define (unpack-path matrix src dst)
  99)



;; Ok, let's see if I can actually remember my scheme.
;; This produces output that uses certain "shorthands" like MIN.
(define (generate-constraints)

  ;(define edges (filt 'edge))
  ;(define links (filt 'link))  
  (define edges (vector->list (hashtable-keys edge-bw))) ;; All edges that have been registered
  (define links (vector->list (hashtable-keys links-tbl)))
  (define ops   (vector->list (hashtable-keys op-cpu))) 
  (define nodes (vector->list (hashtable-keys node-cpu)))

  (define (binary var) `((>= ,var 0) (<= ,var 1)))

  (define (assignvar op node)  (symbol-append 'assign_op '_ node))
  (define assignvars (map (curry apply assignvar) (cartesian-product ops nodes)))
  
  ;; Variables for the real bandwidth used on real edges:
  ;(define (edgevar   n1 n2)    (symbol-append 'edgebw_  n1 '_ n2))
  ;(define edgevars   (map (lambda (pr) (edgevar (car pr) (cdr pr))) edges))

  (define (linkvar   n1 n2)    (symbol-append 'linkbw_  n1 '_ n2))
  (define linkvars   (map (lambda (pr) (linkvar (car pr) (cdr pr))) links))


  (ASSERT (not (null? queries)))  
 
  (cons*
     
   ;; Assignment variables: binary variables representing the op/node assignment (quadratic)
   (list
    "Constrain the sum to 1 -- only can assign op to one node:"      
    `(= 1 (+ ,@assignvars))
    "Force assignment variables to be binary:" 
    (map binary assignvars))

   "Transitively close the graph by introducing virtual edges."
   "  There may be more than one virtual edge between nodes (different paths)."
   
   (map-distinct-pairs
    (lambda (a b) 
	  ;; For every pair of real nodes we introduce a virtual edge.		
	  (let ([var (symbol-append 'virtlinkbw_ a '_ b)]
		[canon (hashtable-ref links-canonical (cons a b) #f)])
	    ;; If a real edge exists:
	    (printf "Contains ~a\n" (cons a b))		   
	    (if canon
		`(= ,var ,(linkvar (car canon) (cdr canon)))
		;; Insert the sum of the edges on the shortest path:
		`(= ,var MAGIC)
		))) 
    (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) nodes))
   

   ;(list "Force edge vars to be binary:" (map binary edgevars))
   
   

   "Introduce variables for the bandwidth consumed by each query edge" 
   (map (lambda (quer) 
	  (define var (symbol-append 'SUMBW_ quer))
	  `(= ,var )
	  )
     queries)

   "Introduce path variables for every path through a query"
   
   "Define the latency of a query as its most latent path"

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

(printf "Links:\n")
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

