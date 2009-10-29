#! /bin/bash
#|
# This works with either PLT or chez right now (ikarus doesn't have hashtables yet)
exec regiment.chez i --script $0 $*
exec regiment.plt i --script $0 $*
|#

;; One of: latency, bandwidth, bottleneck
;(define optimization-objective 'bandwidth)
(define optimization-objective 'latency)

(define (read-qopt prt)
  (let loop ((prt prt))
    (apply append 
	   (map (lambda (line) 
		  (match line
		    [(include ,file) (loop (open-input-file file))]
		    [,else (list line)]))
	     (filter (compose not null?)
	       (port->linelists prt))))))

(define raw 
  (read-qopt 
   ;; A bunch of spurious arguments because of the way this is invoked (above):
   (if (= (length (command-line)) 6)
       (open-input-file (rac (command-line)))
       (begin 
	 (printf "// File argument not given, reading problem description from stdin...\n")
	 (current-input-port))
       ;;(error 'formulate_ilp "This script takes one argument: a .qopt file.")
       )))

;;==============================================================================

(define latencies (make-eq-hashtable))

;; An edge is a pair (src . dst), and each edge has a bandwidth.
(define edge-bw   (make-hashtable equal-hash equal?))

;; This is used simply as a set to keep track of all links (without
;; duplicate entries for the reverse links).
(define links-tbl (make-hashtable equal-hash equal?))

;; This one, on the other hand, has duplicate entries for reverse links:
;; Representing an undirected graph is a little tricky.
;;   Do we use the key (a . b) or (b . a) to reach into the hash table?
;;   This maps each link (pair) to its canonical form.
(define links-canonical (make-hashtable equal-hash equal?))

;; Links are undirected.  We make entries for (a . b) and (b . a)
;; We could (should?) treat separate directions independently.
(define link-bw   (make-hashtable equal-hash equal?)) 
(define link-lat  (make-hashtable equal-hash equal?)) 

;; Should probably refactor this data to store a record per-op:
(define op-cpu      (make-eq-hashtable))
(define op-pin      (make-eq-hashtable))
(define op-query    (make-eq-hashtable))
(define op-neighbors (make-eq-hashtable)) ;; (Outgoing) neighbors
(define op-incoming  (make-eq-hashtable)) ;; The reverse.

(define node-pinned  (make-eq-hashtable))
(define node-cpu     (make-eq-hashtable))

;; A table mapping a node to its neighbors.
;; This is another way of encoding the graph:
(define node-neighbors (make-eq-hashtable))


(define (filt sym)
  (map cdr 
    (filter (lambda (l) (eq? sym (car l)))
      raw)))
(define queries  (map car (filt 'query)))
(define query->ops (make-eq-hashtable))


;;========================================
;; HELPERS:

;(define (map-append fn ls) (apply append (map fn ls)))
(define (map-append fn ls)
  (let loop ([ls ls])
    (if (null? ls) '()
	(append (fn (car ls)) (loop (cdr ls))))))

(define (hashtable-cons! tab k v)
  (hashtable-set! tab k (cons v (hashtable-ref tab k '()))))

(define (sliding-window win advance ls)
  (define N (length ls))
  (let loop ([n N] [ls ls])
    (if (fx< n win) '()
	(cons (list-head ls win)
	      (if (fx> n advance)
		    (loop (fx- n advance)
			  (list-tail ls advance))
		    '())))))

(define (map-path-links fn path)
  (map (lambda (x) (fn (car x) (cadr x)))
    (sliding-window 2 1 path)))


(define (cartesian-product a* b*)
  (apply append
   (map (lambda (a)
	  (map (lambda (b) (list a b)) b*))
     a*)))


;; Cartesian product:
(define (map-all-pairs f ls)
  (define acc '())
  (for-each (lambda (x) 
	      (for-each (lambda (y) (set! acc (cons (f x y) acc)))
		ls))
    ls)
  (reverse! acc))

;; Selects all distinct pairings (discounting symmetry and selecting with removal)
(define (map-distinct-pairs f ls)
  (let loop1 ((l1 ls))
    (if (null? l1) '()
	(let loop2 ((l2 (cdr l1)))
	  (if (null? l2) 
	      (loop1 (cdr l1))
	      (cons (f (car l1) (car l2))
		    (loop2 (cdr l2))))
	  ))))


;(define (path-not-found a b) #f)
(define (path-not-found a b) (error 'shortest-path "Disconnected graph.  Path not found between ~a and ~a" a b))

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
  (define inf     (greatest-fixnum))     ;; Positive infinity (approximately)
  (define indices (make-eq-hashtable N)) ;; Reencode the node names as numbers.
  (define revinds (make-eq-hashtable N)) ;; Remember the reverse mapping to decode.
  (define table   (make-vector N #f))    ;; Reencode the graph as a vector.
  (define (do-pos x y)
    (let outer ((x x)
		(y y)
		(seen '()))
    (define (not-seen? n) (not (memq n seen)))
;     (printf "outer ~a ~a ~a\n" (hashtable-ref revinds x #f) (hashtable-ref revinds y #f)
; 	    (map (lambda (x) (hashtable-ref revinds x #f)) seen))
;     (when (equal? (get cost x y) inf) (printf "   UMM, we already 'know' the cost but it's: ~a\n" inf))
    
    ;; Before there was a bug where we'd search for a path in and pollute the cost matrix with inf's:
    ;; Now we need to recognize that an 'inf' doesn't meen that we're done.
    (let ([cur (get cost x y)])
     (if ;cur
         (and cur (not (fx= cur inf)))
	 cur
	;; Consider posible neighbors:
	(let loop ([ls (filter not-seen? (vector-ref table x))]
		   [bestnode #f]
		   [bestcost inf])
	  ;(printf "   LOOP  ~s ~s ~s\n" ls bestnode bestcost)
	  (if (null? ls)
	      ;; If we've considered all neighbors we must have the best.
	      ;; Note, might set the cost to "inf":
	      (begin (set paths x y bestnode)
		     (set cost  x y bestcost)
		     bestcost)
	      ;; Consider the path from a neighbor to the destination.
	      (let ([result (outer (car ls) y (cons (car ls) seen))])
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
			    )))))))))))
  ;; Map node names onto indices:
  ;; ASSUMES: that there is an entry for every node.
  (for-eachi (lambda (i ls)
	       (hashtable-set! indices (car ls) i)
	       (hashtable-set! revinds i (car ls)))
	     graph)
  ;; This version does not make that assumption:
 ;(let loop ([i 0]))

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
		(cdr ls)))
    encoded)
  ;; Now do a "tug" on each position, to make sure they're all computed:
  ;; Deactivate this to do it lazily:
  (if #f (for x = 0 to (sub1 N)
	      (for y = 0 to (sub1 N)
		   (do-pos x y))))
    
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
		    #f))))))))


(define f
  (shortest-paths 
  '([a b c]
    [b c]
    [c d]
    [d e]
    [e f]
    [f g]
    [g a]
    )))
;(printf "Computing path: ") (pretty-print (f 'a 'd))
;(printf "Computing reverse path:  ") (pretty-print (f 'd 'a))


;;==============================================================================

;; Loads the meta-data into global variables and returns network graph
;; and a list of query graphs.
(define (load-globals raw)
  ;; This maps nodes back to the queries that they are part of.
  ;(define reverse-lookup (make-eq-hashtable))
  (for-each 
      (lambda (entry)
	(void)
	(match entry
	  ;; An edge can have multiple targets (hypergraph):
	  [(edge ,src bw ,bw  -> ,dst* ...)
	   (hashtable-set! op-neighbors src 
			   (append dst* (hashtable-ref op-neighbors src '())))	  
	   (for-each (lambda (dst) 
		       (hashtable-set!  edge-bw (cons src dst) bw)
		       (hashtable-cons! op-incoming dst src))
	     dst*)]

	  [(pin ,op ,node)	   
	   (when (hashtable-contains? op-pin op)
	     (error 'load-globals "Corrupt data file, cannot pin this node more than once: ~s" op))
	   (hashtable-set! op-pin op node)
	   (hashtable-set! node-pinned node
			   (set-cons:list op (hashtable-ref node-pinned node '())))
	   ]
	  [(op ,name   cpu ,cpucost)   (hashtable-set! op-cpu name cpucost)]
	  [(node ,name cpu ,cpucap)    (hashtable-set! node-cpu name cpucap)]


	  ;; Links go both directions:
	  [(link ,src bw ,band lat ,latency <-> ,dst)
	   ;; There are two potential representations for this edge (A-B and B-A):
	   (define pr1 (cons src dst))
	   (define pr2 (cons dst src))
	   (if (hashtable-contains? links-tbl pr2)
	       (begin 
		 ;; Otherwise pr2 has already been entered as the canonical name.	
		 (void)
		 (error 'load-globals "I forgot when this would happen... ~a ~a" src dst)
		 )
	       (begin		 
		 ;; Register under the canonical "name", which is pr1:
		 (hashtable-set! links-tbl pr1 #t)
		 (hashtable-set! links-canonical pr1 pr1)
		 (hashtable-set! links-canonical pr2 pr1)
		 (hashtable-cons! node-neighbors src dst)
		 (hashtable-cons! node-neighbors dst src)
	       ))

	   ;; We set metadata 'under both names:
	   (hashtable-set! link-bw  pr1 band)
	   (hashtable-set! link-bw  pr2 band)
	   (hashtable-set! link-lat pr1 latency)
	   (hashtable-set! link-lat pr2 latency)]

	  ;; Ignored, handled above:
	  [(query ,name ,opnames ...)
	   (hashtable-set! query->ops name opnames)
	   (for-each (lambda (op) (hashtable-set! op-query op name))  opnames)]
	  ;; All the network names, not needed here (or ever really).
	  [(network ,name* ...) (void)]))
    raw))



;; Ok, let's see if I can actually remember my scheme.
;; This produces output that uses certain "shorthands" like MIN.
(define (generate-constraints)
  (define edges (vector->list (hashtable-keys edge-bw))) ;; All edges that have been registered
  (define links (vector->list (hashtable-keys links-tbl)))
  (define ops   (vector->list (hashtable-keys op-cpu))) 
  (define nodes (vector->list (hashtable-keys node-cpu)))

  ;; For tracking which virt edges pass through real ones. 
  ;; (Specifically it keeps track of the virtual bandwidth vars.)
  (define incident-virtuals   (make-hashtable equal-hash equal?))

  (define (binary var) `((>= ,var 0) (<= ,var 1)))

  ;; [2009.10.27] Hack, use double underscore to separate:
  (define (assignvar op node)  (symbol-append 'assign_ op '__ node))

  (define assignvars (map (lambda (a)
			    (map (lambda (b) (assignvar a b)) nodes))
		       ops))
  (define all_assignvars (apply append assignvars))
  
  ;; Variables for the real bandwidth used on real edges:
  ;(define (edgevar   n1 n2)    (symbol-append 'edgebw_  n1 '_ n2))
  ;(define edgevars   (map (lambda (pr) (edgevar (car pr) (cdr pr))) edges))

  (define (linkvar    n1 n2)    (symbol-append 'linkbw_  n1 '_ n2))
  ;(define (linklatvar n1 n2)    (symbol-append 'linklat_  n1 '_ n2))

  (define (virtlinklat-var a b)  (symbol-append 'virtlink_lat_ a '_ b))
  (define (virtlinkbw-var  a b)  (symbol-append 'virtlink_bw_ a '_ b))

  ;(define linkvars   (map (lambda (pr) (linkvar (car pr) (cdr pr))) links))
  (define (edgelatvar n1 n2)    (symbol-append 'edgelat_  n1 '_ n2))
  (define (querylatvar q)       (symbol-append 'querylatency_  q))

  ;; A (list) graph representation, directed edges (and we put them in both directions to simulate undirected).
  (define network-graph
    (let-values ([(keys vals) (hashtable-entries node-neighbors)])
      (vector->list (vector-map cons keys vals))))
  (define pathfinder (shortest-paths network-graph))

  ;; Sort them just for display purposes:   
  (define sorted-nodes (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) nodes))    

  (define virtuals ;; Compute virt latency constraints early to populate incident-virtuals table.
    (list 
     """Transitively close the graph by introducing virtual edges."
     "  We assume shortest path routing to establish virtual/physical correspondence." 
     "  Virtual edge latency is the sum of edges traversed." 
     (map-distinct-pairs
      (lambda (a b)
	;; For every pair of real nodes we introduce a virtual edge.		
	;; [2009.10.28] This is tricky... we introduce both forward and reverse variables for LATENCY.
	;; But currently we only need forward variables for BANDWIDTH.
	(let* ([var   (virtlinklat-var a b)]
	       [var2  (virtlinklat-var b a)]
	       [bwvar (virtlinkbw-var a b)]
	       [pair (cons a b)]
	       ;; Then convert to canonical form (if such an edge exists):
	       [canon (hashtable-ref links-canonical pair #f)])
	  ;; If a real edge exists:
	  (if canon
	      (begin 
		;; We introduce a virtual edge, but we make it synonymous with the real edge:
		;(hashtable-cons! incident-virtuals pair bwvar)
		(hashtable-cons! incident-virtuals canon bwvar)
		`((= ,var  ,(ASSERT (hashtable-ref link-lat canon #f)))
		  (= ,var2 ,(ASSERT (hashtable-ref link-lat canon #f)))))

	      ;; FIXME:
	      ;;; [2009.10.27] ??? Hmm... we don't have a canonical form for virtual edges, eh?
	      
	      ;; The sum of the edge latencies on the shortest path:
              (let ([path (pathfinder a b)])
		(eprintf "// TRAVERSING PATH... ~a \n" path)
		(if (not path) 
		    (path-not-found a b)
		    (let ([sum (apply + 
				      (map-path-links
				       (lambda (src dst)
					 ;; Register that this virtual edge brings load to each of these physical edge:
					 ;; FIXME:  I think this was wrong:
					  ;;; ?????????????????????????//
					  ;(hashtable-cons! incident-virtuals (cons dst src) bwvar)
					  ;;; ?????????????????????????//
					  (hashtable-cons! incident-virtuals (cons src dst) bwvar)
					  
					  (ASSERT (hashtable-ref link-lat (cons src dst) #f)))
					path))])		      
		    ;'() ;(error 'generate-constraints " could not find path between ~a and ~a\n" a b)
		      `((= ,var  ,sum)
			(= ,var2 ,sum)))
		    ))
	      ))
	)
      sorted-nodes)))

  (when (null? queries)
    (error 'generate-constraints
	   " cannot generate ILP problem.  There were no queries in the problem description."))
 
  (list
     
   ;; Assignment variables: binary variables representing the op/node assignment (quadratic)
   (list
    "Assignment: Constrain the sum to 1 -- only can assign op to one node:"      
    (map (lambda (ls) `(= 1 (+ ,@ls))) assignvars)
    " Force assignment variables to be binary:" 
    (map binary all_assignvars)
    """ Pin some ops to nodes."
    (map (lambda (op)
	   (define pin (hashtable-ref op-pin op #f))
	   (if pin
	       ;" please pin I mean..."
	       `(= 1 ,(assignvar op pin))
	       '())
	   )
      ops)
    )

   (list 
    """Constrain bandwidth at each node to be less than its maximum."
    (map (lambda (node)
	   (define cpu (ASSERT (hashtable-ref node-cpu node #f)))
	   `(>= ,cpu
		(+ ,@(map (lambda (op) 
			    `(* ,(ASSERT (hashtable-ref op-cpu op #f))
				,(assignvar op node)))
		       ops))))
      nodes))

   virtuals ;; stick them in:
      
   """The bandwidth consumed on each physical edge is the sum of all virtual edges passing through."
   (map (lambda (pr) 
	  (define incidents (hashtable-ref incident-virtuals pr '()))
	  ;; If there are no virtual edges, we use the real edge as a proxy:
	  (if (null? incidents) 
	      ;'()
	      ;; But that means there must NOT be virtual edge between these neighbors.
	      (error 'physical-bandwidth "double check this code path")

	      `(= ,(linkvar (car pr) (cdr pr)) 
		  (+ ,@incidents))))
     links)
   "And the bandwidth on each physical edge must be less than its physical limit."
   (map (lambda (pr) 	  
	  `(<= ,(linkvar (car pr) (cdr pr)) 
	       ,(ASSERT (hashtable-ref link-bw pr #f))))
     links)
       
   """Now begin to tie together the op assignments and the affected edges."
   (map-distinct-pairs
    (lambda (a b)
      `(= ,(virtlinkbw-var a b)
	  ;; If a query edge is broken, then its bw spills onto the virtual link:
	  (+ ,@(map (lambda (edge) 			     
		      `(+ (* ,(ASSERT (hashtable-ref edge-bw edge #f))
			     (AND ,(assignvar (car edge) a)
				  ,(assignvar (cdr edge) b)))
			  ;; And alas we could cross the link in either direction:
			  (* ,(ASSERT (hashtable-ref edge-bw edge #f))
			     (AND ,(assignvar (car edge) b)
				  ,(assignvar (cdr edge) a)))
			  ))
		 edges))))
    sorted-nodes)

   """ Ditto for edge latencies."
   (map (lambda (edge) 
	  `(= ,(edgelatvar (car edge) (cdr edge))
	      ;; For each edge in the query, the latency is a latency of whatever virtual edge we were assigned.
	      (+ ,@(map-all-pairs
		    (lambda (a b)
		      (if (eq? a b) 
			  ;; Being assigned to the same node doesn't incur latency:
			  0
			  (let () ;([canon (ASSERT (hashtable-ref links-canonical (cons a b) #f))])
			    ;; It is impossible for a query edge to span across disconnected parts of the network:
			    (if (pathfinder a b)
				`(* ;,(virtlinklat-var  (car canon) (cdr canon))
				  ,(virtlinklat-var  a b)
				    (AND ,(assignvar (car edge) a)
					 ,(assignvar (cdr edge) b)))			 
				0 ;; We can't actually fully support disconnected graphs yet.
			      ;(path-not-found a b)
				))
			  ))
		    sorted-nodes))))
     edges)
   
   
   ;; [2009.10.22] Ok... what were the other optimization metrics that
   ;; we were considering?  We talked about at least these:
   ;;
   ;;   (1) Minimizing bottleneck.  Minimize the breaking point if all
   ;;   data rates increase homogeneously.  This would be the worst of
   ;;   the most loaded CPU and the most loaded network link.
   ;;
   ;;   (2) Minimizing total network usage.  That is, minimiizing $$$
   ;;   spent on bandwidth.  
   ;;
   ;;   (3) And latency too... that's what I did first.
   ;; 
   (match optimization-objective
     [latency
      (list
       """The latency of a query is defined as the worst latency of any of its source->sink paths."   
       "  Currently we look at only the shortest path for each source/sink pair, not every path."
       "  We also only consider network latency and not varation in compute latency." 
       (map (lambda (query)
	      (define theseops (hashtable-ref query->ops query '()))	  
	      (define graph 
		(map (lambda (op)
		       (cons op (hashtable-ref op-neighbors op '())))
		  theseops))
	      ;; Based on the topology of the query we define sources as
	      ;; nodes with no incoming edges and sinks as those with no
	      ;; outgoing ones.
	      (define sinks
		(map car 
		  (filter (lambda (entry) (null? (cdr entry))) graph)))
	      (define sources
		(filter (lambda (op) (null? (hashtable-ref op-incoming op '())))
		  theseops))
	      (define pathfind (shortest-paths graph))
	      
	      `(= ,(querylatvar query)
		  (overMAX ; underMAX
		   ,@(map
			 (lambda (src) 
			   (map-append (lambda (sink) 
					 `(+ ,@(map-path-links edgelatvar (ASSERT (pathfind src sink)))))
				       sinks))
		       sources))))
	 queries)

       """Minimize sum of query latencies."
       `(OBJECTIVE (+ ,@(map querylatvar queries)))
       )]

     [bandwidth 
      ;; This one is easy: just the sum of bandwidth on real network links:
      (list """Minimize sum of network link bandwidth."
	    `(OBJECTIVE (+ ,@(map (lambda (pr) (linkvar (car pr) (cdr pr)))
			       links))))]

     [bottleneck 
      (list
       ;; Minimize the worst bottleneck.
       
       ;; FINISHME
       )]
     [,other (error 'generate-constraints "Do not recocognize optimzation objective: ~a" other)]
     )
   
   """Make vars integral:"  
   `(int . ,all_assignvars)

   ))


(define (flatten-constraints nested)  
  (reverse! ;; Cosmetic.
   (let loop ([x nested] [acc '()])
     (cond
      [(null? x) acc]
      [(pair? x) 
       (if (symbol? (car x))
	   (cons x acc)
	   (loop (cdr x)
		 (loop (car x) acc)))]
      [(string? x) (cons x acc)]
      [else (error 'flatten-constraints "Got an invalid object: ~s" x)]))))

;;================================================================================

;; Desugar to a normal ILP

;; This is what I came up with for AND:
; (AND a b) => c     | c <= a, c <= b, c > a+b-2

;; And from the NSDI paper:
; (XOR a b) => c + d | 0 <= a-b + c <= 1, 0 <= b-a + d <= 1, 
;;
;; WAIT, looks like that can allow c/d to be spuriously high, and
;; requires that the objective function minimize them.
;; Is there a tight solution??


;; We can define overMAX/underMAX and overMIN/underMIN,
;; But we can't define a true MIN or MAX.
; x = (overMAX a b) => x >= a, x >= b 

;; NOTE: This could do some simple inlining as well, but hopefully the
;; ILP solvers are smart enough that that wouldn't help. 
;; NOTE2: I did it anyway.  One consequence of this is you will not
;; see "assign_" variables printed for the PINNED nodes.
(define (desugar-constraints lst)
  (define inlines (make-eq-hashtable)) 
  (define (Expr e)
    (match e
      [,s (guard (symbol? s)) 
	  (values (hashtable-ref inlines s s) '())]
      [,n (guard (number? n)) (values n '())]
      [(,arith ,[e* c**] ...)
       (guard (memq arith '(+ - *)))
       ;; lp_solve is lazy... it won't even do a little arithmetic:
       (values (if (andmap number? e*)
		   (simple-eval `(,arith ,@e*))
		   ;; TODO: Make it a binary op:
		   `(,arith ,@e*))
	       (apply append! c**))]

      [(overMax ,rand)  (Expr rand)]

      [(overMAX ,[rand* c**] ...)
       (define var (unique-name "omax"))
       (values var
	       (append! (map (lambda (rand) 
			       `(>= ,var ,rand))
			  rand*)
			(apply append! c**)))]
      
      ;; POSSIBLE EXPR DUPLICATION
      [(AND ,[a c1*] ,[b c2*])
       (define var (unique-name "c"))
       (values var  
	       (append 
		`((<= ,var ,a)
		  (<= ,var ,b)
		  (> ,var (+ ,a ,b -2)))
		(append! c1* c2*))
	       
	       )]

      [(,other ,rand* ...)
       (guard (memq other '(underMAX overMIN underMIN XOR)))
       (error "desugar-constraints: ~a not implemented yet." other)]))
  
  ;; We first need to do a scan to fill up the table of inlinable vars:
  (for-each (lambda (cstrt)
	      (match cstrt
		[(= ,var ,num) (guard (symbol? var) (number? num))
		 (hashtable-set! inlines var num)]
		[(= ,num ,var) (guard (symbol? var) (number? num))
		 (hashtable-set! inlines var num)]	       		
		[,else (void)]))
    lst)

  ;; Now process all the constraints:
  (map (lambda (cnstrt)
	 (match cnstrt		
	   [(,op ,[Expr -> e1 c1*] ,[Expr -> e2 c2*])
	    (guard (memq op '(= < > <= >=)))
	    ;; Lame, lp_solve won't accept NUM = NUM constraints.
	    (if (and (number? e1) (number? e2))
		(begin (ASSERT (simple-eval `(,op ,e1 ,e2))) 
		       '() 
		       ;(list (format " ~a ~a ~a // <- suppressed" e1 op e2))
		       )
		(cons `(,op ,e1 ,e2)
		      (append c1* c2*)))]
	   [,str (guard (string? str)) str]
	   [(int ,v* ...) `(int . ,v*)]
	   [(OBJECTIVE ,[Expr -> e c*])
	    (append c* `((OBJECTIVE ,e)))]
	   ))
    lst)
  ) ;; End desugar

;; ============================================================

;; Output in a form suitable for an ilp solver
(define (print-ilp cstrts)
  (define (Expr e) 
    (match e
      [,s (guard (symbol? s)) (display s)]
      [,n (guard (number? n)) (display n)]

      [(,arith ,e* ...)       
       (guard (memq arith '(+ - * overMAX AND)))
       (let loop ([ls e*])
	 (if (null? (cdr ls))
	     (Expr (car ls))
	     (begin
	       (Expr (car ls))
	       (printf " ~a " arith)
	       (loop (cdr ls)))))]
      ))
  (printf "// Script-Generated ILP formulation: \n\n")
  (printf "// Objective Function:\n")
  ;; Print the objective function first:  
  (match (let loop ([ls cstrts])
	   (if (and (pair? (car ls)) (eq? (caar ls) 'OBJECTIVE))
	       (car ls) (loop (cdr ls))))
    [(OBJECTIVE ,e)
     (printf "min: ")
     (Expr e)
     (printf ";\n\n")])

  (for-each 
      (lambda (line)
	(match line
	  [,str (guard (string? str))
		(if (equal? str "")
		    (newline)
		    (printf "// ~a\n" str))
		]

	  [(int ,v* ...)
	   (printf "int ")
	   (for-each display (insert-between ", " v*))
	   (printf ";\n")]

	  [(,op ,e1 ,e2) (guard (memq op '(= < > <= >=)))
	   ;(if (eq? op '=) (newline))
	   (printf " ")
	   (Expr e1)
	   ;; VERY ODD: What are the semantics of "<" for lp_solve?
	   (case op
	     [(<) (printf " <= -1 + ")]
	     [(>) (printf " >=  1 + ")]
	     [else (printf " ~a " op)])
	   (Expr e2)
	   (printf ";\n")]
	  
	  [(OBJECTIVE ,_) 
	   (printf "// (See objective function above, moved to top of file.)\n")]
	  ))
    cstrts))


;;================================================================================



(unique-name-counter 0)

(load-globals raw)

; (hashtab-for-each (lambda (a b) (printf "  ~a ~a\n" a b)) 
; 		    links-tbl)
; (printf "\nNEIGHBORS:\n")
; (hashtab-for-each (lambda (a b) (printf "  ~a ~a\n" a b)) 
; 		  node-neighbors)
;  (printf "================================================================================\n\n\n")



; (printf "================================================================================\n\n\n")
; (pretty-print (flatten-constraints (generate-constraints)))

; (printf "================================================================================\n\n\n")
;(pretty-print (flatten-constraints (desugar-constraints (flatten-constraints (generate-constraints)))))

; (printf "================================================================================\n\n\n")

;(print-ilp (flatten-constraints (desugar-constraints (flatten-constraints (generate-constraints)))))


(printf "// About to start generating....\n")(flush-output-port (current-output-port))
(define a (generate-constraints))
(printf "// GENERATED CONSTRAINTS...\n")(flush-output-port (current-output-port))

(if (file-exists? "intermediate.ss")
    (delete-file  "intermediate.ss"))
(with-output-to-file "intermediate.ss"
  (lambda ()
    (pretty-print a)
    ))

(define b (flatten-constraints a))
(define c (desugar-constraints b))
(define d (flatten-constraints c))

;(print-ilp b)
(print-ilp d)


;(inspect y)(inspect x)
;(inspect (desugar-constraints x))
;(print-ilp (flatten-constraints (desugar-constraints x)))
;

