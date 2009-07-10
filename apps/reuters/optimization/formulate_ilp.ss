#! /bin/bash
#|
exec regiment.plt i --script $0 $*
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

;; Edge names:
;; A table of tables:


(define (read-qopt fn)
 (filter (compose not null?)
      (file->linelists fn)))

;; Loads the meta-data into global variables and returns network graph
;; and a list of query graphs.
(define (load-globals fn)
  (define qgraphs '())
  (define netgraph '())
  (define raw (read-qopt fn))
  (define (filt sym)
    (map cdr 
      (filter (lambda (l) (eq? sym (car l)))
	raw)))
  ;; This maps nodes back to the queries that they are part of.
  (define reverse-lookup (make-eq-hashtable))
  (for-each
      (match-lambda ((,name . ,opnames))
	(for-each (lambda (op) (hashtable-set! reverse-lookup op name))
	  opnames))
      (filt 'query))
  (for-each 
      (lambda (entry)
	(void)
	(match entry
	  ;; An edge can have multiple targets (hypergraph):
	  [(edge ,src ,bw  -> ,dst* ...)
	   (for-each (lambda (dst) (hashtable-set! edge-bw (cons src dst) bw)) 
	     dst*)]

	  [(pin ,op ,node)	   (void)]
	  [(op ,name ,cpucost) (void)]

	  [(node ,name ,cpucap) (void)]

	  
	  [(link ,src ,band ,latency -> ,dst)
	   (define pr (cons src dst))
	   (hashtable-set! link-bw  pr band)
	   (hashtable-set! link-lat pr latency)]

	  ;; Ignored, handled above:
	  [(query ,_ ...) (void)]	  

	  [(network ,_ ...) (void)]

	  ))
    raw)

  
  )


;;================================================================================

(pretty-print (read-qopt "test.qopt"))

(load-globals "test.qopt")

;(pretty-print )

(pretty-print (values->list (hashtable-entries edge-bw)))

(pretty-print (values->list (hashtable-entries link-bw)))
(pretty-print (values->list (hashtable-entries link-lat)))



(printf "Done\n")

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