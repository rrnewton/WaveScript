;; RRN: I lifted this from SLIB.

(chez:module topsort-module (tsort topological-sort test-this these-tests test-tsort cyclic?)

	(import scheme)
	(import (except helpers test-this these-tests))
	
	(include "generic/util/tsort.ss")
)

;(require tsort)
;(tsort '((a b) (b c) (c a)
;	     (d e) (e f))
;           eq?)

;(define x (make-hash-table))
;(define put hash-table-put!)
;(define get hash-table-get)

