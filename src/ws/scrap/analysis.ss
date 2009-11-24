
;; Contiguity type:

;; <density> := bottom
;;           | (contig-area ,diam ,area <density>)
;;           | (sparse-area ,diam ,area <density>)
;;           | top
;;           | localval

;;           | (area <contig>)
;;           | (contig <diam> <area>)

;; Diameter and Area:
;; (<diam> <area>)
;; <diam> = bottom | <int> | wholediam | top
;; <area> = bottom | <int> | wholearea | top
;; (3 5)
;; (3 bottom)
;; (wholediam wholearea)

;; Approximation (node in the lattice)::
;; (<density-type> <diam+2area>)

(define transfer
  (lambda (op args type)
    (match (list fun args type)
       [(rrcluster ,args ,denstype)
	`[(contig-area ,denstype) (,diam ,area)]]
       [(sparsify ,args 
		  ((,sparse-or-contig ,childtype) 
		   (diam ,area)))
	`[(sparse-area ,childtype) (,diam ,area)]]

       [,other (error 'transfer "can't handle: ~s ~s ~s" op argsg type)])))


;(define cost
;  (lambda (annotated-prog)
;    (match type
;       [localval 1]
;       [


(define-testing test-this (default-unit-tester
		    "Analysis Pass: determine continuity"
		    these-tests))

