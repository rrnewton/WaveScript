
(chez:module hashtab (make-default-hash-table (hashtab-get immediate?) (hashtab-set! immediate?) hashtab-for-each hashtab-remove!)
	
	;; This is implementation specific, these are the types for which eq? <=> equal?
	(define (immediate? x)
	  (or (fixnum? x) 
					; (flonum? x) ;; Floats don't follow eq? properly.
					; (char? x)   ;; Chars don't follow eq? properly.
	      (symbol? x)
	      (null? x)
	      (boolean? x)
	      ))

	; =========================================  
	;; This defines a *simple* and unified interface into hash-tables.
	;; It assumes *equal?* type key equivalence!
	;; It returns #f for a failed hashtab-get (which is sloppy, but that's slib)

	;; First we require hash-tables from slib:
	#;
	(begin
	(define ___ (require 'hash-table))
	(define (make-default-hash-table) (make-hash-table 5)) ;50))
	(define hashtab-get (hash-inquirer equal?))
	(define hashtab-set! (hash-associator equal?))
	(define hashtab-for-each hash-for-each)
	(define hashtab-remove! (hash-remover equal?)))
	
	;; [2005.10.18]
	;; Switching this to chez's native hash tables rather than slib's:
	;; NOTE: Chez does *not* support 'equal?' style indexing.
	(begin
	  (define make-default-hash-table #%make-hash-table)
	  (define hashtab-remove! #%remove-hash-table!)
	  (define (hashtab-for-each f ht) (#%hash-table-for-each ht f))
	  (IFDEBUG  
	   (begin
	     (define (hashtab-get ht k)
	       ;; [2006.03.01] Allowing non-immediate indices, because
	       ;; I want to use hash tables in this way now.  Both
	       ;; Chez and PLT support eq? style indexing behavior.

	       ;(if (not (immediate? k)) (error 'hashtab-get "this key is not an atom: ~s" k))
	       (#%get-hash-table ht k #f))
	     (define (hashtab-set! ht k v)
	       ;(if (not (immediate? k)) (error 'hashtab-set! "this key is not an atom: ~s" k))
	       (#%put-hash-table! ht k v)))
	   (begin 
	     (define (hashtab-get ht k) (#%get-hash-table ht k #f))
	     (define hashtab-set! #%put-hash-table!)))
	  )	      
	)
