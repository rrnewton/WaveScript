;; This is just an simple, agreed upon interface between the PLT and Chez platforms.

;; [2005.10.27]  Just factored this out of helpers.ss

(module hashtab mzscheme
	(provide make-default-hash-table hashtab-get hashtab-set! hashtab-for-each hashtab-remove!)
		 
  (define (make-default-hash-table) (make-hash-table))
  (define (hashtab-get t s) (hash-table-get t s (lambda () #f)))
  (define hashtab-set! hash-table-put!)
  (define (hashtab-for-each f h) (hash-table-for-each h f))
  (define hashtab-remove!  hash-table-remove!)
  
  )
