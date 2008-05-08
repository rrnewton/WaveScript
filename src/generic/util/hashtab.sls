#!r6rs

;;;; [2008.04.24] R6RS has hash tables.  But WS is already
;;;; standardized across a particular interface, so here we wrap R6RS
;;;; hash tables.

(library (ws util hashtab)
 (export make-default-hash-table 
	 hashtab?
	 hashtab-get  
	 hashtab-set! 
	 hashtab-for-each hashtab-remove!)
 (import (rnrs)) 

   (define (void) (if #f #t))

   ;; Equal? based by default:
   ;(define (make-default-hash-table . _) (make-hashtable equal-hash equal?))
   ;; Eq? based by default:
   (define make-default-hash-table (case-lambda [() (make-eq-hashtable)] [(n) (make-eq-hashtable n)]))

   (begin 
     (define hashtab? hashtable?)
     (define hashtab-remove! hashtable-delete!)
     (define (hashtab-for-each f ht) 
       (call-with-values (lambda () (hashtable-entries ht))
	 (lambda (keys vals)
	   (define bound (vector-length keys))
	   (let loop ([i 0])
	     (if (fx=? i bound) (void)
		 (begin 
		   (f (vector-ref keys i) (vector-ref vals i))
		   (loop (fx+ 1 i))))))))
     (define (hashtab-get ht k) (hashtable-ref ht k #f))
     (define hashtab-set! hashtable-set!))
  

 )  ;; End library
