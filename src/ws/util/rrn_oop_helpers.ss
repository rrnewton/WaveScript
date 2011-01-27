
(module rrn_oop_helpers mzscheme
  (require "../../plt/common.ss")
  (provide make-class
	   class-name class-super class-fields class-methods class-methodtable class-field-count
	   make-object
	   ;object-methodtable
	   object-fieldvec object-class
	   lookup-class
	   sum-across-hierarchy
	   method-depth
	   field-depth
	   find-method-index
	   )
  (chezimports)

(reg:define-struct (class name super fields methods methodtable field-count)) 
(reg:define-struct (object methodtable fieldvec class))

(define (lookup-class name table)
  (cond
   [(null? table) #f]
   [(eq? name (class-name (car table))) (car table)]
   [else (lookup-class name (cdr table))]))
(define (sum-across-hierarchy accessor)
  (lambda (name table)
    (let loop ([class (lookup-class name table)])
      (if class 
	  (+ (length (accessor class))
	     (loop (class-super class)))
	  0))))
(define method-depth (sum-across-hierarchy class-methods))
(define field-depth  (sum-across-hierarchy class-fields))
(define (find-method-index name table methname)
  (let loop ([class (lookup-class name table)])
    (if class 
	(let ([ind (list-find-position methname (class-methods class))])
	  (if ind 	      
	      (+ ind (- (vector-length (class-methodtable class))  (length (class-methods class))))
	      (loop (class-super class))))
	#f)))

) ;; End module
