;;;; constants.ss
;;;; These are the chez specific constant/datatype definitions.
;======================================================================


;; Could be define-structure or define-record.   <br> 
;; This defines the record-representation used throughout the regiment code.
(define-syntax reg:define-struct
  (syntax-rules ()
    [(_ (name field ...))  (begin (define-record name (field ...))
				  (define ___ (record-reader 'name (type-descriptor name))))]))
;   [(_ (name field ...))  (define-structure (name field ...))]))

;; Uses introspection to make a record spill its guts.
(define (reg:struct->list x)
  (let ((type (#%record-type-descriptor x)))
    (map (lambda (name)
	   ((#%record-field-accessor type name) x))
      (#%record-type-field-names type))))


(include "generic/constants.ss")
