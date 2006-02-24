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


;; [2006.02.23] <br>
;; This is a new version which makes records non-generative so that
;; they can be marshalled to and from files without binary
;; incompatibilities.. <br><br>
#;(define-syntax (reg:define-struct x)
  (define (uniquify-symbol x) (read (open-input-string (format "#{~s ~:*~s}" x))))
  (syntax-case x ()
    [(_ (name fields ...))
     ;; Switch between these to use define-structure instead if you like:

#;     (with-syntax ([newname (datum->syntax-object #'_(uniquify-symbol (datum name)))]
		   [genned (datum->syntax-object #'_ (gensym))])
       #'(begin (define-record newname (fields ...))
		;; Also make it read with the plain name.  This does waste space in the symbol table.
		(define genned (record-reader 'name (type-descriptor newname)))
		))

     #'(define-structure (name fields ...))
     ]))

;; Uses introspection to make a record spill its guts.
(define (reg:struct->list x)
  (let ((type (#%record-type-descriptor x)))
    (map (lambda (name)
	   ((#%record-field-accessor type name) x))
      (#%record-type-field-names type))))


(include "generic/constants.ss")
