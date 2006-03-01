;;;; constants.ss
;;;; These are the chez specific constant/datatype definitions.
;======================================================================

;; Could be define-structure or define-record.   <br> 
;; This defines the record-representation used throughout the regiment code. <br>
;; UNCOMMENT ONLY ONE OF THESE.

;======================================================================

(chez:module chez_constants (reg:define-struct reg:struct? reg:struct->list)
  ;(import scheme)

  ; Defined using RECORDS:
  ; ======================================================================
  ;; This version uses generative records and should work if you want to use it.
  (define-syntax reg:define-struct
    (syntax-rules ()
      [(_ (name field ...))
       (begin (define-record name (field ...))
	      ;; Allows the reader to read in regiment records.  System should work without this:
	      (define reg:struct-dummy-val (record-reader 'name (type-descriptor name)))
	      )]))

  ;; [2006.02.23] <br>
  ;; This is a new version which makes records non-generative so that
  ;; they can be marshalled to and from files without binary
  ;; incompatibilities.. <br><br>
  ;;   [2006.02.24] <br>
  ;; This works but it is too harsh a restriction to require each name
  ;; to only be defined once.  What if I wish to run too simulations
  ;; in a row!
  ;;   Instead my approach will be, if I really need to unmarshal
  ;; records, I will convert them upon reading to the newer records by
  ;; read/writing them to a string buffer!!
  #;
  (define-syntax (reg:define-struct x)
    (define (uniquify-symbol x) (read (open-input-string (format "#{~s ~:*~s}" x))))
    (syntax-case x ()
		 [(_ (name fields ...))
		  ;; Switch between these to use define-structure instead if you like:		  
		  (with-syntax ([newname (datum->syntax-object #'_(uniquify-symbol (datum name)))]
				;[genned (datum->syntax-object #'_ (gensym))]
				)
		    #'(begin (define-record newname (fields ...))
			     ;; Also make it read with the plain name.  
			     (define reg:struct-dummy-val (record-reader 'name (type-descriptor newname)))
			     ))
		  ]))

  (define reg:struct? record?)
  ;; Uses introspection to make a record spill its guts.
  (define (reg:struct->list x)
    (let ((type (#%record-type-descriptor x)))
      (map (lambda (name)
	     ((#%record-field-accessor type name) x))
	(#%record-type-field-names type))))

  ;; Defined using 
  ;;======================================================================
  #|
  (begin ;; This begin-block defines structs's as chez structures (vectors).
    (define reg:define-struct_name-table (make-hash-table))
    (define-syntax reg:define-struct
      (syntax-rules ()
	[(_ (name field ...))  
	 (begin (define-structure (name field ...))
	      (define reg:struct-dummy-val 
		(put-hash-table! reg:define-struct_name-table 'name (void))))]))
    (define reg:struct? 
      (let ([lookup-failure (gensym)])
	(lambda (x)
	  (and (vector? x) 
	       (>= (vector-length x) 1)
	       (not (eq? (get-hash-table reg:define-struct_name-table 
					 (vector-ref x 0) lookup-failure)
			 lookup-failure))))))
    (define (reg:struct->list x) (cdr (vector->list x)))
    |#

) ;; End module

(import chez_constants)

(include "generic/constants.ss")
