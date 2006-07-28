;;;; constants.ss
;;;; These are the chez specific constant/datatype definitions.
;======================================================================

;; Could be define-structure or define-record.   <br> 
;; This defines the record-representation used throughout the regiment code. <br>
;; UNCOMMENT ONLY ONE OF THESE.

;======================================================================


(chez:module chez_constants 
    ;; Exports:
    ( reg:struct? reg:struct->list reg:list->struct reg:define-struct 
		  reg:include
		  IFCHEZ IF_GRAPHICS IF_THREADS
		  )
  (import scheme)
  
  ;; Pre-processor macro for switching between Chez/PLT versions.
  (define-syntax IFCHEZ (syntax-rules () [(_ chez plt) chez]))

  ;; This preprocessor form is used like an #IFDEF, evaluate code only
  ;; if we've got a GUI loaded.
  (define-syntax IF_GRAPHICS
    (lambda (x)
      (syntax-case x ()
		   [(_ E1 E2)
		    ;; The swl script sets this variable:
		    ;; When we build through SWL, we link in the SWL code.  Otherwise not.
		    (if (getenv "SWL_ROOT")
			#'E1
			#'E2)]
		   [(_ E1)
		    #'(IF_GRAPHICS E1 (void))])))

  (define-syntax IF_THREADS
    (lambda (x)
      (syntax-case x ()
	[(_ E1 E2)
	 (if (top-level-bound? 'fork-thread)
	     #'E1 #'E2)]
	[(_ E1) #'(IF_THREADS E1 (void))])))
  
  ;; This is a common syntax for including other source files inline.
  ;; I use it in both PLT and Chez.
  ;; DOESN'T WORK YET!!! (Not in PLT.  Sigh.) [2006.03.13]
  (define-syntax reg:include
    (lambda (x)
      (syntax-case x (REGIMENTD)
       [(_ str* ...)
	(let ([lst (datum (str* ...))])
	  (unless (andmap string? lst)
	    (error 'reg:include "bad syntax, expects strings: ~a\n" lst))
	  (with-syntax ([file (datum->syntax-object 
			       #'_ 
			       (apply string-append 
				      (cons (getenv "REGIMENTD")
					    (cons "/src"
						  (map (lambda (str)
							 (string-append "/" str))
						    lst)))))])
	    #'(include file)))])))

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
  ;; [2006.03.01] Modified this to add the record name in the front.
  (define (reg:struct->list x)
    (let ((type (#%record-type-descriptor x)))
      (cons (string-copy (record-type-name type))
       (map (lambda (name)
	      ((#%record-field-accessor type name) x))
	 (#%record-type-field-names type)))))
  ;; This constructs a new record, given an instance of the record type.
  (define (reg:list->struct template args)
    (let ((type (record-type-descriptor template)))
      (apply (record-constructor type)
	     args)))

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

;(import chez_constants)

(include "../generic/constants.ss")
(import constants) ;; Certain bindings are re-exported through the generic module.
;(import chez_constants)
