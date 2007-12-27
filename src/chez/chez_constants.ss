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
		  IFCHEZ 
		  IF_GRAPHICS IF_THREADS
		  cond-expand
		  hash-percent path->string
		  )
  (import scheme)

  ;; [2007.12.27] Considering switching over to a cond-expand system.
  (define-syntax cond-expand
    (lambda (syn)
      (syntax-case syn (and or not else 
			    chez plt larceny graphics threads)
      ;; Standard clauses:
      ((cond-expand) (syntax-error #'syn "Unfulfilled cond-expand"))
      ((cond-expand (else body ...))  #'(begin body ...))
      ((cond-expand ((and) body ...) more-clauses ...) #'(begin body ...))
      ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
       #'(cond-expand (req1 (cond-expand ((and req2 ...) body ...) more-clauses ...)) more-clauses ...))
      ((cond-expand ((or) body ...) more-clauses ...) #'(cond-expand more-clauses ...))
      ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
       #'(cond-expand (req1 (begin body ...)) (else (cond-expand ((or req2 ...) body ...) more-clauses ...))))
      ((cond-expand ((not req) body ...) more-clauses ...)
       #'(cond-expand (req (cond-expand more-clauses ...)) (else body ...)))

      ;; Enabled features (or potentially enabled):
      ((cond-expand (chez body ...) more-clauses ...) #'(begin body ...))
      ((cond-expand (graphics body ...) more-clauses ...) 
       (if (getenv "SWL_ROOT")  #'(begin body ...)
	   #'(cond-expand more-clauses ...)))
      ((cond-expand (threads body ...) more-clauses ...)
       (if (top-level-bound? 'fork-thread)
	   #'(begin body ...)
	   #'(cond-expand more-clauses ...)))

      ;; Otherwise, it's disabled:
      ((cond-expand (feature-id body ...) more-clauses ...) #'(cond-expand more-clauses ...)))))
  
  ;; Pre-processor macro for switching between Chez/PLT versions.
  (define-syntax IFCHEZ 
    (syntax-rules (chez plt) 
      ;[(_ a)   (cond-expand [chez a] [else (begin)])]
      [(_ a b) (cond-expand [chez a] [plt b])]))

  ;; This preprocessor form is used like an #IFDEF, evaluate code only
  ;; if we've got a GUI loaded.
  (define-syntax IF_GRAPHICS
    (syntax-rules (chez plt) 
      [(_ a b) (cond-expand [(and chez graphics) a] [else b])]
      [(_ a)   (cond-expand [(and chez graphics) a] [else (void)])]))

  (define-syntax IF_THREADS
    (syntax-rules (chez plt) 
      [(_ a b) (cond-expand [(and chez threads) a] [else b])]
      [(_ a)   (cond-expand [(and chez threads) a] [else (void)])]))

  
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

    ;; This is a hack to emulate Chez's #%prim syntax without breaking
    ;; other readers that can't handle it.
    (define-syntax (hash-percent syn)
      (syntax-case syn ()
	[(_ prim) (datum->syntax-object #'_ `(let () (import scheme) ,(datum prim)))]))

    ;; MISC: This is for PLT compat:
    (define path->string (lambda (x) x))
   
) ;; End module


(include "../generic/constants.ss")
(import constants) ;; Certain bindings are re-exported through the generic module.

