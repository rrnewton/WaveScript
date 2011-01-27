
(module rrn_oop mzscheme
  (require "../../plt/common.ss"
           "rrn_oop_helpers.ss"
	   )
  (provide 
	    new-object
            )
  (chezprovide (define-class ))
  (chezimports rrn_oop_helpers)

  (require-for-syntax "rrn_oop_helpers.ss")
  (require-for-syntax "../../plt/common.ss")
  
  (cond-expand [(plt) (require-for-syntax "rrn_oop_helpers.ss")
		      (require-for-syntax "common.ss")
                      ]
	       [else (begin)])
  
  ;; Experimenting with an OOP system that would resolve method slots at
  ;; macro expansion time.
  
  ;; It has the restriction that a given method name can only be used in
  ;; one class (hierarchy).
(define-syntax define-class 
  (lambda (x)    
    (syntax-case x (fields method)
      [(_ name super (fields fld* ...) (method methname* body*) ...)
       (let* ([old-table (if (top-level-bound? '*class-table*)
			     (top-level-value '*class-table*) '())]
	      ;; Indexes into method table for overloaded methods.
	      [overload_inds (map (curry find-method-index (datum super) old-table) (datum (methname* ...)))]
	      ;; Only new methods:
	      [filtered_methods (filter id (map (lambda (nm i) (and (not i) nm)) (datum (methname* ...)) overload_inds))]
	      [num-new-methods (length filtered_methods)]
	      [mdepth (method-depth (datum super) old-table)]
	      [fdepth (field-depth (datum super) old-table)]
	      ;; Method table indexes for overloads AND new method.
	      [combined_inds (let loop ([ls overload_inds] [i mdepth])
			       (cond
				[(null? ls) '()]
				[(car ls) (cons (car ls) (loop (cdr ls) i))]
				[else (cons i (loop (cdr ls) (add1 i)))]))])
	 (printf "Generating new class ~s, methods ~s, indexes: ~s nonoverloads: ~s\n" 
		 (datum name) (datum (methname* ...)) combined_inds filtered_methods)
	 (define-top-level-value
	   '*class-table*
	   (cons 
	    (make-class (datum name)
			(lookup-class (datum super) old-table)
			(datum (fld* ...)) filtered_methods
			;; place holder for method table?
			(make-vector (+ mdepth num-new-methods) #f)
			(+ fdepth (length (datum (fld* ...)))))
	    old-table))
	 (printf "new table: ~s\n" (top-level-value '*class-table*))
	 (printf "mdepth ~s fdepth ~s\n" mdepth fdepth)
	 (with-syntax ([(methind ...) (datum->syntax-object #'_ combined_inds)]
		       [(fldind ...) (datum->syntax-object 
				      #'_ (map (curry + fdepth) (iota (length (datum (fld* ...))))))]
		       [(getters ...) (datum->syntax-object 
				       #'_ (map (lambda (fld) (symbol-append 'get- fld)) 
					     (datum (fld* ...))))]
		       [(setters ...) (datum->syntax-object 
				       #'_ (map (lambda (fld) (symbol-append 'set- fld '!)) 
					     (datum (fld* ...))))]
		       ;[(filtered ...) (datum->syntax-object #'_ filtered_methods)]
		       [copy-existing-table 
			(if (datum super)
			    ;(lookup-class 'super (top-level-value '*class-table*))
			    #`(vector-blit! (class-methodtable super) vec 0 0 #,mdepth)
			    #'(void))])
	   #'(begin 
	       (define-syntax methname* 
		 (syntax-rules () 
		   [(_ ob . args) 
		    (let ([tmp ob])
		      ((vector-ref (class-methodtable (object-class tmp)) methind) tmp 
		       ;; Also supply the super method:
		       ;; Haven't thought of the best way to do this yet:
		       #f ;(vector-ref (class-supertable (object-class tmp)))		       
		       #;
		       (let ([sup (class-super (object-class tmp))])
			 (if (and sup (> (vector-length (class-methodtable sup))  methind))
			     (vector-ref (class-methodtable sup) methind)
			     (lambda _ (error 'methname* "No super exists for class ~s" 
					      (class-name (object-class tmp))))))
		       . args))]))
	       ...
	       (define-syntax getters (syntax-rules () [(_ ob) (vector-ref (object-fieldvec ob) fldind)]))
	       ...
	       (define-syntax setters (syntax-rules () [(_ ob x) (vector-set! (object-fieldvec ob) fldind x)]))
	       ...
	       ;; At runtime, initialize the method table:
	       (define name
		 ;(lookup-class (datum name) (top-level-value '*class-table*))
		 (let* ([class (lookup-class (datum name) (top-level-value '*class-table*))]
			[vec (class-methodtable class)])
		   copy-existing-table
		   (vector-set! vec methind body*)
		   ...
		   class)))))])))


(define (new-object class)
  (make-object (class-methodtable class) 
	       (make-vector (class-field-count class) #f)
	       class))

;;================================================================================
;; Examples:
(define-class foo #f
  (fields i j k)
  (method bar (lambda (self super x) x))
  (method baz (lambda (self super x) (add1 x)))
  ;(constructor (lambda () (set-i! self 33) (set-j! self 44)))
  )

(define-class foo2 foo
  (fields n m)
  (method bot (lambda (self super x) (* 2 x)))
  ;; Overload:
  ;(method baz (lambda (self super x) (printf "   SELF: ~s, supercall ~s\n" self #f) (sub1 x)))
  )
;(super self #f x)

(define x (new-object foo))
(define y (new-object foo2))


(printf "\n============\n\n")

(pp (expand '(bar ob 3 4 5)))
(pp (expand '(baz ob 3 4 5)))

(pp (expand '(get-k ob)))
(pp (expand '(set-k! ob 986)))

(pp (expand '(get-m ob)))

(pp (top-level-value '*class-table*))

(newline)
(pp x)
(pp y)

(printf "<foo>  bar method: ~s\n" (bar x 9))
(printf "<foo>  baz method: ~s\n" (baz x 9))
(printf "<foo> fields: ~s ~s ~s\n" (get-i x) (get-j x) (get-k x))
(printf "<foo2> baz method: ~s\n" (baz y 9))
(printf "<foo2> bot method: ~s\n" (bot y 9))
(printf "<foo2> fields: ~s ~s ~s ~s ~s\n" (get-i y) (get-j y) (get-k y) (get-n y) (get-m y))


) ;; End module


