
;;;; This file defines an ADT for type environments used by the type checker.
;;;; It also defines the represenation for types themselves.

;;;; .author Ryan Newton

(module type_environments mzscheme
  (require ;`"../../plt/common.ss"
;           "prim_defs.ss"
           "../../plt/iu-match.ss"
;           "../../plt/chez_compat.ss"
           "../constants.ss"
           "../util/helpers.ss"
#;
	   (all-except "../compiler_components/regiment_helpers.ss"
                       regiment-type-aliases
                       regiment-basic-primitives
                       local-node-primitives
                       regiment-constants
                       regiment-distributed-primitives
                       )
           )

  (provide 
           num-types

	   type? 
	   distributed-type?
	   arrow-type?
	   polymorphic-type?          	   
	   constant-typeable-as? 
	   known-size?
	   type->width

;            instantiate-type
; 	   export-type 
; 	   prim->type

           make-tvar
	   reset-tvar-generator
	   make-tcell
	   tcell->name
           
	   empty-tenv
	   tenv?
	   tenv-lookup
	   tenv-is-let-bound?
	   tenv-extend
	   tenv-extend-pattern
	   tenv-append
	   tenv-map

	   )

  (chezimports constants rn-match)

;; Added a subkind for numbers, here are the types in that subkind.
(define num-types '(Int Float Double Complex 
		    Int16
		    ;; Eventually:
		    ;; Int8 Int16 Int64 Double Complex64
			))

; ----------------------------------------
;;; Representation for Types:

;;; TODO: If I wanted an added degree of safety, I could make types
;;; and instantiated-types totally disjoint; at the cost of boxing
;;; them.  Currently I believe I'm being sloppy in places.

(define (valid-type-symbol? s)
  (let ([str (symbol->string s)])
    (and (> (string-length str) 0)
	 (char-upper-case? (string-ref str 0)))))
(define (valid-typevar-symbol? s)
  (let ([str (symbol->string s)])
    (and (> (string-length str) 0)
	 (char-lower-case? (string-ref str 0)))))

;; Our types aren't a very safe datatype, but this predicate tries to give some assurance.
(define (instantiated-type? t . extra-pred)
  (define (id x) x)
  (IFCHEZ (import rn-match) (void))
  (match t
    [,s (guard (symbol? s)) (valid-type-symbol? s)]
    ;[(,qt ,v)          (guard (memq qt '(quote NUM)) (symbol? v)) (valid-typevar-symbol? v)]
    [(,qt (,v . #f))   (guard (memq qt '(quote NUM)) (symbol? v)) (valid-typevar-symbol? v)]
    [(,qt (,v . ,[t])) (guard (memq qt '(quote NUM)) (symbol? v)) (and t (valid-typevar-symbol? v))]
    [#(,[t] ...) (andmap id t)] 
    [(,[arg] ... -> ,[ret]) (and ret (andmap id  arg))]
    [(Struct ,name) (symbol? name)] ;; Adding struct types for output of nominalize-types.
    [(LATEUNIFY #f ,[t]) t]
    [(LATEUNIFY ,[t1] ,[t2]) (and t1 t2)]
    ;; Including Ref:
    [(,C ,[t] ...) (guard (symbol? C) (not (memq C '(quote NUM)))) (andmap id t)]
    [,s (guard (string? s)) #t] ;; Allowing strings for uninterpreted C types.
    ;[(,ptr ,[name]) (guard (eq-any? ptr 'Pointer 'ExclusivePointer)) name]
    [,oth (if (null? extra-pred) #f 
	      ((car extra-pred) oth))]))
(define (type? t)
  (IFCHEZ (import rn-match) (void))
  (instantiated-type? t 
    (lambda (x) 
      (match x 
	[(,qt ,v) (guard (memq qt '(quote NUM)) (symbol? v)) (valid-typevar-symbol? v)]
	[,other #f]))))

;; Does it contain the monad?
(define (distributed-type? t)
  (define (id x) x)
  (IFCHEZ (import rn-match) (void))
  (match t
    ;; TODO: FIXME: Use the type alias table, don't check for Region/Anchor directly:
    [Region #t]
    [Anchor #t]
    [(Area ,_) #t]
    [(Stream ,_) #t]    
    [,s (guard (symbol? s)) #f]
    [#(,[t] ...) (ormap id t)]
    [(,qt ,v) (guard (memq qt '(quote NUM)) (symbol? v)) #f]
    [(,qt (,v . #f))  (guard (memq qt '(quote NUM)) (symbol? v)) 
     (warning 'distributed-type? "got type var with no info: ~s" v)
     #t] ;; This COULD be.
    [(,qt (,v . ,[t])) (guard (memq qt '(quote NUM)) (symbol? v)) t]
    [(,[arg] ... -> ,[ret]) (or ret (ormap id  arg))]
    [(Struct ,name) #f] ;; Adding struct types for output of nominalize-types.
    [(LUB ,a ,b) (error 'arrow-type? "don't know how to answer this for LUB yet.")]
    [(,C ,[t] ...) (guard (symbol? C)) (ormap id t)]
    [,else #f]))

;; Does it contain any type-vars?
(define (polymorphic-type? t)
  (define (id x) x)
  (IFCHEZ (import rn-match) (void))
  (match t
    ;; TODO: FIXME: Use the type alias table, don't check for Region/Anchor directly:
    [,s (guard (symbol? s)) #f]
    [(,qt ,v) (guard (memq qt '(quote NUM)) (symbol? v)) #t]
    [(,qt (,v . #f))  (guard (memq qt '(quote NUM)) (symbol? v)) 
     (warning 'polymorphic-type? "got type var with no info: ~s" v)
     #t] ;; This COULD be.
    [(,qt (,v . ,[t])) (guard (memq qt '(quote NUM)) (symbol? v)) t]
    [#(,[t] ...) (ormap id t)]
    [(,[arg] ... -> ,[ret]) (or ret (ormap id  arg))]
    [(Struct ,name) #f] ;; Adding struct types for output of nominalize-types.
    [(LUB ,a ,b) (error 'polymorphic-type? "don't know how to answer this for LUB yet.")]
    ;; Including Ref:
    [(,C ,[t] ...) (guard (symbol? C)) (ormap id t)]
    [,else #f]))

;; This means *is* an arrow type, not *contains* an arrow type.
(define (arrow-type? t)
  (IFCHEZ (import rn-match) (void))
  (match t
    [(quote (,v . #f)) #f]
    [(quote (,v . ,[rhs])) rhs]
    [(NUM ,_) #f] ;; NUM types shouldnt be arrows!
    [(,t1 ... -> ,t2) #t]
    [(LUB ,a ,b) (error 'arrow-type? "don't know how to answer this for LUB yet.")]
    [,else #f]))

(define constant-typeable-as? 
  (lambda (c ty)
#;
    (cond 
     [(and (fixnum? c) (eq? ty 'Int))   (and (< c (expt 2 31)) (> c (- (expt 2 31))))]
     [(and (fixnum? c) (eq? ty 'Int16)) (and (< c (expt 2 15)) (> c (- (expt 2 15))))]
     [(and (flonum? c))                 (eq? ty 'Float)]
     [else #f])

    (if (eq? c 'BOTTOM) #t
	(match ty
	  [Int   (guard (fixnum? c))  (and (< c (expt 2 31)) (> c (- (expt 2 31))))]
	  [Int16 (guard (fixnum? c))  (and (< c (expt 2 15)) (> c (- (expt 2 15))))]
	  [Float   (flonum? c)]
	  [Double  (flonum? c)]
	  [Complex (cflonum? c)]
	  [Bool  (boolean? c)]
	  [String (string? c)]
	  [(List ,t) (and (list? c) (andmap (lambda (x) (constant-typeable-as? x t)) c))]
	  [#()   (eq? c 'UNIT)]
	  ;[else #f]
	  ))
    ))


  ;; Does this type have a known size?
  ;; Matters for making tuples into structs.
  (define (known-size? t)
    (define (id x) x)
    (match t
      [,simple (guard (symbol? simple)) #t]
      ;; TODO: FIXME: Use the type alias table, don't check for Region/Anchor directly:
      [(,qt ,v) (guard (memq qt '(quote NUM)) (symbol? v)) #f]
      [(,qt (,v . ,[t])) (guard (memq qt '(quote NUM)) (symbol? v)) t]
      ;; This shouldn't be in a tuple anyway:
      [(,[arg] ... -> ,[ret]) #f]

      [(Struct ,name)         #t]
      ;; These are pointers, doesn't matter what the type inside is.
      [(Sigseg ,_)            #t]
      [(Array  ,_)            #t]
      [(List   ,_)            #t]
      [(Pointer   ,_)         #t]
      [(ExclusivePointer  ,_) #t]
      [(,C ,[t] ...) (guard (symbol? C)) (andmap id t)]
      [#(,[t] ...) (andmap id t)]
      [,else #f]))

  ;; TODO, needs to handle sum types properly.
  ;;
  ;; NOTE: this is specific to the C++ backend... but it also holds
  ;; for the external representation in files.
  ;;
  ;; NOTE: This doesn't actually correspond to the sizeof the C struct used to represent the tuple.
  ;; The C compiler is at liberty to do things like word-align the struct.
  (define type->width
    (case-lambda
      [(t) (type->width t #f)]
      [(t sumdecls) 
       (match t
	 [Int16 2]
	 [Int 4] ;; INTS ARE 16 BIT FOR NOW!!! FIXME FIXME
	 ;; HACK:
	 [(Sigseg #(,[w*] ...)) (apply + w*)]
	 [(Sigseg ,t)         (type->width t)]
	 [Float  4]
	 [Double 8]
	 ;;[Complex ]    
	 
	 ;[#() 1] ;????
	 [#(,[t*] ...) (apply + t*)]

	 [(Sum ,TC)
	  (ASSERT sumdecls)
	  (printf "TYPE->WIDTH UNFINISHED!\n")
	  (inspect sumdecls)
	  (exit 3)
	  (assq TC sumdecls)
	  ]
	 
	 [,other (error 'type->width "do not know the size of this type: ~s" other)]
	 )]))
  
; ----------------------------------------
;;; Representation for type variables  
  
(define tvar-generator-count 0)
;; Makes a unique type variable.
(define make-tvar
  (let* ([vars (list->vector (map symbol->string 
			       '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))]
	 [len (vector-length vars)]
	 )
    (lambda ()
      (let loop ([ind tvar-generator-count] [acc '()])
	(cond
	 [(< ind len)  
	  (set! tvar-generator-count (fx+ 1 tvar-generator-count))
	  (string->symbol (apply string-append (cons (vector-ref vars ind) acc)))]
	 [else (loop (fx- (fxquotient ind len) 1)
		     (cons (vector-ref vars (fxremainder ind len)) acc))]))
      )))

;; Resets the unique name counter.
(define (reset-tvar-generator) (set! tvar-generator-count 0))

;; A tcell is like a tvar (quoted, representing a variable).  Except
;; instead of just containing a symbolic name, it is a cons cell also
;; including constraints on that variables type.  It's a place for
;; accumulating information on a variable.
(define make-tcell
  (case-lambda
    [() `(quote (,(make-tvar) . #f))]
    [(x)`(quote (,(make-tvar) . ,x))]))

(define (tcell->name x)
  (IFCHEZ (import rn-match) (void))
  (match x
    [(,qt (,n . ,t)) (guard (memq qt '(quote NUM))) (DEBUGASSERT (symbol? n))    n]
    [,else (error 'tcell->name "bad tvar cell: ~s" x)]))

  
  
  
; ----------------------------------------
;;; Type Environment ADT

;;; Type environments bind variable names to types.
;;; Currently, these may be instantiated or uninstantiated types.
;;; <br><br>

;;; [2007.02.21] Changing the tenv representation.  Now we wish to
;;; ultimately use the least-upper-bound of all the reference-sites as
;;; the type of a let-bound variable.  I'm internally changing the
;;; "flag" field to store #f for non-let-bound variables, and to be a
;;; type for let-bound variables.  That type will be a backup copy of
;;; the most general type.  The normal type field will contain the LUB
;;; of the reference-sites.

;; Constructs an empty type environment.
(define empty-tenv 
  ;; Uses a unique identifier.
  (let ([ident (gensym "tenv")])
    (lambda () (list ident))))

;; Predicate testing for type environments.
(define (tenv? x)
  (IFCHEZ (import rn-match) (void))
  (match x
    ;; Format: [VAR, TYPE, Is-Let-Bound?-FLAG]
    [(,tenvsym [,v* ,t* ,flag*] ...)
     (and (eq? tenvsym (car (empty-tenv)))
	  (andmap symbol? v*)	  
	  (andmap type? t*);(andmap instantiated-type? t*)
	  (andmap (lambda (x) (or (boolean? x) (type? x))) flag*)
	  )]
    [,else #f]))
;; Retrieves a type if a binding exists for sym, otherwise #f.
(define (tenv-lookup tenv sym)
  (DEBUGASSERT (tenv? tenv))
  (let ([entry (assq sym (cdr tenv))])
    (if entry (cadr entry) #f)))
;; This returns #t if the let-bound flag for a given binding is set.
;; If sym is lambda-bound or is unbound in the type environment, #f is
;; returned.
(define (tenv-is-let-bound? tenv sym)
  (DEBUGASSERT (tenv? tenv))
  (let ([entry (assq sym (cdr tenv))])
    (if entry (caddr entry) #f)))

;; Extends a type environment.
;; .param tenv The type env to extend.
;; .param syms The names to bind.
;; .param vals The types to bind.
;; .param flag Optional flag: #t for let-bound, #f (default) for lambda-bound.
;; .returns A new type environment.
(define (tenv-extend tenv syms types . flag)
  (IFCHEZ (import rn-match) (void))
  (DEBUGASSERT (tenv? tenv))
  (DEBUGASSERT (andmap type? types))
  (let ([flag (if (null? flag) #f (if (car flag) #t #f))])
    (cons (car tenv)
	  (append (map (lambda (v t) 
			 (if flag
			     (match t
			       ;; Expects the type to be a type var:
			       [(quote ,pr) (ASSERT pair? pr)
				;; CONSTRUCT A DELAYED UNIFY:
				;; This is the scratch-pad on which we'll record the info from the call-sites. 

				(let ([cell (make-tcell (cdr pr))])
				  (set-cdr! pr 
				    (if (inferencer-enable-LUB) `(LATEUNIFY #f ,cell) cell)))
				
				(list v `(quote ,pr) #t)
				])
			     (list v t #f)))
		    syms types)
		  (cdr tenv))
	  )))

;; This extends a type environment, retrieving the variables to bind
;; by deconstructing a pattern.
;; Ultimately this will be a hotspot, so we should remove the use of map/append/etc below:
(trace-define (tenv-extend-pattern tenv pat type)
  ;; This builds up a list of the vars & types:
  (define (extendpat-loop pat type)
    (match pat
      [,v (guard (symbol? v)) (list (vector v type))]
      [#(,p* ...) 
       (apply append (map extendpat-loop p* (vector->list type)))]      
      ))
  
  (match (extendpat-loop pat type)
    [(#(,v* ,t*) ...) (tenv-extend tenv v* t*)]))


(define (tenv-append . tenvs)
  (cons (car (empty-tenv)) 
	(apply append (map cdr tenvs))))
;; Applies a function to all types in a type enviroment.
(define (tenv-map f tenv)
  (IFCHEZ (import rn-match) (void))
  (DEBUGASSERT (tenv? tenv))
  (cons (car tenv)
	(map 
	    (lambda (x) 
	      (match x 
		[(,v ,t ,flag) `(,v ,(f t) ,flag)]
		[,other (error 'recover-type "bad tenv entry: ~s" other)]))
	  (cdr tenv))))

) ;; End module
