;;;; .title The Regiment Type Inferencer.

; type := T                ; symbol - an atomic type
; type := 'a               ; quoted symbol - a type variable
; type := (T type ...)     ; a type constructor
; type := (type -> type)   ; a function type
; type := #(type ...)      ; a tuple type

; And internally, the algorithm uses these constructions:
; type := '(a . type)      ; a type variable with binding
; type := '(a . #f)        ; a type variable with no info

; And for our particular brand of let-bound polymorphism:
; A place to record type constraints and LUB/unify later:
; type := (LATEUNIFY lub type) 
; lub  := #f
; lub  := type
; lub  := (LUB type type)

;; Also see the primitive type definitions in prim_defs.ss

;; Primary ENTRY POINTS are:
;;  type-expression (obsolete)
;;  annotate-expression
;;  annotate-program
;;  types-compat?

;;  validate-types [2006.10.12] TODO: check the existing types, should be less expensive

;; [2007.02.21] Adding LUBs to the internal type grammar.

;; TODO: [2006.04.28] Add a "normalize" procedure that gets rid of any
;; type aliases.  This will remove most of the need for types-compat?

;; [2007.04.05] Previously I had an overly restrictive ban on
;; instantiation of Ref types.  Now I've implemented the SML style
;; "value restriction".

(module hm_type_inference mzscheme
  (require ;`"../../plt/common.ss"
           "prim_defs.ss"
           "../../plt/iu-match.ss"
           "../../plt/chez_compat.ss"
           "../constants.ss"
           "../util/helpers.ss"
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
	   constant-typeable-as? 
	   inferencer-let-bound-poly

	   make-tvar-generator
	   make-tvar
	   reset-tvar-generator

	   make-tcell

	   tcell->name

	   empty-tenv
	   tenv?
	   tenv-lookup
	   tenv-is-let-bound?
	   tenv-extend
	   tenv-append
	   tenv-map
	   
;	   resolve-type-aliases

           instantiate-type
	   export-type do-late-unify! 
	   prim->type
	   type?
	   distributed-type?
	   arrow-type?
	   polymorphic-type?
	   ;id
	   ;inject-polymorphism
	   
	   recover-type
	   type-const
;	   type-lambda
	   type-app
;	   type-letrec
           
	   annotate-expression
	   annotate-program
	   strip-types do-all-late-unifies!
	   
	   print-var-types

	   types-compat?
           
	   test-inferencer

;; These should be hidden:
	   types-equal!
           type-expression
;	   tvar-equal-type!
;	   no-occurrence!
;	   safe-export-type
;	   raise-type-error
;	   raise-occurrence-check
;	   raise-wrong-number-of-arguments

	   sumdecls->tenv grab-init-tenv
	   )

  (chezimports constants
	       ;; Switching for better speed:
	       ;rn-match
	       )

;; This controls whether let-bound-polymorphism is allowed at all.
(define inferencer-let-bound-poly (make-parameter #t))

;; If this is enabled, the type assigned to a let-bound variable is
;; lowered to the LUB of its call-site requirements, rather than the
;; most general type.
;;   This is only turned off for debugging purposes...
(define ENABLELUB #t)

;; Added a subkind for numbers, here are the types in that subkind.
(define num-types '(Int Float Complex 
		    Int16
		    ;; Eventually:
		    ;; Int8 Int16 Int64 Double Complex64
			))  
;(define mutable-constructors '(Ref Array HashTable))
;(define mutable-constructors '(Ref Array ))
(define mutable-constructors '(Ref ))

; ======================================================================
;;; Helpers

(define (safe-export-type t)
  (if (type? t) (export-type t) `(NOT-A-TYPE ,t)))

(define type-error error)

(define (get-location x)
  (match x
    [(src-pos #((,fn) ,off1 ,ln1 ,col1 ,off2 ,ln2 ,col2) ,_)
     (++ (format "in file ~s\n   "  fn)
	 (if (= ln1 ln2)
	     (format "on line ~s, columns ~s through ~s " ln1 col1 col2)
	     (format "between line/col ~s:~s and ~s:~s " ln1 col1 ln2 col2)))]
    [,x 
     ;; SUPER HACKISH:
     (let ([pos (deep-assq 'src-pos x)])
       (match pos
	 [#f "Unknown source location."]
	 [(src-pos #((,fn) ,off1 ,ln1 ,col1 ,off2 ,ln2 ,col2) ,_)
	  (format "within file ~s\n   in the viscinity of line ~s column ~s"
		  fn ln1 col1)
	  ]))]))

;; Raises a generic type error at a particular expression.
(define (raise-type-mismatch t1 t2 exp)
  (type-error 'type-checker
	 (++ "\n";"\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
	     "Type mismatch: ~a doesn't match ~a \n"
	     "\nLocation:\n   ~a\n" ;; Location
	     "\nExpression: (in abstract syntax)\n   ~s \n")
	 (safe-export-type t1) (safe-export-type t2) 
	 ;; Approximate location:
	 (get-location exp)
	 exp))
;; Raises an error indicating that we have a loop in our tvar pointers.
(define (raise-occurrence-check tvnum t2 exp)
  (type-error 'type-checker
	 "Can't unify: ~s occurs in type ~s in expression ~s~%" 
	 tvnum t2 ;(export-type t2) 
	 exp))
;; Raises a wrong-number-of-arguments error.
(define (raise-wrong-number-of-arguments t1 t2 exp)
  (type-error 'type-checker
	 "Different numbers of arguments:\n      ~s: ~s\n  and ~s: ~s\n in ~s\n"
	 (- (length t1) 2) (safe-export-type t1) 
	 (- (length t2) 2) (safe-export-type t2) exp))

; ----------------------------------------

(define make-tvar-generator
  (lambda ()
    (let* ([vars '(a b c d e f g h i j k l m n o p q r s t u v w x y z 
		     ;;alpha beta gamma
		     )]
	   [len (length vars)]
	   [count 0]
	   )
      (lambda ()
	(let loop ([ind count] [acc '()])
	  (cond
	   [(< ind len)  
	    (set! count (add1 count))
	    (apply symbol-append (cons (list-ref vars ind) acc))]
	   [else (loop (sub1 (quotient ind len))
		       (cons (list-ref vars (remainder ind len)) acc))]))
	))))

;; Makes a unique type variable.
(define make-tvar (make-tvar-generator))
;; Resets the unique name counter.
(define (reset-tvar-generator) (set! make-tvar (make-tvar-generator)))

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
				(set-cdr! pr `(LATEUNIFY #f ,(make-tcell (cdr pr))))
				(list v `(quote ,pr) #t)
				])
			     (list v t #f)))
		    syms types)
		  (cdr tenv))
	  )))
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


; ----------------------------------------

;; This associates new mutable cells with all tvars.
;; It also renames all the tvars to assure uniqueness.
;; The "nongeneric" vars are ones that do not receive new mutable cells.
;; 
;; This is because, when instantiating a rator for application
;; let-bound type variables are reinstantiated, whereas lambda-bound ones are not.
(define instantiate-type 
  (case-lambda 
    [(t) (instantiate-type t '())]
    [(t nongeneric)
     (IFCHEZ (import rn-match) (void))
     (let* ((tenv (empty-tenv))
	 (result 
	  (let loop ((t t))
	   (match t
	     [#f #f]
	     [,s (guard (symbol? s)) s]

	     ;; A mutable cell, we can't reinstantiate this:
	     [(,Ref ,t) (guard (memq Ref mutable-constructors))
	      ;; But we have an issue, it *does* need to be instantiated the first time.
	      (match (or (deep-assq 'quote t) (deep-assq 'NUM t))
		[#f 
		; (inspect (list "ref type with no mutable cell: " t))
		 ;; It shouldn't matter whether we recur here:
		 `(,Ref ,t)]
		[(,qt (,v . ,_)) (guard (memq qt '(quote NUM)))
		 ;(inspect (list "mutable cell within ref type" (cons v _) " in " t))
		 `(,Ref ,t)]
		[(,qt ,v)  (guard (memq qt '(quote NUM)))
		 (DEBUGASSERT symbol? v)
		 ;(inspect (list "non-cell within ref type, instantiating once!" v " in " t))
		 `(,Ref ,(loop t))]
		)]
	     
	     [(,Ref . ,t) (guard (memq Ref mutable-constructors))
	      (error 'instantiate-type "haven't implemented multi-argument mutable-type constructors")]

             ;; This type variable is non-generic, we do not copy it.
	     [(,qt ,cell) 
	      (guard (memq qt '(quote NUM)) (pair? cell) (memq (car cell) nongeneric))
              `(,qt ,cell)] ;; Don't reallocate the cell (or touch its RHS)
	     ;; Otherwise make/lookup the new cell and attach it.
	     [(,qt ,x) 
	      (guard (memq qt '(quote NUM))
		     (or (symbol? x) (pair? x)))
	      (let* ([var (if (symbol? x) x (car x))]
		     [entry (tenv-lookup tenv var)])
		(or entry
		    (let ((newtype `(,qt ,(cons (make-tvar) 
						  (if (pair? x) (loop (cdr x)) #f)))))
		      ;; After that loop var should still not occur in the env!
		      (DEBUGASSERT (not (tenv-lookup tenv var)))
		      (set! tenv (tenv-extend tenv (list var) (list newtype)))
		      newtype)))]

	     [(,[arg*] ... -> ,[res]) ; Ok to loop on ellipses.
	      `(,@arg* -> ,res)]
	     [#(,[t*] ...) (apply vector t*)]	     
	     [(,constructor ,[args] ...)
	      (guard (symbol? constructor))
	      `(,constructor ,@args)]
	     [,other (error 'instantiate-type "bad type: ~a" other)]
	     ))))
       (DEBUGASSERT (type? result))
       result)]))

;; This takes away the mutable cells, thereby converting to the
;; external representation.
;; It also takes away LUB types.
(define (export-type t)
  (IFCHEZ (import rn-match) (void))
  (match t
    [,s (guard (symbol? s)) s]
    ['(,n . ,v) (if v (export-type v) `(quote ,n))]
    [',n `(quote ,n)]
    [(NUM ,v) (guard (symbol? v)) `(NUM ,v)]
    [(NUM (,v . ,t)) (if t (export-type t) `(NUM ,v))]
    [(LATEUNIFY #f ,[b]) `(LATEUNIFY #f ,b)]
    [(LATEUNIFY ,[a] ,[b])
     `(LATEUNIFY ,a ,b)]
    [(,[arg*] ... -> ,[res])
     `(,@arg* -> ,res)]
    ;; Including Ref:
    [(,s ,[t*] ...) (guard (symbol? s))
     `(,s ,@t*)]
    [#(,[t*] ...) (apply vector t*)]
    [,other (error 'export-type "bad type: ~s" other)]))

;; [2007.02.21]
;; HACK: including this unifier and unifying each of these again:
;; Shouldn't have to do this, but there's a problem with the design.
(define (do-lub!!! t UNIFIER)
  (IFCHEZ (import rn-match) (void))
  (let ([rands 
	 (match t
	   [(LUB ,[a] ,[b]) (append a b)]
	   [,oth            (list oth)])])
    (for-each (lambda (t) (types-equal! t (instantiate-type UNIFIER '())
					"unknown code location"))
      rands)
    (let ([exported (map export-type rands)])
      ;(printf "  FINAL LUBS: ~s\n" exported)
      (foldl1 LUB exported))
    ))

;; This traverses the type and does any LATEUNIFY's
(define (do-late-unify! t)
  (IFCHEZ (import rn-match) (void))
  (match t
    [,s (guard (symbol? s))                  (void)]
    [(quote ,pr)
     (match pr
       [(,v . (LATEUNIFY #f ,b))
	;; No call sites to unify.
	(set-cdr! pr b)
	(do-late-unify! b)]
       [(,v . (LATEUNIFY ,a ,b))
        ;(printf "LATEUNIFY ~s ~s\n" a b)
	(if ENABLELUB
	    ;; Comupute the LUB and then unify that with the most general
	    ;; type.  That's our answer.
	    (let* ([lub (do-lub!!! a b)]
		   [tc (make-tcell b)])
	      (types-equal! (instantiate-type lub '()) 
			    tc ;(inspect/continue tc)
			    "unknown location")
	      (set-cdr! pr tc))
	    ;; Otherwise just the most general type.
	    (set-cdr! pr b))]
       [(,v . ,oth) (if oth (do-late-unify! oth) (void))])]
    [(LATEUNIFY ,a ,b)
     (error 'do-late-unify! "found LATEUNIFY not in mutable cell: ~s" `(LATEUNIFY ,a ,b))]
    ;[',n `(quote ,n)]
    [(NUM ,v) (guard (symbol? v))            (void)]
    [(NUM (,v . ,t)) (if t (do-late-unify! t) (void))]
    [(,[arg*] ... -> ,[res])                 (void)]
    ;; Including Ref:
    [(,s ,[t] ...) (guard (symbol? s))       (void)]
    [#(,[t*] ...)                            (void)]
    [,other (error 'do-late-unify! "bad type: ~s" other)]))

;; Looks up a primitive and produces an instantiated version of its arrow-type.
(define (prim->type p)
  (let ((entry (or ;(assq p regiment-basic-primitives)
		   ;(assq p regiment-constants)
		   ;(assq p regiment-distributed-primitives)
		   (assq p (regiment-primitives))
		   )))
    (unless entry
      (error 'prim->type "primitive was not found: ~a" p))
    (match entry
      [(,name ,type) (instantiate-type type '())]
      [(,name ,args ,ret)
       (instantiate-type `(,@args -> ,ret) '())])))


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
    [(,[arg] ... -> ,[ret]) (and ret (andmap id  arg))]
    [(Struct ,name) (symbol? name)] ;; Adding struct types for output of nominalize-types.
    [(LATEUNIFY #f ,[t]) t]
    [(LATEUNIFY ,[t1] ,[t2]) (and t1 t2)]
    ;; Including Ref:
    [(,C ,[t] ...) (guard (symbol? C) (not (memq C '(quote NUM)))) (andmap id t)]
    [#(,[t] ...) (andmap id t)]   
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
    [(,qt ,v) (guard (memq qt '(quote NUM)) (symbol? v)) #f]
    [(,qt (,v . #f))  (guard (memq qt '(quote NUM)) (symbol? v)) 
     (warning 'distributed-type? "got type var with no info: ~s" v)
     #t] ;; This COULD be.
    [(,qt (,v . ,[t])) (guard (memq qt '(quote NUM)) (symbol? v)) t]
    [(,[arg] ... -> ,[ret]) (or ret (ormap id  arg))]
    ;; This really should not be:
    [(,Ref ,[t]) (guard (memq Ref mutable-constructors)) (ASSERT not t) #f]
    [(Struct ,name) #f] ;; Adding struct types for output of nominalize-types.
    [(LUB ,a ,b) (error 'arrow-type? "don't know how to answer this for LUB yet.")]
    [(,C ,[t] ...) (guard (symbol? C)) (ormap id t)]
    [#(,[t] ...) (ormap id t)]
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
    [(,[arg] ... -> ,[ret]) (or ret (ormap id  arg))]
    [(Struct ,name) #f] ;; Adding struct types for output of nominalize-types.
    [(LUB ,a ,b) (error 'polymorphic-type? "don't know how to answer this for LUB yet.")]
    ;; Including Ref:
    [(,C ,[t] ...) (guard (symbol? C)) (ormap id t)]
    [#(,[t] ...) (ormap id t)]
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
    ;; This should not be either!
    [(,Ref ,[t]) (guard (memq Ref mutable-constructors)) (ASSERT not t) #f]
    [,else #f]))

  
  
  
  
; ======================================================================

;;; The main type checker.

(define (type-expression expr tenv)
  (mvlet ([(e typ) (annotate-expression expr tenv '())])        
    (do-all-late-unifies! e)    
    typ))

;; Used for recovering types for particular expressions within an already type-annotated program.
;; NOTE: *Assumes* that the program is *correctly* typed.
;; Returns an "exported" type... i.e. with mutable cells stripped.
;; .param exp - Expression
;; .param tenv - Type Environment
(define (recover-type exp tenv)
;  (IFCHEZ (import rn-match) (void)) ;; Doesn't work yet with nested ellipses...
  (DEBUGASSERT (tenv? tenv))
  (let l ((exp exp))
    (match exp 
      [(lambda ,formals ,types ,body)
       `(,@types -> ,(recover-type body (tenv-extend tenv formals types)))]
      ;; Being lenient and accepting potential annotations in the
      ;; letrec binds.  This is necessary for running "recover type"
      ;; on the intermediate forms of the later compiler passes.
      [(,letrec ([,lhs* ,type* ,optional_annots ... ,rhs*] ...) ,body)
       (guard (memq letrec '(letrec lazy-letrec)))
       (recover-type body (tenv-extend tenv lhs* type*))]

      [(if ,t ,[c] ,[a]) 
       (let ([a (instantiate-type a '())] 
	     [c (instantiate-type c '())])
	 (types-equal! c a `(if ,t ??? ???))
	 (export-type c))]

      [(tuple ,[t*] ...) (list->vector t*)]
      [(tupref ,n ,len ,[t]) (vector-ref t (qinteger->integer n))]
      [(unionN ,[t*] ...) 
       (ASSERT (not (null? t*)))
       (ASSERT all-equal?  t*)
       (match (types-compat? '(Stream 'a) (car t*))
	 [(Stream ,t) `(Stream #(Int ,t))]
	 )
       ]

      ;; Since the program is already typed, we just use the arrow type of the rator:
      ;[(,[rat] ,rand ...) (rac rat)]
      [,other 
       (export-type (type-expression other (tenv-map instantiate-type tenv)))   ])))

;; Assign a basic type to a constant.
(define (type-const c)
  (cond
   [(flonum? c) 'Float]
   [(cflonum? c) 'Complex]
   [(integer? c) 'Int]
   [(string? c) 'String] 
   [(boolean? c) 'Bool]
   
   [(vector? c) `(Array ,(if (zero? (vector-length c))
			     ''anytype
			     (type-const (vector-ref c 0))))]

   [(eq? c 'BOTTOM) (make-tcell)]
   [(eq? c 'UNIT)   #()]
   
   ;; Temp, not sure if we're going to support symbols or not in the final language:
   [(symbol? c) 'Symbol]
   [(null? c) `(List ,(make-tcell))]
   [(list? c)
    (let ([types (map type-const c)])
      (let ([t1 (car types)])
	(for-each (lambda (t) (types-equal! t t1 c))
	  (cdr types))
	`(List ,t1)))]
   [else (error 'type-const "could not type: ~a" c)]))

;; Assign a type to a procedure application expression.
(define (type-app rator rattyp rands exp tenv non-generic-tvars)
  (DEBUGASSERT (tenv? tenv))
  (let ([result (make-tcell)])
    (types-equal! rattyp `(,@rands -> ,result) exp)
    result))

; ======================================================================

(define constant-typeable-as? 
  (lambda (c ty)
#;
    (cond 
     [(and (fixnum? c) (eq? ty 'Int))   (and (< c (expt 2 31)) (> c (- (expt 2 31))))]
     [(and (fixnum? c) (eq? ty 'Int16)) (and (< c (expt 2 15)) (> c (- (expt 2 15))))]
     [(and (flonum? c))                 (eq? ty 'Float)]
     [else #f])

    (IFCHEZ (import rn-match) (void))
    (if (eq? c 'BOTTOM) #t
	(match ty
	  [Int   (guard (fixnum? c))  (and (< c (expt 2 31)) (> c (- (expt 2 31))))]
	  [Int16 (guard (fixnum? c))  (and (< c (expt 2 15)) (> c (- (expt 2 15))))]
	  [Float (flonum? c)]
	  [Complex (cflonum? c)]
	  [Bool  (boolean? c)]
	  [String (string? c)]
	  [(List ,t) (and (list? c) (andmap (lambda (x) (constant-typeable-as? x t)) c))]
	  [#()   (eq? c 'UNIT)]
	  ;[else #f]
	  ))
    ))

;;; Annotate expressions/programs with types.
  
;; This is an alternative version of type-expression above which
;; simultaneously infers types and annotates the binding forms
;; (letrec, lambda) with per-variable type information.  As such, this
;; is a bunch of duplicated code, and poses risks for bugs.
;; <br> <br>
;;
;; Note, doesn't necessarily handle already annotated programs.
;;
;; .param exp - expression
;; .param tenv - type environment 
;; .param nongeneric - list of type variables that appear in lambda-arguments. (as opposed to lets)
;; .returns 2 values - annotated expression and expression's type
(define (annotate-expression exp tenv nongeneric)
  (define (extract-optional optional)
    (map (lambda (existing-type)
	   (case (length existing-type)
	     [(0) #f]
	     [(1) (car existing-type)]
	     ;; We assume the first one is a type, and the rest are extra metadata:
	     [else (ASSERT (type? (car existing-type)))
		   (car existing-type)]))
      optional))
  ;; Here's the main loop:
  (letrec ([l (lambda (exp)
    (match exp ;; NO DIRECT RECURSION ALLOWED:
      [,c (guard (simple-constant? c)) (values c (type-const c))]
      [(quote ,c)               (values `(quote ,c) (type-const c))]
      ;; Make sure it's not bound:
      [,prim (guard (symbol? prim) (not (tenv-lookup tenv prim)) (regiment-primitive? prim))
	     (values prim (prim->type prim))]

      ;; Here's the magic:
      [,v (guard (symbol? v))
	  (let ((entry (tenv-lookup tenv v)))
	    (if entry 
                (cond
                  ;; Let-bound polymorphism: 
		 [(tenv-is-let-bound? tenv v)
		   ;; Here's another call site which affects the LUB type assigned to the let-bound var.
		   (let ()
		     ;(printf "LETBOUND: ~s\n" v)
		     ;(unless (null? nongeneric) (printf "  NONGENERIC: ~s\n"  nongeneric))
		     ;(inspect entry)

		     (match entry
		       [(quote (,tv . (LATEUNIFY ,lubs ,general)))
			(let ([this-site (instantiate-type general nongeneric)])
			  ;(printf "Let-bound var! ~s with general type:  ~a\nlub at this site:\n  ~a\n" tv general this-site)
			  ;; CAREFUL!  Here we mutate the lubs on that LATEUNIFY.
			  (DEBUGASSERT (curry eq? 'LATEUNIFY) (cadadr entry))
			  (set-car! (cddadr entry)  
				    (if lubs
					`(LUB ,lubs ,this-site)
					this-site))
			  ;; We return the type for *this* varref.
			  (values v this-site)
			  )]
		       [,oth (error 'annotate-expression "let-bound var should be bound to LATEUNIFY: ~s" oth)])		   
		     )]
                  [else                   
                   (values v entry)])
		(error 'annotate-expression "no binding in type environment for var: ~a" v)))]
      [(if ,[l -> te tt] ,[l -> ce ct] ,[l -> ae at])
       (types-equal! tt 'Bool te) ;; This returns the error message with the annotated expression, oh well.
       (types-equal! ct at exp)
       (values `(if ,te ,ce ,ae) ct)]
      
      ;; Wavescope: this could be a set! to a state{} bound variable:
      [(set! ,v ,[l -> e et])  
       (let ([newexp `(set! ,v ,e)])
	 ;; The mutable var must be a Ref!
	 (types-equal! (ASSERT (tenv-lookup tenv v))
		       `(Ref ,et)
		       newexp)
	 ;; returns unit type:
	 (values newexp #()))]

#;
      [(for (,i ,[l -> start st]) ,[l -> end et] ,[bod bt])
       (let ([expr `(for [,i ,start ,end] ,bod)])
	 (unless (types-compat? st et) (raise-type-mismatch start end expr))
	 (values expr #()))]

      [(while ,[l -> tst tt] ,[l -> bod bt])
       (let ([expr `(while ,tst ,bod)])
	 (unless (types-compat? 'Bool tt) (raise-type-mismatch start end expr))
	 (values expr #()))]


      [(unionN ,[l -> e* t*] ...)
       (ASSERT (not (null? t*)))
       (let ([exp `(unionN ,@e*)])
	 ;; Make sure they're all equal:
	 (foldl (lambda (a b) (types-equal! a b exp) a)
	   (car t*) (cdr t*))
	 (values exp
		 (match (types-compat? '(Stream 'a) (car t*))
		   [(Stream ,t) `(Stream #(Int ,(instantiate-type t nongeneric)))]))
	 )]

      [(tuple ,[l -> e* t*] ...)  (values `(tuple ,@e*) (list->vector t*))]
      [(tupref ,n ,len ,[l -> e t])
       (unless (and (qinteger? n) (qinteger? len))
	 (error 'annotate-expression 
		"invalid tupref syntax, expected constant integer index/len, got: ~a/~a" n len))
       (values `(tupref ,n ,len ,e)
	       (let ((newtypes (list->vector (map (lambda (_) (make-tcell)) (iota (qinteger->integer len))))))
		 (types-equal! t newtypes exp)
		 (vector-ref newtypes (qinteger->integer n))))]
      
      [(begin ,[l -> exp* ty*] ...)
       (values `(begin ,@exp*) (last ty*))]

      [(for (,i ,[l -> start ty1] ,[l -> end ty2]) ,bod)
       ;; For now assume i is an integer...
       (types-equal! ty1 'Int exp)
       (types-equal! ty2 'Int exp)
       (let ([tenv (tenv-extend tenv (list i) '(Int) #f)])
	 (mvlet ([(bod ty) (annotate-expression bod tenv nongeneric)])
	   (values `(for (,i ,start ,end) ,bod) ty)))]

      
      [(lambda (,v* ...) ,bod) (annotate-lambda v* bod 
						(map (lambda (_) `(quote ,(make-tvar))) v*)
						tenv nongeneric)]
      [(lambda (,v* ...) ,types ,bod) (annotate-lambda v* bod types tenv nongeneric)]

      [(let ([,id* ,optional ... ,rhs*] ...) ,bod)
       (annotate-let id* rhs* bod (extract-optional optional) tenv nongeneric)]

      ;; TODO: Doesn't actually take optional types into account. FIXME FIXME
      ;; Allowing annotations, but ignoring them.
      [(,letrec ([,id* ,optional ... ,rhs*] ...) ,bod)  (guard (memq letrec '(letrec lazy-letrec)))
       (annotate-letrec id* (extract-optional optional) rhs* bod tenv nongeneric letrec)]


      ;; DUPLICATING! these two cases to give good error messages for badly typed apps:
      [(src-pos ,p (,prim ,[l -> rand* t*] ...))
       (guard (regiment-primitive? prim)
	      (not (memq prim '(tuple unionN))))
       (DEBUGASSERT (andmap type? t*))
       (values `(,prim ,@rand*)
	       (type-app prim (prim->type prim) t* exp tenv nongeneric))]
      [(src-pos ,p (,app ,origrat ,[l -> rand* t*] ...))
       (guard (memq app '(app construct-data)))
       (DEBUGASSERT (andmap type? t*))
       (mvlet ([(rator t1) (l origrat)])
	 (values `(src-pos ,p (,app ,rator ,@rand*))
		 (type-app origrat t1 t* exp tenv nongeneric)))]
     
      [(,prim ,[l -> rand* t*] ...)
       (guard (regiment-primitive? prim))
       (DEBUGASSERT (andmap type? t*))
       (values `(,prim ,@rand*)
	       (type-app prim (prim->type prim) t* exp tenv nongeneric))]

      ;[(app ,rat ,rands* ...)  (l `(,rat ,rands* ...))]

      [(,app ,origrat ,[l -> rand* t*] ...)
       (guard (memq app '(app construct-data)))
       (DEBUGASSERT (andmap type? t*))
       (mvlet ([(rator t1) (l origrat)])
	 (values `(,app ,rator ,@rand*)
		 (type-app origrat t1 t* exp tenv nongeneric)))]


      ;; Incorporate type assertions.
      ;; ----------------------------------------

      ;; Push ascriptions inside lambdas:
      ;; This helps make the grammar a little nicer subsequently.
      ;; UNFINISHED      
      ;[(assert-type ,ty (lambda () () ,bod)) ......]

      ;; This is a special case for constants.
      [(assert-type ,ty (quote ,n))
       (if (constant-typeable-as? n ty)
	   (values `(assert-type ,ty (quote ,n))  ty)
	   (error 'hm_type_inference "constant ~s was labeled with type ~s which doesn't match"
		  `(quote ,n) ty))]
      [(assert-type ,ty ,[l -> e et])
       (let ([newexp `(assert-type ,ty ,e)])	 
	 (types-equal! (instantiate-type ty '()) et newexp)
	 (values `(assert-type ,ty ,e)
		 et))]

      [(src-pos ,p ,[l -> e et]) (values `(src-pos ,p ,e) et)]
      ;; ----------------------------------------

      ;; Allowing unlabeled applications for now:
      [(,rat ,rand* ...) (guard (not (regiment-keyword? rat)))
       (warning 'annotate-expression "allowing arbitrary rator: ~a\n" rat)
       (l `(app ,rat ,@rand*))]

      [,other (error 'annotate-expression "could not type, unrecognized expression: ~s" other)]
      ))]) ;; End main-loop "l"    

    ;; Initiate main loop:
    #;(let ([result (l exp)])
      ;; On the way out we do late unifications:
      (do-all-late-unifies! result)
      result
      )
  (l exp)
  ))

;; Internal helper.
(define (valid-user-type! t)
  (unless (or (not t) (type? t))
    (error 'type-checker "invalid explicit type annotation: ~s" t)))

;; Assign a type to a procedure declaration.
;; .param ids   The lambda formals.
;; .param body  The lambda body.
;; .param tenv  The type environment.
;; .returns Two values: a new (annotated) expression, and a type.
(define annotate-lambda
  (lambda (ids body inittypes tenv nongeneric)
    (IFCHEZ (import rn-match) (void))
    (DEBUGASSERT (= (length ids) (length inittypes)))
    (ASSERT (andmap symbol? ids)) ;; For now, no patterns.
    (for-each valid-user-type! inittypes)
    ;; No optional type annotations currently.
    (let* ([argtypes  (map (lambda (_) (make-tcell)) ids)]
           [newnongen (map (lambda (x) (match x ['(,n . ,_) n])) argtypes)])
      ;; Now unify the new type vars with the existing annotations.
      (map (lambda (new old)
	     (if old (types-equal! new (instantiate-type old '())
			   `(lambda ,ids ,inittypes ,body))))
	argtypes inittypes)

      (mvlet ([(newbod bodtype) (annotate-expression body (tenv-extend tenv ids argtypes)
                                                     (append newnongen nongeneric))])
        (values `(lambda ,ids ,argtypes ,newbod)
                `(,@argtypes -> ,bodtype))))))

;; [2006.07.18] WS: This is for plain-old lets.  No recursion.
(define annotate-let
  (lambda (id* rhs* bod inittypes tenv nongeneric)
    (define (f e) (annotate-expression e tenv nongeneric))
    (IFCHEZ (import rn-match) (void))
    (ASSERT (andmap symbol? id*)) ;; For now, no patterns.
    (for-each  valid-user-type! inittypes)
    (match rhs*
      [(,[f -> newrhs* rhsty*] ...)
       ;(printf "ANNLET: ~s   ~s\n" rhsty* inittypes)
       (let* ([newtypes (map (lambda (new old) 
			       (if old (types-equal! new (instantiate-type old '())
						     `(let ,(map list id* inittypes rhs*) ,bod)))
			       (make-tcell new)
			       )
			  rhsty* inittypes)]
	      [tenv 
	       (tenv-extend tenv id* newtypes (inferencer-let-bound-poly))
	       #;
	       (if (not (inferencer-let-bound-poly))
		   (tenv-extend tenv id* newtypes #f)
		   (foldl (lambda (tenv)
			    )))
		
	       ])
	 (mvlet ([(bod bodty) (annotate-expression bod tenv nongeneric)])
	   (values `(let ,(map list id* newtypes newrhs*) ,bod) bodty)))])))

;; Assign a type to a Regiment letrec form.  
;; .returns 2 values: annoted expression and type
(define annotate-letrec 
  (let ([map-ordered2 
         (lambda (f ls1 ls2)
           (let loop ([ls1 ls1]
                      [ls2 ls2]
                      [acc '()])
             (cond
               [(and (null? ls1) (null? ls2)) (reverse! acc)]
               [(or (null? ls1) (null? ls2)) (error 'map-ordered2 "lists are different lengths")]
               [else
                (cons (f (car ls1) (car ls2)) 
                      (loop (cdr ls1) (cdr ls2) acc))])))])
    (lambda (id* existing-types rhs* bod tenv nongeneric letrecconstruct) 
      (ASSERT (andmap symbol? id*)) ;; For now, no patterns.

      (for-each  valid-user-type! existing-types)
      ;; Make new cells for all these types
      (let* ([rhs-types (map (lambda (_) (make-tcell)) id*)]
	     ;; Unify rhs-types with pre-existing types.
	     [_ (map (lambda (new old) 
		       (if old 
			   (types-equal! new (instantiate-type old '())
					 `(letrec (,'... [,new ,old] ,'...) ,'...))))
		  rhs-types existing-types)]
             [tenv (tenv-extend tenv id* rhs-types (inferencer-let-bound-poly))])
        ;; Unify all these new type variables with the rhs expressions
        (let ([newrhs* 
               (map-ordered2 (lambda (type rhs)
			       (IFCHEZ (import rn-match) (void))
			       (match type
				 [(quote (,v . ,_))
				  (DEBUGASSERT tenv? tenv)
				  ;; For our own RHS we are "nongeneric".
				  (mvlet ([(newrhs t) (annotate-expression rhs tenv (cons v nongeneric))])
				    (types-equal! type t rhs)
                                    ;(inspect `(GOTNEWRHSTYPE ,t ,type))
				    newrhs)
				  ]))
                             rhs-types rhs*)])
          (mvlet ([(bode bodt) (annotate-expression bod tenv nongeneric)])
            (values `(,letrecconstruct ,(map list id* rhs-types newrhs*) ,bode) bodt)))))))


;; This lifts export-type over expressions.
(define (export-expression e)
  (IFCHEZ (import rn-match) (void))
  (match e ;; match-expr
    [,c (guard (simple-constant? c)) c]
    [,v (guard (symbol? v)) v]
    [(quote ,c)       `(quote ,c)]
    [,prim (guard (symbol? prim) (regiment-primitive? prim))  prim]
    [(assert-type ,[export-type -> t] ,[e]) `(assert-type ,t ,e)]
    [(src-pos ,p ,[e]) `(src-pos ,p ,e)]
    [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
    [(lambda ,v* (,[export-type -> t*] ...) ,[bod]) `(lambda ,v* ,t* ,bod)]
    [(tuple ,[e*] ...) `(tuple ,@e*)]
    [(tupref ,n ,[e]) `(tupref ,n ,e)]
    [(unionN ,[e*] ...) `(unionN ,@e*)]
    [(set! ,v ,[e]) `(set! ,v ,e)]
    [(begin ,[e] ...) `(begin ,@e)]
    [(for (,i ,[s] ,[e]) ,[bod]) `(for (,i ,s ,e) ,bod)]
    [(while ,[tst] ,[bod]) `(while ,tst ,bod)]
    [(let ([,id* ,[export-type -> t*] ,[rhs*]] ...) ,[bod])
     `(let ,(map list id* t* rhs*) ,bod)]
    [(,letrec ([,id* ,[export-type -> t*] ,[rhs*]] ...) ,[bod])
     (guard (memq letrec '(letrec lazy-letrec)))
     `(,letrec ,(map list id* t* rhs*) ,bod)]
    [(app ,[rat] ,[rand*] ...) `(app ,rat ,@rand*)]
    [(construct-data ,[rat] ,[rand*] ...) `(construct-data ,rat ,@rand*)]
    [(,prim ,[rand*] ...) (guard (regiment-primitive? prim))
     `(,prim ,@rand*)]
    ;; HACK HACK HACK: Fix this:
    ;; We cheat for nums, vars, prims: 
    ;;[,other other]; don't need to do anything here...
    ;;       [,other (error 'annotate-program "bad expression: ~a" other)]
    ))

;; This traverses the expression and does any LATEUNIFY's
(define (do-all-late-unifies! e)
  (IFCHEZ (import rn-match) (void))
  (match e ;; match-expr
    [,c (guard (simple-constant? c))                                 (void)]
    [,v (guard (symbol? v))                                   (void)]
    [(quote ,c)                                               (void)]
    [,prim (guard (symbol? prim) (regiment-primitive? prim))  (void)]
    ;; The type occurring here isn't instantiated (thus doesn't contain late unifies)
    [(assert-type ,t ,[e])                                    (void)]
    [(src-pos ,t ,[e])                                        (void)]
    [(if ,[t] ,[c] ,[a])                                      (void)]
    [(lambda ,v* (,[do-late-unify! -> t*] ...) ,[bod])         (void)]
    [(tuple ,[e*] ...)                                        (void)]
    [(tupref ,n ,[e])                                         (void)]
    [(unionN ,[e*] ...)                                       (void)]
    [(set! ,v ,[e])                                           (void)]
    [(begin ,[e] ...)                                         (void)]
    [(for (,i ,[s] ,[e]) ,[bod])                              (void)]
    [(while ,[tst] ,[bod])                                    (void)]
    [(let ([,id* ,[do-late-unify! -> t*] ,[rhs*]] ...) ,[bod]) (void)]
    [(,letrec ([,id* ,[do-late-unify! -> t*] ,[rhs*]] ...) ,[bod])
     (guard (memq letrec '(letrec lazy-letrec)))              (void)]
    [(app ,[rat] ,[rand*] ...)                                (void)]
    [(construct-data ,[rat] ,[rand*] ...)                     (void)]
    [(,prim ,[rand*] ...) (guard (regiment-primitive? prim))  (void)]
    ))

;; This simply removes all the type annotations from an expression.
;; This would  be a great candidate for a generic traversal:
(define (strip-types p)
;  (IFCHEZ (import rn-match) (void))
  (define (process-expression e)
    (match e
    [(lambda ,v* ,optionaltypes ,[bod]) `(lambda ,v* ,bod)]
    [(lambda ,v* ,[bod])                `(lambda ,v* ,bod)]
    [(,let ([,id* ,optionaltype ... ,[rhs*]] ...) ,[bod])    
     (guard (memq let '(let let* letrec lazy-letrec)))
     `(,let ([,id* ,rhs*] ...) ,bod)]
   
    [,c (guard (simple-constant? c)) c]
    [(quote ,c)       `(quote ,c)]
    [(return ,[e]) `(return ,e)]
    [,var (guard (symbol? var)) var]
    [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
    
    [(set! ,v ,[e]) `(set! ,v ,e)]
    [(begin ,[e] ...) `(begin ,e ...)]
    [(for (,i ,[s] ,[e]) ,[bod]) `(for (,i ,s ,e) ,bod)]
    [(while ,[tst] ,[bod]) `(while ,tst ,bod)]
    [(iterate ,[f] ,[s]) `(iterate ,f ,s)]

    [(unionN ,[args] ...) `(unionN ,args ...)]
    [(tuple ,[args] ...) `(tuple ,args ...)]
    [(tupref ,n ,m ,[x]) `(tupref ,n ,m ,x)]

    [(assert-type ,t ,[e]) e]
    [(src-pos     ,p ,[e]) e]

    [(app ,[rat] ,[rand*] ...) `(app ,rat ,rand* ...)]
    [(construct-data ,[rat] ,[rand*] ...) `(construct-data ,rat ,rand* ...)]
    
    [(,prim ,[rand*] ...)
     (guard (regiment-primitive? prim))
     `(,prim ,rand* ...)]
    
    [,other (error 'strip-types "bad expression: ~a" other)]
    ))
  (match p 
    [(,lang '(program ,expr ,_ ...)) 
     `(,lang '(program ,(process-expression expr) ,_ ...))]
    [,expr (process-expression expr)]
    ;[,other (error 'strip-types "Bad program, maybe missing boilerplate: \n~s\n" other)]
    ))
  



;; This annotates the program, and then exports all the types to their
;; external form (stripped of mutable cells on the tvars).
;; .param a program with or without boilerplate.
;; .returns 1 or 2 values: new program and toplevel type
;;                         If input is with-boilerplate, output is one value.
;; 
;; NOTE!  We have to run the type checking TWICE to make everything settle down.
;; This is a result of our strategy of recording LUB types for lets.
(define (annotate-program p)
  (IFCHEZ (import rn-match) (void))
  (match p
    [(,lang '(program ,e ,meta ...))
     ;(annotate-program-once (annotate-program-once p))
     (annotate-program-once p)
     ]
    [,oth
     (let-values ([(e t) (annotate-program-once oth)])
       (values e t)
       ;(annotate-program-once e)
       )]))

;; This is the real thing:
(define (annotate-program-once p)
  (IFCHEZ (import rn-match) (void))
  (let ([Expr 
	 (lambda (p tenv)
	   (mvlet ([(e t) (annotate-expression p tenv '())])
	     (do-all-late-unifies! e)
	     ;; Now strip mutable cells from annotated expression.
	     (values (export-expression e)
		     (export-type t)))
	   )])

    ;; Accepts either with-boilerplate or without.
    (match p
      [(,lang '(program ,bod ,metadat* ... ,type))
       (ASSERT type? type)
       (mvlet ([(e t) (Expr bod (sumdecls->tenv
				 (cdr (or (assq 'union-types metadat*) '(union-types)))))])
	 `(typechecked-lang '(program ,e ,@metadat* ,t)))]
      [,other 
       (Expr other (empty-tenv))])))

(define (sumdecls->tenv decl*)
  (IFCHEZ (import rn-match) (void))
  (define (sumdecl->tbinds decl tenv)
    (match decl 
      [((,name) [,tycon* ,ty*] ... ) (guard (symbol? name))
       (tenv-extend (empty-tenv) tycon* (map (lambda (ty) `(,ty -> (Sum ,name))) ty*))
       ]))
  (foldl sumdecl->tbinds (empty-tenv) decl*))

;; This is a front-end to the above which takes the list of metadata
;; associated with a toplevel '(program ...) form and builds the init-tenv.
(define (grab-init-tenv metadata)
  (sumdecls->tenv 
   (cdr (or (assq 'union-types metadata)
	    '(union-types)))))

      
; ======================================================================

;;; The unifier.

;; This is a front-end to the unifier which uses it to tell you if two
;; types are compatible.  This is inefficient, but is necessitated by
;; the way I wrote the unifier.  (It throws an error on failed unification.)
;; 
;; types-compat? returns its second arguments post-unified form.
(define (types-compat? t1 t2)
  (call/1cc 
   (lambda (k) 
     (fluid-let ([type-error (lambda args (k #f))])
       ;; We export first, this makes absolutely sure that we don't 
       ;; destroy any of the mutable state in t1 or t2.
       (let ([inst1 (make-tcell (instantiate-type (export-type t1) '()))]
	     [inst2 (make-tcell (instantiate-type (export-type t2) '()))])
;	 (printf "Input types: ~s\n" (cons t1 t2) )
;	 (printf "Initial instantiations: ~s\n" (cons inst1 inst2) )
	 (types-equal! inst1 inst2 (void))
;	 (printf "Final instantiations: ~s\n" (cons inst1 inst2) )
	 ;;(k (export-type inst1))
	 (k (export-type inst2))
	 )))))

;; This asserts that two types are equal.  Mutates the type variables
;; to reflect this constraint.
(define (types-equal! t1 t2 exp)
  (IFCHEZ (import rn-match) (void))
  (DEBUGASSERT (and (type? t1) (type? t2)))
  (DEBUGASSERT (compose not procedure?) exp)
  (match (list t1 t2)
    [[,x ,y] (guard (eqv? t1 t2)) (void)]

    [[(LATEUNIFY ,_ ,t1) ,t2]  (types-equal! t1 t2 exp)]
    [[,t1 (LATEUNIFY ,_ ,t2)]  (types-equal! t1 t2 exp)]

    [[',tv1 ',tv2] (guard (eqv? tv1 tv2)) (void)] ;; alpha = alpha
    [[',tv ,ty] (tvar-equal-type! t1 t2 exp)]
    [[,ty ',tv] (tvar-equal-type! t2 t1 exp)]
    [[,x ,y] (guard (symbol? x) (symbol? y))
     (raise-type-mismatch x y exp)]

    [[(NUM ,tv1) (NUM ,tv2)] (tvar-equal-type! t1 t2 exp)]
    [[(NUM ,x) ,numty]   (guard (symbol? numty) (memq numty num-types))
     (tvar-equal-type! t1 numty exp)]
    [[,numty   (NUM ,x)] (guard (symbol? numty) (memq numty num-types))
     (tvar-equal-type! t2 numty exp)]

;; [2007.03.15] Type aliases already resolved by resolve-type-aliases:
#;
    ;; If one of them is a symbol, it might be a type alias.
    [[,x ,y] (guard (or (symbol? x) (symbol? y)))
     (let ([sym    (if (symbol? x) x y)]
	   [nonsym (if (symbol? x) y x)])
       (let ([entry (assq sym regiment-type-aliases)])
	 (if entry 
	     ;; Instantiate the alias and unify.
	     (types-equal! (instantiate-type (cadr entry) '()) nonsym exp)
	     (raise-type-mismatch x y exp))))]

    [[#(,x* ...) #(,y* ...)]
     (guard (= (length x*) (length y*)))
     (for-each (lambda (t1 t2) (types-equal! t1 t2 exp)) x* y*)]

    ;; Ref will fall under this category:
    [[(,x1 ,xargs ...) (,y1 ,yargs ...)]
     (guard (symbol? x1) (symbol? y1)
	    (not (memq '-> xargs))
	    (not (memq '-> yargs))
	    (= (length xargs) (length yargs)))
     (if (not (eq? x1 y1))
	 (type-error 'types-equal! "type constructors do not match: ~a and ~a in ~a" x1 y1 exp))
;     (types-equal! x1 y1 exp)
     (for-each (lambda (t1 t2) (types-equal! t1 t2 exp)) xargs yargs)]

;; [2005.12.07] Just got a "wrong number of arguments" error that might be a match bug.
;;    [[(,xargs ... -> ,x) (,yargs ... -> ,y)] 
    ;; Working around this in a lame way:
    [[,x  (,yargs ... -> ,y)] 
     (match x 
       [(,xargs ... -> ,x)
	(if (not (= (length xargs) (length yargs)))
	    (raise-wrong-number-of-arguments t1 t2 exp))
	(for-each (lambda (t1 t2) (types-equal! t1 t2 exp))
	  xargs yargs)
	(types-equal! x y exp)]
       [,other (type-error 'types-equal!
		      "procedure type ~a\nDoes not match: ~a\n\nUnexported versions: ~a\n  ~a\n"
		      (export-type `(,@yargs -> ,y))
		      (export-type other)
		      `(,@yargs -> ,y)
		      other)])]
    [,otherwise (raise-type-mismatch t1 t2 exp)]))
 
;; This helper mutates a tvar cell while protecting against cyclic structures.
(define (tvar-equal-type! tvar ty exp)
  (IFCHEZ (import rn-match) (void))
  (DEBUGASSERT (type? ty))
  (match tvar ;; Type variable should be a quoted pair.
    [(,qt ,pr) (guard (memq qt '(NUM quote)))
     (if (not (pair? pr))
	 (error 'tvar-equal-type! "bad tvar here, no cell: ~a" tvar))
     (if (cdr pr)

	 (types-equal! (cdr pr) ty exp)

	 (begin (no-occurrence! (tcell->name tvar) ty exp)
		(set-cdr! pr ty)))]))

;; This returns the least-upper bound of two types.  That is, the
;; least-general type that is a superset of both input types.
(define (LUB t1 t2)
  (IFCHEZ (import rn-match) (void))
;  `(LUB ,t1 ,t2)
  ;; UNFINISHED:

  (match (list t1 t2)
    [[,x ,y] (guard (eqv? x y)) x]
    ;[[',tv1 ',tv2] (guard (eqv? tv1 tv2)) ] ;; alpha = alpha

    ;; Two num-types join at NUM:
    [[,x ,y] (guard (memq x num-types) (memq y num-types))
     `(NUM ,(make-tvar))]

    ;; This is a mismatch of basic types:
    [[,x ,y] (guard (symbol? x) (symbol? y)) `(quote ,(make-tvar))]

    [[',tv ',ty] (ASSERT symbol? tv) (ASSERT symbol? ty)
     `(quote ,(if (eqv? tv ty) tv (make-tvar )))]
    [[',tv ,ty]  (ASSERT symbol? tv)
     `(quote ,tv)]
    [[,ty ',tv] (ASSERT symbol? tv)
     `(quote ,tv)]

    [[(NUM ,tv) (NUM ,ty)] (DEBUGASSERT symbol? tv) (DEBUGASSERT symbol? ty)
     `(NUM ,(if (eqv? tv ty) tv (make-tvar)))]
    [[(NUM ,tv) ,ty] (guard (symbol? ty) (memq ty num-types))
     (DEBUGASSERT symbol? tv)
     `(NUM ,tv)]
    [[,ty (NUM ,tv)]  (guard (symbol? ty) (memq ty num-types))
     (DEBUGASSERT symbol? tv)
     `(NUM ,tv)]

    ;; A NUM and anything else is just TOP:
    [[,ty (NUM ,tv)] (DEBUGASSERT symbol? tv) `(quote ,(make-tvar))]
    [[(NUM ,tv) ,ty] (DEBUGASSERT symbol? tv) `(quote ,(make-tvar))]

;; [2007.03.15] Type aliases already resolved by resolve-type-aliases:
#;
    ;; If one or both of them is a symbol, it might be a type alias.
    [[,x ,y] (guard (or (symbol? x) (symbol? y)))
     (let* ([success #f]
	    [dealias (lambda (s) 
		       (cond [(assq s regiment-type-aliases) => 			      
			      (lambda (entry)
				(set! success #t)
				(instantiate-type (cadr entry)))]
			     [else s]))]
	    [x* (dealias x)]
	    [y* (dealias y)])
       (if success
	   (LUB x* y*) ;; Now take the LUB:
	   ;; Otherwise they're just mismatched basic types:
	   `(quote ,(make-tvar))
	   ))]

    [[#(,x* ...) #(,y* ...)]
     (if (= (length x*) (length y*))
	 (list->vector (map (lambda (t1 t2) (LUB t1 t2)) x* y*))
	 ;; Otherwise, the LUB is top:
	 `(quote ,(make-tvar))
	 )]
    ;; Tuple and non-tuple:
    [[#(,x* ...) ,y]  `(quote ,(make-tvar))]
    [[,y #(,x* ...)]  `(quote ,(make-tvar))]


    ;; Two type constructors (Ref falls under this umbrella)
    [[(,x1 ,xargs ...) (,y1 ,yargs ...)]
     (guard (symbol? x1) (symbol? y1)
	    (not (memq '-> xargs))
	    (not (memq '-> yargs))
	    (= (length xargs) (length yargs)))
     (if (not (eq? x1 y1))
	 `(quote ,(make-tvar))	 
	 (cons x1 (map (lambda (t1 t2) (LUB t1 t2)) xargs yargs))
	 )]

    ;; Just one type constructor gives us TOP:
    [[,x ,y] (guard (list? x) (not (list? y)))  `(quote ,(make-tvar))]
    [[,x ,y] (guard (list? y) (not (list? x)))  `(quote ,(make-tvar))]

    [[,x  (,yargs ... -> ,y)]
     (match x 
       [(,xargs ... -> ,x)
	(if (not (= (length xargs) (length yargs)))
	    `(quote ,(make-tvar)) ;; Return top
	    `(,@(map LUB xargs yargs) -> ,(LUB x y))
	    )]
       [,other `(quote ,(make-tvar))])]
    
    [,otherwise (error 'LUB "this function is probably not finished, types unmatch: ~s and ~s" t1 t2)]
    [,otherwise (raise-type-mismatch t1 t2 "unknown expression (LUB)")]
    ))
  
  
;; This makes sure there are no cycles in a tvar's mutable cell.
;; .returns #t if there are no loops, or throws an error otherwise.
(define (no-occurrence! tvar ty exp)
  (IFCHEZ (import rn-match) (void))
  (DEBUGASSERT (type? ty))
  ;; This is a lame hack to repair certain cycles.
  (if (match ty
	[(,qt (,tyvar . ,_)) (guard (memq qt '(NUM quote)) (eq? tyvar tvar)) #t]
	[(,qt (,tyvar . ,[tyt])) (guard (memq qt '(NUM quote))) tyt]
	[,else #f])

      ;; HACK: VERIFY CORRECTNESS::
      ;; Ok, this is recursive, but it's A=B=A, not some more complex
      ;; recursive type constraint.
      (begin 
	(when (regiment-verbose)
	  (warning 'no-occurrence! "encountered A=B=A type constraint: ~s" ty))
	(match ty
	  [(,qt ,tvarpair)	   
	   ;; Ouch, mutating in the guard... Nasty.
	   (guard  (memq qt '(quote NUM))
		   (match tvarpair
		     [(,outer . (,qt (,inner . ,targettyp))) 
		      (guard (memq qt '(NUM quote)) (eq? inner tvar))
		      ;; Short circuit the equivalence, this doesn't destroy
		      ;; information that's not already encoded.
		      (set-cdr! tvarpair targettyp)
		      (when (regiment-verbose)
			(printf "  SHORT CIRCUITED: ~s to ~s\n" outer targettyp))
		      ]
		     [(,outer . (,qt ,[deeper])) (guard (memq qt '(NUM quote))) (void)]
		     [else (error 'no-occurrence! "this is an implementation bug.")]))
	   ;; Guard already did the work:
	   (void)]	  	 
	  [,other (error 'no-occurrence! "there's a bug in this implementation. Unhandled: ~s" other)])
	)
      
  (match ty
    [#f #t]
    [,s (guard (symbol? s)) #t]
    [(,qt (,tyvar . ,[tyt])) (guard (memq qt '(NUM quote)))
     (if (equal? tyvar tvar)
	 (raise-occurrence-check tvar ty exp))]
    [(,[arg*] ... -> ,[res]) res]
    [(,C ,[t*] ...) (guard (symbol? C)) #t] ; Type constructor
    [#(,[t*] ...) #t]
;    [,other (inspect (vector other tvar))]
    [,other (error 'no-occurrence! "malformed type: ~a" ty)]
    )))


; ======================================================================
;; Printing the type-signatures inside a large expressions:

;; Prints a type in a WaveScript format way rather than the raw sexp.
(define (print-type t . p)
  (IFCHEZ (import rn-match) (void))
  (let ([port (if (null? p) (current-output-port) (car p))])
    (define (loop t) 
      (match t
	[(quote ,[var]) (++ "'" var)]
	[(NUM ,[var]) (++ "#" var)]
	[(-> ,[b]) (++ "() -> " b)]
	;[(,[left] -> ,[right]) (++ "(" left ") -> " right)]
	[(,left -> ,[right]) 
	 (if (arrow-type? left)
	     (++ "(" (loop left) ") -> " right)
	     (++ (loop left) " -> " right))]
	[(,[arg*] ... -> ,[b]) (++ "(" (apply string-append (insert-between ", " arg*)) ")"
				   " -> "b)]
	[#(,[x*] ...)
	 (++ "(" (apply string-append (insert-between " * " x*)) ")")]
	;; [2006.12.01] Removing assumption that TC's have only one arg:
	[(,[tc] ,[arg*] ...)
					;	 (++ tc "(" (apply string-append (insert-between ", " arg*)) ")")]
	 (++ "(" tc " " (apply string-append (insert-between " " arg*)) ")")]
	[,sym (guard (symbol? sym))
	      (symbol->string sym)]
	[,other (error 'print-type "bad type: ~s" other)]))
    (display 
     ;; Prettification: we drop the outer parens:
     (match t 
       [(,tc ,[loop -> arg*] ...) (guard (symbol? tc) (not (eq? tc 'quote)))
	(++ (symbol->string tc) " " (apply string-append (insert-between " " arg*)))]
       [,t (loop t)])
     port)))

#|

;; Bad:
((type () s1 (Stream #(Int16 Int16 Int16)))
 (type () x Int16)
 (type BASE (Stream Int16) ()))

;; Correct
((type s1 (Stream #(Int16 Int16 Int16)) ())
 (type x Int16 ())
   (type BASE (Stream Int16) ()))


(match '(foo '(program bod meta meta2 Int))
    [(,lang '(program ,[body] ,meta ...))
     (inspect (vector meta ty))
     (append body `((type BASE ,(last meta) ())))]
    [,oth 9999])

|#

;; Expects a fully typed expression
(define (print-var-types exp max-depth . p)
  (IFCHEZ (import rn-match) (void))
  (let ([port (if (null? p) (current-output-port) (car p))])
    
    (define (get-var-types exp)
      (match exp 

       [(,lang '(program ,[body] ,meta ... ,ty))
	 (append body `((type BASE ,ty ())))]

       [,c (guard (simple-constant? c)) '()]
       [,var (guard (symbol? var))  `()]       
       [(quote ,c)       '()]
       [(assert-type ,t ,[e]) e]
       [(src-pos     ,p ,[e]) e]
       [(set! ,v ,[e]) e]
       [(begin ,[e*] ...) (apply append e*)]
       [(for (,i ,[s] ,[e]) ,[bodls]) (cons `[type ,i Int ()] (append s e bodls))]
       [(while ,[tstls] ,[bodls]) (append tstls bodls)]

       [(if ,[t] ,[c] ,[a]) (append t c a)]
       [(tuple ,[args] ...) (apply append args)]
       [(tupref ,n ,m ,[x]) x]
       [(unionN ,[args] ...) (apply append args)]

       [(,let ([,id* ,t* ,[rhs*]] ...) ,[bod]) 
	(guard (memq let '(let letrec lazy-letrec)))
	(append (apply append 
		       (map (lambda (id t rhsls)
			      `([type ,id ,t ,rhsls]))
			 id* t* rhs*))
		bod)]
       [(lambda ,v* ,t* ,[bodls])   bodls]
       [(,app ,[rat] ,[rand*] ...) (guard (memq app '(app construct-data)))
	(apply append rat rand*)]
       [(,prim ,[rand*] ...)
	 (guard (regiment-primitive? prim))
	 (apply append rand*)]
	[,other (error 'print-var-types "bad expression: ~a" other)]))

   
    ;(inspect (get-var-types exp))
    (let loop ([x (get-var-types exp)] [depth 0] [indent " "])
      (if (= depth max-depth) (void)
	  (match x
	    [() (void)]
	    [(type ,v ,t ,subvars)
	     (unless (eq? v '___VIRTQUEUE___) 	 ;; <-- HACK: 
	       (fprintf port "~a~a :: " indent v)
	       (print-type t port) (newline port))
	     (loop subvars (fx+ 1 depth) (++ indent "  "))]
	    [,ls (guard (list? ls))
		 (for-each (lambda (x) (loop x depth indent))
		   ls)]
	    [,other (error 'print-var-types "bad result from get-var-types: ~a" other)])))
      ))


; ======================================================================
;;; Unit tests.

;; Unit tests.
(define-testing these-tests
  `([(begin (reset-tvar-generator) (let ((x (prim->type 'car))) (set-cdr! (car (cdaddr x)) 99) x))
     ((List '(a . 99)) -> '(a . 99))]

    ["NUM must have vars attached" (type? #((NUM Int) (NUM Float))) #f]
    [(type? '(NUM a)) #t]
    [(type? '(Int -> (NUM a))) #t]

    [(,type-expression '(if #t 1. 2.) (empty-tenv))         Float]
    [(mvlet ([(p t ) (annotate-program '(lambda (x) (g+ x (gint 3))))]) t)
     ((NUM unspecified) -> (NUM unspecified))]
    
    [(export-type ''(f . Int)) Int]
    [(export-type (,type-expression '(+_ 1 1) (empty-tenv))) Int]
    [(export-type (,type-expression '(cons 3 (cons 4 '())) (empty-tenv))) (List Int)]
    [(export-type (,type-expression '(cons 1 '(2 3 4)) (empty-tenv))) (List Int)]
    [(export-type (,type-expression '(cons 1 '(2 3 4.)) (empty-tenv))) error]

    [(export-type (mvlet ([(_ t) (,annotate-lambda '(v) '(+_ v v) '(Int) (empty-tenv) '())]) t))
     (Int -> Int)]
    [(export-type (mvlet ([(_ t) (,annotate-lambda '(v) '(+_ v v) '('alpha) (empty-tenv) '())]) t))
     ;((NUM unspecified) -> (NUM unspecified))
     (Int -> Int)]
    
    ["types-equal!: make sure that (by convention) the first argument is mutated"
     (let ([x ''(a . #f)] [y ''(b . #f)]) (types-equal! x y (empty-tenv)) x)
     '(a . '(b . #f))]
   
    ["Invalid explicitly annotated type"
     (export-type (mvlet ([(_ t) (,annotate-lambda '(v) '(+_ v v) '(String) (empty-tenv) '())]) t))
     error]

    ["Explicitly typed let narrowing identity function's signature"
     (mvlet ([(_ t) (annotate-program '(let ([f (Int -> Int) (lambda (x) x)]) f))]) t)
     (Int -> Int)]
    [(mvlet ([(_ t) (annotate-program '(let ([f (Int -> Int) (lambda (x) x)]) (app f "h")))]) t)
     error]

    [(export-type (mvlet ([(_ t) (,annotate-lambda '(v) 'v '('alpha) (empty-tenv) '())]) t))
     ,(lambda (x)
	(match x
	  [(',a -> ',b) (eq? a b)]
	  [,else #f]))]

    [(export-type (,type-expression '((lambda (v) v) 3) (empty-tenv))) Int]

    [(export-type (,type-expression '(lambda (y) (letrec ([x y]) (+_ x 4))) (empty-tenv)))
     (Int -> Int)]

    [(export-type (,type-expression '(rmap (lambda (n) (sense "light" n)) world) (empty-tenv)))
     (Area Int)]
    
    [(export-type (,type-expression '(tuple 1 2.0 3) (empty-tenv)))
     #3(Int Float Int)]

    [(export-type (,type-expression '(lambda (x) (tupref 0 3 x)) (empty-tenv)))
     ,(lambda (x)
	(match x
	  [(#(,v1 ,_ ,__) -> ,v2) (equal? v1 v2)]
	  [,else #f]))]

    [(export-type (,type-expression 
		   '(letrec ([f (lambda (x) x)])
		      (tuple (app f 3) "foo" f))
		  (empty-tenv)))
     #(Int String ('unspecified -> 'unspecified))
     ;#(Int String (Int -> Int))
     ]
  

  ["Types-compat?"
   (types-compat? '(Stream Node) '(Stream 'a) )
   (Stream Node)]
  [(types-compat? '(Stream Node) '(Stream Node)) (Stream Node)]
  ;; This is kind of lame:
;  [(types-compat? '(Stream Node) 'Anchor) Anchor]
;  [(types-compat? 'Anchor 'Anchor) Anchor]
;  [(types-compat? 'Anchor '(Area Int)) #f]
;  [(types-compat? 'Region '(Area 'a)) (Area Node)]  
  [(types-compat? '(NUM g) 'Float) Float]

  ["Lambda bound arrow types are not polymorphic."
     (export-type (,type-expression '(lambda (f) (tuple (app f 3) f)) (empty-tenv)))
     ,(lambda (x) 
	(match x
	  [((Int -> ,v1) -> #(,v2 (Int -> ,v3)))
	   (guard (equal? v1 v2) (equal? v2 v3)) 
	   #t]
	  [,else #f]))]
  ["Non polymorphic funs cannot be applied differently."
   (export-type (,type-expression '(lambda (f) (tuple (app f 3) (app f "foo") f)) (empty-tenv)))
   error]
  
  [(export-type (,type-expression 
		 '(letrec ()
		    (smap2
		       (lambda (n1 n2) (tuple n1 n2))
		       (anchor-at 50 10)
		       (anchor-at 30 40))) (empty-tenv)))
   (Stream #(Node Node))]


  ["A letrec-bound identity function never applied"
   (mvlet ([(p t) (annotate-program '(letrec ([f (lambda (x) x)]) 3))]) p)
   ,(lambda (x)
      (match x
	[(letrec ([f (,v1 -> ,v2) (lambda (x) (,v3) x)]) 3)
	 (guard (equal? v1 v2) (equal? v2 v3)) #t]
	[,else #f]))]

  ["A free non-generic variable in a lambda abstraction"
   (mvlet ([(p t) (annotate-program '(lambda (x)
				       (letrec ([f (lambda (y) x)])
					 f)))])
     t)
   ,(lambda (x)
      (match x 
	[(',v -> (',_ -> ',v2))
	 (eq? v v2)]
	[,_ #f]))]

  ;; This one doesn't actually verify shared structure:
  [(instantiate-type '((#5='(a . #f) -> #6='(b . #f)) (Area #5#) -> (Area #6#)))
   ((#7='(unspecified . #f) -> #8='(unspecified . #f)) (Area #7#) -> (Area #8#))]

  ["instantiate-type: Make sure we don't copy internal nongeneric vars."
   (instantiate-type ''(at '(av . #f) -> '(av . #f)) '(av))
   '(unspecified '(av . #f) -> '(av . #f))]
  
  ;; Now let's see about partially annotated programs.
  ["Partially (erroneously) annotated letrec"
   (mvlet ([(p t) (annotate-program '(letrec ([i String 3]) (+_ i 59)))]) p)
   error]

  ["An example function from nested_regions_folded.ss"
   (mvlet ([(p t) (annotate-program '(letrec ([sumhood (lambda (reg) 
					  (letrec ([thevals (rmap (lambda (n) (cons (nodeid n) '()))
								  reg)])
					    (rfold append '() thevals)))])
				       sumhood))])
     t)
   ((Area Node) -> (Stream (List Int)))]

  ["Here's the captured bug, letrec problem."
   (mvlet ([(p t) (annotate-program '(lambda (n_7)  (Node)
					     (letrec ([resultofthevals_1 (List 'aay) 
									 (cons tmpbasic_12 '())]
						      [tmpbasic_12 Int (nodeid n_7)])
					       resultofthevals_1)))])
     t)
   (Node -> (List Int))]

  ["This compiled version of the same fun gets too general a type"
   (mvlet ([(p t) (annotate-program ' (lambda (reg_5)
			 ((Area Node))
                    (letrec
                      ((thevals_6
                         (Area (List Int))
                         (rmap tmpnonprim_13 reg_5))
                        (resultofsumhood_3
                          (Stream (List Int))
                          (rfold tmpnonprim_14 '() thevals_6))
                        (tmpnonprim_13
                          (Node -> (List Int))
                          (lambda (n_7)
                            (Node)
                            (lazy-letrec
                              ((resultofthevals_1
                                 (List Int)
                                 (cons tmpbasic_12 '()))
                                (tmpbasic_12 Int (nodeid n_7)))
                              resultofthevals_1)))
                        (tmpnonprim_14
                          ((List Int) (List Int) -> (List Int))
                          (lambda (a_9 b_8)
                            ((List Int) (List Int))
                            (lazy-letrec
                              ((resultofanonlambda_2
                                 (List Int)
                                 (append a_9 b_8)))
                              resultofanonlambda_2))))
                      resultofsumhood_3)))]) t)
   ((Area Node) -> (Stream (List Int)))]


  ["Now let's test the NUM subkind."
   (mvlet ([(p t) (annotate-program '(let ([f ((NUM a) -> (NUM a)) (lambda (x) x)])
				       (tuple (app f 3) (app f 4.0) f)))])
     t)
   #(Int Float ((NUM unspecified) -> (NUM unspecified)))]

  ["Now let's test something outside the bounds of the NUM subkind" 
   (mvlet ([(p t) (annotate-program '(let ([f ((NUM a) -> (NUM a)) (lambda (x) x)])
				       (tuple (app f 3) (app f 4.0) (app f "et") f)))])
     t)
   error]


  ;; TODO: FIXME: THIS REPRESENTS A BUG:
  ["LUB: Here we should infer the less general type: plain Int:"
   (deep-member? 'NUM
    (annotate-program 
    '(foolang '(program
		   (letrec ([readings (Area (List Int)) 
				      (rmap
				       (lambda (n) (Node)
					       (cons (sense "temp" n)
						     (cons 1 '())))
				       world)]
			    [aggr 't1 (lambda (x y) ('t2 't3)
					      (cons (g+ (car x) (car y))
						    (cons (g+ (car (cdr x))
							      (car (cdr y)))
							  '())))]
			    [sums (Stream (List Int)) 
				  (rfold aggr (cons 0 (cons 0 '()))
					 readings)]
			    )
		     sums)
		 (union-types)
		 (Stream Int)))))
   #f]

  ["This is an ok use of polymorphism"
   (export-type (,type-expression 
		 '(lambda (g)
		    (letrec ([f (lambda (x) x)])
		      (tuple (app f 3) (app f #t))))
		 (empty-tenv)))
   unspecified]
  ["This should not be allowed by the type system:" 
   (export-type (,type-expression 
		 '(lambda (g)
		    (letrec ([f g])
		      (tuple (app f 3) (app f #t))))
		 (empty-tenv)))
   error]

  ["Nor should this (same thing with let)" 
   (export-type (,type-expression 
		 '(lambda (g)
		    (let ([f g])
		      (tuple (app f 3) (app f #t))))
		 (empty-tenv)))
   error]


  ["This is an identity function with LUB type Num a -> Num a"
   (values->list (annotate-program
		  '(let ([f 'a (lambda (x) x)]) (tuple (app f '3) (app f '4.5)))))
   ,(lambda (x)
      (match x
       	[((let ([f ((NUM ,v1) -> (NUM ,v2)) (lambda (x) (,unspecified) x)])
	    (tuple (app f '3) (app f '4.5)))
	  #(Int Float))
	 (eq? v1 v2)]
	[,else #f]))]

  #;
  ;; Should we type-check with patterns in there?
  [(mvlet ([(p t) (annotate-program '(lambda (#(_ t)) (> t 90)))]) t)
   ??
   ]

  ))

(define-testing test-this (default-unit-tester "Hindley Milner Type Inferencer" these-tests))
;; Unit tester.
(define test-inferencer test-this)

;; We should make sure that all the prim_defs actually have valid types!
(DEBUGMODE
 (for-each (lambda (primdef)
	     (match primdef
	       ;; Some lame exceptions for tuples and tuprefs:
	       ;; TODO: FIXME:
	       [(tuple ,_ ...) (void)]
	       [(tupref ,_ ...) (void)]

	       [(,p ,t) (guard (symbol? p)) (DEBUGASSERT (type? t))]
	       [(,p ,args ,ret) (guard (symbol? p))
		(DEBUGASSERT type? ret)
		(DEBUGASSERT (curry andmap type?) args)]
	       [,else (error 'hm_type_inferencer "bad entry in primitive table: ~s" else)]))
   (regiment-primitives)))

) ; End module. 

;(require hm_type_inference) (test-inferencer)


#|

(lambda (g)
  ('(j quote
       (q quote
          (p LATEUNIFY
             (LUB '(o Bool -> '(n . #f)) '(m Int -> '(l . #f)))
             '(k .
                 #0=(LATEUNIFY
                      (LUB '(af quote
                                (ae LATEUNIFY
                                    (LUB '(ad Bool -> '(ac . #f))
                                         '(ab Int -> '(aa . #f)))
                                    '(z Bool -> '(ag . #f))))
                           '(an quote
                                (am LATEUNIFY
                                    (LUB '(al Bool -> '(ak . #f))
                                         '(aj Int -> '(ai . #f)))
                                    '(ah Int -> '(ao . #f)))))
                      '(y quote
                          (x LATEUNIFY
                             (LUB '(w Bool -> '(v . #f))
                                  '(u Int -> '(t . #f)))
                             '(s . #f)))))))))
  (letrec ([f '(r . #0#) g]) (tuple (app f 3) (app f #t))))
((LATEUNIFY
   (LUB (Bool -> 'n) (Int -> 'l))
   (LATEUNIFY
     (LUB (LATEUNIFY
            (LUB (Bool -> 'ac) (Int -> 'aa))
            (Bool -> 'ag))
          (LATEUNIFY (LUB (Bool -> 'ak) (Int -> 'ai)) (Int -> 'ao)))
     (LATEUNIFY (LUB (Bool -> 'v) (Int -> 't)) 's)))
  ->
  #('ao 'ag))

|#

