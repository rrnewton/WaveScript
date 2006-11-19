;;;; .title The Regiment Type Inferencer.

; type := T                ; symbol - an atomic type
; type := 'a               ; quoted symbol - a type variable
; type := (T type ...)     ; a type constructor
; type := (type -> type)   ; a function type
; type := #(type ...)      ; a tuple type

; And internally, the algorithm uses these constructions:
; type := '(a . type)      ; a type variable with binding
; type := '(a . #f)        ; a type variable with no info


;; See the primitive type definitions in prim_defs.ss

;; Primary ENTRY POINTS are:
;;  type-expression (obsolete)
;;  annotate-expression
;;  annotate-program
;;  types-compat?

;;  validate-types [2006.10.12] TODO: check the existing types, should be less expensive


;; TODO: [2006.04.28] Add a "normalize" procedure that gets rid of any
;; type aliases.  This will remove most of the need for types-compat?

(module hm_type_inference mzscheme
  (require ;`"../../plt/common.ss"
           "../../plt/iu-match.ss"
           "../../plt/chez_compat.ss"
           "../constants.ss"
           ;"hashtab.ss"
           (all-except "../../plt/helpers.ss" test-this these-tests)
	   (all-except "../../plt/regiment_helpers.ss" test-this these-tests 
                       regiment-type-aliases
                       regiment-basic-primitives
                       local-node-primitives
                       regiment-constants
                       regiment-distributed-primitives
                       )
           "../../plt/prim_defs.ss"
           )

  (provide 
           num-types

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
	   
           instantiate-type
	   export-type
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
	   strip-types
	   
	   print-var-types

	   types-compat?
           
	   these-tests
	   test-this
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

	   )

  (chezimports constants)


;; Added a subkind for numbers, here are the types in that subkind.
(define num-types '(Int Float Complex 
			;; Eventually:
			; Int8 Int16 Int64 Float32 
			))

; ======================================================================
;;; Helpers

(define (safe-export-type t)
  (if (type? t) (export-type t) `(NOT-A-TYPE ,t)))

(define type-error error)

;; Raises a generic type error at a particular expression.
(define (raise-type-mismatch t1 t2 exp)
  (type-error 'type-checker
	 "Type mismatch: ~a doesn't match ~a in ~s~%"
	 (safe-export-type t1) (safe-export-type t2) exp))
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
  (match x
    [(,qt (,n . ,t)) (guard (memq qt '(quote NUM))) (DEBUGASSERT (symbol? n))    n]
    [,else (error 'tcell->name "bad tvar cell: ~s" x)]))

; ----------------------------------------
;;; Type Environment ADT

;; Constructs an empty type environment.
(define empty-tenv 
  ;; Uses a unique identifier.
  (let ([ident (gensym "tenv")])
    (lambda () (list ident))))

;; Predicate testing for type environments.
(define (tenv? x)
  (match x
    ;; Format: VAR, TYPE, Is-Let-Bound? FLAG
    [(,tenvsym [,v* ,t* ,flag*] ...)
     (and (eq? tenvsym (car (empty-tenv)))
	  (andmap symbol? v*)
	  (andmap type? t*)
	  (andmap boolean? flag*))]
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
  (DEBUGASSERT (tenv? tenv))
  (DEBUGASSERT (andmap type? types))
  (let ([flag (if (null? flag) #f (if (car flag) #t #f))])
    (cons (car tenv)
	  (append (map (lambda (a b) (list a b flag)) syms types)
		  (cdr tenv))
	  )))
(define (tenv-append . tenvs)
  (cons (car (empty-tenv)) 
	(apply append (map cdr tenvs))))
;; Applies a function to all types in a type enviroment.
(define (tenv-map f tenv)
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

;; This is because, when instantiating a rator for application
;; let-bound type variables are reinstantiated, whereas lambda-bound ones are not.
(define instantiate-type 
  (case-lambda 
    [(t) (instantiate-type t '())]
    [(t nongeneric)
     (let* ((tenv (empty-tenv))
	 (result 
	  (let loop ((t t))
	   (match t
	     [#f #f]
	     [,s (guard (symbol? s)) s]

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
	      `(,constructor ,args ...)]
	     [,other (error 'instantiate-type "bad type: ~a" other)]
	     ))))
       (DEBUGASSERT (type? result))
       result)]))

;; This takes away the mutable cells, thereby converting to the
;; external representation.
(define (export-type t)
  (match t
    [,s (guard (symbol? s)) s]
    ['(,n . ,v) (if v (export-type v) `(quote ,n))]
    [',n `(quote ,n)]    
    [(NUM ,v) (guard (symbol? v)) `(NUM ,v)]
    [(NUM (,v . ,t)) (if t (export-type t) `(NUM ,v))]
    [(,[arg*] ... -> ,[res])
     `(,arg* ... -> ,res)]
    [(,s ,[t] ...) (guard (symbol? s))
     `(,s ,t ...)]
    [#(,[t*] ...) (apply vector t*)]
    [,other (error 'export-type "bad type: ~s" other)]))

;; Looks up a primitive and produces an instantiated version of its arrow-type.
(define (prim->type p)
  (let ((entry (or (assq p regiment-basic-primitives)
		   (assq p regiment-constants)
		   (assq p regiment-distributed-primitives)
		   )))
    (unless entry
      (error 'prim->type "primitive was not found: ~a" p))
    (match entry
      [(,name ,type) (instantiate-type type '())]
      [(,name ,args ,ret)
       (instantiate-type `(,@args -> ,ret) '())])))

;; This isn't a very safe datatype, but this predicate tries to give some assurance.
(define (type? t)
  (define (id x) x)
  (define (valid-type-symbol? s)
    (let ([str (symbol->string s)])
      (and (> (string-length str) 0)
	   (char-upper-case? (string-ref str 0)))))
  (define (valid-typevar-symbol? s)
    (let ([str (symbol->string s)])
      (and (> (string-length str) 0)
	   (char-lower-case? (string-ref str 0)))))
  (match t
    [,s (guard (symbol? s)) (valid-type-symbol? s)]
    [(,qt ,v)          (guard (memq qt '(quote NUM)) (symbol? v)) (valid-typevar-symbol? v)]
    [(,qt (,v . #f))   (guard (memq qt '(quote NUM)) (symbol? v)) (valid-typevar-symbol? v)]
    [(,qt (,v . ,[t])) (guard (memq qt '(quote NUM)) (symbol? v)) (and t (valid-typevar-symbol? v))]
    [(,[arg] ... -> ,[ret]) (and ret (andmap id  arg))]
    [(Struct ,name) (symbol? name)] ;; Adding struct types for output of nominalize-types.
    [(,C ,[t] ...) (guard (symbol? C)) (andmap id t)]
    [#(,[t] ...) (andmap id t)]   
    [,else #f]))

;; Does it contain the monad?
(define (distributed-type? t)
  (define (id x) x)
  (match t
    ;; TODO: FIXME: Use the type alias table, don't check for Region/Anchor directly:
    [Region #t]
    [Anchor #t]
    [(Area ,_) #t]
    [(Signal ,_) #t]    
    [,s (guard (symbol? s)) #f]
    [(,qt ,v) (guard (memq qt '(quote NUM)) (symbol? v)) #f]
    [(,qt (,v . #f))  (guard (memq qt '(quote NUM)) (symbol? v)) 
     (warning 'distributed-type? "got type var with no info: ~s" v)
     #t] ;; This COULD be.
    [(,qt (,v . ,[t])) (guard (memq qt '(quote NUM)) (symbol? v)) t]
    [(,[arg] ... -> ,[ret]) (or ret (ormap id  arg))]
    [(Struct ,name) #f] ;; Adding struct types for output of nominalize-types.
    [(,C ,[t] ...) (guard (symbol? C)) (ormap id t)]
    [#(,[t] ...) (ormap id t)]
    [,else #f]))

;; Does it contain any type-vars?
(define (polymorphic-type? t)
  (define (id x) x)
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
    [(,C ,[t] ...) (guard (symbol? C)) (ormap id t)]
    [#(,[t] ...) (ormap id t)]
    [,else #f]))

;; This means *is* an arrow type, not *contains* an arrow type.
(define (arrow-type? t)
  (match t
    [(quote (,v . #f)) #f]
    [(quote (,v . ,[rhs])) rhs]
    [(NUM ,_) #f] ;; NUM types shouldnt be arrows!
    [(,t1 ... -> ,t2) #t]
    [,else #f]))

; ======================================================================

;;; The main type checker.

(define (type-expression expr tenv)
  (mvlet ([(_ typ) (annotate-expression expr tenv '())])
    typ))

;; Used for recovering types for particular expressions within an already type-annotated program.
;; NOTE: *Assumes* that the program is *correctly* typed.
;; Returns an "exported" type... i.e. with mutable cells stripped.
;; .param exp - Expression
;; .param tenv - Type Environment
(define (recover-type exp tenv)
  (DEBUGASSERT (tenv? tenv))
  (let l ((exp exp))
    (match exp 
      [(lambda ,formals ,types ,body)
       `(,types ... -> ,(recover-type body (tenv-extend tenv formals types)))]
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
      ;; Since the program is already typed, we just use the arrow type of the rator:
      ;[(,[rat] ,rand ...) (rac rat)]
      [,other 
       (export-type (type-expression other (tenv-map instantiate-type tenv)))   ])))

;; Assign a basic type to a constant.
(define (type-const c)
  (cond
   [(and (number? c) (inexact? c)) 'Float]
   [(integer? c) 'Int]
   [(string? c) 'String] 
   [(boolean? c) 'Bool]
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
  (let l ((exp exp))
    (match exp 
      [,c (guard (constant? c)) (values c (type-const c))]
      [(quote ,c)               (values `(quote ,c) (type-const c))]
      [,prim (guard (symbol? prim) (regiment-primitive? prim))
	     (values prim (prim->type prim))]
      [,v (guard (symbol? v))
	  (let ((entry (tenv-lookup tenv v)))
	    (if entry 
		;; Let-bound polymorphism:
		(if (and (tenv-is-let-bound? tenv v)
		    ;; TEMP: HACK:
		    ;; Until we fix lazy-letrec to work for the now-strict language...
		    ;; We can only polymorphically instantiate arrow types!!
			 (arrow-type? entry)
			 )
		    (begin 
		      ;(printf "Let-bound var! ~s with (arrow ~s) type ~a\n" v (arrow-type? entry) entry)
		      (values v (instantiate-type entry nongeneric)))
		    (values v entry))
		(error 'annotate-expression "no binding in type environment for var: ~a" v)))]
      [(if ,[l -> te tt] ,[l -> ce ct] ,[l -> ae at])
       (types-equal! tt 'Bool te) ;; This returns the error message with the annotated expression, oh well.
       (types-equal! ct at exp)
       (values `(if ,te ,ce ,ae) ct)]
      
      ;; Wavescope: this could be a set! to a state{} bound variable:
      [(set! ,v ,[l -> e et])  
       (let ([newexp `(set! ,v ,e)])
	 (types-equal! (ASSERT (tenv-lookup tenv v))
		       et newexp)
	 ;; returns unit type:
	 (values newexp #()))]
      [(for (,i ,[l -> start st]) ,[l -> end et] ,[bod bt])
       (let ([expr `(for [,i ,start ,end] ,bod)])
	 (unless (types-compat? st et) (raise-type-mismatch start end expr))
	 (values expr #()))]

      [(tuple ,[l -> e* t*] ...)  (values `(tuple ,e* ...) (list->vector t*))]
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
      
      [(,prim ,[l -> rand* t*] ...)
       (guard (regiment-primitive? prim))
       (DEBUGASSERT (andmap type? t*))
       (values `(,prim ,rand* ...)
	       (type-app prim (prim->type prim) t* exp tenv nongeneric))]

      ;[(app ,rat ,rands* ...)  (l `(,rat ,rands* ...))]
      [(app ,origrat ,[l -> rand* t*] ...)
       (DEBUGASSERT (andmap type? t*))
       (mvlet ([(rator t1) (l origrat)])
	 (values `(app ,rator ,rand* ...)
		 (type-app origrat t1 t* exp tenv nongeneric)))]

      ;; Allowing unlabeled applications for now:
      [(,rat ,rand* ...) (guard (not (regiment-keyword? rat)))
       (warning 'annotate-expression "allowing arbitrary rator: ~a\n" rat)
       (l `(app ,rat ,rand* ...))]

      [,other (error 'annotate-expression "could not type, unrecognized expression: ~s" other)]
      )))

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
    (ASSERT (andmap symbol? ids)) ;; For now, no patterns.
    (for-each valid-user-type! inittypes)
    ;; No optional type annotations currently.
    (let* ([argtypes  (map (lambda (_) (make-tcell)) ids)]
           [newnongen (map (lambda (x) (match x ['(,n . ,_) n])) argtypes)])
      ;; Now unify the new type vars with the existing annotations.
      (map (lambda (new old)
	     (if old (types-equal! new (instantiate-type old) 
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
    (ASSERT (andmap symbol? id*)) ;; For now, no patterns.
    (for-each  valid-user-type! inittypes)
    (match rhs*
      [(,[f -> newrhs* rhsty*] ...)
       ;(printf "ANNLET: ~s   ~s\n" rhsty* inittypes)
       (let* ([newtypes (map (lambda (new old) 
			       (if old (types-equal! new (instantiate-type old) 
					    `(let ([,id* ,inittypes ,rhs*] ...) ,bod)))
			      new)
			  rhsty* inittypes)]
	      [tenv (tenv-extend tenv id* newtypes #t)])
	 (mvlet ([(bod bodty) (annotate-expression bod tenv nongeneric)])
	   (values `(let ([,id* ,newtypes ,newrhs*] ...) ,bod) bodty)))])))

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
			   (types-equal! new (instantiate-type old) 
					 `(letrec (,'... [,new ,old] ,'...) ,'...))))
		  rhs-types existing-types)]
             [tenv (tenv-extend tenv id* rhs-types #t)])
        ;; Unify all these new type variables with the rhs expressions
        (let ([newrhs* 
               (map-ordered2 (lambda (type rhs)
                               (mvlet ([(newrhs t) (annotate-expression rhs tenv nongeneric)])
                                 (types-equal! type t rhs)
                                 newrhs))
                             rhs-types rhs*)])
          (mvlet ([(bode bodt) (annotate-expression bod tenv nongeneric)])
            (values `(,letrecconstruct ,(map list id* rhs-types newrhs*) ,bode) bodt)))))))
  
;; This annotates the program, and then exports all the types to their
;; external form (stripped of mutable cells on the tvars).
(define (annotate-program p)
  (let ([Expr 
	 (lambda (p)
	   (mvlet ([(e t) (annotate-expression p (empty-tenv) '())])
	     ;; Now strip mutable cells from annotated expression.
	     (values 
	      (match e ;; match-expr
		[,c (guard (constant? c)) c]
		[,v (guard (symbol? v)) v]
		[(quote ,c)       `(quote ,c)]
		[,prim (guard (symbol? prim) (regiment-primitive? prim))
		       prim]
		[(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
		[(lambda ,v* ,t* ,[bod]) `(lambda ,v* ,(map export-type t*) ,bod)]
		[(tuple ,[e*] ...) `(tuple ,e* ...)]
		[(tupref ,n ,[e]) `(tupref ,n ,e)]

		[(set! ,v ,[e]) `(set! ,v ,e)]
		[(begin ,[e] ...) `(begin ,e ...)]
		[(for (,i ,[s] ,[e]) ,[bod]) `(for (,i ,s ,e) ,bod)]

		[(let ([,id* ,t* ,[rhs*]] ...) ,[bod])
		 `(let ([,id* ,(map export-type t*) ,rhs*] ...) ,bod)]
		[(,letrec ([,id* ,t* ,[rhs*]] ...) ,[bod])
		 (guard (memq letrec '(letrec lazy-letrec)))
		 `(,letrec ([,id* ,(map export-type t*) ,rhs*] ...) ,bod)]
		[(app ,[rat] ,[rand*] ...) `(app ,rat ,rand* ...)]

		[(,prim ,[rand*] ...)
		 (guard (regiment-primitive? prim))
		 `(,prim ,rand* ...)]
		
		

		;; HACK HACK HACK: Fix this:
		;; We cheat for nums, vars, prims: 
		;[,other other]; don't need to do anything here...
					;       [,other (error 'annotate-program "bad expression: ~a" other)]
		)
	      (export-type t)))
	   )])
    ;; Accepts either with-boilerplate or without.
    (match p
      [(,lang '(program ,[Expr -> e t] ,type))
       (ASSERT (type? type))
       `(,lang '(program ,e ,t))]
      [,other (Expr other)])))

;; This simply removes all the type annotations from an expression.
;; This would  be a great candidate for a generic traversal:
(define (strip-types p)
  (define (process-expression e)
    (match e
    [(lambda ,v* ,optionaltypes ,[bod]) `(lambda ,v* ,bod)]
    [(lambda ,v* ,[bod])                `(lambda ,v* ,bod)]
    [(,let ([,id* ,optionaltype ... ,[rhs*]] ...) ,[bod])      (guard (memq let '(let let* letrec lazy-letrec)))     
     `(,let ([,id* ,rhs*] ...) ,bod)]
   
    [,c (guard (constant? c)) c]
    [(quote ,c)       `(quote ,c)]
    [,var (guard (symbol? var)) var]
    [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
    [(tuple ,[e*] ...) `(tuple ,e* ...)]
    [(tupref ,n ,[e]) `(tupref ,n ,e)]
    
    [(set! ,v ,[e]) `(set! ,v ,e)]
    [(begin ,[e] ...) `(begin ,e ...)]
    [(for (,i ,[s] ,[e]) ,[bod]) `(for (,i ,s ,e) ,bod)]
    [(iterate ,[f] ,[s]) `(iterate ,f ,s)]

    [(tuple ,[args] ...) `(tuple ,args ...)]
    [(tupref ,n ,m ,[x]) `(tupref ,n ,m ,x)]

    [(assert-type ,t ,e) e]
    [(app ,[rat] ,[rand*] ...) `(app ,rat ,rand* ...)]    
    
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
  (DEBUGASSERT (and (type? t1) (type? t2)))
  (match (list t1 t2)
    [[,x ,y] (guard (eqv? t1 t2)) (void)]
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
    [[(,x1 ,xargs ...) (,y1 ,yargs ...)]
     (guard (symbol? x1) (symbol? y1)
	    (not (memq '-> xargs))
	    (not (memq '-> yargs))
	    (= (length xargs) (length yargs)))
     (if (not (eq? x1 y1))
	 (type-error 'types-equal! "type constructors do not match: ~a and ~a in ~a" x1 y1 exp))
;     (types-equal! x1 y1 exp)
     (for-each (lambda (t1 t2) (types-equal! t1 t2 exp)) xargs yargs)]

;    [[(,xargs ... -> ,x) (,yargs ... -> ,y)] ;; [2005.12.07] Just got a "wrong number of arguments" error that might be a match bug.

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
		      "procedure type ~a does not match: ~a"
		      `(,@yargs -> ,y) other)])]
    [,otherwise (raise-type-mismatch t1 t2 exp)]))
		   
;; This helper mutates a tvar cell while protecting against cyclic structures.
(define (tvar-equal-type! tvar ty exp)
  (DEBUGASSERT (type? ty))
  (match tvar ;; Type variable should be a quoted pair.
    [(,qt ,pr) (guard (memq qt '(NUM quote)))
     (if (not (pair? pr))
	 (error 'tvar-equal-type! "bad tvar here, no cell: ~a" tvar))
     (if (cdr pr)

	 (types-equal! (cdr pr) ty exp)

	 (begin (no-occurrence! (tcell->name tvar) ty exp)
		(set-cdr! pr ty)))]))

;; This makes sure there are no cycles in a tvar's mutable cell.
;; .returns #t if there are no loops, or throws an error otherwise.
(define (no-occurrence! tvar ty exp)
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
	  [(quote ,tvarpair)
	   ;; Ouch, mutating in the guard... Nasty.
	   (guard  (match tvarpair
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
	  [,else (error 'no-occurrence! "there's a bug in this implementation.")])
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

;; Prints a type in an ML-ish way rather than the raw sexp.
(define (print-type t . p)
  (let ([port (if (null? p) (current-output-port) (car p))])
   (display 
    (let loop ([t t])
      (match t
	[(quote ,[var]) (++ "'" var)]
	[(-> ,[b]) (++ "() -> " b)]
	[(,[arg*] ... -> ,[b]) (++ (apply string-append (insert-between ", " arg*))
				   " -> "b)]
	[#(,[x*] ...)
	 (++ "(" (apply string-append (insert-between ", " x*)) ")")]
	[(,[tc] ,[arg])
	 (++ tc" "arg)]
	[,sym (guard (symbol? sym))
	      (symbol->string sym)]
	[,other (error 'print-type "bad type: ~s" other)]))
    port)))

;; Expects a fully typed expression
(define (print-var-types exp . p)
  (let ([port (if (null? p) (current-output-port) (car p))])
    
    (define (get-var-types exp)
      (match exp ;; match-expr

	[(,lang '(program ,[body] ,ty)) 
	 (append body `((type BASE ,ty ())))]

       [,c (guard (constant? c)) '()]
       [,var (guard (symbol? var))  `()]       
       [(quote ,c)       '()]
       [(set! ,v ,[e]) e]
       [(begin ,[e*] ...) (apply append e*)]
       [(for (,i ,[s] ,[e]) ,[bodls]) (cons `[type ,i Int ()] bodls)]

       [(if ,[t] ,[c] ,[a]) (append t c a)]
       [(tuple ,[args] ...) (apply append args)]
       [(tupref ,n ,m ,[x]) x]

       [(,let ([,id* ,t* ,[rhs*]] ...) ,[bod]) 
	(guard (memq let '(let letrec lazy-letrec)))
	(append (apply append 
		       (map (lambda (id t rhsls)
			      `([type ,id ,t ,rhsls]))
			 id* t* rhs*))
		bod)]
       [(lambda ,v* ,t* ,[bodls])   bodls]
       [(tuple ,[e*] ...) (apply append e*)]
       [(tupref ,n ,m ,[e]) e]

       [(app ,[rat] ,[rand*] ...) (apply append rat rand*)]
	[(,prim ,[rand*] ...)
	 (guard (regiment-primitive? prim))
	 (apply append rand*)]
	[,other (error 'print-var-types "bad expression: ~a" other)]))

   
    ;(inspect (get-var-types exp))
    (let loop ([x (get-var-types exp)] [indent " "])
      (match x
	[() (void)]
	[(type ,v ,t ,subvars)
	 (unless (eq? v '___VIRTQUEUE___) 	 ;; <-- HACK: 
	   (fprintf port "~a~a : " indent v)
	   (print-type t port) (newline port))
	 (loop subvars (++ indent "  "))]
	[,ls (guard (list? ls))
	     (for-each (lambda (x) (loop x indent))
	       ls)]
	[,other (error 'print-var-types "bad result from get-var-types: ~a" other)]))
      ))


; ======================================================================
;;; Unit tests.

;; Unit tests.
(define these-tests
  `([(begin (reset-tvar-generator) (let ((x (prim->type 'car))) (set-cdr! (car (cdaddr x)) 99) x))
     ((List '(a . 99)) -> '(a . 99))]

    ["NUM must have vars attached" (type? #((NUM Int) (NUM Float))) #f]
    [(type? '(NUM a)) #t]
    [(type? '(Int -> (NUM a))) #t]

    [(,type-expression '(if #t 1. 2.) (empty-tenv))         Float]
    [(mvlet ([(p t ) (annotate-program '(lambda (x) (g+ x (gint 3))))]) t)
     ((NUM unspecified) -> (NUM unspecified))]

    
    [(export-type ''(f . Int)) Int]
    [(export-type (,type-expression '(+ 1 1) (empty-tenv))) Int]
    [(export-type (,type-expression '(cons 3 (cons 4 '())) (empty-tenv))) (List Int)]
    [(export-type (,type-expression '(cons 1 '(2 3 4)) (empty-tenv))) (List Int)]
    [(export-type (,type-expression '(cons 1 '(2 3 4.)) (empty-tenv))) error]

    [(export-type (mvlet ([(_ t) (,annotate-lambda '(v) '(+ v v) '(Int) (empty-tenv) '())]) t))
     (Int -> Int)]
    [(export-type (mvlet ([(_ t) (,annotate-lambda '(v) '(+ v v) '('alpha) (empty-tenv) '())]) t))
     ;((NUM unspecified) -> (NUM unspecified))
     (Int -> Int)]
    
    ["types-equal!: make sure that (by convention) the first argument is mutated"
     (let ([x ''(a . #f)] [y ''(b . #f)]) (types-equal! x y (empty-tenv)) x)
     '(a . '(b . #f))]
   
    ["Invalid explicitly annotated type"
     (export-type (mvlet ([(_ t) (,annotate-lambda '(v) '(+ v v) '(String) (empty-tenv) '())]) t))
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
     
    [(export-type (,type-expression '(lambda (y) (letrec ([x y]) (+ x 4))) (empty-tenv)))
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
   (types-compat? 'Anchor '(Signal 'a) )
   (Signal Node)]
  [(types-compat? 'Anchor '(Signal Node)) (Signal Node)]
  ;; This is kind of lame:
  [(types-compat? '(Signal Node) 'Anchor) Anchor]
  [(types-compat? 'Anchor 'Anchor) Anchor]
  [(types-compat? 'Anchor '(Area Int)) #f]
  [(types-compat? 'Region '(Area 'a)) (Area Node)]  
  [(types-compat? '(NUM g) 'Float) Float]

  ["Lambda bound arrow types are not polymorphic."
     (export-type (,type-expression '(lambda (f) (tuple (f 3) f)) (empty-tenv)))
     ,(lambda (x) 
	(match x
	  [((Int -> ,v1) -> #(,v2 (Int -> ,v3)))
	   (guard (equal? v1 v2) (equal? v2 v3)) 
	   #t]
	  [,else #f]))]
  ["Non polymorphic funs cannot be applied differently."
   (export-type (,type-expression '(lambda (f) (tuple (f 3) (f "foo") f)) (empty-tenv)))
   error]
  
  [(export-type (,type-expression 
		 '(letrec ()
		    (smap2
		       (lambda (n1 n2) (tuple n1 n2))
		       (anchor-at 50 10)
		       (anchor-at 30 40))) (empty-tenv)))
   (Signal #(Node Node))]
  ["This should not be allowed by the type system:" 
   (export-type (,type-expression 
		 '(lambda (g)
		    (letrec ([f g])
		      (tuple (f 3) (f #t))))
		 (empty-tenv)))
   error]
  ["Whereas this is ok."
   (export-type (,type-expression 
		 '(lambda (g)
		    (letrec ([f (lambda (x) x)])
		      (tuple (f 3) (f #t))))
		 (empty-tenv)))
   unspecified]

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
   (mvlet ([(p t) (annotate-program '(letrec ([i String 3]) (+ i 59)))]) p)
   error]

  ["An example function from nested_regions_folded.ss"
   (mvlet ([(p t) (annotate-program '(letrec ([sumhood (lambda (reg) 
					  (letrec ([thevals (rmap (lambda (n) (cons (nodeid n) '()))
								  reg)])
					    (rfold append '() thevals)))])
				       sumhood))])
     t)
   ((Area Node) -> (Signal (List Int)))]

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
                          (Signal (List Int))
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
   ((Area Node) -> (Signal (List Int)))]


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


  #;
  ;; Should we type-check with patterns in there?
  [(mvlet ([(p t) (annotate-program '(lambda (#(_ t)) (> t 90)))]) t)
   ??
   ]

  ))

(define test-this (default-unit-tester "Hindley Milner Type Inferencer" these-tests))
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

	       [(,p ,t) (guard (symbol? p)) (DEBUGASSERT(type? t))]
	       [(,p ,args ,ret) (guard (symbol? p))
		(DEBUGASSERT (type? ret))
		(DEBUGASSERT (andmap type? args))]
	       [,else (error 'hm_type_inferencer "bad entry in primitive table: ~s" else)]))
   regiment-primitives))

) ; End module. 



#;
(define bug 
  '(letrec ([matrix (lambda (rows cols init)
                   (letrec ([arr (app makeArray rows nullarr)])
                     (begin
                       (for (i 0 (- rows 1))
                            (arr-set! arr i (app makeArray cols init)))
                       arr)))]
         [m_get (lambda (mat row col)
                  (letrec ([r (arr-get mat row)]) (arr-get r col)))]
         [m_set (lambda (mat row col val)
                  (letrec ([r (arr-get mat row)])
                    (begin (arr-set! r col val) (tuple))))]
         [m_mult (lambda (m1 m2)
                   (letrec ([m3 (app matrix
                                     (app length m1)
                                     (app length (arr-get m2 0))
                                     (app m_get m1 0 0))])
                     m3))])
  (app matrix 3 4 5.0))

  )




;> (instantiate-type '(#0='(j . #f) -> #0#))
;(#0='(k . #f) -> #0#)
;> (instantiate-type '(#0='(j . #f) -> #0#) '(j))
;('#0=(j . #f) -> '#0#)

; let f = ref (fun x -> x) in (!f 3, !f 3.0);;  -- ERR
; let f = [fun x -> x] in (hd f 3, hd f 3.0);;  

; fun f -> (f 3, f 4.0)  -- ERR 
