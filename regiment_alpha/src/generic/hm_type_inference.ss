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


(module hm_type_inference mzscheme
  (require "../plt/iu-match.ss"
           (lib "include.ss")
           "../generic/constants.ss"
           ;"hashtab.ss"
           (all-except "../plt/helpers.ss" test-this these-tests)
	   (all-except "../plt/regiment_helpers.ss" test-this these-tests 
                       regiment-type-aliases
                       regiment-basic-primitives
                       local-node-primitives
                       regiment-constants
                       regiment-distributed-primitives
                       )
           "../plt/prim_defs.ss"
           )

  (provide 
;           regiment-type-aliases
;	   regiment-basic-primitives
;	   local-node-primitives
;	   regiment-constants
;	   regiment-distributed-primitives

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
	   tenv-map
	   
           instantiate-type
	   export-type
	   prim->type
	   type?
	   ;id
	   ;inject-polymorphism
	   
;           type-expression
	   recover-type
	   type-const
;	   type-lambda
	   type-app
;	   type-letrec
           
	   annotate-expression
	   annotate-program

;	   types-equal!
;	   tvar-equal-type!
;	   no-occurrence!
;	   safe-export-type
;	   raise-type-error
;	   raise-occurrence-check
;	   raise-wrong-number-of-arguments

           
	   these-tests
	   test-this
	   test-inferencer
	   )

  (chezimports constants)

; ======================================================================
;;; Helpers

(define (safe-export-type t)
  (if (type? t) (export-type t) `(NOT-A-TYPE ,t)))

;; Raises a generic type error at a particular expression.
(define (raise-type-mismatch t1 t2 exp)
  (error 'type-checker
	 "Type mismatch: ~s doesn't match ~s in ~s~%"
	 (safe-export-type t1) (safe-export-type t2) exp))
;; Raises an error indicating that we have a loop in our tvar pointers.
(define (raise-occurrence-check tvnum t2 exp)
  (error 'type-checker
	 "Can't unify: ~s occurs in type ~s in expression ~s~%" 
	 tvnum t2 ;(export-type t2) 
	 exp))
;; Raises a wrong-number-of-arguments error.
(define (raise-wrong-number-of-arguments t1 t2 exp)
  (error 'type-checker
	 "Different numbers of arguments ~s and ~s in ~s\n"
	 (safe-export-type t1) (safe-export-type t2) exp))

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
(define (make-tcell) `(quote (,(make-tvar) . #f)))

(define (tcell->name x)
  (match x
    [(quote (,n . ,t)) (DEBUGASSERT (symbol? n))    n]
    [,else (error 'tcell->name "bad tvar cell: ~s" x)]))

; ----------------------------------------
;;; Type Environment ADT

;; Constructs an empty type environment.
(define (empty-tenv) '())
;; Predicate testing for type environments.
(define (tenv? x)
  (match x
    ;; Format: VAR, TYPE, Is-Let-Bound? FLAG
    [([,v* ,t* ,flag*] ...)
     (and (andmap symbol? v*)
	  (andmap type? t*)
	  (andmap boolean? flag*))]
    [,else #f]))
;; Retrieves a type if a binding exists for sym, otherwise #f.
(define (tenv-lookup tenv sym)
  (DEBUGASSERT (tenv? tenv))
  (let ([entry (assq sym tenv)])
    (if entry (cadr entry) #f)))
;; This returns #t if the let-bound flag for a given binding is set.
;; If sym is lambda-bound or is unbound in the type environment, #f is
;; returned.
(define (tenv-is-let-bound? tenv sym)
  (DEBUGASSERT (tenv? tenv))
  (let ([entry (assq sym tenv)])
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
    (append (map (lambda (a b) (list a b flag)) syms types) tenv)))
;; Applies a function to all types in a type enviroment.
(define (tenv-map f tenv)
  (DEBUGASSERT (tenv? tenv))
  (map 
      (lambda (x) 
	(match x 
	  [(,v ,t ,flag) `(,v ,(f t) ,flag)]
	  [,other (error 'recover-type "bad tenv entry: ~s" other)]))
    tenv))

; ----------------------------------------
#|
(define (tenv-lookup tenv sym)
  (DEBUGASSERT (tenv? tenv))
  (set! tenv (vector->list tenv))
  (let ([entry (assq sym tenv)])
    (if entry (cadr entry) #f)))
(define (tenv-extend tenv syms vals)
  (DEBUGASSERT (tenv? tenv))
  (set! tenv (vector->list tenv))
  (list->vector (append (map list syms vals) tenv)))
(define (tenv-map f tenv)
  (DEBUGASSERT (tenv? tenv))
  (set! tenv (vector->list tenv))
  (list->vector 
   (map 
      (lambda (x) 
	(match x 
	  [(,v ,t) `(,v ,(f t))]
	  [,other (error 'recover-type "bad tenv entry: ~s" other)]))
    tenv)))
(define (empty-tenv) (vector))
(define (tenv? x)
  (match x
    [#([,v* ,t*] ...)
     (and (andmap symbol? v*)
	  (andmap type? t*))]
    [,else #f]))
|#
; ----------------------------------------

;; This associates new mutable cells with all tvars.
;; It also renames all the tvars to assure uniqueness.
(define (instantiate-type t)
  (let* ((tenv (empty-tenv))
	 (result 
	  (let loop ((t t))
	   (match t
	     [,s (guard (symbol? s)) s]
	     ;; This type variable has a cell.  Replace it:
	     [(quote (,n . ,v))
	      (let ((entry (tenv-lookup tenv n)))
		;; If there's already a cell allocated, we should use it.
		(if entry
		    entry
		    (let ((newtype `(quote ,(cons (make-tvar) (if v (loop v) v)))))
		      (DEBUGASSERT (not (tenv-lookup tenv n))) ;; 'n should not occur in v!
		      (set! tenv (tenv-extend tenv (list n) (list newtype)))
		      newtype)))]
	     ;; This is a type variable with no cell attached.  Make one and attach it.
	     [(quote ,n) (guard (symbol? n))
	      (let ((entry (tenv-lookup tenv n)))
		(or entry
		    (let ((newtype `(quote (,(make-tvar) . #f))))
		      (set! tenv (tenv-extend tenv (list n) (list newtype)))
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
    result))

;; This takes away the mutable cells, thereby converting to the
;; external representation.
(define (export-type t)
  (match t
    [,s (guard (symbol? s)) s]
    ['(,n . ,v) (if v (export-type v) `(quote ,n))]
    [',n n]
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
      [(,name ,type) (instantiate-type type)]
      [(,name ,args ,ret)
       (instantiate-type `(,@args -> ,ret))])))

(define (type? t)
  (define (id x) x)
  (match t
    [,s (guard (symbol? s)) #t]
    [',v (guard (symbol? v)) #t]
    ['(,v . #f) (guard (symbol? v)) #t]
    ['(,v . ,[t]) (guard (symbol? v)) t]
    [(,[arg] ... -> ,[ret]) (and ret (andmap id  arg))]
    [(,C ,[t] ...) (guard (symbol? C)) (andmap id t)]
    [#(,[t] ...) (andmap id t)]
    [,else #f]))

; ======================================================================

;;; The main type checker.

;; If it's a let-bound function re-instantiate it for polymorphism:
;(define (inject-polymorphism tenv v ty)

;  (if (tenv-is-let-bound? tenv v)
;      (instantiate-type entry)
;      entry)

;; [2006.03.19]
;; PHASING THIS OUT.
;; The only advantage this has over just using annotate-expression is efficiency,
;; and that's just not worth duplicating code at this point.
;; 
;; Assign a type to an expression.
#;
(define (type-expression exp tenv)
    (DEBUGASSERT (tenv? tenv))
  (let l ((exp exp))
    (match exp 
      [,c (guard (constant? c)) (type-const c)]
      [(quote ,c) (type-const c)]
      [,prim (guard (symbol? prim) (regiment-primitive? prim))
	     (prim->type prim)]
      [,v (guard (symbol? v)) 
	  (let ((entry (tenv-lookup tenv v)))
	    (or entry
		(error 'type-expression "no binding in type environment for var: ~a" v)))]
      [(if ,te ,[l -> c] ,[l -> a])
       (let ((tt (l te)))
	 (types-equal! tt 'Bool te)
	 (types-equal! c a exp)
	 c)]
      [(tuple ,[l -> arg*] ...)  (apply vector arg*)]
      [(tupref ,n ,len ,[l -> t])
       (unless (and (qinteger? n) (qinteger? len))
	 (error 'type-expression 
		"invalid tupref syntax, expected constant integer index/len, got: ~a/~a" n len))
       (let ((newtypes (list->vector (map (lambda (_) (make-tcell)) (iota (qinteger->integer len))))))
	 (types-equal! t newtypes exp)
	 (vector-ref newtypes (qinteger->integer n)))]

      [(lambda (,v* ...) ,bod) (type-lambda v* bod tenv)]
      [(letrec ([,id* ,rhs*] ...) ,bod)
       ;(guard (memq letrec '(lazy-letrec letrec)))
       (type-letrec id* rhs* bod tenv)]

      ;; TODO: Doesn't actually take optional types into account. FIXME FIXME
      [(lambda (,v* ...) ,types ,bod)
       ;; No optional type annotations currently.
       (type-lambda v* bod tenv)]
      [(lazy-letrec ([,id* ,type* ,rhs*] ...) ,bod)
       ;(guard (memq letrec '(lazy-letrec letrec)))
       (type-letrec id* rhs* bod tenv)]

      [(,prim ,[l -> rand*] ...)
       (guard (regiment-primitive? prim))
       (DEBUGASSERT (andmap type? rand*))
       (type-app prim (prim->type prim) rand* exp tenv)]
      [(,rator ,[l -> rand*] ...)
       (DEBUGASSERT (andmap type? rand*))
       (let ((rattyp (l rator)))
	 (type-app rator rattyp rand* exp tenv))]

      [,other (error 'type-expression "unknown expression: ~s" other)]
      )))

(define (type-expression expr tenv)
  (mvlet ([(_ typ) (annotate-expression expr tenv)])
    typ))

;; Used for recovering types for particular expressions within an already type-annotated program.
(define (recover-type exp tenv)
  (DEBUGASSERT (tenv? tenv))
  (let l ((exp exp))
    (match exp 
      [(lambda ,formals ,types ,body)
       `(,types ... -> ,(recover-type body (tenv-extend tenv formals types)))]
      [(,letrec ([,lhs* ,type* ,rhs*] ...) ,body)
       (guard (memq letrec '(letrec lazy-letrec)))
       (recover-type body (tenv-extend tenv lhs* type*))]

      [(if ,t ,[c] ,[a]) 
       (let ([a (instantiate-type a)] 
	     [c (instantiate-type c)])
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
   [(integer? c) 'Integer]
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

;; Assign a type to a procedure declaration.
;; .param ids   The lambda formals.
;; .param body  The lambda body.
;; .param tenv  The type environment.
#;
(define type-lambda
  (lambda (;texps 
	   ids body tenv)
    ;(let ((arg-types (expand-optional-type-expressions texps tenv)))
    (let* ([argtypes (map (lambda (_) (make-tcell)) ids)]
	   [result (type-expression body (tenv-extend tenv ids argtypes))])
      `(,@argtypes -> ,result))))

;; Assign a type to a procedure application expression.
(define (type-app rator rattyp rands exp tenv)
  (DEBUGASSERT (tenv? tenv))
 ;;;;;;;;;;;;;;;  (printf "Rator: ~s ~s \n" rator (tenv-is-let-bound? tenv rator))
  (let ([result (make-tcell)])
    (if (and (symbol? rator) 
;#f
	     (tenv-is-let-bound? tenv rator))
	(begin 
	  ;; First we must assert that it is an arrow type of appropriate arity:
#;	  (let ([newtype `(,@(map (lambda (_) (make-tcell)) rands) ->
			   ,(make-tcell))])
	    (types-equal! rattyp newtype exp))
	  ;; By instantiating a new type for the rator we allow let bound polymorphism.
	  (types-equal! (instantiate-type rattyp) `(,@rands -> ,result) exp)
  ;; BUT, any further restrictions to the ...................................................................................................................................................................................
)
	(types-equal! rattyp `(,@rands -> ,result) exp))
    ;(inspect (vector (export-type rator) rands result))
    result))

;; Assign a type to a Regiment letrec form.
; (define type-letrec
;   (lambda (ids rands body tenv)
;     (let ((tenv-for-rands
;             (extend-tenv
;               ids
;               (types-of-expressions rands tenv)
;               tenv)))
;       (type-of-expression body tenv-for-rands))))
#;
(define (type-letrec id* rhs* bod tenv)
  ;; Make new cells for all these types
  (let* ([rhs-types (map (lambda (_) (make-tcell)) id*)]
	 [tenv (tenv-extend tenv id* rhs-types #t)]) ; Pass flag to indicate let-bound.
    ;; Unify all these new type variables with the rhs expressions
    (for-each (lambda (type rhs)
		(types-equal! type (type-expression rhs tenv) rhs))
      rhs-types rhs*)
    (type-expression bod tenv)))

; ======================================================================

;;; Annotate expressions/programs with types.

;; This is an alternative version of type-expression above which
;; simultaneously infers types and annotates the binding forms
;; (letrec, lambda) with per-variable type information.  As such, this
;; is a bunch of duplicated code, and poses risks for bugs.
;; <br> <br>
;; Note, doesn't handle lazy-letrec, or already annotated programs.
(define (annotate-expression exp tenv)
  (define (map-ordered2 f ls1 ls2)
    (let loop ([ls1 ls1]
	       [ls2 ls2]
	       [acc '()])
      (cond
       [(and (null? ls1) (null? ls2)) (reverse! acc)]
       [(or (null? ls1) (null? ls2)) (error 'map-ordered2 "lists are different lengths")]
       [else
	(cons (f (car ls1) (car ls2)) 
	      (loop (cdr ls1) (cdr ls2) acc))])))
  (let l ((exp exp))
    (match exp 
      [,c (guard (constant? c)) (values c (type-const c))]
      [(quote ,c)               (values `(quote ,c) (type-const c))]
      [,prim (guard (symbol? prim) (regiment-primitive? prim))
	     (values prim (prim->type prim))]
      [,v (guard (symbol? v))
	  (let ((entry (tenv-lookup tenv v)))
	    (if entry 
		(values v entry)
		(error 'annotate-expression "no binding in type environment for var: ~a" v)))]
      [(if ,[l -> te tt] ,[l -> ce ct] ,[l -> ae at])
       (types-equal! tt 'Bool te)
       (types-equal! ct at exp)
       (values `(if ,te ,ce ,ae) ct)]

      [(lambda (,v* ...) ,body)
       ;; No optional type annotations currently.
       (let ([argtypes (map (lambda (_) (make-tcell)) v*)])
	 (mvlet ([(newbod bodtype) (annotate-expression body (tenv-extend tenv v* argtypes))])
	   (values `(lambda ,v* ,argtypes ,newbod)
		   `(,@argtypes -> ,bodtype))))]
      [(tuple ,[l -> e* t*] ...)  (values `(tuple ,e* ...) (list->vector t*))]
      [(tupref ,n ,len ,[l -> e t])
       (unless (and (qinteger? n) (qinteger? len))
	 (error 'annotate-expression 
		"invalid tupref syntax, expected constant integer index/len, got: ~a/~a" n len))
       (values `(tupref ,n ,len ,e)
	       (let ((newtypes (list->vector (map (lambda (_) (make-tcell)) (iota (qinteger->integer len))))))
		 (types-equal! t newtypes exp)
		 (vector-ref newtypes (qinteger->integer n))))]

      [(letrec ([,id* ,rhs*] ...) ,bod)
       ;; Make new cells for all these types
       (let* ([rhs-types (map (lambda (_) (make-tcell)) id*)]
	      [tenv (tenv-extend tenv id* rhs-types #t)])
	 ;; Unify all these new type variables with the rhs expressions
	 (let ([newrhs* 
		(map-ordered2 (lambda (type rhs)
		       (mvlet ([(newrhs t) (annotate-expression rhs tenv)])
			 (types-equal! type t rhs)
			 (printf "LETREC: type ~a rhs ~a\n" (export-type t) rhs)
			 newrhs))
		  rhs-types rhs*)])
	   (mvlet ([(bode bodt) (annotate-expression bod tenv)])
	     (values `(letrec ([,id* ,rhs-types ,newrhs*] ...) ,bode) bodt))))]

      [(,prim ,[l -> rand* t*] ...)
       (guard (regiment-primitive? prim))
       (values `(,prim ,rand* ...)
	       (type-app prim (prim->type prim) t* exp tenv))]

      ;[(app ,rat ,rands* ...)  (l `(,rat ,rands* ...))]
      [(app ,origrat ,[l -> rand* t*] ...)
       (mvlet ([(rator t1) (l origrat)])
	 (values `(app ,rator ,rand* ...)
		 (type-app origrat t1 t* exp tenv)))]
      ;; Allowing unlabeled applications for now:
      [(,rat ,rand* ...)  (l `(app ,rat ,rand* ...))]
      )))

;; This annotates the program, and then exports all the types to their
;; external form (stripped of mutable cells on the tvars).
(define (annotate-program p)
  (mvlet ([(e t) (annotate-expression p (empty-tenv))])
    ;; Now strip mutable cells from annotated expression.
    (values 
     (match e
       [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
       [(lambda ,v* ,t* ,[bod]) `(lambda ,v* ,(map export-type t*) ,bod)]
       [(tuple ,[e*] ...) `(tuple ,e* ...)]
       [(tupref ,n ,[e]) `(tupref ,n ,e)]
       [(letrec ([,id* ,t* ,[rhs*]] ...) ,[bod])
	`(letrec ([,id* ,(map export-type t*) ,rhs*] ...) ,bod)]
       [(,[rat] ,[rand*] ...) `(,rat ,rand* ...)]
       [,other other])   ; don't need to do anything here...
     (export-type t))))
      
; ======================================================================

;;; The unifier.

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
    ;; If one of them is a symbol, it might be a type alias.
    [[,x ,y] (guard (or (symbol? x) (symbol? y)))
     (let ([sym    (if (symbol? x) x y)]
	   [nonsym (if (symbol? x) y x)])
       (let ([entry (assq sym regiment-type-aliases)])
	 (if entry 
	     (types-equal! (instantiate-type (cadr entry)) nonsym exp)
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
	 (error 'types-equal! "type constructors do not match: ~a and ~a in ~a" x1 y1 exp))
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
       [,other (error "procedure type ~a does not match: "
		      `(,@yargs -> ,y) other)])]
    [,otherwise (raise-type-mismatch t1 t2 exp)]))
		   
;; This helper mutates a tvar cell while protecting against cyclic structures.
(define (tvar-equal-type! tvar ty exp)
  (DEBUGASSERT (type? ty))
  (match tvar ;; Type variable should be a quoted pair.
    [',pr 
     (if (not (pair? pr))
	 (error 'tvar-equal-type! "bad tvar here: ~a" tvar))
     (if (cdr pr)

	 (types-equal! (cdr pr) ty exp)

	 (begin (no-occurrence! (tcell->name tvar) ty exp)
		(set-cdr! pr ty)))]))

;; This makes sure there are no cycles in a tvar's mutable cell.
;; .returns #t if there are no loops, or throws an error otherwise.
(define (no-occurrence! tvar ty exp)
  (DEBUGASSERT (type? ty))
  (match ty
    [#f #t]
    [,s (guard (symbol? s)) #t]
    ['(,tyvar . ,[tyt])
     (if (equal? tyvar tvar)
	 (raise-occurrence-check tvar ty exp))]
    [(,[arg*] ... -> ,[res]) res]
    [(,C ,[t*] ...) (guard (symbol? C)) #t] ; Type constructor
    [#(,[t*] ...) #t]
;    [,other (inspect (vector other tvar))]
    [,other (error 'no-occurrence! "malformed type: ~a" ty)]
    ))


; ======================================================================
;;; Unit tests.

;; Unit tests.
(define these-tests
  `([(begin (reset-tvar-generator) (let ((x (prim->type 'car))) (set-cdr! (car (cdaddr x)) 99) x))
     ((List '(a . 99)) -> '(a . 99))]
    [(,type-expression '(if #t 1. 2.) (empty-tenv))         Float]
    [(export-type (,type-expression '(+ 1 1) (empty-tenv))) Integer]
;    [(export-type (,type-lambda '(v) '(+ v v) (empty-tenv))) (Integer -> Integer)]
    [(export-type (,type-expression '(cons 3 (cons 4 '())) (empty-tenv))) (List Integer)]
    [(export-type (,type-expression '(cons 1 '(2 3 4)) (empty-tenv))) (List Integer)]
    [(export-type (,type-expression '(cons 1 '(2 3 4.)) (empty-tenv))) error]

#;    [(export-type (,type-lambda '(v) 'v (empty-tenv)))
     ,(lambda (x)
	(match x
	  [(',a -> ',b) (eq? a b)]
	  [,else #f]))]

    [(export-type (,type-expression '((lambda (v) v) 3) (empty-tenv))) Integer]
     
    [(export-type (,type-expression '(lambda (y) (letrec ([x y]) (+ x 4))) (empty-tenv)))
     (Integer -> Integer)]

    [(export-type (,type-expression '(rmap (lambda (n) (sense 'light n)) world) (empty-tenv)))
     (Area Integer)]
    
    [(export-type (,type-expression '(tuple 1 2.0 3) (empty-tenv)))
     #3(Integer Float Integer)]

    [(export-type (,type-expression '(lambda (x) (tupref 0 3 x)) (empty-tenv)))
     ,(lambda (x)
	(match x
	  [(#(,v1 ,_ ,__) -> ,v2) (equal? v1 v2)]
	  [,else #f]))]

    [(export-type (,type-expression 
		   '(letrec ([f (lambda (x) x)])
		      (tuple (f 3) "foo" f))
		  (empty-tenv)))
     #(Integer String ('unspecified -> 'unspecified))
     ;#(Integer String (Integer -> Integer))
     ]
  
#;    ["Lambda bound arrow types are not polymorphic."
     (export-type (,type-expression '(lambda (f) (tuple (f 3) f)) (empty-tenv)))
     ,(lambda (x) 
	(match x
	  [((Integer -> ,v1) -> #(,v2 (Integer -> ,v3)))
	   (guard (equal? v1 v2) (equal? v2 v3)) #t]
	  [,else #f]))]

#;    ["Non polymorphic funs cannot be applied differently."
     (export-type (,type-expression '(lambda (f) (tuple (f 3) (f "foo") f)) (empty-tenv)))
     error]
    
    [(export-type (,type-expression 
		   '(letrec ()
		      (smap2
		       (lambda (n1 n2) (tuple n1 n2))
		      (anchor-at 50 10)
		      (anchor-at 30 40))) (empty-tenv)))
     (Signal #(Node Node))]


    [(mvlet ([(p t) (annotate-program '(letrec ([f (lambda (x) x)]) 3))]) p)
     ,(lambda (x)
	(match x
	  [(letrec ([f (,v1 -> ,v2) (lambda (x) (,v3) x)]) 3)
	   (guard (equal? v1 v2) (equal? v2 v3)) #t]
	  [,else #f]))]

    ;; This one doesn't actually verify shared structure:
    [(instantiate-type '((#5='(a . #f) -> #6='(b . #f)) (Area #5#) -> (Area #6#)))
     ((#7='(unspecified . #f) -> #8='(unspecified . #f)) (Area #7#) -> (Area #8#))]
#;
    ["This should not be allowed by the type system:" 
     (export-type (,type-expression 
		   '(lambda (g)
		      (letrec ([f g])
			(tuple (f 3) (f #t))))
		   (empty-tenv)))
     error]
#;
   ["Test type-app for a particular situation that arose."
    (type-app 'myfun '#0='(k . #1='(_c  . #f)) '(#2='(h quote (j . Region)))
		'(app myfun hood)
		'((result '(l . #f) #t) (myfun #0# #t) (hood #2# #t) (n '(g . Node) #f)
		  (_threshold '(f . #f) #t) (temp '(e . #f) #t) (abovethresh '(d . #f) #t)
		  (count-nbrs #1# #t) (heat-events '(b . #f) #t)
		  (local-results '(a . #f) #t)))
    ????
    ]

    ))

(define test-this (default-unit-tester "Hindley Milner Type Inferencer" these-tests))
;; Unit tester.
(define test-inferencer test-this)

) ; End module. 
