
;;;; .title Generic Tree Walker for Regiment CORE.

;;;; [2006.08.27] This is (will be) used by several passes which
;;;; operate on Regiment Core language.  I've used such a mechanism
;;;; for TML for a long time, but am only now porting my Regiment
;;;; passes over to this method.

;;;; Hopefully this will be wide enough to treat WaveScope and Regiment.

;;;; See tml_generic_traverse.ss for more description.

;;;; NOTE: Must run AFTER alpha-renaming, assumes keyword names are
;;;; not used as variables.

; =======================================================================
; Notes:

; [2006.08.27] Writing wavescript_nominalize-types.ss to use this.


; =======================================================================


(module reg_core_generic_traverse  mzscheme
  (require (lib "include.ss")  
           "../constants.ss"
           "../../plt/iu-match.ss"
	   "prim_defs.ss"
	   ;(lib "compat.ss")
           (all-except "../../plt/helpers.ss" filter)
           (all-except "../compiler_components/regiment_helpers.ss" filter)
           "../compiler_components/hm_type_inference.ss"
           )
  
  ;; Insure provision of verify-regiment:
  (provide core-generic-traverse
	   core-generic-traverse/types
           test-this test-core-generic-traverse
	   binding-form? 
	   binding-form->other-exprs binding-form->scoped-exprs
	   binding-form->vars binding-form->types
	   core-free-vars
	   )

  (chezimports prim_defs
	       (except helpers test-this these-tests)
	       ;regiment_helpers
	       )

;; Here is a simple procedural interface.

;; TODO: CONSOLIDATE: Rewrite the define-pass [Bindings ...] clause to use these helpers:

;; This is for double-checking our work below.
(define (binding-form? x)
  ;; This doesn't verify the *validity* of the form:
  (and (pair? x)
       (memq (car x) '(for let let* letrec lazy-letrec lambda))
       ))
;; Returns in-scope expressions froma a binding form.
(define (binding-form->scoped-exprs x)
  (match x
    [(,letrec ([,lhs* ,ty* ,rhs*] ...) ,bod) (guard (memq letrec '(letrec lazy-letrec)))
     (cons bod rhs*)]
    [(lambda ,vars ,types ,bod) (list bod)]
    [(let ,_ ,bod) (list bod)]
    [(for (,i ,st ,en) ,bod) (list bod)]
    [(let* . ,_) (error 'binding-form->scoped-exprs "doesn't really make sense with let*: ~s" x)]
    [,other (error 'binding-form->scoped-exprs "not a binding form: ~s" x)]
    ))
;; Returns not-in-scope expressions from a binding form.
(define (binding-form->other-exprs x)
  (match x
    [(,letrec . ,_) (guard (memq letrec '(letrec lazy-letrec))) '()]
    [(lambda . ,_) '()]
    [(let ([,lhs* ,ty* ,rhs*] ...) ,bod) rhs*]
    [(for (,i ,st ,en) ,bod) (list st en)]
    [(let* . ,_) (error 'binding-form->other-exprs "doesn't really make sense with let*: ~s" x)]
    [,other (error 'binding-form->other-exprs "not a binding form: ~s" x)]
    ))
;; Returns bound-vars from a binding form.
(define (binding-form->vars x)
  (match x
    [(,letrec ([,lhs* ,ty* ,rhs*] ...) ,bod) (guard (memq letrec '(letrec lazy-letrec))) 
     lhs*]
    [(lambda (,vars ...) ,types ,bod)       vars]
    [(let ([,lhs* ,ty* ,rhs*] ...) ,bod)    lhs*]
    [(for (,i ,st ,en) ,bod)            (list i)]
    [(let* . ,_) (error 'binding-form->vars "doesn't really make sense with let*: ~s" x)]
    [,other (error 'binding-form->vars "not a binding form: ~s" x)]
    ))
;; Returns types from a binding form.
(define (binding-form->types x)
  (match x
    [(,letrec ([,lhs* ,ty* ,rhs*] ...) ,bod) (guard (memq letrec '(letrec lazy-letrec)))
     ty*]
    [(lambda (,vars ...) (,types ...) ,bod) types]
    [(let ([,lhs* ,ty* ,rhs*] ...) ,bod)    ty*]
    [(for (,i ,st ,en) ,bod)                '(Int)]
    [(let* . ,_) (error 'binding-form->types "doesn't really make sense with let*: ~s" x)]
    [,other (error 'binding-form->types "not a binding form: ~s" x)]
    ))

;================================================================================

;; Generic traversal over Regiment Expressions.
(define core-generic-traverse 
  ;; .param driver driver
  ;; .param fuser  fuser
  ;; .param expr   expression
  ;;
  ;; The "driver" takes the first shot at an expression, transforms the
  ;; subcases that it wants to, and then hands the rest on to its
  ;; continuation to do the automated traversal. The automated
  ;; traversal, in turn, uses the "fuse" function to glue back together
  ;; the parts of the tree.  The fuse function is passed a list of child
  ;; exprss and another continuation representing the "default fuser" which
  ;; just puts the expression back together like it was before (given the child terms).
  ;;       <br><br>
  ;; Types:    <br>
  ;;   driver : expr, (expr -> 'intermediate) -> 'result)                <br>
  ;;   fuse : 'intermediate list, (expr list -> expr) -> 'intermediate)  <br>
  ;;   e : expr                                                          <br>
  ;; Return value: 'result                                               <br>
  (let ()
    (define (build-traverser driver fuse e)
;      (call/cc inspect)
      (let loop ((e e))	
	(driver e 
	  ;; This is the fallthrough/autolooper function that is passed to the driver.
          (case-lambda 
	    ;; In this case we have a change to the driver function as we go down.
	    [(expr newdriver) (build-traverser newdriver fuse e)]
	    [(expression)       	      
	     (match  expression
;	  [,x (guard (begin (printf "~nCoreGenTrav looping: ") (display-constrained (list x 50)) (newline) #f)) 3]

	  [,const (guard (constant? const)) (fuse () (lambda () const))]
	  [,num (guard (number? num)) (error 'core-generic-traverse "unquoted literal: ~s" num)]

          ;; This is for debugging, we just don't touch it:
	  ;[(BLACKBOX ,expr ...)             (fuse () (lambda () `(BLACKBOX ,expr ...)))]
	  ;[(dbg ,[loop -> rand*] ...)  (fuse rand* (lambda rand* `(dbg ,rand* ...)))]

	  ;; We don't put any restrictions (HERE) on what can be in a quoted constant:
	  [(quote ,const)                (fuse ()      (lambda () `(quote ,const)))]
	  [,var (guard (symbol? var))    (fuse ()      (lambda () var))]

	  [(if ,[loop -> a] ,[loop -> b] ,[loop -> c])
	   (fuse (list a b c) (lambda (x y z) `(if ,x ,y ,z)))]

	  ;; These are special syntax:
	  [(tupref ,n ,m ,[loop -> exp])
	   (DEBUGASSERT fixnum? n)
	   (DEBUGASSERT fixnum? m)
	   ;(inspect `(tupref ,n ,m ,exp))
	   (fuse (list exp) (lambda (exp) `(tupref ,n ,m ,exp)))]

	  [(,varargkeyword ,[loop -> args] ...)
	   (guard (memq varargkeyword '(unionN tuple)))
	   (fuse args (lambda args `(,varargkeyword ,args ...)))]

	  ;; Adding this special syntax as well (output of nominalize-types)
	  [(make-struct ,name  ,[loop -> args] ...)
	   (guard (symbol? name))
	   (fuse args (lambda args `(make-struct ,name . ,args)))]
	  [(struct-ref ,[loop -> expr] ,fldname)
	   (guard (symbol? fldname))
	   (fuse (list expr) (lambda (expr) `(struct-ref ,expr ,fldname)))]

	  ;; No looping on types.
	  ;; Let is treated same as letrec because we're not maintaining environment.
	  [(,letrec ([,lhs* ,typ* ,[loop -> rhs*]] ...) ,[loop -> bod])
	   (guard (memq letrec '(letrec lazy-letrec let)))
	   ;; By convention, you get the body first:
	   (fuse (cons bod rhs*)
		 (lambda (x . y*) `(,letrec ([,lhs* ,typ* ,y*] ...) ,x)))]

	  ;; Letrec's are big and hairy enough that we try to give slightly better error messages.
	  [(letrec ([,lhs* ,rhs*] ...) ,bod)
	   (warning 'core-generic-traverse "letrec does not have types:\n ~s\n\n" 
		  `(letrec ([,lhs* ,rhs*] ...) ,bod))
	   (inspect `(letrec ([,lhs* ,rhs*] ...) ,bod))
	   (error 'core-generic-traverse "")]
	  [(letrec ,other ...)
	   (warning 'core-generic-traverse "letrec is badly formed:\n  ~s\n\n" 
		    `(letrec  ,other ...))
	   (inspect `(letrec ,other ...))
	   (error 'core-generic-traverse "")]

	  ;; Again, no looping on types.  This is an expression traversal only.
	  [(lambda (,v* ...) (,t* ...) ,[loop -> e])
	   (fuse (list e) (lambda (x) `(lambda (,v* ...) (,t* ...) ,x)))]
	  [(lambda (,v* ...) ,e)
	   (warning 'core-generic-traverse "lambda does not have types:\n ~s\n\n" 
		    `(lambda (,v* ...) ,e))
	   (inspect `(lambda (,v* ...) ,e))
	   (error 'core-generic-traverse "")]
	  [(lambda ,other ...)
	   (warning 'core-generic-traverse "lambda is badly formed:\n  ~s\n\n" 
		    `(lambda  ,other ...))
	   (inspect `(lambda ,other ...))
	   (error 'core-generic-traverse "")]
	  
	  ; WAVESCRIPT
	  ; ========================================
	  ; Because of WaveScript we have effects and other stuff:
	  ; (Well, at least we have effects in the object language.)
	  ; Should enforce a barrier.  (For the time being, syntactic.)

	  [(set! ,v ,[loop -> e])        (fuse (list e)    (lambda (x) `(set! ,v ,x)))]

	  ;; Always run make-begin, hope this is safe:
	  [(begin ,[loop -> xs] ...)     (fuse xs       (lambda ls (make-begin `(begin ,ls ...))))]
	  [(for (,i ,[loop -> start] ,[loop -> end]) ,[loop -> body])
	   (fuse (list start end body)
		 (lambda (st en bod) `(for (,i ,st ,en) ,bod)))]

	  ; ========================================

	  ;; Applications must be tagged explicitely.
	  [(app ,[loop -> rator] ,[loop -> rands] ...)
	   (fuse (cons rator rands) (lambda (x . ls)`(app ,x ,ls ...)))]
	  [(assert-type ,t ,[loop -> e])
	   (fuse (list e) (lambda (x) `(assert-type ,t ,x)))]

	  [(,prim ,[loop -> rands] ...)
	   (guard (or (regiment-primitive? prim)
		      (basic-primitive? prim)))
	   (fuse rands (lambda ls `(,prim . ,ls)))]

	  [,otherwise (warning 'core-generic-traverse "bad expression: ~s" otherwise)
		      (inspect otherwise)
		      (error 'core-generic-traverse "")]
	  )]	    ))))

  ;; Main body of core-generic-traverse:
  (case-lambda 
    [(d f e) (build-traverser d f e)]
    [(d f) (lambda (e) (build-traverser d f e))]
    [(d) (lambda (e) (build-traverser d (lambda (ls k) (apply k ls)) e))]
    )))

;; This version carries around the type environment and always presents it to the user driver.
(define core-generic-traverse/types
  (let ()
    (define (traverser drive fuse initial-tenv)
      (let loop ([tenv initial-tenv])
	;; We wrap the user's driver:
	(define (newdriver x autoloop)
	   ;; We dispatch to the user first, if they touch
	   ;; letrec/lambda/etc they'd better handle the tenv themselves.
	  (drive x tenv
		 ;; Here we wrap the autoloop function to be type-aware
		 (case-lambda 
		   [(x tenv)
		    (DEBUGASSERT (tenv? tenv))
		    (match x 
		      ;; We overload the cases that require modifying the tenv.
		      [(lambda (,v* ...) (,ty* ...) ,bod)
		       (fuse (list ((loop (tenv-extend tenv v* ty*)) bod))
			     (lambda (x) `(lambda (,v* ...) (,ty* ...) ,x)))]
		      [(,letrec ([,lhs* ,ty* ,rhs*] ...) ,bod)
		       (guard (memq letrec '(letrec lazy-letrec)))
		       (let* ([newtenv (tenv-extend tenv lhs* ty*)]
			      [f (loop newtenv)])
			 (fuse (cons (f bod) (map f rhs*))
			       (lambda (x . y*) `(,letrec ([,lhs* ,ty* ,y*] ...) ,x))))]

		      [(for (,i ,[(loop tenv) -> st] ,[(loop tenv) -> en]) ,bod)
		       (let ([newtenv (tenv-extend tenv (list i) '(Int))])
			 (fuse (list st en ((loop newtenv) bod))
			       (lambda (st en bod) `(for (,i ,st ,en) ,bod)))
			 )]
		      
		      [(let ([,lhs* ,ty* ,[(loop tenv) -> rhs*]] ...) ,bod)
		       (let ([newtenv (tenv-extend tenv lhs* ty*)])
			 (fuse (cons ((loop newtenv) bod) rhs*)
			       (lambda (bod . rhs*) `(let ([,lhs* ,ty* ,rhs*] ...) ,bod))))
		       ]

		      ;; If it's not one of these we use the old generic-traverse autoloop.
		     ;; This will in turn call newdriver again from the top.
		      [,other 
		       (DEBUGASSERT (compose not binding-form?) other)
		       (autoloop other)])]
		   [other (error 'core-generic-traverse/types
			       "user driver function called fallthrough function (autolooper) with wrong number of args, expected expr & tenv:\n~s"
			       other)])))
	;; Now we call the original generic-traverse
	(core-generic-traverse newdriver fuse)))
    (case-lambda
      [(d)     (lambda (e) ((traverser d (lambda (ls k) (apply k ls)) (empty-tenv)) e))]
      [(d f)   (lambda (e) ((traverser d f (empty-tenv)) e))]
      [(d f e)             ((traverser d f (empty-tenv)) e)]
      [(d f e tenv)        ((traverser d f tenv) e)])))

; ================================================================================
;;; Derived utilities.

;; This should be a general-purpose free-vars utility for the core lang.
(define (core-free-vars exp) 
  (core-generic-traverse
   (lambda (x fallthru)
     (match x        
       [,v (guard (symbol? v)) (list v)]
       [,form (guard (binding-form? form))

	      (let ([scoped (binding-form->scoped-exprs form)]
		    [vars (binding-form->vars form)]
		    [others (binding-form->other-exprs form)]
		    )			      
;	      (inspect `([scoped ,scoped] [vars ,vars] [others ,others] ))

	      (union  
	       (difference (apply append (map core-free-vars scoped))
			   vars)
	       (apply append (map core-free-vars others))
	       )

		)]
       [,x (fallthru x)]))
   (lambda (ls k) (apply union ls))
   exp
   ))

; ================================================================================

(define these-tests
  `(	     
    ["Simple generic traversal with type environment"
     (core-generic-traverse/types 
      (lambda (x tenv loop) (loop x tenv))
      (lambda (ls k) (apply k ls))
      '(+ '3 '4))
     (+ '3 '4)]
    
    ["run traversal over rfold & letrec"
     (core-generic-traverse/types 
      (lambda (x tenv loop) (loop x tenv))
      (lambda (ls k) (apply k ls))          
      '(rfold + 0
	      (letrec ([myhood Region (khood (anchor-at 50 10) 2)])
		(rmap nodeid myhood))))
     (rfold + 0 (letrec ((myhood Region (khood (anchor-at 50 10) 2))) (rmap nodeid myhood)))]

    ["run traversal over lambda"
     (core-generic-traverse/types 
      (lambda (x tenv loop) (loop x tenv))
      (lambda (ls k) (apply k ls))
      '(lambda (x) (Int) (+ '3 '4)))
     (lambda (x) (Int) (+ '3 '4))]
    
    ["core-free-vars "
     (core-free-vars '(audioFile '"countup.raw" '2 x))
     (x)]

    ;; TODO: To test this mechanism we should have some really big
    ;; source files that we read in, and run a generic traversal where
    ;; we convert all variables/numbers to strings.  Then, any
    ;; non-keyword symbols left in the program mean the generic traversal is
    ;; broken.

    ))

(define test-this (default-unit-tester
		    "Regiment Core Generic-Traverse: abstracts tree-walks over intermediate code."
		    these-tests))

(define test-core-generic-traverse test-this)

) ;; End module


