
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
;	   (all-except "../../plt/rn-match.ss" match-lambda let-match)
	   "prim_defs.ss"
	   ;(lib "compat.ss")
           (all-except "../util/helpers.ss" filter)
           (all-except "../compiler_components/regiment_helpers.ss" filter)
           "../compiler_components/type_environments.ss"
           )
  
  ;; Insure provision of verify-regiment:
  (provide core-generic-traverse
	   core-generic-traverse/types
           test-this test-core-generic-traverse
	   binding-form? 
	   binding-form->unscoped-exprs binding-form->scoped-exprs
	   binding-form->vars binding-form->types
	   binding-form-visit-knowncode
	   core-free-vars
	   )

  (chezimports prim_defs
	       (except helpers test-this these-tests)
	       ;regiment_helpers
	       ;; Switching this much-used module over to my faster match:
	       rn-match
	       )

;;============================================================
;;; Procedural Interface 

;;; Here is a simple procedural interface that allows generic treatment
;;; of binding forms.

;; TODO: CONSOLIDATE: Rewrite the define-pass [Bindings ...] clause to use these helpers:

;; Note: There are two relevent ways to partition the subexpressions
;; contained in a binding form.  You can separate out the expressions
;; that are in the scope of the bound variables, vs. those that are
;; not.  Or you can separate out the variable bindings for which we
;; have code (let-style) vs. those we do not (lambda).
;;   Here we provide methods for both.

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
(define (binding-form->unscoped-exprs x)
  (match x
    [(,letrec . ,_) (guard (memq letrec '(letrec lazy-letrec))) '()]
    [(lambda . ,_) '()]
    [(let ([,lhs* ,ty* ,rhs*] ...) ,bod) rhs*]
    [(for (,i ,st ,en) ,bod) (list st en)]
    [(let* . ,_) (error 'binding-form->unscoped-exprs "doesn't really make sense with let*: ~s" x)]
    [,other (error 'binding-form->unscoped-exprs "not a binding form: ~s" x)]
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

;; Extracts a many-holed context for rebuilding the form.
#;
(define (binding-form->k x)
  (match x
    [(,letrec ([,lhs* ,ty* ,rhs*] ...) ,bod) (guard (memq letrec '(letrec lazy-letrec)))
     (lambda (lhs* ty* rhs* other ))
     ty*]
    [(lambda (,vars ...) (,types ...) ,bod) types]
    [(let ([,lhs* ,ty* ,rhs*] ...) ,bod)    ty*]
    [(for (,i ,st ,en) ,bod)                '(Int)]
    [(let* . ,_) (error 'binding-form->types "doesn't really make sense with let*: ~s" x)]
    [,other (error 'binding-form->types "not a binding form: ~s" x)]
    ))

;; Some of the bindings we have the code for (e.g. let).  This returns
;; those bindings along with a way to reconstruct the original form.
;;   For bindings we don't have the code for, the rhs is "#f".
;;   ORTHOGONAL to ->scoped vs. ->unscoped above.
;; .returns 5 values: vars types knownrhs otherexpr reconstructor
(define (binding-form-visit-knowncode x)
  (match x
    [(,lett ([,lhs* ,ty* ,rhs*] ...) ,bod) 
     (guard (memq lett '(letrec lazy-letrec let let*)))
     (values lhs* ty* rhs* (list bod)
	     (lambda (lhs* ty* rhs* other) 
	       (DEBUGASSERT (and (list? other) (fx= 1 (length other))))
	       `(,lett ,(map list lhs* ty* rhs*)
		       ,(car other))))]
    [(lambda (,vars ...) (,types ...) ,bod) 
     (values vars types (make-list (length types) #f) (list bod)
	     (lambda (lhs* ty* rhs* other)
	       (DEBUGASSERT (and (list? other) (fx= 1 (length other))))
	       (DEBUGASSERT (eq? #f (ormap id rhs*)))
	       `(lambda ,lhs* ,ty* ,(car other))))]
    ;; This is a weird case.  I guess i is bound to start.
    [(for (,i ,st ,en) ,bod)
     (values (list i) '(Int) (list st) (list en bod)
	     (lambda (lhs* ty* rhs* other)
	       `(for (,(car lhs*) ,(car rhs*) ,(car other))
		    ,(cadr other))))]
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

	  [,const (guard (simple-constant? const)) (fuse () (lambda () const))]
	  [,num (guard (number? num)) (error 'core-generic-traverse "unquoted literal: ~s" num)]

          ;; This is for debugging, we just don't touch it:
	  ;[(BLACKBOX ,expr ...)             (fuse () (lambda () `(BLACKBOX ,expr ...)))]
	  ;[(dbg ,[loop -> rand*] ...)  (fuse rand* (lambda rand* `(dbg ,rand* ...)))]

	  ;; We don't put any restrictions (HERE) on what can be in a quoted constant:
	  [(quote ,const)                (fuse ()      (lambda () `(quote ,const)))]
	  [,var (guard (symbol? var))    (fuse ()      (lambda () var))]

	  [(if ,[loop -> a] ,[loop -> b] ,[loop -> c])
	   (fuse (list a b c) (lambda (x y z) `(if ,x ,y ,z)))]

	  [(wscase ,[loop -> val] (,TC ,[loop -> rhs*]) ...)
;	   (inspect pat*)  (inspect rhs*)
	   (fuse (cons val rhs*)
		 (lambda (v . r*) `(wscase ,v . ,(map list TC r*))))]

	  ;; These are special syntax:
	  [(tupref ,n ,m ,[loop -> exp])
	   (DEBUGASSERT fixnum? n)
	   (DEBUGASSERT fixnum? m)
	   ;(inspect `(tupref ,n ,m ,exp))
	   (fuse (list exp) (lambda (exp) `(tupref ,n ,m ,exp)))]

	  ;; Being VERY lenient.  Vector is here just because it's used in static-elaborate. [2007.03.06]
	  [(,varargkeyword ,[loop -> args] ...)
	   (guard (memq varargkeyword '(unionN tuple vector)))
	   (fuse args (lambda args `(,varargkeyword ,@args)))]

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
		 (lambda (x . y*) `(,letrec ,(map list lhs* typ* y*) ,x)))]

	  ;; Letrec's are big and hairy enough that we try to give slightly better error messages.
	  [(letrec ([,lhs* ,rhs*] ...) ,bod)
	   (warning 'core-generic-traverse "letrec does not have types:\n ~s\n\n" 
		  `(letrec ([,lhs* ,rhs*] ...) ,bod))
	   (inspect `(letrec ,(map list lhs* rhs*) ,bod))
	   (error 'core-generic-traverse "")]
	  [(letrec ,other ...)
	   (warning 'core-generic-traverse "letrec is badly formed:\n  ~s\n\n" 
		    `(letrec ,@other))
	   (inspect `(letrec ,@other))
	   (error 'core-generic-traverse "")]

	  ;; Again, no looping on types.  This is an expression traversal only.
	  [(lambda (,v* ...) (,t* ...) ,[loop -> e])
	   (fuse (list e) (lambda (x) `(lambda ,v* ,t* ,x)))]
	  [(lambda (,v* ...) ,e)
	   (warning 'core-generic-traverse "lambda does not have types:\n ~s\n\n" 
		    `(lambda ,v* ,e))
	   (inspect `(lambda ,v* ,e))
	   (error 'core-generic-traverse "")]
	  [(lambda ,other ...)
	   (warning 'core-generic-traverse "lambda is badly formed:\n  ~s\n\n" 
		    `(lambda ,@other))
	   (inspect `(lambda ,@other))
	   (error 'core-generic-traverse "")]
	  
	  ; WAVESCRIPT
	  ; ========================================
	  ; Because of WaveScript we have effects and other stuff:
	  ; (Well, at least we have effects in the object language.)
	  ; Should enforce a barrier.  (For the time being, syntactic.)

	  [(set! ,v ,[loop -> e])        (fuse (list e)    (lambda (x) `(set! ,v ,x)))]

	  ;; Always run make-begin, hope this is safe:
	  [(begin ,[loop -> xs] ...)     
	   (fuse xs       (lambda ls (make-begin `(begin ,@ls))))]
	  [(for (,i ,[loop -> start] ,[loop -> end]) ,[loop -> body])
	   (fuse (list start end body)
		 (lambda (st en bod) `(for (,i ,st ,en) ,bod)))]
	  [(while ,[loop -> test] ,[loop -> body])
	   (fuse (list test body)
		 (lambda (tst bod) `(while ,tst ,bod)))]

	  ; ========================================

	  ;; Applications must be tagged explicitely.
	  [(app ,[loop -> rator] ,[loop -> rands] ...)
	   (fuse (cons rator rands) (lambda (x . ls)`(app ,x ,@ls)))]
	  [(construct-data ,tc ,[loop -> rand]) 
	   (fuse (list rand) (lambda (r) `(construct-data ,tc ,r)))]
	  [(foreign-app ',realname ,[loop -> rator] ,[loop -> rands] ...)
	   (fuse (cons rator rands) (lambda (x . ls) `(foreign-app ',realname ,x ,@ls)))]

	  [(,annot ,t ,[loop -> e])
	   (guard (memq annot '(assert-type src-pos)))
	   (fuse (list e) (lambda (x) `(,annot ,t ,x)))]

	  ;; Annotations:
;	  [(src-pos     ,p ,[loop -> e]) (fuse (list e) (lambda (x) `(src-pos     ,p ,x)))]
;	  [(assert-type ,t ,[loop -> e]) (fuse (list e) (lambda (x) `(assert-type ,t ,x)))]

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
		    (DEBUGASSERT tenv? tenv)
		    (match x 
		      ;; We overload the cases that require modifying the tenv.
		      [(lambda (,v* ...) (,ty* ...) ,bod)
		       (fuse (list ((loop (tenv-extend tenv v* ty*)) bod))
			     (lambda (x) `(lambda ,v* ,ty* ,x)))]
		      [(,letrec ([,lhs* ,ty* ,rhs*] ...) ,bod)
		       (guard (memq letrec '(letrec lazy-letrec)))
		       (let* ([newtenv (tenv-extend tenv lhs* ty*)]
			      [f (loop newtenv)])
			 (fuse (cons (f bod) (map f rhs*))
			       (lambda (x . y*) `(,letrec ,(map list lhs* ty* y*) ,x))))]

		      [(for (,i ,[(loop tenv) -> st] ,[(loop tenv) -> en]) ,bod)
		       (let ([newtenv (tenv-extend tenv (list i) '(Int))])
			 (fuse (list st en ((loop newtenv) bod))
			       (lambda (st en bod) `(for (,i ,st ,en) ,bod)))
			 )]
		      
		      [(let ([,lhs* ,ty* ,[(loop tenv) -> rhs*]] ...) ,bod)
		       (let ([newtenv (tenv-extend tenv lhs* ty*)])
			 (fuse (cons ((loop newtenv) bod) rhs*)
			       (lambda (bod . rhs*) `(let ,(map list lhs* ty* rhs*) ,bod))))
		       ]
	
		      ;; We don't have to handle case, because it doesn't directly bind:
#;
		      [(wscase ,test (,pat* ,rhs*) ...)
		       (let* ([type (recover-type test tenv)]     ;; TEMP!! INEFFICIENT!!
			      [val  ((loop tenv) -> val)]
			      [newrhs (map (lambda (pat rhs)
					    (let ([newtenv (tenv-extend-pattern tenv pat TYPE?)])
					      ((loop newtenv) rhs)))
				       pat* rhs*)])
			 (fuse (cons val rhs*)
			       (lambda (v . r*) `(wscase ,v . ,(map list pat* r*))))
			 )]

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
;; Could use a more efficient set representation...
(define (core-free-vars exp) 
  (core-generic-traverse
   (lambda (x fallthru)
     (match x        
       [,v (guard (symbol? v)) 
	   (if (regiment-primitive? v)
	       '()
	       (list v))]
       [,form (guard (binding-form? form))

	      (let ([scoped (binding-form->scoped-exprs form)]
		    [vars (binding-form->vars form)]
		    [others (binding-form->unscoped-exprs form)]
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

(define-testing these-tests
  `(	     
    ["Simple generic traversal with type environment"
     (core-generic-traverse/types 
      (lambda (x tenv loop) (loop x tenv))
      (lambda (ls k) (apply k ls))
      '(+_ '3 '4))
     (+_ '3 '4)]
    
    ["run traversal over rfold & letrec"
     (core-generic-traverse/types 
      (lambda (x tenv loop) (loop x tenv))
      (lambda (ls k) (apply k ls))          
      '(rfold +_ 0
	      (letrec ([myhood Region (khood (anchor-at 50 10) 2)])
		(rmap nodeid myhood))))
     (rfold +_ 0 (letrec ((myhood Region (khood (anchor-at 50 10) 2))) (rmap nodeid myhood)))]

    ["run traversal over lambda"
     (core-generic-traverse/types 
      (lambda (x tenv loop) (loop x tenv))
      (lambda (ls k) (apply k ls))
      '(lambda (x) (Int) (+_ '3 '4)))
     (lambda (x) (Int) (+_ '3 '4))]
    
    ["core-free-vars "
     (core-free-vars '(audioFile '"countup.raw" '2 x))
     (x)]

    ;; TODO: To test this mechanism we should have some really big
    ;; source files that we read in, and run a generic traversal where
    ;; we convert all variables/numbers to strings.  Then, any
    ;; non-keyword symbols left in the program mean the generic traversal is
    ;; broken.

    ))

(define-testing test-this (default-unit-tester
		    "Regiment Core Generic-Traverse: abstracts tree-walks over intermediate code."
		    these-tests))

(define test-core-generic-traverse test-this)

) ;; End module


