#!r6rs

;;;; .title The Regiment Type Inferencer.

;;;; Below is the representation for types used by the type checker.
;;;; Note: this may get out of date.  A better thing to check is the
;;;; machine-checked grammar for types listed in grammars.ss
;;;;
;;;; type := T                ; symbol - an atomic type           
;;;; type := 'a               ; quoted symbol - a type variable
;;;; type := (T type ...)     ; a type constructor
;;;; type := (type -> type)   ; a function type
;;;; type := #(type ...)      ; a tuple type
;;;; type := (Pointer string) ; a special case
;;;;
;;;; And internally, the algorithm uses these constructions:
;;;; type := '(a . type)      ; a type variable with binding
;;;; type := '(a . #f)        ; a type variable with no info
;;;;
;;;; And for our particular brand of let-bound polymorphism:
;;;; A place to record type constraints and LUB/unify later:
;;;; type := (LATEUNIFY lub type) 
;;;; lub  := #f
;;;; lub  := type
;;;; lub  := (LUB type type)

;;;; Also see the primitive type definitions in prim_defs.ss

;;;; Primary ENTRY POINTS are:
;;;;  type-expression (obsolete)
;;;;  annotate-expression
;;;;  annotate-program
;;;;  types-compat?

;;  validate-types [2006.10.12] TODO: check the existing types, should be less expensive

;; [2007.02.21] Adding LUBs to the internal type grammar.

;; TODO: [2006.04.28] Add a "normalize" procedure that gets rid of any
;; type aliases.  This will remove most of the need for types-compat?

;; [2007.04.05] Previously I had an overly restrictive ban on
;; instantiation of Ref types.  Now I've implemented the SML style
;; "value restriction".

(library  (ws compiler_components hm_type_inference)
  (export
           instantiate-type
	   export-type 
	   prim->type

	   do-late-unify! 
	   recover-type
	   type-const
;	   type-lambda
	   type-app
;	   type-letrec
           
	   annotate-expression export-expression
	   annotate-program
	   strip-binding-types do-all-late-unifies!
	   strip-annotations
	
	   print-var-types print-type show-type
	   dealias-type
	   realias-type

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

	   grab-init-tenv
	   sumdecls->tenv sum-instance!
	   src-pos->string

	   get-snippet get-location
	   peel-outer-typevars
	   value-expression?
	   )
  (import  (except (rnrs (6)) error)
	   (rnrs mutable-pairs (6))
	   (ws compat compat)
	   (ws globals)
	   (ws util helpers)
	   (ws util reg_macros)
	   (ws util hashtab)
	   (ws compiler_components prim_defs)
	   (ws compiler_components reg_core_generic_traverse)
           (ws compiler_components type_environments)
	   (ws compiler_components wavescript_helpers)
#;
	   (except (ws compiler_components wavescript_helpers)
		   wavescript-type-aliases
		   wavescript-basic-primitives
		   local-node-primitives
		   wavescript-constants
		   wavescript-distributed-primitives)
	   ;; Wow, and for this pass too... after the call/cc fix rn-match is slower under PLT:
	   (ws util iu-match)
	   ;;(ws util rn-match) ;; TEMPTOGGLE
           )



;(cond-expand [plt (provide (all-from "type_environments.ss"))] [else])


; ======================================================================
;;; Helpers

(define (safe-export-type t)
  (if (type? t) (export-type t) `(NOT-A-TYPE ,t)))

(define (type-error sym str . args)
  (warning sym (apply format str args))
  ;(exit 1)
  (error sym "")
  )

;; TODO: Move to another file:
(define src-pos->string
  (lambda (pos)
    (match pos
      [#f "unknown"]
      [#((,fn) ,off1 ,ln1 ,col1 ,off2 ,ln2 ,col2)
       (** (format "in file ~s\n   "  fn)
	   (if (= ln1 ln2)
	       (format "on line ~s, columns ~s through ~s " ln1 col1 col2)
	       (format "between line/col ~s:~s and ~s:~s " ln1 col1 ln2 col2)))])))

(define (get-location x)
  (match x
    [(src-pos ,pos ;#((,fn) ,off1 ,ln1 ,col1 ,off2 ,ln2 ,col2) 
	      ,_)
     (src-pos->string pos)]
    [,x 
     ;; SUPER HACKISH:
     (let ([pos (deep-assq 'src-pos x)])
       (match pos
	 [#f "Unknown source location."]
	 [(src-pos #((,fn) ,off1 ,ln1 ,col1 ,off2 ,ln2 ,col2) ,_)
	  (format "within file ~s\n   in the vicinity of line ~s column ~s"
		  fn ln1 col1)
	  ]))]))


;; This is similar to get-location but gets the actual snippet.
(define (get-snippet x)
  (match x
    [(src-pos #((,fn) ,off1 ,ln1 ,col1 ,off2 ,ln2 ,col2) ,exp)
     (define (get-prefix str num) (substring str 0 (min (sub1 (string-length str)) num)))
     (if (file-exists? fn)
	 (let ([port (open-input-file fn)])
	   (let ([lines '()])
	     (rep (sub1 ln1) (read-line port)) ;; Ignore leading text
	     (rep (add1 (- ln2 ln1)) (set! lines (cons (read-line port) lines)))
	     ;; Cap each line with a newline:
	     (set! lines (map (lambda (ln) (** ln "\n")) lines))
	     (set! lines (reverse lines))

	     ;; Prune the first line:
	     (set! lines (cons (** (make-string col1 #\space) 
				   (if (> col1 (string-length (car lines)))
				       ;; This shouldn't happen, but it did.
				       (car lines) ;; Give up on pruning.
				       (substring (car lines) col1 
						  (string-length (car lines)))))
			       (cdr lines)))

	     ;; Line cap: Arbitrary limit on length of expression to print:
	     (let ([linecap 10])	       
	       (if (> (length lines) linecap)
		   (begin 
		     (set! lines (list-head lines linecap))
		     (set! lines (append lines '("... (expression to long to print)\n"))))
		   ;; Prune the last line:
		   (set! lines
			 (append (rdc lines)
				 (list (get-prefix (rac lines) col2))))))
	     (close-input-port port)	     	     	    	  
	     (apply string-append lines)))
	 (get-snippet exp))]
    [,x 
     (format "(in abstract syntax)\n   ~a\n" 
	     ;; This is HACKISH, and error-prone, but it makes messages nicer:
	     (with-output-to-string
	       (lambda ()
		 (pretty-print (if (or (equal? x (void)) (string? x)) x (strip-annotations x)))))
             )]))

;; Raises a generic type error at a particular expression.
(define (raise-type-mismatch msg t1 t2 exp)
  (type-error 'type-checker
	 (** "\n";"\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
	     "Type mismatch: \"~a\" doesn't match \"~a\" \n~a"
	     "\nLocation:\n   ~a\n" ;; Location
	     "\nExpression:\n~a\n")
	 (show-type (safe-export-type t1))
	 (show-type (safe-export-type t2))
	 msg
	 ;; Approximate location:
	 (get-location exp)
	 ;(format "(in abstract syntax)\n   ~s\n" exp)
	 (get-snippet exp)
	 ))
;; Raises an error indicating that we have a loop in our tvar pointers.
(define (raise-occurrence-check tvnum t2 exp)
  (type-error 'type-checker
	 "Can't unify: ~s occurs in type ~s\n\nSource Location:\n  ~a\n" 
	 tvnum t2 ;(export-type t2) 
	 (get-location exp)))
;; Raises a wrong-number-of-arguments error.
(define (raise-wrong-number-of-arguments t1 t2 exp)
  (type-error 'type-checker
	 "Different numbers of arguments:\n      ~s: ~a\n  and ~s: ~a\n\nSource Location\n  ~a\nExpression:\n~a\n"
	 (- (length t1) 2) (show-type (safe-export-type t1))
	 (- (length t2) 2) (show-type (safe-export-type t2))
	 (get-location exp)
	 (get-snippet exp)
	 ))

; ----------------------------------------

;; This associates new mutable cells with all tvars.
;; It is used either to freshen an instantiated type, or instantiate an uninstantiated one.
;; It also renames all the tvars to assure uniqueness.
;; The "nongeneric" vars are ones that do not receive new mutable cells.
;; 
;; This is because, when instantiating a rator for application
;; let-bound type variables are reinstantiated, whereas lambda-bound ones are not.
;; 
;; Allowing a third argument -- a boolean that specifies whether fresh
;; names should be used, or whether to keep the old names.
(define instantiate-type 
  (case-lambda 
    [(t) (instantiate-type t '())]
    [(t nongeneric) (instantiate-type t nongeneric #t)]
    [(t nongeneric fresh-names)
     (let* ((env '()) ;; Mutated below
	 (result 
	  (let loop ((t t))
	   (match t ;; No recursion 
	     [#f #f]
             ;; This type variable is non-generic, we do not copy it.
	     [(,qt ,cell) 
	      (guard (tvar-quotation? qt); (eq-any? qt 'quote 'NUM)
		     (memq (if (pair? cell) (car cell) cell) nongeneric))
	      (ASSERT pair? cell)
              `(,qt ,cell)] ;; Don't reallocate the cell (or touch its RHS)
	     ;; Otherwise make/lookup the new cell and attach it.
	     [(,qt ,x) 
	      (guard (tvar-quotation? qt);(eq-any? qt 'quote 'NUM)
		     (or (symbol? x) (pair? x)))
	      (let* ([var (if (symbol? x) x (car x))]
		     [entry (assq var env)])
		(if entry
		    (cdr entry)
		    (let ((newtype `(,qt ,(cons (if fresh-names (make-tvar) var)
						(if (pair? x) (loop (cdr x)) #f)))))
		      ;; After that loop var should still not occur in the env!
		      (DEBUGASSERT (not (assq var env)))
		      (set! env (cons (cons var newtype) env))
		      newtype)))]
	     [,s (guard (symbol? s)) s]
	     
	     ;; [2008.04.09] Attempted minor optimazition:
	     [,vec (guard (vector? vec)) (vector-map loop vec)]
	     ;[#(,t* ...) (apply vector (mapright loop t*))]
	     [(,arg* ... -> ,res) ; Ok to loop on ellipses.
	      (let* ([arg* (mapright loop arg*)]
		     [res (loop res)])
		`(,@arg* -> ,res))]

	     ;; [2008.11.09] NOTE: instantiated Record types must have
	     ;; a tcell separating them from their row.
	     [(Record ,row) 
	      `(Record ,(match row
			  ;; It's already got a tvar:
			  [',_ (loop row)]
			  [,oth (define tc (make-tcell))
				(tcell-set! tc (loop oth))
				tc]))]

	     [(,constructor ,args ...)
	      (guard (symbol? constructor))
	      `(,constructor ,@(mapright loop args))]

	     [,s (guard (string? s)) s] ;; Allowing strings for uninterpreted C types.
	     [,other (error 'instantiate-type "bad type: ~a" other)]
	     ))))
       (DEBUGASSERT type? result)
       result)]))

;; This takes away the mutable cells, thereby converting to the
;; external representation.
;; It also takes away LUB types.
(define (export-type t)
  (match t
    [(,qt ,v) (guard (tvar-quotation? qt) (symbol? v)) (list qt v)]
    [(,qt (,v . ,t)) (guard (tvar-quotation? qt))
     (if t (export-type t) (list qt v))]
    ;['(,n . ,v) (if v (export-type v) `(quote ,n))]
    ;[',n        `(quote ,n)]
    ;[(NUM ,v) (guard (symbol? v)) `(NUM ,v)]
    ;[(NUM (,v . ,t)) (if t (export-type t) `(NUM ,v))]

    [,s (guard (symbol? s)) s]    
    [(LATEUNIFY #f ,[b]) `(LATEUNIFY #f ,b)]
    [(LATEUNIFY ,[a] ,[b])
     `(LATEUNIFY ,a ,b)]
    [#(,[t*] ...) (apply vector t*)]
    [(,[arg*] ... -> ,[res])
     `(,@arg* -> ,res)]
    ;; Including Ref, Record, Row
    [(,s ,[t*] ...) (guard (symbol? s)) `(,s ,@t*)]
    [,s (guard (string? s)) s] ;; Allowing strings for uninterpreted C types.
    [,other (error 'export-type "bad type: ~s" other)]))


;; [2007.02.21]
;; HACK: including this unifier and unifying each of these again:
;; Shouldn't have to do this, but there's a problem with the design.
(define (do-lub!!! t UNIFIER)
  (let ([rands 
	 (match t
	   [(LUB ,[a] ,[b]) (append a b)]
	   [,oth            (list oth)])])
    (for-each (lambda (t) (types-equal! t (instantiate-type UNIFIER '())
					"unknown code location" ""))
      rands)
    (let ([exported (map export-type rands)])
      ;(printf "  FINAL LUBS: ~s\n" exported)
      (foldl1 LUB exported))
    ))

;; This traverses the type and does any LATEUNIFY's
;; It destructively removes them from the type.
(define (do-late-unify! t)
  (match t
    [(quote ,pr)
     (match pr
       [(,v . (LATEUNIFY #f ,b))
	;; No call sites to unify.
	(set-cdr! pr b)
	(do-late-unify! b)]
       [(,v . (LATEUNIFY ,a ,b))
        ;(printf "LATEUNIFY ~s ~s\n" a b)
	;; FIXME FIXME: If this is not enabled... lets never accumulate the LUB
	(if (inferencer-enable-LUB)
	    ;; Comupute the LUB and then unify that with the most general
	    ;; type.  That's our answer.
	    (let* ([lub (do-lub!!! a b)]
		   [tc (make-tcell b)])
	      (types-equal! (instantiate-type lub '()) 
			    tc ;(inspect/continue tc)
			    "unknown location" "")
	      (set-cdr! pr tc))
	    ;; Otherwise just the most general type.
	    (set-cdr! pr b))]
       [(,v . ,oth) (if oth (do-late-unify! oth) (void))])]
    [,s (guard (symbol? s))                  (void)]
    [#(,[t*] ...)                            (void)]
    [(LATEUNIFY ,a ,b)
     (error 'do-late-unify! "found LATEUNIFY not in mutable cell: ~s" `(LATEUNIFY ,a ,b))]
    ;[',n `(quote ,n)]
    ;; Catch NUM and ROW:
    [(,qt ,v) (guard (symbol? v) (tvar-quotation? qt))            (void)]
    [(,qt (,v . ,t)) (guard (tvar-quotation? qt)) (if t (do-late-unify! t) (void))]
  
    ;; FIXME TODO: Handle row variables
    [(,[arg*] ... -> ,[res])                 (void)]
    ;; Including Ref, Record, Row
    [(,s ,[t] ...) (guard (symbol? s))       (void)]
    [,s (guard (string? s)) s] ;; Allowing strings for uninterpreted C types.
    [,other (error 'do-late-unify! "bad type: ~s" other)]))

;; Looks up a primitive and produces an instantiated version of its arrow-type.
(define (prim->type p)
  (let ((entry (or ;(assq p wavescript-basic-primitives)
		   ;(assq p wavescript-constants)
		   ;(assq p wavescript-distributed-primitives)
		   ;(assq p (wavescript-primitives))
		   (wavescript-primitive? p)
		   ;(get-primitive-entry p)
		   )))
    (unless entry
      (error 'prim->type "primitive was not found: ~a" p))
    (match entry
      [(,type) (instantiate-type type '())]
      [(,args ,ret)
       (instantiate-type `(,@args -> ,ret) '())])))


(define (valid-type-symbol? s)
  (let ([str (symbol->string s)])
    (and (> (string-length str) 0)
	 (char-upper-case? (string-ref str 0)))))
(define (valid-typevar-symbol? s)
  (let ([str (symbol->string s)])
    (and (> (string-length str) 0)
	 (char-lower-case? (string-ref str 0)))))
  

  
; ======================================================================

;;; The main type checker.

(define (type-expression expr tenv)
  (let-values ([(e typ) (annotate-expression expr tenv '())])        
    (do-all-late-unifies! e)    
    typ))

;; Used for recovering types for particular expressions within an already type-annotated program.
;; NOTE: *Assumes* that the program is *correctly* typed.
;; Returns an "exported" type... i.e. with mutable cells stripped.
;; .param exp - Expression
;; .param tenv - Type Environment
(define (recover-type exp tenv)
  ;(IFCHEZ (import rn-match) (void)) ;; Doesn't work yet with nested ellipses...
  ;; [2007.05.14] WEIRD... now if I try this, I seem to get system test 114 nondeterministically failing.
  ;; Actually, this seems to expose an existing error.  I can get it to come out through repeated testing.
  ;; Hmm.. we should run the unit tests multiple times in random order..

  (DEBUGASSERT (tenv? tenv))
  (let l ((exp exp))
    (match exp 
      [(lambda ,formals ,types ,body)
       `(,@types -> ,(recover-type body (tenv-extend tenv formals types)))]
      ;; Being lenient and accepting potential annotations in the
      ;; letrec binds.  This is necessary for running "recover type"
      ;; on the intermediate forms of the later compiler passes.
      [(,letrec ([,lhs* ,type* . ,_] ...) ,body)
       (guard (memq letrec '(letrec lazy-letrec)))
       (recover-type body (tenv-extend tenv lhs* type*))]

      ;; Should this really bother recovering both types?
      ;; It could just assume that the program is correctly
      ;; typechecked at this point and recover *one* type.
      ;; But, alas, that might be too general a type.
      [(if ,t ,[c] ,[a]) 
       (let ([a (instantiate-type a '())] 
	     [c (instantiate-type c '())])
         (types-equal! c a `(if ,t ??? ???) "(Branches of if must have same type.)\n")
         (export-type c))]
      [(wscase ,x  [,pat* ,[rhs*]] ...)
       (let ([inst* (map (lambda (x) 
                           (match x 
                             [(,_ ... -> ,ret) (instantiate-type ret '())]))
                      rhs*)])
         (foldl1 (lambda (a b) (types-equal! a b '(case ...) "(Branches of case must have same type.)\n"))
                 inst*)
         (export-type (car inst*)))]
      
      [(tuple ,[t*] ...) (list->vector t*)]
      [(tupref ,n ,len ,[t]) (vector-ref t (qinteger->integer n))]
      [(wsrecord-select ',name ,[rec])
       (match rec
	 [(Record ,row) ; '(,v . ,row)
	  (let loop ([row row])
	    (match row
	      [(Row ,nm ,ty ,tail) (if (eq? nm name) ty (loop tail))]
	      [,else (error 'recover-type "wsrecord-select Couldn't look up label ~a in record type ~a" name rec)]))])]
      [(wsrecord-extend ',name ,[xty] ,[rec])
       (match rec [(Record ,row) `(Record (Row ,name ,xty ,row))])]
      [(wsrecord-restrict ',name ,[rec])
       (match rec
	 [(Record ,row) 
	  `(Record 
	    ,(let loop ([row row])
	       (match row
		 [(Row ,nm ,ty ,tail) (if (eq? nm name) tail `(Row ,nm ,ty ,(loop tail)))]
		 [,else (error 'recover-type "wsrecord-restrict Couldn't look up label ~a in record type ~a" name rec)])))])]

      [(unionN ,annot ,[t*] ...) 
       (ASSERT (not (null? t*)))
       (ASSERT all-equal?  t*) ;; Requires that they're literally equal sexps...
       (match (types-compat? '(Stream 'a) (car t*))
         [(Stream ,t) `(Stream #(Int ,t))]
         )
       ]
      
      [(__cast_num ',from ',to ,_) to]
      
      ;; Since the program is already typed, we just use the arrow type of the rator:
      ;[(,[rat] ,rand ...) (rac rat)]
      [,other 
       (export-type (type-expression other (tenv-map instantiate-type tenv)))   ])))

;; Assign a basic type to a constant.
;; TODO: if this is to handle sum types, it needs to take the type defs also.
;; [2007.10.02] TODO: AUDIT THIS.  For integer constants perhaps it should infer a generic type...
(define (type-const c)
  (cond
   [(double? c) 'Double]
   [(flonum? c) 'Float]
   [(cflonum? c) 'Complex]
   ;[(integer? c) 'Int]
   [(integer? c) (make-numcell)]
   
   [(char? c)   'Char]
   [(string? c) 'String] 
   [(boolean? c) 'Bool]
   
   [(vector? c) `(Array ,(if (zero? (vector-length c))
			     (make-tcell) ;''anytype
			     (type-const (vector-ref c 0))))]

   [(tuple? c) (list->vector (map type-const (tuple-fields c)))]
   
   [(wsrecord? c) 
    `(Record 
      ,(let loop ([ls (wsrecord-pairs c)])
	 (if (null? ls)
	     (make-tcell)
	     `(Row ,(caar ls) ,(type-const (cdar ls)) ,(loop (cdr ls))))))]

   ;; This is strange, but we need an actual representation of "no value" or "any value" at some points.
   [(eq? c 'BOTTOM) (make-tcell)]
   [(eq? c 'UNIT)   '#()]
   
   ;; Temp, not sure if we're going to support symbols or not in the final language:
   [(symbol? c) 'Symbol]
   [(null? c) `(List ,(make-tcell))]
   [(list? c)
    (let ([types (map type-const c)])
      (let ([t1 (car types)])
	(for-each (lambda (t) (types-equal! t t1 `',c "(Elements of list must have same type.)\n"))
	  (cdr types))
	`(List ,t1)))]
   
   ;; Should we allow constant sigsegs?
   ;[(eq? c nullseg-object) ]
   [(sigseg? c) 
    (if (zero? (vector-length (sigseg-vec c)))
	`(Sigseg ,(make-tcell))
	`(Sigseg (type-const (vector-ref (sigseg-vec c)))))]
   [(timebase? c) 'Timebase]
   
   [else (error 'type-const "could not type: ~a" c)]))

;; Assign a type to a procedure application expression.
(define (type-app rattyp rands exp tenv non-generic-tvars)
  (DEBUGASSERT (tenv? tenv))
  (let ([result (make-tcell)])
    (types-equal! rattyp `(,@rands -> ,result) exp "(Argument to function of wrong type.)\n")
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
;;
;; .param exp - expression
;; .param tenv - type environment (NOTE: binds vars to INSTANTIATED types)
;; .param nongeneric - list of type variables that appear in lambda-arguments. (as opposed to lets)
;; .returns 2 values - annotated expression and expression's type (both w/ INSTANTIATED types)
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
  ;(printf "annotate-expression, Looping: ~a\n" exp)(flush-output-port (current-output-port))
  ;; Here's the main loop:
  (letrec ([l (lambda (exp)

    (match exp ;; NO DIRECT RECURSION ALLOWED!!! GO THROUGH "l" LOOP ABOVE!!!

      [(quote ,c)               (values `(quote ,c) (type-const c))]
      ;; Make sure it's not bound:
#;
      [,prim (guard (symbol? prim) (wavescript-primitive? prim) (not (tenv-lookup tenv prim)))
             (values prim (prim->type prim))]
      
      ;; Here's the magic:
      [,v (guard (symbol? v))
	  (if (and (wavescript-primitive? v) (not (tenv-lookup tenv v)))
	      (values v (prim->type v))
          (let ((entry (tenv-lookup tenv v)))
            (if entry 
                (cond
                 ;; Let-bound polymorphism: 
                 [(tenv-is-let-bound? tenv v)
                  (let ()
                                        ;(printf "LETBOUND: ~s\n" v)
                                        ;(unless (null? nongeneric) (printf "  NONGENERIC: ~s\n"  nongeneric))
                                        ;(inspect entry)
                    
                    (match entry
                      ;; Here's another call site which affects the LUB type assigned to the let-bound var.
                      [(quote (,tv . (LATEUNIFY ,lubs ,general)))
                       (DEBUGASSERT (inferencer-enable-LUB))
                       (let ([this-site (instantiate-type general nongeneric)])
			  ;(printf "Let-bound var! ~s with general type:  ~a\nlub at this site:\n  ~a\n" tv general this-site)
                         ;; CAREFUL!  Here we mutate the lubs on that LATEUNIFY.
                         (DEBUGASSERT (curry eq? 'LATEUNIFY) (cadadr entry))
                         (set-car! (cddadr entry)  
                                   (if lubs
                                       `(LUB ,lubs ,this-site)
                                       this-site))
                         ;; We return the type for *this* varref.
                         (values v this-site))]
                      
                      [(quote (,tv . ,general))
                       (DEBUGASSERT (not (inferencer-enable-LUB)))
                       (values v (instantiate-type general nongeneric))
                       ])
                    )]
                 [else                   
                  (values v entry)])
                (error 'annotate-expression "no binding in type environment for var: ~a" v))))]

      [(if ,[l -> te tt] ,[l -> ce ct] ,[l -> ae at])
       (types-equal! tt 'Bool te "(Conditional's test expression must have boolean type.)\n") ;; This returns the error message with the annotated expression, oh well.
       (types-equal! ct at exp "(Branches of conditional must have same type.)\n")
       (values `(if ,te ,ce ,ae) ct)]

      ;; Case statements are somewhat complex to typecheck.
      [(wscase ,[l -> val valty] (,TC* ,[l -> rhs* rhsty*]) ...)
;       (DEBUGASSERT (curry andmap type?) rhsty*)
       (values `(wscase ,val ,@(map list TC* rhs*))
	       ;; First we collect constraints of each branch upon the val type, in the process also gathering the RHS types.
	       (let ([inst* (map (lambda (tc rhsty) 				   				   
				   (if (eq? tc default-case-symbol)
				       rhsty ;; Not an app...
				       (type-app rhsty (sum-instance! tenv valty tc) exp tenv nongeneric)
				       ))
			      TC* rhsty*)])
;		 (DEBUGASSERT (curry andmap type?) inst*)
		 ;; Make sure all those return types are consistent:
		 (foldl1 (lambda (a b) 
			   (DEBUGASSERT type? a)
			   (DEBUGASSERT type? b)
			   (types-equal! a b exp "(Branches of case must have same type.)\n")
			   a)
			 inst*)
		 (car inst*)))]
      
      ;; Wavescope: this could be a set! to a state{} bound variable:
      [(set! ,v ,[l -> e et])  
       (let ([newexp `(set! ,v ,e)])
         ;; The mutable var must be a Ref!
         (types-equal! (ASSERT (tenv-lookup tenv v))
                       `(Ref ,et)
                       newexp "(Cannot assign to incompatible type.)\n")
         ;; returns unit type:
         (values newexp '#()))]

#;
      [(for (,i ,[l -> start st]) ,[l -> end et] ,[bod bt])
       (let ([expr `(for [,i ,start ,end] ,bod)])
         (unless (types-compat? 'Int (types-compat? st et)) 
           (raise-type-mismatch "(Start/End of for-loop must be integers.)\n" start end expr))
         (values expr '#()))]
      
      [(while ,[l -> tst tt] ,[l -> bod bt])
       (let ([expr `(while ,tst ,bod)])
         (unless (types-compat? 'Bool tt) 
           (raise-type-mismatch "(While loop expects boolean test.)\n" 'Bool tt expr))
         (values expr '#()))]

      
      [(unionN ,annot ,[l -> e* t*] ...)
       (ASSERT (not (null? t*)))
       (let ([exp `(unionN ,annot ,@e*)]
             [first (car t*)])
         ;; Make sure they're all equal:
         (for-each (lambda (t2) 
                     (types-equal! first t2 exp "(All streams into unionList or unionN must be same type.)\n"))
           (cdr t*))
         ;; Make sure it's resolved as a stream type:
         (types-equal! first (instantiate-type '(Stream 'anything) '()) exp "unionN must take streams")
         ;; Now we add in that integer tag to the type.
         (values exp
                 (match (deep-assq 'Stream first)
                   [(Stream ,elt) `(Stream #(Int ,elt))])))]


      [(tuple ,[l -> e* t*] ...)  (values `(tuple ,@e*) (list->vector t*))]
      ;; FIXME: It *might* be profitable to use lazy evaluation on formatting these error messages.
      ;; Processing the strings is probably somewhat expensive.
      [(tupref ,n ,len ,[l -> e t])
       (unless (and (qinteger? n) (qinteger? len))
         (error 'annotate-expression 
                "invalid tupref syntax, expected constant integer index/len, got: ~a/~a" n len))
       (values `(tupref ,n ,len ,e)
               (let ((newtypes (list->vector (map (lambda (_) (make-tcell)) (iota (qinteger->integer len))))))
                 (types-equal! t newtypes exp (format "(Attempt to accesss field ~a of a tuple with the wrong type.)\n" n))
                 (vector-ref newtypes (qinteger->integer n))))]
      
      ;; This is a primitive:
      [(wsrecord-select ',name ,[l -> e recty])
       (define tc (make-tcell))
       (define elt (make-tcell))
       (tcell-set! tc `(Row ,name ,elt ,(make-tcell)))
       (types-equal! recty `(Record ,tc) exp 
		     (format "(Attempt to access record field '~a' within incompatible type ~a)\n" name recty))
       (values `(wsrecord-select ',name ,e) elt)]

      [(wsrecord-extend ',name ,[l -> el elt] ,[l -> rec ty])   
       (define tc1 (make-tcell))
       (define tc2 (make-tcell))
       (tcell-set! tc2 `(Row ,name ,elt ,tc1))
       (types-equal! ty `(Record ,tc1) exp
		     (format "(Attempt to extend non-record type '~a' with field label ~a)\n" ty name))
       (values `(wsrecord-extend ',name ,el ,rec)
	       `(Record ,tc2))]

      ;; Remember, there may be duplicate fields, this must be careful to remove the LAST ADDED:
      ;; Of course, this enforces that its record argument have the field label being removed.
      [(wsrecord-restrict ',name ,[l -> e recty])       
       ;; TEMP: SORT THIS OUT:  Make consistent with helpers.ss
       (define (assq-remove-first key ls)
	 (let loop ((ls ls))
	   (cond
	    [(null? ls) #f]
	    [(eq? (caar ls) key) (cdr ls)]
	    [else (cons (car ls) (loop (cdr ls)))])))

       (define tc2 (make-tcell))
       (tcell-set! tc2 `(Row ,name ,(make-tcell) ,(make-tcell)))
       (types-equal! recty `(Record ,tc2) exp
		     (format "(Attempt to remove nonexistent field label '~a' from type ~a)\n" name recty))

       ;; This part is tricky.  We do not want to replace any of the
       ;; mutable tcell's for the fields contained within the returned
       ;; record type.  However, we do want to rebuild the linked list
       ;; spine (of Row's) so that it does not contain the label.
       ;; Essentially, the row-variables are being replaced, but not
       ;; normal type variables.  However, we currently use the same
       ;; physical representation for the two.
       (values `(wsrecord-restrict ',name ,e)
	       `(Record 
		 ,(let loop ([recty tc2])
		    (match recty
		      ['(,var . ,[ty]) 
		       (define newtc (make-tcell))
		       (tcell-set! newtc ty)
		       newtc]
		      [(Row ,nm ,ty ,super)
		       (if (eq? name nm)
			   super
			   `(Row ,nm ,ty ,(loop super)))]))))]
#;
      [(,missed . ,_) (guard (memq missed '(wsrecord-select wsrecord-restrict wsrecord-extend)))
       (error "MISSED RECORD OP: ~a\n" (cons missed _))]
      
      [(begin ,[l -> exp* ty*] ...)
       (values `(begin ,@exp*) (last ty*))]

      [(for (,i ,[l -> start ty1] ,[l -> end ty2]) ,bod)
       ;; For now assume i is an integer...
       (types-equal! ty1 'Int exp "(Starting point for range of for-loop must be an integer.)\n")
       (types-equal! ty2 'Int exp "(Ending point for range of for-loop must be an integer.)\n")
       (let ([tenv (tenv-extend tenv (list i) '(Int) #f)])
         (let-values ([(bod ty) (annotate-expression bod tenv nongeneric)])
           (values `(for (,i ,start ,end) ,bod) 
		   ;; ty ;; -- This was an ERROR:
		   '#()
		   )))]
      
      ;; This is not just a normal primitive:
      [(__cast_num ',from ',to ,[l -> x xty]) 
       (values `(__cast_num ',from ',to ,x) to)]
      
      [(lambda (,v* ...) ,bod) (annotate-lambda v* bod 
                                                (map (lambda (_) `(quote ,(make-tvar))) v*)
                                                tenv nongeneric)]
      [(lambda (,v* ...) ,types ,bod) (annotate-lambda v* bod types tenv nongeneric)]

      [(let ([,id* . ,tail*] ...) ,bod)
       (let ([ty*  (extract-optional (map rdc tail*))]
             [rhs* (map last tail*)])
         (DEBUGASSERT (curry andmap type?) (filter id ty*))
         (annotate-let id* rhs* bod ty* tenv nongeneric))]
      
      [(,letrec ([,id* . ,tail*] ...) ,bod)  (guard (memq letrec '(letrec lazy-letrec)))
       (let ([ty*  (extract-optional (map rdc tail*))]
             [rhs* (map last tail*)])
         (DEBUGASSERT (curry andmap type?) (filter id ty*))
         (annotate-letrec id* ty* rhs* bod tenv nongeneric letrec))]

      ; FIXME: a bit of a hack since these need to be typechecked both as a function application, and independently,
      ;        without and with annotations, respectively;
      ;        also, these can probably just be generalized with a guard for the annotations
      [(_merge ,annot ,[l -> s1 t1] ,[l -> s2 t2])
       (values `(_merge ,annot ,s1 ,s2) (type-app (prim->type '_merge) `((List Annotation) ,t1 ,t2) exp tenv nongeneric))]
      [(readFile ,annot ,[l -> args* ty*] ...)
       (values `(readFile ,annot ,@args*) (type-app (prim->type 'readFile) `((List Annotation) ,@ty*) exp tenv nongeneric))]
      [(__readFile ,annot ,[l -> args* ty*] ...)
       (values `(__readFile ,annot ,@args*) (type-app (prim->type '__readFile) `((List Annotation) ,@ty*) exp tenv nongeneric))]

      [(timer ,annot ,[l -> args* ty*] ...)
       (values `(timer ,annot ,@args*) (type-app (prim->type 'timer) `((List Annotation) ,@ty*) exp tenv nongeneric))]
      


      ;; BEGIN DUPLICATING! these cases to give good error messages for badly typed apps:
      [(src-pos ,p (_merge ,annot ,[l -> s1 s1t] ,[l -> s2 s2t]))
       (DEBUGASSERT (andmap type? `((List Annotation) ,s1t ,s2t)))
       (values `(_merge ,annot ,s1 ,s2)
               (type-app (prim->type '_merge) `((List Annotation) ,s1t ,s2t) exp tenv nongeneric))]

      [(src-pos ,p (readFile ,annot ,[l -> rand* t*] ...))
       (DEBUGASSERT (andmap type? `((List Annotation) ,@t*)))
       (values `(readFile ,annot ,@rand*)
               (type-app (prim->type 'readFile) `((List Annotation) ,@t*) exp tenv nongeneric))]

      [(src-pos ,p (timer ,annot ,[l -> rand* t*] ...))
       (DEBUGASSERT (andmap type? `((List Annotation) ,@t*)))
       (values `(timer ,annot ,@rand*)
               (type-app (prim->type 'timer) `((List Annotation) ,@t*) exp tenv nongeneric))]

      [(src-pos ,p (iterate ,annot ,[l -> f ft] ,[l -> s st]))
       (DEBUGASSERT (andmap type? `((List Annotation) ,ft ,st)))
       (values `(iterate ,annot ,f ,s)
               (type-app (prim->type 'iterate) `((List Annotation) ,ft ,st) exp tenv nongeneric))]

      [(src-pos ,p (,prim ,[l -> rand* t*] ...))
       (guard (wavescript-primitive? prim)
              (not (eq-any? prim 'tuple 'unionN 'wsrecord-extend 'wsrecord-restrict 'wsrecord-select)))
       (DEBUGASSERT (andmap type? t*))
       (values `(,prim ,@rand*)
               (type-app (prim->type prim) t* exp tenv nongeneric))]
      [(src-pos ,p (,app ,origrat ,[l -> rand* t*] ...))
       (guard (eq-any? app 'app 'construct-data))
       (DEBUGASSERT (andmap type? t*))
       (let-values ([(rator t1) (l origrat)])
         (values `(src-pos ,p (,app ,rator ,@rand*))
                 (type-app  t1 t* exp tenv nongeneric)))]
      ;; END DUPLICATING!!!

      [(iterate ,annot ,[l -> f ft] ,[l -> s st])
       (DEBUGASSERT (andmap type? `((List Annotation) ,ft ,st)))
       (values `(iterate ,annot ,f ,s)
               (type-app (prim->type 'iterate) `((List Annotation) ,ft ,st) exp tenv nongeneric))]

      ;; These cases are still around in case there's no source-info.
      [(,prim ,[l -> rand* t*] ...)
       (guard (wavescript-primitive? prim))
       (DEBUGASSERT (andmap type? t*))
       (values `(,prim ,@rand*)
               (type-app (prim->type prim) t* exp tenv nongeneric))]

      [(,app ,origrat ,[l -> rand* t*] ...)
       (guard (eq-any? app 'app 'construct-data))
       (DEBUGASSERT (andmap type? t*))
       (let-values ([(rator t1) (l origrat)])
         (values `(,app ,rator ,@rand*)
		 (type-app t1 t* exp tenv nongeneric)))]
     
      ;; Incorporate type assertions.
      ;; ----------------------------------------

      ;; Push ascriptions inside lambdas:
      ;; This helps make the grammar a little nicer subsequently.
      ;; UNFINISHED      
      ;[(assert-type ,ty (lambda () () ,bod)) ......]

      ;; This is a special case for constants.
      [(assert-type ,ty (quote ,n))
       (if (constant-typeable-as? n ty)
	   (values `(assert-type ,ty (quote ,n))  
		   (instantiate-type ty '()))
	   (error 'hm_type_inference "constant ~s was labeled with type ~s which doesn't match"
		  `(quote ,n) ty))]
      [(assert-type ,ty ,[l -> e et])
       (let ([newexp `(assert-type ,ty ,e)])	 
	 (types-equal! (instantiate-type ty '()) et newexp
		       "(Type incompatible with explicit type annotation.)\n")
	 (values newexp et))]

      ;; ----------------------------------------

      ;; Foreign applications.  Same problem with code duplication:
#;
      [(src-pos ,p (foreign-app ',realname ,origrat ,[l -> rand* t*] ...))
       (DEBUGASSERT (andmap type? t*))
       (let-values ([(rator t1) (l origrat)])
         (values `(src-pos ,p (foreign-app ',realname ,rator ,@rand*))
                 (type-app t1 t* exp tenv nongeneric)))]
      [(foreign-app ',realname ,origrat ,[l -> rand* t*] ...)
       (DEBUGASSERT (andmap type? t*))
       (let-values ([(rator t1) (l origrat)])
         (values `(foreign-app ',realname ,rator ,@rand*)
                 (type-app t1 t* exp tenv nongeneric)))]
      
      [(src-pos ,p ,[l -> e et]) (values `(src-pos ,p ,e) et)]
      
      ;; [2008.01.21] Adding limited support for post-nominalize types language:
      [(struct-ref ,type ,fld ,[l -> e t])  
       (values `(struct-ref ,type ,fld ,e) type)]
      [(make-struct ,type ,[l -> e* t*] ...)
       (values `(make-struct ,type ,@e*) `(Struct ,type))]

      ;; [2008.01.22] Sigh, adding to support output of insert-refcounts:
      ;; We probably shouldn't even need to typecheck the subexpression--it should be simple.
      [(,refcnt ,ty ,e)
       (guard (refcount-form? refcnt))
       (ASSERT simple-expr? e)
       (values `(,refcnt ,ty ,e) '#())]
      #;
      [(,refcnt ,ty ,[l -> e et]) 
       (guard (refcount-form? refcnt))
       (values `(,refcnt ,ty ,e) '#())]

      ;; [2008.04.08] This is an annotation introduced much later in the compiler.
      [(static-allocate ,[l -> x xt]) (values `(static-allocate ,x) xt)]

      ;; Allowing unlabeled applications for now:
      [(,rat ,rand* ...) (guard (not (wavescript-keyword? rat)))
       (warning 'annotate-expression "allowing arbitrary rator: ~a\n" rat)
       (l `(app ,rat ,@rand*))]
      
      [,c (guard (simple-constant? c)) (values c (type-const c))]            

      [,other (error 'annotate-expression "could not type, unrecognized expression: ~s" other)]
      ))]) ;; End main-loop "l"    

    ;; Initiate main loop:
    #;
(let ([result (l exp)])
      ;; On the way out we do late unifications:
      (do-all-late-unifies! result)
      result
      )
  (let-values ([(e t) (l exp)])
    (DEBUGASSERT instantiated-type? t)
    (values e t))
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
    (DEBUGASSERT (= (length ids) (length inittypes)))
    (ASSERT (andmap symbol? ids)) ;; For now, no patterns.
    (for-each valid-user-type! inittypes)
    ;; No optional type annotations currently.
    (let* ([argtypes  (map (lambda (_) (make-tcell)) ids)]
           [newnongen (map (lambda (x) (match x ['(,n . ,_) n])) argtypes)])
      ;; Now unify the new type vars with the existing annotations.
      (map (lambda (new old)
	     (if old (types-equal! new (instantiate-type old '())
			   `(lambda ,ids ,inittypes ,body)
			   "(Function's argument type must match the annotation.)\n")))
	argtypes inittypes)

      (let-values ([(newbod bodtype) (annotate-expression body (tenv-extend tenv ids argtypes)
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
       (let* ([newtype* (map (lambda (new old) 
			       (if old (types-equal! new (instantiate-type old '())
						     `(let ,(map list id* inittypes rhs*) ,bod)
						     "(Let's argument type must match the annotation.)\n"))
			       (make-tcell new)
			       )
			  rhsty* inittypes)]
	      [tenv (extend-with-maybe-poly id* rhs* newtype* tenv)])
	 (let-values ([(bod bodty) (annotate-expression bod tenv nongeneric)])
	   (values `(let ,(map list id* newtype* newrhs*) ,bod) bodty)))]
      )))



;; Extend a type environment and turn on let-bound polymorphism based
;; on parameter settings and on whether the right-hand-side meets the
;; value restriction.
(define (extend-with-maybe-poly lhs* rhs* newtype* tenv)
  ;; Let bound polymorphism.  We use the "value restriction" like ML:
  ;; I must admit, I think this is quite lame.
  (let loop ([lhs* lhs*] [rhs* rhs*] 
	     [newtype* newtype*]
	     [tenv tenv])
#;
    (if (and (not (null? lhs*)) (eq? (car lhs*) 'zip2_sametype))
	(inspect (vector (value-expression? (car rhs*))			 
			 (inferencer-let-bound-poly)			 
			 (car newtype*)
			 (car rhs*) 
			 )))

    (if (null? lhs*) tenv
	(loop (cdr lhs*) (cdr rhs*) (cdr newtype*)
	      (tenv-extend tenv (list-head lhs* 1) (list-head newtype* 1)
			   (and (inferencer-let-bound-poly)
				(value-expression? (car rhs*))))))))

;; Is it an expression directly returning a value rather than
;; performing arbitrary computation.
(define (value-expression? x)
  (match x
    [,v (guard (symbol? v)) #t]
    [(quote ,c)      #t]
    [(lambda . ,_)   #t]
    ;; Special case:
    [(gint ,[e])           e]
    [(src-pos ,_ ,[e])     e]
    [(assert-type ,_ ,[e]) e]
    ;; Include other prims??  Arith?
    
    [,else #f]
    ))

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
					 `(letrec (,'... [,new ,old] ,'...) ,'...)
					 "Recursive Let's argument type must match the annotation.\n")))
		  rhs-types existing-types)]
             [tenv (extend-with-maybe-poly id* rhs* rhs-types tenv)])
        ;; Unify all these new type variables with the rhs expressions
        (let ([newrhs* 
               (map-ordered2 (lambda (type rhs)
			       (match type
				 [(quote (,v . ,_))
				  (DEBUGASSERT tenv? tenv)
				  ;; For our own RHS we are "nongeneric".
				  (let-values ([(newrhs t) (annotate-expression rhs tenv (cons v nongeneric))])
				    (types-equal! type t rhs 
						  "(possibly recursive) binding does not match its explicit annotation.")
                                    ;(inspect `(GOTNEWRHSTYPE ,t ,type))
				    newrhs)
				  ]))
                             rhs-types rhs*)])
          (let-values ([(bode bodt) (annotate-expression bod tenv nongeneric)])
            (values `(,letrecconstruct ,(map list id* rhs-types newrhs*) ,bode) bodt)))))))


;; This lifts export-type over expressions.
(define (export-expression e)
  (match e ;; match-expr
    [,v (guard (symbol? v)) v]
    [(quote ,c)       `(quote ,c)]
    [,prim (guard (symbol? prim) (wavescript-primitive? prim))  prim]
    [(assert-type ,[export-type -> t] ,[e]) `(assert-type ,t ,e)]
    [(src-pos ,p ,[e]) `(src-pos ,p ,e)]
    [(lambda ,v* (,[export-type -> t*] ...) ,[bod]) `(lambda ,v* ,t* ,bod)]

    [(iterate ,annot ,[f] ,[s]) `(iterate ,annot ,f ,s)]
    [(unionN ,annot ,[args] ...) `(unionN ,annot ,@args)]
    [(_merge ,annot ,[s1] ,[s2]) `(_merge ,annot ,s1 ,s2)]
    [(readFile ,annot ,[args] ...) `(readFile ,annot ,@args)]
    [(__readFile ,annot ,[args] ...) `(__readFile ,annot ,@args)]
    [(timer ,annot ,[args] ...) `(timer ,annot ,@args)]

    ;; All these simple cases just recur on all arguments:
    [(,simplekwd ,[args] ...)
     (guard (or (eq-any? simplekwd 'if 'tuple 'begin 'while 'app 'foreign-app 'construct-data 'static-allocate)
		(wavescript-primitive? simplekwd)))
     `(,simplekwd ,@args)]

    [(set! ,v ,[e]) `(set! ,v ,e)]
    [(for (,i ,[s] ,[e]) ,[bod]) `(for (,i ,s ,e) ,bod)]
    [(let ([,id* ,[export-type -> t*] ,[rhs*]] ...) ,[bod])
     `(let ,(map list id* t* rhs*) ,bod)]
    [(,letrec ([,id* ,[export-type -> t*] ,[rhs*]] ...) ,[bod])
     (guard (memq letrec '(letrec lazy-letrec)))
     `(,letrec ,(map list id* t* rhs*) ,bod)]

    [(wscase ,[x] [,pat* ,[rhs*]] ...) `(wscase ,x ,@(map list pat* rhs*))]
    [,c (guard (simple-constant? c)) c]

#|
    [(,ignrfst . ,rest) (guard (ignore-leading? ignrfst))
     (define skipped (ignore-leading-how-many ignrfst))
     (cons (append (list-head ) (map (list-tail ))))
     ]

|#
    [(struct-ref ,nm ,fld ,[e])   `(struct-ref ,nm ,fld ,e)]
    [(make-struct ,nm ,[e*] ...)  `(make-struct ,nm ,@e*)]
    [(,refcnt ,ty ,[e]) (guard (refcount-form? refcnt))
     `(,refcnt ,ty ,e)]
#|
    [(tupref ,n ,[e]) `(tupref ,n ,e)]
    [(wsrecord-extend   ,n ,[e1] ,[e2])  `(wsrecord-extend ,n ,e1 ,e2)]
    [(wsrecord-select   ,n ,[e])         `(wsrecord-select ,n ,e)]
    [(wsrecord-restrict ,n ,[e])         `(wsrecord-restrict ,n ,e)]
|#

    ;[(static-allocate ,[x]) `(static-allocate ,x)]
    
    ;; HACK HACK HACK: Fix this:
    ;; We cheat for nums, vars, prims: 
    ;;[,other other]; don't need to do anything here...
    ;;       [,other (error 'annotate-program "bad expression: ~a" other)]
    ))

;; This traverses the expression and does any LATEUNIFY's
(define (do-all-late-unifies! e)
  (match e ;; match-expr
    [,v (guard (symbol? v))                                   (void)]
    [(quote ,c)                                               (void)]
    [,prim (guard (symbol? prim) (wavescript-primitive? prim))  (void)]
    ;; The type occurring here isn't instantiated (thus doesn't contain late unifies)
    [(assert-type ,t ,[e])                                    (void)]
    [(src-pos ,t ,[e])                                        (void)]
    [(lambda ,v* (,[do-late-unify! -> t*] ...) ,[bod])        (void)]

    [(tupref ,n ,[e])                                         (void)]

    [(set! ,v ,[e])                                           (void)]

    ;; All these simple cases just recur on all arguments:

    [(iterate ,annot ,[f] ,[s])                               (void)]
    [(unionN ,annot ,[args] ...)                              (void)]
    [(_merge ,annot ,[s1] ,[s2])                              (void)]
    [(readFile ,annot ,[args] ...)                            (void)]
    [(__readFile ,annot ,[args] ...)                            (void)]
    [(timer ,annot ,[args] ...)                               (void)]

    [(,simplekwd ,[args] ...)
     (guard (or (eq-any? simplekwd 'if 'tuple 'begin 'while 'app 'foreign-app 'construct-data)
		(wavescript-primitive? simplekwd)))
     (void)]

    [(for (,i ,[s] ,[e]) ,[bod])                              (void)]
    [(let ([,id* ,[do-late-unify! -> t*] ,[rhs*]] ...) ,[bod]) (void)]
    [(,letrec ([,id* ,[do-late-unify! -> t*] ,[rhs*]] ...) ,[bod])
     (guard (memq letrec '(letrec lazy-letrec)))              (void)]
    [(wscase ,[x] [,pat* ,[rhs*]] ...) `(wscase ,x ,@(map list pat* rhs*))]
    [,c (guard (simple-constant? c))                                 (void)]

;    [(wsrecord-extend   ,n ,[e1] ,[e2])                       (void)]
;    [(wsrecord-restrict ,n ,[e])                              (void)]
;    [(wsrecord-select   ,n ,[e])                              (void)]

    [(struct-ref ,nm ,fld ,[e])                               (void)]
    [(make-struct ,nm ,[e*] ...)                              (void)]
    [(,refcnt ,ty ,[e])  (guard (refcount-form? refcnt)) (void)]
    [(static-allocate ,[x])                                   (void)]
    ))




;; This simply removes all the type annotations from an expression.
;; This would  be a great candidate for a generic traversal:
(define (strip-binding-types p)  
  (define (process-expression e)
    (match e
    [(lambda ,v* ,optionaltypes ,[bod]) `(lambda ,v* ,bod)]
    [(lambda ,v* ,[bod])                `(lambda ,v* ,bod)]
    [(,lett ([,id* . ,tail*] ...) ,[bod])
     (guard (memq lett '(let let* letrec lazy-letrec)))
     (let ([rhs* (map process-expression (map last tail*))])     
       `(,lett ,(map list id* rhs*) ,bod))]
    [(quote ,c)       `(quote ,c)]
;    [(return ,[e]) `(return ,e)]
    [,var (guard (symbol? var)) var]
    
    [(set! ,v ,[e]) `(set! ,v ,e)]
    [(for (,i ,[s] ,[e]) ,[bod]) `(for (,i ,s ,e) ,bod)]

    [(iterate-bench ,a ,t ,n ,ht ,std ,[f] ,[s]) `(iterate-bench ,a ,t ,n ,ht ,std ,f ,s)]
    [(unionN-bench ,a ,t ,n ,ht ,std ,[s*] ...) `(unionN-bench ,a ,t ,n ,ht ,std ,@s*)]
    [(_merge-bench ,a ,t ,n ,ht ,std ,[s1] ,[s2]) `(_merge-bench ,a ,t ,n ,ht ,std ,s1 ,s2)]
    [(timer-bench ,a ,t ,n ,ht ,std ,[freq] ,[num-tuples]) `(timer-bench ,a ,t ,n ,ht ,std ,freq ,num-tuples)]

    ;; [2007.11.05] rrn: Generalizing:
    [(,streamop ,annot ,[arg*] ...) (guard (temp-hack-stream-primitive? streamop))
     `(,streamop ,annot ,@arg*)]
    [(tupref ,n ,m ,[x]) `(tupref ,n ,m ,x)]

    ;; [2008.08.24] No longer stripping annotations here, that's a separate matter.
    [(,annot ,p ,[e]) (guard (annotation? annot)) `(,annot ,p ,e)]

    ;; This accomodates ws.early:
    [(readFile-wsearly ,[fn] ,[str] ,ty) `(readFile-wsearly ,fn ,str ,ty)]
    [(delay ,[e]) `(delay ,e)]

    ;; All these simple cases just recur on all arguments:
    [(,simplekwd ,[args] ...)
     (guard (or (eq-any? simplekwd 'if 'tuple 'begin 'while 'app 'foreign-app 'construct-data)
		(wavescript-primitive? simplekwd)))
     `(,simplekwd ,@args)]

    [(wscase ,[x] [,pat* ,[rhs*]] ...) `(wscase ,x ,@(map list pat* rhs*))]

;    [(wsrecord-extend   ,n ,[e1] ,[e2])  `(wsrecord-extend ,n ,e1 ,e2)]
;    [(wsrecord-select   ,n ,[e])         `(wsrecord-select ,n ,e)]
;    [(wsrecord-restrict ,n ,[e])         `(wsrecord-restrict ,n ,e)]

    [,c (guard (simple-constant? c)) c]
    [,other (error 'strip-binding-types "bad expression: ~a" other)]
    ))
  (match p 
    [(,lang '(program ,expr ,_ ...)) 
     `(,lang '(program ,(process-expression expr) . ,_))]
    [,expr (process-expression expr)]
    ;[,other (error 'strip-binding-types "Bad program, maybe missing boilerplate: \n~s\n" other)]
    ))


;; Optionally takes symbols indicating which annotations to strip.
;; Otherwise, strips all.
(define strip-annotations
  (let ()
    (define tostrip #f)
    (define (Expr x fallthru)
      (match x
	[(using ,M ,[e]) `(using ,M ,e)]
	[(,annot ,_ ,[e]) (guard (annotation? annot)
				 (or (not tostrip) 
				     (and tostrip (memq annot tostrip))))
	 e]
	[,other (fallthru other)]))
    (lambda (p . args)
      (fluid-let ([tostrip (if (null? args) #f args)])
	(match p 
	  [(,lang '(program ,[E] ,_ ...))  `(,lang '(program ,E ,@_))]
	  [,expr ((core-generic-traverse Expr) expr)]
	  )
	))))

;; This annotates the program, and then exports all the types to their
;; external form (stripped of mutable cells on the tvars).
;; .param a program with or without boilerplate.
;; .returns 1 or 2 values: new program and toplevel type
;;                         If input is with-boilerplate, output is one value.
;; 
(define (annotate-program p)
  (let ([Expr 
	 (lambda (p tenv)
	   (let-values ([(e t) (annotate-expression p tenv '())])
	     (do-all-late-unifies! e)
	     ;; Now strip mutable cells from annotated expression.
	     (values (export-expression e)
		     (export-type t)))
	   )])

    ;; Accepts either with-boilerplate or without.
    (match p
      [(,lang '(program ,bod ,metadat* ... ,type))
       (ASSERT type? type)
       (let-values ([(e t) (Expr bod (sumdecls->tenv
				      (cdr (or (assq 'union-types metadat*) '(union-types)))))])
         `(typechecked-lang '(program ,e ,@metadat* ,t)))]
      
      ;; [2008.01.22] I'm adding this for the output of insert-refcounts.
      ;; Rather than reconstructing a complete program, this
      ;; typechecks the parts in isolation.  Thus, while existing type
      ;; annotations may be missing inside individual pieces, the
      ;; interfaces between them better be correctly specified.
      ;; (For example, the output type of each iterate.)
      [(,lang 
	'(graph (const (,cbv* ,cbty* ,cbexp*) ...)
		(init  ,init* ...)
		(sources ((name ,s_nm*) (output-type ,s_ty*) (code ,scode*) (outgoing ,down* ...)) ...)
		(operators (,op* (name ,itername*) 
				 (output-type ,o_ty*)
				 (code ,itercode)
				 (incoming ,o_up* ...)
				 (outgoing ,o_down* ...)
				 ) ...)
		(sink ,base ,basetype)
		,meta* ...))
       ;(define sumtenv (sumdecls->tenv (cdr (or (assq 'union-types meta*) '(union-types)))))
       (define fulltenv 
         (tenv-extend (sumdecls->tenv (cdr (or (assq 'union-types meta*) '(union-types))))
		      (append cbv* itername* s_nm*)
		      (append cbty* o_ty* s_ty*)))
       (define (Doit x) (first-value (Expr x fulltenv)))
       (define _scode (map Doit scode*))
       (define _itercode (map Doit itercode))
       `(,lang
	 '(graph (const (,cbv* ,cbty* ,(map Doit cbexp*)) ...)
		 (init ,@(map Doit init*))
		 (sources ((name ,s_nm*) (output-type ,s_ty*) (code ,_scode) (outgoing ,down* ...)) ...)
		 (operators (,op* (name ,itername*) 
				  (output-type ,o_ty*)
				  (code ,_itercode)
				  (incoming ,o_up* ...)
				  (outgoing ,o_down* ...)) ...)
		 (sink ,base ,basetype)
		 ,@meta*))]

      [(,lang (quote (,sym . ,_))) (guard (memq sym '(graph program)))
       (warning 'annotate-program "this looks like a bad program: ")
       (inspect p)]

      [,other (Expr other (empty-tenv))])))




;; This converts union-type declarations into a type environment in
;; which all the constructors aroe bound to appropriate function types.
(define (sumdecls->tenv decl*)
  (define (sumdecl->tbinds decl tenv)
    (match decl 
      [((,name ,typarg* ...) [,tycon* . ,ty**] ... ) (guard (symbol? name))
       (tenv-extend tenv tycon* 
		    (map (lambda (ty*)
			   ;; Make an instantiated type var because tenv-extend expects that.
			   (make-tcell
			    ;; TEMP, trying instantiating here:
			    (instantiate-type `(,@ty* -> (Sum ,name ,@typarg*))
					      )))
		      ty**)
		    #t ;; data constructors are *always* polymorphic
		    )
       ]))
  (foldl sumdecl->tbinds (empty-tenv) decl*))

;; This is a front-end to the above which takes the list of metadata
;; associated with a toplevel '(program ...) form and builds the init-tenv.
(define (grab-init-tenv metadata)
  (sumdecls->tenv 
   (cdr (or (assq 'union-types metadata)
	    '(union-types)))))

;; Like peel-annotations, but gets rid of extra type-var bindings on the outside.
(define (peel-outer-typevars ty)
  (match ty 
    [(quote (,v . ,[ty])) ty]
    [(LATEUNIFY ,_ ,[general]) general]
    [,oth oth]))

;; This takes a (Sum t) type and instantiates it for a particular variant.
;; .returns  The types of the data-constructor's fields.

;; Also, as a side-effect 'sumty' (which must be instantiated) is
;; constrained to be compatible with the returned constructor arg types.
(define (sum-instance! tenv sumty variant-name)
  (ASSERT (not (eq? variant-name default-case-symbol)))
  (let ([type (tenv-lookup tenv variant-name)])
    (unless type
      (error 'sum-instance! "This variant \"~s\" is not a member of the sum type." variant-name))
    (let ([arrowty (instantiate-type type)])
      (match (peel-outer-typevars arrowty)
	[(,arg* ... -> ,ret) 
	 (let ([cells (map (lambda (_) (make-tcell)) arg*)])
	   ;(inspect `(,@cells -> ,(instantiate-type sumty)))
	   ;(types-equal! arrowty  `(,@cells -> ,(instantiate-type sumty)) "" "<Intrnal: sum-instance>")
	   (types-equal! arrowty  `(,@cells -> ,sumty) "" "<Intrnal: sum-instance!>")
					;(map export-type cells)
	   cells ;; Return instantiated type.
					;(map instantiate-type (map export-type cells))
	   )]))))
      
; ======================================================================

;;; The unifier.

;; This is a front-end to the unifier which uses it to tell you if two
;; types are compatible.  This is inefficient, but is necessitated by
;; the way I wrote the unifier.  (It throws an error on failed unification.)
;; 
;; types-compat? returns its second arguments post-unified form.
(define (types-compat? t1 t2)
  (call/ec
   (lambda (k) 
     (fluid-let ([type-error (lambda args (k #f))])
       ;; We export first, this makes absolutely sure that we don't 
       ;; destroy any of the mutable state in t1 or t2.
       (let ([inst1 (make-tcell (instantiate-type (export-type t1) '()))]
	     [inst2 (make-tcell (instantiate-type (export-type t2) '()))])
;	 (printf "Input types: ~s\n" (cons t1 t2) )
;	 (printf "Initial instantiations: ~s\n" (cons inst1 inst2) )
	 (types-equal! inst1 inst2 (void) "<void>")
;	 (printf "Final instantiations: ~s\n" (cons inst1 inst2) )
	 ;;(k (export-type inst1))
	 (k (export-type inst2))
	 )))))

;; This asserts that two types are equal.  Mutates the type variables
;; to reflect this constraint.
;; Returns nothing.
(define (types-equal! t1 t2 exp msg) ; maybemsg
;  (define msg (if (null? maybemsg) "" (car maybemsg)))
;  (ASSERT (not (null? msg)))
  (DEBUGASSERT instantiated-type? t1) (DEBUGASSERT instantiated-type? t2)
  (DEBUGASSERT (compose not procedure?) exp)
  (match (cons t1 t2)
    [[,x . ,y] (guard (eqv? t1 t2)) (void)]

    [[(LATEUNIFY ,_ ,t1) . ,t2]  (types-equal! t1 t2 exp msg)]
    [[,t1 . (LATEUNIFY ,_ ,t2)]  (types-equal! t1 t2 exp msg)]

    [[',tv1 . ',tv2] (guard (eqv? tv1 tv2)) (void)] ;; alpha = alpha
    [[',tv . ,ty] (tvar-equal-type! t1 t2 exp msg)]
    [[,ty . ',tv] (tvar-equal-type! t2 t1 exp msg)]
    [[,x . ,y] (guard (symbol? x) (symbol? y))
     ;; TODO: Raise a better error if one of the symbols is not an alias or a builtin:
     (define extramsg "")
     (unless (memq x built-in-atomic-types)
       (string-append extramsg (format "\nThis is not a built-in type or an alias: ~a\n" x)))
     (unless (memq y built-in-atomic-types) 
       (string-append extramsg (format "\nThis is not a built-in type or an alias: ~a\n" y)))     
;     (unless (or (memq x built-in-atomic-types) (eq? x 'DummyType))
;       (warning 'types-equal "This is not a built-in type or an alias: ~a\n" x))
;     (unless (or (memq y built-in-atomic-types) (eq? y 'DummyType))
;       (warning 'types-equal "This is not a built-in type or an alias: ~a\n" y))
     (raise-type-mismatch (string-append msg extramsg) x y exp)]

    [[(NUM ,tv1) . (NUM ,tv2)] (tvar-equal-type! t1 t2 exp msg)]
    [[(NUM ,x) . ,numty]   (guard (symbol? numty) (memq numty num-types))
     (tvar-equal-type! t1 numty exp msg)]
    [[,numty .   (NUM ,x)] (guard (symbol? numty) (memq numty num-types))
     (tvar-equal-type! t2 numty exp msg)]

    ;; FIXME TODO: Handle ROW variables...

    [[#(,x* ...) . #(,y* ...)]
     (guard (= (length x*) (length y*)))
     (for-each (lambda (t1 t2) (types-equal! t1 t2 exp msg)) x* y*)]

    ;; Records - this is the tricky part    
    [[(Record ,rowx) . (Record ,rowy)]
     ;; By convention, both rows must be wrapped in a tcell:
     (let-match (['(,var . ,tyx) rowx]
		 ['(,var . ,tyy) rowy])
       (cond
	;; If either one of them simply an unset tvar, we set it.
	[(not tyx) (tcell-set! rowx rowy)]
	[(not tyy) (tcell-set! rowy rowx)]
	[else 
	 ;; Otherwise they both have at least one 'Row'.
	 ;; To be order-agnostic we build a hash table for both row sets:
	 (let ([table1 (make-default-hash-table)]
	       [table2 (make-default-hash-table)])
	   ;; Returns the row variable (cell) of the final tail, or #f if the row is closed:
	   (define (scan-fields row table)
	     (match row ;; no recursion
	       [#() #f] ;; A closed record.
	       ['(,var . ,ty)  (if ty (scan-fields ty table) row)]
	       [(Row ,name ,ty ,tail)
		(let* ([result (scan-fields tail table)]
		       [prev (hashtab-get table name)])
		  (hashtab-set! table name (cons ty (or prev '())))
		  result)]
	       [,else (error 'types-equal! "unmatched row variable: ~s" else)]))

	   ;; If we need to, we reallocate the spine so as to close the row:
	   (define (maybe-close-it row)
	     (if (or xclosed? yclosed?)
		 (let loop ([row row])
		   (match row
		     [(Row ,nm ,ty ,[tail]) `(Row ,nm ,ty ,tail)]
		     ['(,var . ,ty) (if ty (loop ty) '#())]
		     [#() '#()]))
		 row))
	   (define xtail (scan-fields rowx table1))
	   (define ytail (scan-fields rowy table2))
	   (define shared-tail? (eq? xtail ytail))
	   ;; As in Leijen's 2005 paper, we must be wary of record types with the same tail:
	   (define (no-shared-tail!)
	     (when shared-tail?
	       (raise-type-mismatch "Could unify record types with identical base type, but different extensions" 
				    `(Record ,rowx) `(Record ,rowy) exp)))
	   
	   (define xclosed? (not xtail))
	   (define yclosed? (not ytail)) 

	   (define (row-append name tyls row)
	     (if (null? tyls) row
		 `(Row ,name ,(car tyls) 
		       ,(row-append name (cdr tyls) row))))

	   ;; This represents any new fields to be added in the future:
	   (define common-fate (make-tcell))

	   ;; This will accumulate all the fields in the combined record type:
	   (define row-acc (if (or xclosed? yclosed?) '#() common-fate))
	   ;; This accumulates those constraints that need to be added to rowx/rowy:
	   (define xacc row-acc)
	   (define yacc row-acc)

	   ;(printf "Unifying rows:\n  ~a\n  ~a\n   xclosed/yclosed: ~a ~a\n" rowx rowy xclosed? yclosed?)

	   ;; Traverse the second type, unifying against entries from the first table.
	   (hashtab-for-each
	    (lambda (name tyls)
	      (define msg (format "field labeled ~a must match" name))
	      (let ([fst (hashtab-get table1 name)])
		(if fst
		  ;; TODO : This woud be a place for some better error messages:
		  ;; It's in both, unify the whole stack of scoped labels.
		  ;; They must have a matching prefix... whichever goes deeper will 
		  ;; determine the depth of the unified type.
		  (let loop ([l1 fst] [l2 tyls])
		    (cond
		     [(null? l1) (set! row-acc (row-append name tyls row-acc))] ;; row2 goes deeper
		     [(null? l2) (set! row-acc (row-append name fst  row-acc))] ;; row1 goes deeper
		     [else (types-equal! (car l1) (car l2) exp msg)
			   (loop (cdr l1) (cdr l2))]))
		  (begin 
		    (set! row-acc (row-append name tyls row-acc))
		    ;; If it's in rowy but not rowx:
		    (when xclosed?
		      (raise-type-mismatch (format "Could not unify closed record type with one containing label ~a" name)
					   `(Record ,rowx) `(Record ,rowy) exp))
		    (set! xacc (row-append name tyls xacc))
		    (no-shared-tail!)))
		))
	    table2)

	   ;; Now, with the relevant fields unified, we just need to
	   ;; combine all fields from both rowx & rowy.	   
	   (hashtab-for-each
	      ;; Acumulate any from rowx that aren't already in rowy.
	      (lambda (name tyls)
		(unless (hashtab-get table2 name)
		  (no-shared-tail!)
		  ;; In this case it's in rowx but not rowy:
		  (when yclosed?
		    (raise-type-mismatch (format "Could not unify closed record type with one containing label ~a" name)
					 `(Record ,rowx) `(Record ,rowy) exp))
		  (set! yacc (row-append name tyls yacc))
		  (set! row-acc (row-append name tyls row-acc))
		  ))
	      table1)	     
	   ;(printf "COMBINED: ~s,\n  xtail: ~s  xacc: ~s\n  ytail: ~s  yacc: ~s\n" row-acc xtail xacc ytail yacc)
	   ;; Now we overwrite them both to the combined row:
	   (tcell-set! rowx row-acc)
	   (tcell-set! rowy row-acc)
	   ;; And restrict the tails of the original types:
	   (unless xclosed? (tcell-set! xtail xacc))
	   (unless yclosed? (tcell-set! ytail yacc))
	   )]))]

#|
    [[(Record ,rowx ,x* ...) . ,_]
     (error 'record-unification "unimplemented2 ~a" _)
     ]
    [[,_ . #(Record ,rowy ,y* ...)]
     (error 'record-unification "unimplemented3")
     ]
|#

    ;; Ref will fall under this category:
    [[(,x1 ,xargs ...) . (,y1 ,yargs ...)]
     (guard (symbol? x1) (symbol? y1)
	    (not (memq '-> xargs))
	    (not (memq '-> yargs))
	    (= (length xargs) (length yargs)))
     (if (not (eq? x1 y1))	 
	(raise-type-mismatch "These constructors do not match." x1 y1 exp)
	;(type-error 'types-equal! "type constructors do not match: ~a and ~a in ~a" x1 y1 exp)
	 )
;     (types-equal! x1 y1 exp)
     (for-each (lambda (t1 t2) (types-equal! t1 t2 exp msg)) xargs yargs)]

;; [2005.12.07] Just got a "wrong number of arguments" error that might be a match bug.
;;    [[(,xargs ... -> ,x) (,yargs ... -> ,y)] 
    ;; Working around this in a lame way:
    [[,x .  (,yargs ... -> ,y)] 
     (match x 
       [(,xargs ... -> ,x)
	(unless (= (length xargs) (length yargs))
	  ;(printf "\nError: ~a\n" msg) ;; Print this before raising the generic message:
	  (raise-wrong-number-of-arguments t1 t2 exp))
	
	;; EXPERIMENTAL: TESTING: TEMPTOGGLE:
	(letrec ([stream? (lambda (ty) (match (peel-outer-typevars ty) [(Stream ,_) #t] [,_ #f]))]
		 [nonstream? 
		  (lambda (ty) 
		    (and (peel-outer-typevars ty) ;; Not an undefined type var.
			 (not (stream? ty)))
		    #;
		    (match (peel-outer-typevars ty) 
		      [(,C . ,_) 
		       (printf "   nonstream constructor?: ~s" C)
		       (not (eq? C 'Stream))]
		      [,_ #f]))])
	  (for-each (lambda (t1 t2) 
		      #;
		      (printf "  TESTING: ~s ~s  strm: ~s/~s nonstrm: ~s/~s\n" (peel-outer-typevars t1) (peel-outer-typevars t2)
			      (stream? t1) (stream? t2)
			      (nonstream? t1) (nonstream? t2))
		      (if (or (and (stream? t1) (nonstream? t2))
			      (and (stream? t2) (nonstream? t1)))
			  ;(inspect (list "Got potential stream coercion:" t1 t2))
			  (printf "  Got potential stream coercion: ~s ~s" 
				  (peel-outer-typevars t1) (peel-outer-typevars t2))
			  ))
	    xargs yargs))

	(for-each (lambda (t1 t2) (types-equal! t1 t2 exp msg))
	  xargs yargs)
	(types-equal! x y exp msg)]
       [,other 
	(raise-type-mismatch "Function type does not match non-function type." 
			     (export-type `(,@yargs -> ,y))
			     (export-type other) 
			     exp)
	#;
	(type-error 'types-equal!
		      "procedure type ~a\nDoes not match: ~a\n\nUnexported versions: ~a\n  ~a\n"
		      (export-type `(,@yargs -> ,y))
		      (export-type other)
		      `(,@yargs -> ,y)
		      other)])]
    
    ;; Strings are the only thing that the eqv? above won't work for:
    ;[[,x . ,y] (guard (string? t1) (string? t2) (string=? t1 t2)) (void)]
    ;; Actually for now these are all the "same" type:
    [[,x . ,y] (guard (string? t1) (string? t2)) (void)]

    [,otherwise (raise-type-mismatch msg t1 t2 exp)]))


 
;; This helper mutates a tvar cell while protecting against cyclic structures.
(define (tvar-equal-type! tvar ty exp msg)
  (DEBUGASSERT (type? ty))
  (match tvar ;; Type variable should be a quoted pair.
    [(,qt ,pr) (guard (tvar-quotation? qt);(memq qt '(NUM quote))
		      )
     (if (not (pair? pr))
	 (error 'tvar-equal-type! "bad tvar here, no cell: ~a" tvar))
     (if (cdr pr)

	 (types-equal! (cdr pr) ty exp msg)

	 (begin (no-occurrence! (tcell->name tvar) ty exp)
		(set-cdr! pr ty)))]))

;; This returns the least-upper bound of two types.  That is, the
;; least-general type that is a superset of both input types.
(define (LUB t1 t2)
;  `(LUB ,t1 ,t2)
  ;; UNFINISHED:

  (match (list t1 t2)
    [[,x ,y] (guard (eqv? x y)) x]
    ;[[',tv1 ',tv2] (guard (eqv? tv1 tv2)) ] ;; alpha = alpha

    ;; Two num-types join at NUM:
    [[,x ,y] (guard (memq x num-types) (memq y num-types))
     `(NUM ,(make-tvar))]

    ;; Two record types? TODO FIXME

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
		       (cond [(assq s wavescript-type-aliases) => 			      
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

    [[(Record ,x* ...) (Record ,y* ...)]
     (error 'record-lub "unimplemented")
     ]
    [[,x (Record ,y* ...)]
     (error 'record-lub "unimplemented")
     ]
    [[(Record ,x* ...) ,y]
     (error 'record-lub "unimplemented")
     ]

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
    [,otherwise (raise-type-mismatch "" t1 t2 "unknown expression (LUB)")]
    ))
  
  
;; This makes sure there are no cycles in a tvar's mutable cell.
;; .returns #t if there are no loops, or throws an error otherwise.
(define (no-occurrence! tvar ty exp)
  (DEBUGASSERT (type? ty))
  ;; This is a lame hack to repair certain cycles.
  (if (match ty
	[(,qt (,tyvar . ,_))     (guard (tvar-quotation? qt) (eq? tyvar tvar)) #t]
	[(,qt (,tyvar . ,[tyt])) (guard (tvar-quotation? qt)) tyt]
	[,else #f])

      ;; HACK: VERIFY CORRECTNESS::
      ;; Ok, this is recursive, but it's A=B=A, not some more complex
      ;; recursive type constraint.
      (begin 
	(when (>= (wavescript-verbosity) 3)
	  (warning 'no-occurrence! "encountered A=B=A type constraint: ~s" ty))
	(match ty
	  [(,qt ,tvarpair)	   
	   ;; Ouch, mutating in the guard... Nasty.
	   (guard  (tvar-quotation? qt);(memq qt '(quote NUM))
		   (match tvarpair
		     [(,outer . (,qt (,inner . ,targettyp))) 
		      (guard (tvar-quotation? qt);(memq qt '(NUM quote))
			     (eq? inner tvar))
		      ;; Short circuit the equivalence, this doesn't destroy
		      ;; information that's not already encoded.
		      (set-cdr! tvarpair targettyp)
		      (when (>= (wavescript-verbosity) 3)
			(printf "  SHORT CIRCUITED: ~s to ~s\n" outer targettyp))
		      ]
		     [(,outer . (,qt ,[deeper])) (guard (tvar-quotation? qt)) (void)]
		     [else (error 'no-occurrence! "this is an implementation bug.")]))
	   ;; Guard already did the work:
	   (void)]	  	 
	  [,other (error 'no-occurrence! "there's a bug in this implementation. Unhandled: ~s" other)])
	)
      
  (match ty
    [#f #t]
    [,s (guard (symbol? s)) #t]
    [(,qt (,tyvar . ,[tyt])) (guard (tvar-quotation? qt))
     (if (equal? tyvar tvar)
	 (raise-occurrence-check tvar ty exp))]
    [#(,[t*] ...) #t]
    [(,[arg*] ... -> ,[res]) res]    
    [,s (guard (string? s)) s] ;; Allowing strings for uninterpreted C types.
    [(,C ,[t*] ...) (guard (symbol? C)) #t] ; Type constructor, including Record, Row
;    [,other (inspect (vector other tvar))]
    [,other (error 'no-occurrence! "malformed type: ~a" ty)]
    )))


; ======================================================================
;; Printing the type-signatures inside a large expressions:


(define (show-type ty)
  (define (loop outer?) 
    (lambda (t)
      (match t
	[(quote ,[var]) (** "'" var)]
	[(NUM ,[var]) (** "#" var)]
	[(ROW ,[var]) (** "ROW" var)] ;; TEMP
	[(-> ,b) (** "() -> " ((loop #t) b))]

	;; One arg functions:
	[(, left -> ,[(loop #t) -> right])
	 (if (arrow-type? left)
	     (** "(" ((loop #t) left) ") -> " right)
	     (**     ((loop #t) left)  " -> " right))]
	[(,[(loop #t) -> arg*] ... -> ,[(loop #t) -> b])
	 (** "(" (apply string-append (insert-between ", " arg*)) ")"
	     " -> "b)]

	[#(,[(loop #t) -> x*] ...)
	 (** "(" (apply string-append (insert-between " * " x*)) ")")]
	
	;; There's a question about when we normalize these type representations.
	[(Record ,row)
	 ;(warning 'record-show-type "unimplemented")
	 ;; TEMP
	 (apply string-append "("
		(let loop ([row row] [acc '()])
		  (define (wrapup) (apply append (reverse! (cons '(")") (insert-between '(", ") acc)))))
		  (match row
		    [(Row ,nm ,[show-type -> ty] ,tail)
		     (loop tail (cons (list (symbol->string nm) ":" ty) acc))]
		    ['(,v . ,_) 
		     ;(error 'show-type "type should not be instantiated, found this within Row: ~a" row)
		     (cons* " _ | " (wrapup))
		     ]
		    [',v (cons* (symbol->string v) " | " (wrapup))]
		    [#() (cons                      "| " (wrapup))])))]

	;; [2006.12.01] Removing assumption that TC's have only one arg:
	[(,[tc] ,arg* ...)
	 (let ([inside (apply string-append (insert-between " " (map (loop #f) arg*)))])
	   (if outer?		 
	       (**     tc " " inside )
	       (** "(" tc " " inside ")")))]
	[,sym (guard (symbol? sym)) (symbol->string sym)]
	[,s (guard (string? s)) (format "~s" s)] ;; Allowing strings for uninterpreted C types.
	[,other (error 'print-type "bad type: ~s" other)])))
  ;; Prettification: we drop the loop parens:
  ((loop #t) ty))

;; Prints a type in a WaveScript format way rather than the raw sexp.
;; Added a hack to omit parens around the outermost type constructor.
(define (print-type t . p)  
  (DEBUGASSERT type? t)
  (let ([port (if (null? p) (current-output-port) (car p))])
    (display (show-type t) port)))



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
  (let ([port (if (null? p) (current-output-port) (car p))]
	[aliases '()])
    (define (get-var-types exp)
      ;(printf "GETTING VAR TYPES\n")(flush-output-port (current-output-port))
      (match exp 

       [(,lang '(program ,[body] ,meta ... ,ty))
        (append body `((type BASE ,ty ())))]

       [,c (guard (simple-constant? c)) '()]
       [,var (guard (symbol? var))  `()]       
       [(quote ,c)       '()]
       [(assert-type ,t ,[e]) e]
       [(src-pos     ,p ,[e]) e]
       [(set! ,v ,[e]) e]
       [(for (,i ,[s] ,[e]) ,[bodls]) (cons `[type ,i Int ()] (append s e bodls))]
       [(tupref ,n ,m ,[x]) x]
       [(lambda ,v* ,t* ,[bodls])   bodls]

       ; FIXME: is this right?
       [(iterate ,annot ,[f] ,[s])
        (append f s)]

; WEIRD: this specific case seems to slow things down!
; But WHY?  print-var-types is run once!
#;
       [(,simplekwd ,[args] ...)
        (guard (or (eq-any? simplekwd 'if 'tuple 'begin 'while 'app 'foreign-app 'construct-data)
                   (wavescript-primitive? simplekwd)))
        (apply append args)]

       [(begin ,[e*] ...) (apply append e*)]
       [(while ,[tstls] ,[bodls]) (append tstls bodls)]
       [(if ,[t] ,[c] ,[a]) (append t c a)]
       [(wscase ,[x] [,pat ,[rhs*]] ...) (apply append x rhs*)]
       [(tuple ,[args] ...) (apply append args)]
       [(unionN ,annot ,[args] ...) (apply append args)]
       [(_merge ,annot ,[s1] ,[s2]) (append s1 s2)]
       [(readFile ,annot ,[args] ...) (apply append args)]
       [(__readFile ,annot ,[args] ...) (apply append args)]
       [(timer ,annot ,[args] ...) (apply append args)]
       [(,app ,[rat] ,[rand*] ...) (guard (memq app '(app foreign-app construct-data)))
        (apply append rat rand*)]

       [(wsrecord-extend   ,n ,[e1] ,[e2])  (append e1 e2)]
       [(wsrecord-select   ,n ,[e])         e]
       [(wsrecord-restrict ,n ,[e])         e]

       ;; This one case brings us from 0 to 30 ms:
       [(,prim ,[rand*] ...)
         (guard (wavescript-primitive? prim))
         (apply append rand*)]


       [(,let ([,id* ,t* ,rhs*] ...) ,[bod])
        (guard (memq let '(let letrec lazy-letrec)))
        (append (apply append 
                       (map (lambda (id t rhs)
			      ;(if (symbol? rhs) (inspect (format "SYMBOL: ~s\n" rhs)))
			      ;(unless (null? rhsls) (inspect rhsls))
			      ;(inspect (included-var-bindings))
                              (if (or (>= (wavescript-verbosity) 2)
                                      (and (not (memq id (included-var-bindings)))
                                           (not (symbol? (peel-annotations rhs)))))
                                  `([type ,id ,t ,(get-var-types rhs)])
                                  '()))
                         id* t* 
                         rhs*))
                bod)]

       [,other (error 'print-var-types "bad expression: ~a" other)]))

    ;; print-var-types body:
    (let ([aliases (cdr (or (project-metadata 'type-aliases exp) '(type-aliases)))])
      (let pvtloop (
                    [x (get-var-types exp)] 
                    [depth 0] [indent " "]
                    )
        (if (= depth max-depth) (void)
            (match x
              [() (void)]
              [(type ,v ,t ,subvars)
               (unless (eq? v '___VIRTQUEUE___) 	 ;; <-- HACK: 
                 (let* ([str (format "~a" v)]
                        [padding ;(modulo (string-length str) 25)
                         (max 0 (- 40 (string-length str)))
                         ])
                   (fprintf port "~a~a~a :: " indent v (make-string padding #\space))
                   )
                 (print-type 
		  ;; For prettyness we reset the tvar counts to not get such ugly tvars:
		  (let ([old-counter (get-tvar-generator-count)]
			[pretty (if (>= (wavescript-verbosity) 3) t (realias-type aliases t))])
		    ;(printf "\nUgly  : ~s\n" t)
		    ;(printf "Pretty: ~s\n" pretty)
		    (reset-tvar-generator 0)
		    (let ([fresh (if (>= (wavescript-verbosity) 5) pretty
				     (export-type (instantiate-type pretty '())))])
		      (reset-tvar-generator old-counter)
		      fresh))
		  port) (fprintf port ";\n"))
               
               (pvtloop subvars (fx+ 1 depth) (** indent "  "))]
              [,ls (guard (list? ls))
                   (for-each (lambda (x) (pvtloop x depth indent))
                     ls)]
              [,other (error 'print-var-types "bad result from get-var-types: ~a" other)]))))
    ))



;; Should take either an instantiated or non-instantiated type.
;; This takes the union types just so it can distinguish whether a type constructor is valid.
(define (dealias-type aliases union-types t)
  (define user-datatypes (map caar (cdr union-types)))
  (define (check-valid-sym s)
    (when (and (symbol? s) (not (memq s built-in-atomic-types)))
      (if (memq s built-in-type-constructors)
	  (error 'dealias-type "Type constructor is missing argument: ~s" s)
	  (error 'dealias-type "This type is not in the alias table, nor is it builtin: ~s\n  Aliases: ~s" 
		 s (map car aliases))))
    s)
  (let loop ([t t])
  (match t

      ;; A type alias with no type arguments:
      [,s (guard (symbol? s))                   
	  (let ([entry (or (assq s aliases)
			   (assq s wavescript-type-aliases))])
	    (check-valid-sym
	     (if entry 
		(begin 		  
		  ;; Make sure the alias has no type arguments:
		  (unless (null? (cadr entry))
                    (error 'dealias-type "this alias should have had arguments: ~s" s))
		  ;; Recursively dealias:
		  (loop (caddr entry)) ;; Instantiate?
		  )
		s)))]
      [',n                                     `(quote ,n)]

      ;;['(,n . ,v)                               (if v (Type v) `(quote ,n))]
      [(NUM ,v) (guard (symbol? v))            `(NUM ,v)]
      [(NUM (,v . ,t))                          (if t (loop t) `(NUM ,v))]

      [#(,[t*] ...)                            (apply vector t*)]
      
      [(,[arg*] ... -> ,[res])                 `(,@arg* -> ,res)]

      [(Pointer ,name)          `(Pointer ,name)]
      [(ExclusivePointer ,name) `(ExclusivePointer ,name)]

      ;[(Record ,[row] (,name* ,[ty*]) ...)      `(Record ,row ,@(map list name* ty*))]
      ;[(Record ,foo ...) (guard (begin (printf "matching record ~a" (cons 'Record foo)) #f)) #f]

      ;; [2009.06.04] Handle Rows, because the field names are not proper types and will generate errors:
      [(Row ,sym ,[ty] ,[row]) `(Row ,sym ,ty ,row)]

      ;; This is simple substitition of the type arguments:
      ;; Handles Record, Row:
      [(,s ,[t*] ...) (guard (symbol? s))

       (let ([entry (or (assq s aliases)
			(assq s wavescript-type-aliases))])
;	 (import iu-match) ;; Having problems!
	 (match entry
	   [#f 
	    (unless (or (memq s built-in-type-constructors)
			(memq s user-datatypes))
	      (error 'dealias-type "This type constructor is not in the alias table, nor is it builtin: ~s\n  Aliases: ~s" 
		    s (map car aliases)))
	    `(,s ,@t*)]

	   [(,v ,rhs) (error 'resolve-type-aliases 
			     "alias ~s should not be instantiated with arguments!: ~s" 
			     s (cons s t*))]
	   [(,v (,a* ...) ,rhs)
	    (let ([result 
		   ;; We're lazy, so let's use the existing machinery
		   ;; to do the substition.  So what if it's a little inefficient?	   
		   (match (instantiate-type `(Magic #(,@a*) ,rhs))
		     ;; We bundle together the LHS* and RHS here so that their mutable cells are shared.
		     [(Magic #(,cells ...) ,rhs)
		      ;; Now use the unifier to set all those mutable cellS:
		      (define newt* (cdr (instantiate-type (cons s t*)))) ;; [2008.01.22] Fixing by adding this.
		      (for-each (lambda (x y) (types-equal! x y "<resolve-type-aliases>" ""))
			cells newt*)
		      (export-type rhs)])])
	      ;; Finally, recursively dealias in case there are more aliases left.
	      (loop result)
	      )]

	   ))]
      [,other (error 'resolve-type-aliases "bad type: ~s" other)]
)))

;; For pretty-printing we want to use the names provided by user type definitions:
(define realias-type
  (let ()
    (define (types-equal!? t1 t2)
      (call/ec 
       (lambda (k)
	 (fluid-let ([type-error (lambda args (k #f))])
	   (types-equal! t1 t2 (void) "<void>")
	   (k t1)))))

    (lambda (aliases t)
      ;; We need to go down inside the PARAMETERS to the alias and realias them as well...
      (define (maybebind origty alias fail)
	(let-values ([(v a* rhs)
(if (= 2 (length alias)) 
    (values (car alias) '() (cadr alias))
    (apply values alias))
#;
;; [2010.01.12] Odd problem compiling under chez 7.9.4:
		      (match alias
			[(,v ,rhs)           (values v '() rhs)]
			[(,v (,a* ...) ,rhs) (values v a* rhs)])])
	  
	  ;(printf "Trying: ~s ~s\n" origty rhs)
	  ;; We don't want to inject any ADDITIONAL constraints into the original type.
	  ;; So first we make sure that our alias matches even if
	  ;; the polymorphism is stripped from the original type.

	  ;; [2008.11.14] Note: In the past there was no need to
	  ;; instantiate the result of type-replace-polymorphic (all
	  ;; variables are gone).  Yet now there's a convention with
	  ;; Records that must be respected:
	  (if (types-equal!? (instantiate-type (type-replace-polymorphic origty (gensym "DummyType") 'Int '#()))
			     (instantiate-type rhs))
	      ;; If that succeeds we match them again without the hack:
	      (match (instantiate-type `(Magic #(,@a*) ,rhs) '() #t)
		;; We bundle together the LHS* and RHS here so that their mutable cells are shared.
		[(Magic #(,cells ...) ,newrhs)
		 ;(printf "It worked, now pulling out a unification for the alias's params: ~a \n" newrhs)
		 ;; Because origty is really a type FRAGMENT, we don't want to rename its type vars here:
		 ;;(ASSERT instantiated-type? origty)
		 (let ([res (types-equal!? newrhs
					   (instantiate-type origty '() #f))])
		   (if res 
		       ;; We feed it back through, possibly further reduce "cells":
		       ;;(try-realias )
		       (begin ;(printf "\nMade types equal: ~s ~s\n" res rhs)(inspect cells)
			      (if (null? cells) v
				  (export-type `(,v ,@cells))))
		       (fail #f)))
		 ])
	      (fail #f))
	  ))
      (define (try-realias t)
	(let realias-loop ([ls aliases])
	  (cond	 
	   [(null? ls) #f] ;; No aliases apply, use original type.
	   [(call/ec (lambda (k) (maybebind t (car ls) k)))
	    => (lambda (newty)  newty)]
	   [else (realias-loop (cdr ls))])))

;      (or (try-realias t) t)

      ;; Walk over the type structure top-to-bottom.  Check at each
      ;; node whether we can re-alias:
      (let l ([ty t])
	(match ty ;; No recursion
	  [,x (guard (or (symbol? x) (string? x)
			 ;; Type variables can't be aliases by themselves.
			 (and (pair? x)
			      (tvar-quotation? (car x));(memq (car x) '(NUM quote))
			      )))
	      x]
	  [#(,t* ...) (or (try-realias ty) (list->vector (map l t*)))]

	  ;; Do records need any special treatment?

	  [(,arg* ... -> ,res) (or (try-realias ty) `(,@(map l arg*) -> ,(l res)))]
	  [(,s ,t* ...) (guard (symbol? s))
	   (or (try-realias ty) `(,s . ,(map l t*)))]
	  [,other (error 'realise-type "bad type: ~s" other)]))      
      )))


;[(realias-type '((Foo Int)) 'Int) Foo]
;[(realias-type '((Foo #(Int Int))) '#(Int Int)) #(Int Int)]


; ======================================================================
;;; Unit tests.

;; [2008.02] Recently changed this policy:
;(define int-constant-type-as 'Int)
(define int-constant-type-as '(NUM unspecified))


;; These use quite a bit of stuff from type_environments.ss as well:
(define-testing test-inferencer  
  (default-unit-tester "Hindley Milner Type Inferencer" 
    `(

    [(',type-expression '(if #t 1. 2.) (empty-tenv))         Float]
    [(let-values ([(p t ) (annotate-program '(lambda (x) (g+ x (gint 3))))]) t)
     (,int-constant-type-as -> ,int-constant-type-as)]

    [(export-type ''(f . Int)) Int]
    [(export-type (',type-expression '(_+_ 1 1) (empty-tenv))) Int]
    [(export-type (',type-expression '(cons 3 (cons 4 '())) (empty-tenv))) (List ,int-constant-type-as)] 
    [(export-type (',type-expression '(cons 1 '(2 3 4)) (empty-tenv)))     (List ,int-constant-type-as)]
    [(export-type (',type-expression '(cons 1 '(2 3 4.)) (empty-tenv))) #|error|# (List Float)]
    [(export-type (type-expression '(cons (assert-type Int '1) '(2 3 4.)) (empty-tenv))) error]

    [(export-type (let-values ([(_ t) (',annotate-lambda '(v) '(_+_ v v) '(Int) (empty-tenv) '())]) t))
     (Int -> Int)]

    [(export-type (let-values ([(_ t) (',annotate-lambda '(v) '(_+_ v v) '('alpha) (empty-tenv) '())]) t))
     ;(,int-constant-type-as -> ,int-constant-type-as)
     (Int -> Int)]

    ;; This one doesn't actually verify shared structure:
    ["instantiate-type"
     ,(let ([ob5 ''(a . #f)]  
	    [ob6 ''(b . #f)])      
	`(instantiate-type '((,ob5 -> ,ob6) (Area ,ob5) -> (Area ,ob6))))
     ,(let ([ob7 ''(unspecified . #f)]
	    [ob8 ''(unspecified . #f)])
	`((,ob7 -> ,ob8) (Area ,ob7) -> (Area ,ob8)))]

    ["instantiate-type: Make sure we don't copy internal nongeneric vars."
     (instantiate-type ''(at '(av . #f) -> '(av . #f)) '(av))
     '(unspecified '(av . #f) -> '(av . #f))]

    ["types-equal!: make sure that (by convention) the first argument is mutated"
     (let ([x ''(a . #f)] [y ''(b . #f)]) (types-equal! x y (empty-tenv) "") x)
     '(a . '(b . #f))]

    ["Invalid explicitly annotated type"
     (export-type (let-values ([(_ t) (',annotate-lambda '(v) '(_+_ v v) '(String) (empty-tenv) '())]) t))
     error]

    ["Explicitly typed let narrowing identity function's signature"
     (let-values ([(_ t) (annotate-program '(let ([f (Int -> Int) (lambda (x) x)]) f))]) t)
     (Int -> Int)]
    [(let-values ([(_ t) (annotate-program '(let ([f (Int -> Int) (lambda (x) x)]) (app f "h")))]) t)
     error]

    [(export-type (let-values ([(_ t) (',annotate-lambda '(v) 'v '('alpha) (empty-tenv) '())]) t))
     ,(lambda (x)
	(match x
	  [(',a -> ',b) (eq? a b)]
	  [,else #f]))]

    [(export-type (',type-expression '((lambda (v) v) 3) (empty-tenv))) ,int-constant-type-as]

    [(export-type (',type-expression '(lambda (y) (letrec ([x y]) (_+_ x 4))) (empty-tenv)))
     (Int -> Int)]

    [(export-type (',type-expression '(rmap (lambda (n) (sense "light" n)) world) (empty-tenv)))
     (Area Int)]
    
    [(export-type (',type-expression '(tuple (assert-type Int '1) 2.0 3) (empty-tenv)))
     #(Int Float ,int-constant-type-as)]
    [(export-type (',type-expression '(lambda (x) (tupref 0 3 x)) (empty-tenv)))
     ,(lambda (x)
	(match x
	  [(#(,v1 ,_ ,__) -> ,v2) (equal? v1 v2)]
	  [,else #f]))]

    [(export-type (',type-expression 

		   '(letrec ([f (lambda (x) x)])
		      (tuple (app f 3) "foo" f))
		  (empty-tenv)))
     #(,int-constant-type-as String ('unspecified -> 'unspecified))
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
     (export-type (',type-expression '(lambda (f) (tuple (app f 3) f)) (empty-tenv)))
     ,(lambda (x) 
	(match x
	  [((,num1 -> ,v1) -> #(,v2 (,num2 -> ,v3)))
	   (guard (equal? v1 v2) (equal? v2 v3))
	   #t]
	  [,else #f]))]
  ["Non polymorphic funs cannot be applied differently."
   (export-type (',type-expression '(lambda (f) (tuple (app f 3) (app f "foo") f)) (empty-tenv)))
   error]
  
  [(export-type (',type-expression 
		 '(letrec ()
		    (smap2
		       (lambda (n1 n2) (tuple n1 n2))
		       (anchor-at 50 10)
		       (anchor-at 30 40))) (empty-tenv)))
   (Stream #(Node Node))]


  ["A letrec-bound identity function never applied"
   (let-values ([(p t) (annotate-program '(letrec ([f (lambda (x) x)]) 3))]) p)
   ,(lambda (x)
      (match x
	[(letrec ([f (,v1 -> ,v2) (lambda (x) (,v3) x)]) 3)
	 (guard (equal? v1 v2) (equal? v2 v3)) #t]
	[,else #f]))]

  ["A free non-generic variable in a lambda abstraction"
   (let-values ([(p t) (annotate-program '(lambda (x)
				       (letrec ([f (lambda (y) x)])
					 f)))])
     t)
   ,(lambda (x)
      (match x 
	[(',v -> (',_ -> ',v2))
	 (eq? v v2)]
	[,_ #f]))]
  
  ;; Now let's see about partially annotated programs.
  ["Partially (erroneously) annotated letrec"
   (let-values ([(p t) (annotate-program '(letrec ([i String 3]) (_+_ i 59)))]) p)
   error]

  ["An example function from nested_regions_folded.ss"
   (let-values ([(p t) (annotate-program '(letrec ([sumhood (lambda (reg) 
					  (letrec ([thevals (rmap (lambda (n) (cons (nodeid n) '()))
								  reg)])
					    (rfold append '() thevals)))])
				       sumhood))])
     t)
   ((Area Node) -> (Stream (List Int)))]

  ["Here's the captured bug, letrec problem."
   (let-values ([(p t) (annotate-program '(lambda (n_7)  (Node)
					     (letrec ([resultofthevals_1 (List 'aay) 
									 (cons tmpbasic_12 '())]
						      [tmpbasic_12 Int (nodeid n_7)])
					       resultofthevals_1)))])
     t)
   (Node -> (List Int))]

  ["This compiled version of the same fun gets too general a type"
   (let-values ([(p t) (annotate-program ' (lambda (reg_5)
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
   (let-values ([(p t) (annotate-program '(let ([f ((NUM a) -> (NUM a)) (lambda (x) x)])
				       (tuple (app f 3) (app f 4.0) f)))])
     t)
   #(,int-constant-type-as Float ((NUM unspecified) -> (NUM unspecified)))]

  ["Now let's test something outside the bounds of the NUM subkind" 
   (let-values ([(p t) (annotate-program '(let ([f ((NUM a) -> (NUM a)) (lambda (x) x)])
				       (tuple (app f 3) (app f 4.0) (app f "et") f)))])
     t)
   error]


  ;; TODO: FIXME: THIS REPRESENTS A BUG:
  ["LUB: Here we should infer the less general type: plain Int:"
   (',deep-member? 'NUM
    (parameterize ([inferencer-enable-LUB #t])
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
		    (Stream Int))))))
   #f]

  ["This is an ok use of polymorphism"
   (export-type (',type-expression 
		 '(lambda (g)
		    (letrec ([f (lambda (x) x)])
		      (tuple (app f 3) (app f #t))))
		 (empty-tenv)))
   unspecified]
  ["This should not be allowed by the type system:" 
   (export-type (',type-expression 
		 '(lambda (g)
		    (letrec ([f g])
		      (tuple (app f 3) (app f #t))))
		 (empty-tenv)))
   error]

  ["Nor should this (same thing with let)" 
   (export-type (',type-expression 
		 '(lambda (g)
		    (let ([f g])
		      (tuple (app f 3) (app f #t))))
		 (empty-tenv)))
   error]


  ["This is an identity function with LUB type Num a -> Num a"
   (call-with-values
    (lambda () (parameterize ([',inferencer-enable-LUB #t])
      (annotate-program
       '(let ([f 'a (lambda (x) x)]) (tuple (app f '3) (app f '4.5))))))
    list)
   ,(lambda (x)
      (match x
       	[((let ([f ((NUM ,v1) -> (NUM ,v2)) (lambda (x) (,unspecified) x)])
	    (tuple (app f '3) (app f '4.5)))
	  #(,int-constant-type-as Float))
	 (eq? v1 v2)]
	[,else #f]))]

  ["Type a complex constant"
   (type-const (vector [make-tuple (list '#() 0)]))
   (Array #((Array unspecified) ,int-constant-type-as))]

  ["Verify the shared structure of a type"
   (begin (',reset-tvar-generator) (let ((x (prim->type 'car))) (set-cdr! (car (cdaddr x)) 99) x))
   ((List '(a . 99)) -> '(a . 99))]

  ["Valid use of record extension in if branches"
   (export-type (type-expression '(lambda (r) (if '#t (wsrecord-extend 'X '2 r) (wsrecord-extend 'X '3 r))) (empty-tenv)))
   ((Record 'unspecified) -> (Record (Row X (NUM unspecified) 'unspecified)))]

  ["INVALID use of record extension in if branches"
   (type-expression '(lambda (r) (if '#t (wsrecord-extend 'X '2 r) (wsrecord-extend 'Y '3 r))) (empty-tenv))
   error]
 
  #;
  ;; Should we type-check with patterns in there?
  [(let-values ([(p t) (annotate-program '(lambda (#(_ t)) (> t 90)))]) t)
   ??
   ]

  )))

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
   (wavescript-primitives)))


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


