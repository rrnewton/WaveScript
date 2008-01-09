

;;;; This defines a handful of small passes used by WaveScript that
;;;; don't quite each deserve their own files.


(module small-ws-passes mzscheme
  (require "../../plt/common.ss"
	   "../../plt/hashtab.ss"	   
	   "normalize_query/ws-remove-complex-opera.ss"
	   "optimizations/rewrite_opts.ss"
	   "../compiler_components/type_environments.ss"
	   )
  (provide 
           introduce-lazy-letrec
	   lift-polymorphic-constant
	   unlift-polymorphic-constant
	   strip-irrelevant-polymorphism
	   lift-immutable-constants
	   strip-src-pos
	   ;purify-letrec  ;; Disabled
	   standardize-iterate
	   kill-polymorphic-types
	   ;ws-add-return-statements  ;; Disabled
	   resolve-type-aliases
	   generate-comparison-code
	   generate-printing-code
	   optimize-print-and-show
	   explicit-toplevel-print
	   strip-unnecessary-ascription

	   hide-special-libfuns
	   reveal-special-libfuns

           ; --mic
	   propagate-copies
           )
  (chezimports)
  (require-for-syntax "../../plt/common.ss")


;; [2007.10.21]

;; NOTE: This currently depends on exactly which binding form the
;; parser and the previous desugaring passes decide to output.  For
;; now it just looks for left-left-lambda.
;;
;; We could try to use binding-form-visit-knowncode... but
;; unfortunately it doesn't let us remove the binding right now.
;;
;; FIXME: WE ALSO NEED TO EXTRACT ANY DEPENDENCIES (FREE VARS), NOT
;; JUST THE PARTICULAR BINDING FOR A SPECIAL LIBFUN.
(define-pass hide-special-libfuns
  ;; This accumulates the special-defs that we come across below.
  (define special-defs ())
  [Expr (lambda (xp fallthr)
	  (match xp
	    ;; This is lame... but we repeat eta-prims here:
	    #;
	    [,vr (guard (symbol? vr) (regiment-primitive? vr) (not (memq vr regiment-constants)))
		 `(lambda )]

	    [(app (lambda (,lhs) (,ty) ,[bod]) ,[rhs])
	     (if (memq lhs special-rewrite-libfuns)
		 (begin 
		   (set! special-defs (cons (list lhs ty rhs) special-defs))
		   bod)
		 `(app (lambda (,lhs) (,ty) ,bod) ,rhs))]

#;
	    [(,lett ([,lhs* ,ty* ,[rhs*]] ...) ,[bod])
	     (guard (memq lett '(let let* letrec)))
	     (printf " hmm ~s\n" lhs*)
	     (if ;(not (null? (intersection lhs* special-rewrite-libfuns)))
 	        (memq 'rewindow lhs*)
		;(inspect lhs*)
		 `(,lett ,(map list lhs* ty* rhs*) ,bod))]
	    [,oth (fallthr oth)])
	  )]
;; Here's the pure way to do it:
;  [Fuser (lambda (ls k) (vector (apply append (map (vecref 0) ls)) (apply k (map (vecref 1) ls))))]

  [Program (lambda (pr Expr)
	     (match pr
	       [(,lang '(program ,[Expr -> bod] ,meta* ... ,topty))
		;(inspect (vector 'gotspecial (map car special-defs)))
		(printf "  HIDING: ~s\n" (map car special-defs))
		`(,lang '(program ,bod (special-libfuns ,@special-defs) ,meta* ... ,topty))
		]))]
  
  )

;; [2007.10.22]
(define-pass reveal-special-libfuns
    [Expr (lambda (xp fallthr)
	    (match xp
	      [(,special ,[x*] ...)  (guard (memq special special-rewrite-libfuns))
	       (printf "  REVEALING: ~s\n" special)
	       `(app ,special ,@x*)]
	      [,oth (fallthr oth)]))]
    [Program (lambda (pr Expr)
	       (match pr
		 [(,lang '(program ,[Expr -> bod] ,meta* ... ,topty))
		  (let* ([special (ASSERT (assq 'special-libfuns meta*))]
			 [newmeta (remq special meta*)])
		    ;; This assumes that none of the special defs depend on one another:
		    `(,lang '(program (letrec ,(cdr special) ,bod) ,@newmeta ,topty)))]))])


;; [2007.10.09] 
(define-pass strip-unnecessary-ascription
  (define required-ops '(readFile foreign foreign_source))
  [Expr (lambda (e fallthru)
	  (match e
	    ;; FIXME: HACK: [2007.10.10] Just prune out the polymorphic asserts around constants.
	    [(assert-type ,ty (quote ,[x])) 
	     (guard (not (polymorphic-type? ty)))
	     `(assert-type ,ty ',x)]
       [(assert-type ,t (,op ,annot ,[x*] ...))
        (guard (and (memq op required-ops)
                    (pair? annot)
                    (eq? 'annotations (car annot))))
        `(assert-type ,t (,op ,annot ,@x*))]
	    [(assert-type ,t (,op ,[x*] ...)) 
	     (guard (memq op required-ops))
	     `(assert-type ,t ,(cons op x*))]
	    [(assert-type ,t ,[e]) e]
	    [,oth (fallthru oth)]))])

;; [2007.09.06] little helper pass used in ws.early:
(define-pass strip-src-pos
  [Expr (lambda (e fallthru)
	  (match e
	    [(src-pos ,_ ,[e]) e]
	    [,oth (fallthru oth)]))])


;; [2007.08.02] This kind of thing should not be done in the actual
;; code generators if it can be helped.
;;
;; [2007.12.22] But, as with the printing code, it would be nice to
;; generate function definitions and leave it up to the inliner as to
;; whether its worth inlining.
(define-pass generate-comparison-code
  (define (build-comparison origtype e1 e2)
    (match origtype ;; NO MATCH RECURSION!.
      [(Array ,elt) 
       (let ([arr1 (unique-name "arr1")]
	     [arr2 (unique-name "arr2")]
	     [el1  (unique-name "arrel1")]
	     [el2  (unique-name "arrel2")]
	     [i    (unique-name "i")]
	     [stop (unique-name "stop")]
	     [len  (unique-name "len")])
	 `(let ([,arr1 (Array ,elt) ,e1])
	    (let ([,arr2 (Array ,elt) ,e2])
	      (if ,(build-comparison 'Int `(Array:length ,arr1) `(Array:length ,arr2))
		  (let ([,i (Ref Int) (Mutable:ref '0)])
		    (let ([,stop (Ref Bool) (Mutable:ref '#f)])
		      (let ([,len Int (Array:length ,arr1)])
			(begin
			  (while (if (< (deref ,i) ,len) (not (deref ,stop)) '#f)
				 (let ([,el1 ,elt (Array:ref ,arr1 (deref ,i))])
				   (let ([,el2 ,elt (Array:ref ,arr2 (deref ,i))])
				     (if ,(build-comparison elt el1 el2)
					 (set! ,i (_+_ (deref ,i) '1))
					 (set! ,stop '#t)
					 ))))
			  (not (deref ,stop))))))
		  '#f)
	      )))]

      ;; [2008.01.07] Only for wsc2 at the moment:
      [(List ,elt) (guard (eq? (compiler-invocation-mode) 'wavescript-compiler-c))
       (let ([ptr1 (unique-name "lsptr1")]
	     [ptr2 (unique-name "lsptr2")]
	     [el1  (unique-name "lsel1")]
	     [el2  (unique-name "lsel2")]
	     [stop (unique-name "stop")]
	     [result (unique-name "result")])
	 `(let ([,ptr1 (Ref (List ,elt)) (Mutable:ref ,e1)])
	    (let ([,ptr2 (Ref (List ,elt)) (Mutable:ref ,e2)])
	      (let ([,stop (Ref Bool) (Mutable:ref '#f)])
		(let ([,result (Ref Bool) (Mutable:ref '#f)])
		  (begin
		    (while (not (deref ,stop))
			   (if (List:is_null (deref ,ptr1))			     
			       (if (List:is_null (deref ,ptr2))
				   (set! ,result '#t) ;; Equal
				   (set! ,stop '#t))
			       (if (List:is_null (deref ,ptr2))
				   (set! ,stop '#t)
				   (let ([,el1 ,elt (car (deref ,ptr1))])
				     (let ([,el2 ,elt (car (deref ,ptr2))])
				       (if ,(build-comparison elt el1 el2)
					   (begin 
					     (set! ,ptr1 (cdr (deref ,ptr1)))
					     (set! ,ptr2 (cdr (deref ,ptr2))))
					   (set! ,stop '#t)
					   ))))))
		    (deref ,result))
		  )))))]

      ;; [2007.11.01] Doing tuples here.
      ;; (This actually makes for worse-code for Scheme... could do this conditionally:)
      ;; TODO FIXME: Remove the relevant code from the different backends.      
      [#(,ty0 ,ty* ...)
       (define len (fx+ 1 (length ty0)))
       (define tmp1 '(unique-name "tmptupa"))
       (define tmp2 '(unique-name "tmptupb"))
       `(let ([,tmp1 ,origtype ,e1])
	  (let ([,tmp2 ,origtype ,e2])	    
	    ,(let loop ([types (cons ty0 ty*)]
			[ind 0])
	      (define head (build-comparison (car types) `(tupref ,ind ,len ,tmp1) `(tupref ,ind ,len ,tmp2)))
	      (if (null? (cdr types))
		  head
		  `(if ,head
		       (loop (cdr types) (fx+ 1 ind))
		       '#f)))))]

      ;; For the simple case we just allow the wsequal? to stick around.
      [,_ `(wsequal? (assert-type ,origtype ,e1) ,e2)]))
  [Expr 
   (lambda (x fallthru)
     (match x
       [(wsequal? (assert-type ,ty ,[e1]) ,[e2])
	;(ASSERT simple-expr? e1)
	;(ASSERT simple-expr? e2)
	(build-comparison ty e1 e2)	
	#;
	(maybe-let e1 ty
	  (lambda (e1)
	    (maybe-let )
	    ))
	]
       [(wsequal? . ,_) 
	(error 'generate-comparison-code 
	       "wsequal? was missing type annotation: ~s"
	       `(wsequal? . ,_))]
       [,oth (fallthru oth)]
       ))])


;; [2007.12.18] I had talked about doing this but not got around to
;; it.  Here we distribute print's over string-appends, and eliminate
;; show applied to a string, and print applied to show.
(define-pass optimize-print-and-show
    (define (Expr xp fall)
	    (match xp
	      [(print ,shw)	       
	       (match (peel-annotations shw)
		 [(show ,x) (Expr `(print ,x) fall)]
		 [(string-append ,a ,b) 
		  (Expr `(begin (print (assert-type String ,a))
				(print (assert-type String ,b))) fall)]
		 [,_ `(print ,(Expr shw fall))])]
	      [(show (assert-type String ,strE)) `(assert-type String ,strE)]
	      [,oth (fall oth)]))
  [Expr Expr])

;; [2007.10.28] Factoring this out of emit-c.
;; It builds code to print a particular type of value.
;; It should work across backends.
;; Ideally, this should leave print accepting only strings or scalars.
;;
;; Also, ideally, this should coallesce printing functions for a given
;; type and leave them abstracted as a function.  Otherwise it's just
;; too much code bloat.  Especilly when it comes to sum types.  This
;; should make one traversal to gather the types that are printed and
;; replace calls to "print" with "print_type".  Then, at top-level,
;; definitions for all the necessary "print_type" functions are
;; injected.
(define-pass generate-printing-code
  ;(define ) ;; Mutated below, but scoped out here.

  (define (build-print which ty expr addstr!)
    (define (recur ty expr)
      (or (build-print which ty expr addstr!)
	  (match which
	    [print `(,which (assert-type ,ty ,expr))]
	    [show   (addstr! `(,which (assert-type ,ty ,expr)))])))
    (match ty ;; <- No match recursion
      [(List ,elt)
       (let* ([ptr (unique-name 'ptr)])
	 `(let ([,ptr (Ref (List ,elt)) (Mutable:ref ,expr)])
	    (begin 
	      ,(addstr! ''"[")	      
	      (while (not (wsequal? (deref ,ptr) (assert-type (List ,elt) '())))
		     (begin
		       ,(recur elt `(car (deref ,ptr)))
		       (if (wsequal? (cdr (deref ,ptr))  (assert-type (List ,elt) '()))
			   (tuple)
			   ,(addstr! ''", "))
		       (set! ,ptr (cdr (deref ,ptr)))))
	      ,(addstr! ''"]"))))]


      ;; TEMP FIXME: [2007.12.22] For now only for the new C backend.
      [(Array ,elt) (guard (eq? (compiler-invocation-mode) 'wavescript-compiler-c))
       (let* ([arr (unique-name "arr")]
	      [ind (unique-name "ind")])
	 `(let ([,arr (Array ,elt) ,expr])
	    (begin 
	      ,(addstr! ''"#[")
	      (for (,ind '0 (_-_ (Array:length ,arr) '1))
		  (begin 
		    ,(recur elt `(Array:ref ,arr ,ind))
		    (if (not (wsequal? ,ind (_-_ (Array:length ,arr) '1)))
			,(addstr! ''", ")
			(tuple))))
	      ,(addstr! ''"]"))))]

      [#(,fld* ...)
       (let ([tup (unique-name "tup")]
	     [len (length fld*)])
	 `(let ([,tup ,ty ,expr])
	    (begin
	      ,(addstr! ''"(")
	      ,@(insert-between (addstr! ''", ")
		 (mapi (lambda (i fldty)
		       (recur fldty `(tupref ,i ,len ,tup)))
		  fld*))
	      ,(addstr! ''")"))))]

      [String #f] ;; No change
      
      [(Sum ,ty) #f] ;; TODO: Need to implement sums.

      ;; Scalars we let through.
      ;[,ty (guard (scalar-type? ,ty)) `(,which (assert-type ,ty ,expr))]
      [,ty (guard (scalar-type? ty)) #f]
#;      
      [Int (if (eq? which 'print)
	       `(print (assert-type ty (intToString ,expr)))
	       `(assert-type ty (intToString ,expr)))]      
      [,oth #f]
      ;[,oth (error 'generate-printing-code "Unhandled type: ~s" oth)]
      ))
  [Expr 
   (lambda (x fallthru)
     (match x
       ;; TODO: optimize print(show(...)) and print(string-append(...))

       [(print (assert-type ,ty ,[exp]))
	(or (build-print 'print ty exp (lambda (x) `(print ,x)))
	    `(print (assert-type ,ty ,exp)))]
       [(print . ,_) 
	(error 'generate-printing-code "print was missing type annotation: ~s" `(print . ,_))]
       ;; This is nastier... currently quadradic append behavior.
       ;; The solution, of course, is to expose more (mutable string
       ;; buffers) or, alternatively, to form a list of strings and then append once
       ;;
       ;; [2007.12.18] One simple way to accomplish this is to just expose a destructive string-append!...
       ;; When the first string-append! happens, that tells it to allocate some extra space, 
       ;; because it will happen again.  This is a hack that doesn't require a new type.  But it 
       ;; does require a representation of strings that store the length of the allocation separate 
       ;; from the null terminator (our embedding into arrays enables that).
       [(show (assert-type ,ty ,[exp]))
	(let ([acc (unique-name 'acc)])
	  (let ([printer (build-print 'show ty exp (lambda (x) `(set! ,acc (string-append (deref ,acc) ,x))))])
	    (if printer	       
		`(let ([,acc (Ref String) (Mutable:ref '"")]) (begin ,printer (deref ,acc)))
		`(show (assert-type ,ty ,exp)))))]
       [(show . ,_)(error 'generate-printing-code "show was missing type annotation: ~s" `(print . ,_))]
       [,oth (fallthru oth)]
       ))])


;; Make the toplevel print exlplicit.  Return only unit.
(define-pass explicit-toplevel-print
  [Program 
   (lambda (pr Expr)
     (match pr
       ;; [2008.01.07] Making this work before program "flattening" into one big list of bindings.
       [(,lang '(program ,bod ,meta* ... (Stream ,topty)))
	(let ([x  (unique-name "x")]
	      [vq (unique-name "___VIRTQUEUE___")])
	  `(,lang '(program (iterate 
			     (annotations (name printerbox))
			     (let ()
			       (lambda (,x ,vq)
				 (,topty (VQueue #()))
				 (begin (print (assert-type ,topty ,x))
					(print (assert-type String '"\n"))
					(emit ,vq (tuple))
					,vq)))
			     ,bod)
		     ,@meta* (Stream #()))))]

#;
       [(,lang '(program (letrec ,binds ,bod) ,meta* ... (Stream ,topty)))
	(ASSERT symbol? bod)
	(let ([finalstrm (unique-name "finalstrm")]
	      [x  (unique-name "x")]
	      [vq (unique-name "___VIRTQUEUE___")])
	  `(,lang '(program 
		       (letrec (,@binds 
				 [,finalstrm (Stream #())
				      (iterate 
				       (annotations (name printerbox))
				       (let ()
					 (lambda (,x ,vq)
					   (,topty (VQueue #()))
					   (begin (print (assert-type ,topty ,x))
						  (print (assert-type String '"\n"))
						  (emit ,vq (tuple))
						  ,vq)))
					      ,bod)])
			  ,finalstrm)
		     ,@meta* (Stream #()))))]))])

;(generate-printing-code '(lang '(program (print (assert-type (List Int) '[2])) #())))

;; [2007.05.01] This pulls complex constants up to the top of the program.
;(define-pass lift-complex-constants)

;; This is superficial.
;; This cuts out all the meta data but the union-types, which is all we need towards the end.
#;
(define (prune-meta-data input-gram)
  (let ()
    (define gram 
      (cons '(Program ((quote program) Query ('union-types ((Var Type ...) [Var Type ...] ...) ...) Type))	    
	    (remq (assq 'Program input-gram))))
    (define-pass prune-meta-data	
      [OutputGrammar gram])
    prune-meta-data))

;; Simply transforms letrec into lazy-letrec.
(define-pass introduce-lazy-letrec
    [Expr (lambda (x fallthru)
	    (match x
	      [(free ,_ ,[e]) e]
	      [,other 
	       (match (fallthru other)
		 [(letrec ,rest ...) `(lazy-letrec ,rest ...)]
		 [,other other]) ]))])

(define-pass lift-immutable-constants
    (define acc '()) ;; Accumulates constant bindings.
    [Expr 
     (lambda (xp fallthru)
       (match xp
	 [',const (guard (not (simple-constant? const))			 
			 ;; Arrays are mutable:
			 (not (vector? const)))
	  (let ([tmp (unique-name "tmpconstlift")])
	    (set! acc (cons `(,tmp ,(type-const const) ',const) acc))
	    tmp)]
	 [,oth (fallthru oth)]))]
    [Program (lambda (prog Expr)
	       (fluid-let ([acc '()])
		 (match prog 
		   [(,lang '(program ,[Expr -> bod] ,meta* ...))
		   `(,lang '(program `(let ,acc ,bod)) ,@meta*)])))])

;; [2007.03.17] Including Array:makeUNSAFE here even though it's not "constant"
;; [2007.10.11] Adjusting this to apply to data constructors/destructors as well.
(define-pass lift-polymorphic-constant
    [Expr (lambda (x fallthru)
	    (define (f x) 
	      (let ([tmp (unique-name 'tmp)]
		    [t   (unique-name 'alpha)])
		`(let ([,tmp (quote ,t) ,x]) ,tmp)))
	    (match x
	      [nullseg (f x)]
	      [Array:null (f x)]
	      ['()  (f x)]
	      [(Array:makeUNSAFE ,[n]) (f `(Array:makeUNSAFE ,n))]
	      [(construct-data ,name ,[x*] ...) (f `(construct-data ,name ,@x*))]
	      [(wscase ,[x] (,tag* ,[fun*]) ...) 
	       `(wscase ,(f x) ,@(map list tag* fun*))]
	      ;; Don't touch these:
	      [(foreign        ,x ,y) `(foreign        ,x ,y)]
	      [(foreign_source ,x ,y) `(foreign_source ,x ,y)]

	      [,other (fallthru other)]))])

;; [2007.10.11] Adjusting this to apply to data constructors/destructors as well.
(define-pass unlift-polymorphic-constant
    (define (pconst? x) 
      (match x
	[nullseg #t]
	[Array:null #t]
	['()     #t]
	[()     #t]
	[(Array:makeUNSAFE ,n) #t]
	[(construct-data ,name . ,_) #t]
	[,else   #f]))
  [Expr (lambda (x fallthru)
	  (match x
	    ;; Don't touch these:
	    [(foreign        ,x ,y) `(foreign        ,x ,y)]
	    [(foreign_source ,x ,y) `(foreign_source ,x ,y)]
	   
	    [(let ([,v1 ,t ,c]) ,v2)
	       (guard (eq? v1 v2) (pconst? c))
;; [2007.07.08] Removing this assert because we clean up below:
;	       (ASSERT (lambda (t) (not (polymorphic-type? t))) t)
	       `(assert-type ,t ,c)]
	   
	    [(wscase (let ([,v1 ,t ,[x]]) ,v2) (,tag* ,[fun*]) ...)
	     (guard (eq? v1 v2))
	     `(wscase (assert-type ,t ,x) ,@(map list tag* fun*))]

	    [,c (guard (pconst? c))
		(error 'unlift-polymorphic-constant "missed polymorphic const: ~s" c)]

	    ;; Don't touch these:
	    [(foreign ,x ,y) `(foreign ,x ,y)]
	    [,other (fallthru other)]))])


;; [2007.07.08]
;; Remaining polymorphism at this phase of the compiler is
;; "irrelevent" in the sense that it describes only uninspected values.
;; Thus it is equivalent to insert unit in all such places.
(define-pass strip-irrelevant-polymorphism
    (define dummy-type #()) ;; Type to insert.    
    (define (data-source? e)
      (let ([expr (peel-annotations e)])
	(and (pair? expr) (memq (car expr) '(readFile dataFile)))))

    (define (Type t) (type-replace-polymorphic t dummy-type))
    (define Expr
      (lambda (x fallthru)
	  (match x

	    ;; Strip an assert-type unless it's around a data source.
	    ;; HACK: quoted numbers also need their type annotations.
	    #;
	    [(assert-type ,t ,e) (guard (or (data-source? e) (quoted-num? e)))
	     `(assert-type ,t ,(Expr e fallthru))]

	    #;
	    ;; New hack: we keep any annotations that have NO
	    ;; polymorphism.  These will not be a problem.
	    [(assert-type ,t ,[e]) (guard (not (polymorphic-type? t)))
	     `(assert-type ,t ,e)]

	    ;; Otherwise we remove the type assertion entirely.
	    #;
	    [(assert-type ,_ ,[e])
	     ;(printf "GOT ASSERT: ~s\n" ty)
	     ;`(assert-type ,ty ,e)
	     e
	     ]

	    ;; [2007.10.10] NOW we strip polymorphism even from the ascription:
	    [(assert-type ,[Type -> ty] ,[e])
	     `(assert-type ,ty ,e)]

	    [,oth (fallthru oth)])))
  [Expr Expr]
  [Bindings 
   (lambda (vars types exprs reconstr exprfun)
     (reconstr vars (map Type types) (map exprfun exprs)))])


;; Purify-letrec: makes sure letrec's only bind functions.
#;
(define-pass purify-letrec
    [Expr (lambda (x fallthru)
	    (match x
	      [(letrec ([,v* ,ty* ,[e*]] ...) ,[bod])	       
	       (cond
		[(lambda? e) ]
		[(no-lambda? e) ]
		
		)]

	      [(free ,_ ,[e]) e]
	      [,other 
	       (match (fallthru other)
		 [(letrec ,rest ...) `(lazy-letrec ,rest ...)]
		 [,other other]) ]))])

;; This little pass handles only iterate cases.
(define-pass standardize-iterate
    (define process-expr
      (lambda (x fallthru)
	(match x
	  [(iterate ,annot (let ,binds (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,[strm])
	   `(iterate ,annot (let ,binds (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,strm)]

	  ;; [2007.03.25] This is a bit hackish... but at this
	  ;; late point in the game the program's already been
	  ;; typechecked several times, so it should be ok to
	  ;; throw away this ascription:
	  [(iterate ,annot (assert-type ,t ,lam) ,src)
	   (process-expr `(iterate ,annot ,lam ,src) fallthru)]
	  
	  ;; OPTIMIZATION:
	  ;; This doesn't recursively process the inside of iterates.
	  ;; That's because we can't find iterates within iterates.
	  ;; This does preclude using fuse-passes on this pass.
	  [(iterate ,annot (lambda (,x ,y) (,tyx ,tyy) ,bod) ,[strm])
	   `(iterate ,annot (let () (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,strm)]
	  [(iterate ,_ ...)
	   (error 'standardize-iterate "shouldn't have missed this iterate: ~s" `(iterate ,_ ...))]
	  [,oth (fallthru oth)])
	))
    [Expr process-expr]
    ;[Props 'incomplete-ast-coverage]
    )

(define-pass kill-polymorphic-types
    (define (Type t)
      (match t
	;; Here we turn any remaining type vars into unit:
	[',n  '#()]
	;; Any remaining numeric type vars become Int:
	[(NUM ,v) (guard (symbol? v)) 'Int]

	[,s    (guard (symbol? s))           s]
	[(,[arg*] ... -> ,[res])           `(,arg* ... -> ,res)]
	[(,s ,[t] ...) (guard (symbol? s)) `(,s ,t ...)]
	[#(,[t*] ...)                       (apply vector t*)]
	[,other (error 'kill-polymorphic-types "bad type: ~s" other)]))
  #;
  [Expr (lambda (x fallthru)
	  (match x 
	    [(assert-type ,[Type -> t] ,[e])
	     `(assert-type ,t ,e)]
	    [,oth (fallthru oth)]))]
  [Bindings (lambda (var* ty* expr* reconstr Expr)
	      (reconstr var* (map Type ty*) (map Expr expr*)))])


;; UNUSED
(define-pass ws-add-return-statements
    (define (doit fallthru)
      (lambda (x)	
	(match x 
	  [,x (guard (simple-expr? x)) `(return ,x)]
	  ;[(assert-type ,t ,e)	   ]
	  [(if ,a ,[b] ,[c])      `(if ,a ,b ,c)]
	  [(begin ,e ... ,[last]) `(begin ,@e ,last)]
	  [(let ,binds ,[body])   `(let ,binds ,body)]
	  [(for ,decl ,[body])    `(for ,decl ,body)]

	  [,oth `(return ,(fallthru oth))]
	  )))  
  [OutputGrammar 
   (cons ;'(LetOrSimple ('return Simple))
    '(Expr ('return Simple))
    ws-remove-complex-opera*-grammar)
   ]
  [Expr (lambda (x fallthru)
	    (match x
	      [(iterate ,annot (let ,binds (lambda (,x ,y) (,tyx ,tyy) ,[(doit fallthru) -> bod])) ,strm)
	       `(iterate ,annot (let ,binds (lambda (,x ,y) (,tyx ,tyy) ,bod)) ,strm)]
	      [,oth (fallthru oth)])
	    )])

;; [2007.03.14]
;; This desugars all types within the program by applying all type aliases.
(define-pass resolve-type-aliases
    (define aliases '())    ;; Mutated below:
    (define union-types #f) ;; Mutated below:
    (define Type (lambda (t) (export-type (dealias-type aliases union-types (instantiate-type t)))))
    ;(define Type (lambda (t) (dealias-type aliases t)))
    [Bindings (lambda (v* t* e* reconst Expr)
		(reconst v* (map Type t*) (map Expr e*)))]
    [Expr (lambda (x fallthru)
	    (match x [(assert-type ,[Type -> t] ,[e]) `(assert-type ,t ,e)]
		   [,oth (fallthru oth)]))]
    [Program (lambda(prog Expr)	  
	       (match prog
		 [(,inputlang '(program ,bod ,meta* ... ,type))
		  (fluid-let ([aliases (cdr (or (assq 'type-aliases meta*) 
						'(type-aliases)))]
			      [union-types (or (assq 'union-types meta*) '(union-types))])
		    `(resolve-type-aliases-language
		      '(program ,(Expr bod) 
			        ;,@(remq (assq 'type-aliases meta*) meta*)
			        ,@meta*
				,type)))]))]
    ;; Now we're free of sugars and can use the initial grammar.
    [OutputGrammar initial_regiment_grammar])


; --mic
(define (first-true ls p)
  (cond ((null? ls) #f)
        ((p (car ls)) (car ls))
        (else (first-true (cdr ls) p))))

; --mic
(define-pass propagate-copies
    ; FIXME: count all var. lookup stats
    [Expr (letrec ((do-expr
                    ; substs is a hash table of variable substitions
                    (lambda (x fallthru substs)

                      (match x

                        [(let ((,v1 ,t1 ,v0)) ,body)
                         (guard (and (symbol? v0)
                                     (not (and (pair? t1)
                                               (eq? (car t1) 'Ref)))))
                         (begin
                           (let ((subst-binding (first-true substs (lambda (b) (eq? v0 (car b))))))
                             (fallthru
                              body
                              (lambda (x f)
                                (do-expr x f (cons `(,v1 . ,(if subst-binding (cdr subst-binding) v0))
                                                   substs)))))
                           )]
                        
                        [,var
                         (guard (symbol? var))
                              
                         ; newest
                         (let ((subst-binding (first-true substs (lambda (b) (eq? var (car b))))))
                           (if subst-binding
                               (cdr subst-binding)
                               (fallthru x)))]
                        
                        [,oth (fallthru oth)]))))

            (lambda (x f) (do-expr x f ())) )]
)



#;
     [Program (lambda (p E)
	       (fluid-let ([substs (make-default-hash-table)])
		 )
	       
	       )]
  
) ;; End module
