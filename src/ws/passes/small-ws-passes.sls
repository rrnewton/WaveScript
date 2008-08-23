#!r6rs

;;;; This defines a handful of small passes used by WaveScript that
;;;; don't quite each deserve their own files.


(library (ws passes small-ws-passes)
  (export 
           introduce-lazy-letrec
	   lift-polymorphic-constant
	   unlift-polymorphic-constant
	   strip-irrelevant-polymorphism strip-polymorphic-type
	   lift-immutable-constants
	   lift-closed-monolambdas
	   strip-src-pos
	   remove-IFPROFILE
	   ;purify-letrec  ;; Disabled
	   standardize-iterate
	   ;ws-add-return-statements  ;; Disabled
	   resolve-type-aliases
	   generate-comparison-code
	   generate-printing-code
	   generate-marshal-code
	   insert-marshal-and-comm
	   optimize-print-and-show
	   explicit-toplevel-print
	   strip-unnecessary-ascription

	   hide-special-libfuns
	   reveal-special-libfuns

           ; --mic
	   propagate-copies
	   
	   embed-strings-as-arrays

           )
  (import (except (rnrs (6)) error) (ws common)
	  (ws passes normalize_query ws-remove-complex-opera)
	  (ws passes optimizations rewrite_opts)
	   )

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
  (define special-defs '())
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
;; We strip out ascriptions so we don't have any polymophic hanging around in there:
(define-pass strip-unnecessary-ascription
  (define required-ops '(readFile foreign foreign_source))
  [Expr (lambda (e fallthru)
	  (match e
	    ;; FIXME: HACK: [2007.10.10] Just prune out the polymorphic asserts around constants.
	    ;; [2008.01.28] Nah, throwing out all of these, we will get them back in the proper way.
	    #;
	    [(assert-type ,ty (quote ,x))
	     (guard (not (polymorphic-type? ty)))
	     `(assert-type ,ty ',x)]

	    [(assert-type Double ',val)
	     `',(if (number? val) (make-double val) val)]

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

#;
(define-pass remove-IFPROFILE 
    [Expr (lambda (e fallthru)
	    (match e
	      [(IFPROFILE ,prof ,real) real]
	      [,oth (fallthru oth)]))])

;; This removes IFPROFILE.  It also prunes unused variable bindings of
;; type Stream.  It does this without walking all the code.  Instead,
;; it leverages the fact that all variable references to stream-typed
;; variables will be *outside* of iterate bodies.
;;
;; Assumes: unique names, no complex opera, let only has one binding
(define-pass remove-IFPROFILE 
    [Expr (lambda (exp fallthru)
	    (match exp
	      [,var (guard (symbol? var)) (vector var (list var))]
	      [(IFPROFILE ,prof ,real)
	       (ASSERT symbol? prof)
	       (ASSERT symbol? real)
	       (vector real (list real))]
	      ;; Don't walk over the kernel function:
	      [(iterate ,_ ,strm) (ASSERT symbol? strm) 
	       (vector `(iterate ,_ ,strm) (list strm))]
	      [(let ([,x (Stream ,elt) ,[rhs]]) ,[bod])
	       (let-match ([#(,_rhs ,rv*) rhs]
			   [#(,_bod ,bv*) bod])
		 ;(printf "  Considering: ~s\n" x )
		 (if (memq x bv*)
		     (vector `(let ([,x (Stream ,elt) ,_rhs]) ,_bod) (append rv* bv*))
		     (begin
		       ;(printf "KILLING DEAD COde ~s ~s\n" x _rhs)
		       (vector _bod bv*)))
		 )]
	      ;[(let . ,_) (error 'remove-IFPROFILE "missed let binding")]
	      [,oth (fallthru oth)]))]
    [Fuser (lambda (ls k)
	     (match ls
	       [(#(,e* ,v**) ...)
		(vector (apply k e*) (apply append v**))]))]
    [Program (lambda (prg Expr)
	       (define (topExpr x) (vector-ref (Expr x) 0))
	       (apply-to-program-body topExpr prg))])


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
		  '#f))))]

      ;; [2008.01.07] Only for wsc2 at the moment:
      [(List ,elt) (guard 
		    (and (wsc2-variant-mode? (compiler-invocation-mode))
			 ;; For java we implement the equals method:
			 (not (java-mode? (compiler-invocation-mode)))))
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
			       (begin 
				 (if (List:is_null (deref ,ptr2))
				     (set! ,result '#t) ;; Equal
				     (tuple))
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

      [#() `(begin ,e1 ,e2 '#t)]

      ;; [2007.11.01] Doing tuples here.
      ;; (This actually makes for worse-code for Scheme... could do this conditionally:)
      ;; TODO FIXME: Remove the relevant code from the different backends.      
      [#(,ty0 ,ty* ...)
       (define len (fx+ 1 (length ty*)))
       (maybe-bind-tmp e1 origtype
	(lambda (tmp1)
	  (maybe-bind-tmp e2 origtype
	   (lambda (tmp2)
	     (let loop ([types (cons ty0 ty*)]
			[ind 0])
	       (define head (build-comparison (car types) `(tupref ,ind ,len ,tmp1) `(tupref ,ind ,len ,tmp2)))
	       (if (null? (cdr types))
		   head
		   `(if ,head
			,(loop (cdr types) (fx+ 1 ind))
			'#f)))))))]

      ;; For the simple case we just allow the wsequal? to stick around.
      [,_ `(wsequal? (assert-type ,origtype ,e1) ,e2)]))
  [Expr 
   (lambda (x fallthru)
     (match x
       [(wsequal? (assert-type ,ty ,[e1]) ,[e2])
	;(ASSERT simple-expr? e1)
	;(ASSERT simple-expr? e2)
	(build-comparison ty `(assert-type ,ty ,e1) e2)
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
		       ,(recur elt `(car (deref ,ptr))) ;; we allow this car code to get duplicated.
		       (if (wsequal? (cdr (deref ,ptr))  (assert-type (List ,elt) '()))
			   (tuple)
			   ,(addstr! ''", "))
		       (set! ,ptr (cdr (deref ,ptr)))))
	      ,(addstr! ''"]"))))]


      ;; TEMP FIXME: [2007.12.22] For now only for the new C backend.
      [(Array ,elt) (guard 
		     (and (wsc2-variant-mode? (compiler-invocation-mode))
			  (not (java-mode? (compiler-invocation-mode)))))
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


      [Complex (guard (and (wsc2-variant-mode? (compiler-invocation-mode))
			   (not (java-mode? (compiler-invocation-mode)))))
       (maybe-bind-tmp expr 'Complex
        (lambda (tmp)
	  `(begin ,(recur 'Float `(realpart ,tmp))
		  ,(addstr! ''"+")
		  ,(recur 'Float `(imagpart ,tmp))
		  ,(addstr! ''"i"))))]

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


;; The marshal code generated must walk a data structure twice.  First
;; to determine its size, and second to copy its values.
(define-pass generate-marshal-code
  ;; This generates code to walk over a data structure.  It is
  ;; parameterized by a function that handles the leaf cases --
  ;; scalars.
  (define (traverse-value ty expr doit)
    (match ty ;; <- No match recursion
      [,ty (guard (scalar-type? ty))
	   (doit expr ty)]
      ;; Lists are written in a very simple format for now.  Each car
      ;; is followed by a one-byte CDR, which is either 1, indicating
      ;; that another cell follows, or 0 indicating null.
      ;; Alternatively, could encode it like an array, and put the length at the front.
      [(List ,elt)
       (let* ([ptr (unique-name 'ptr)]
	      [hd (unique-name 'hd)])
	 `(let ([,ptr (Ref (List ,elt)) (Mutable:ref ,expr)])
	    (begin 
		  (while (not (wsequal? (deref ,ptr) (assert-type (List ,elt) '())))
			 (begin
			   ,(doit '(assert-type Uint8 '1) 'Uint8)
			   (let ([,hd ,elt (car (deref ,ptr))])
			     ,(traverse-value elt hd doit))		     		     
			   (set! ,ptr (cdr (deref ,ptr)))))
		  ,(doit '(assert-type Uint8 '0) 'Uint8))
	    ))]
      ;; Arrays have an 'Int' length, followed by the elements in order.
      [(Array ,elt)
       (let* ([arr (unique-name "arr")]
	      [ind (unique-name "ind")])
	 `(let ([,arr (Array ,elt) ,expr])
	    ,(make-begin
	      (list (doit `(assert-type Int (Array:length ,arr)) 'Int)
		    `(for (,ind '0 (_-_ (Array:length ,arr) '1))
			 ,(traverse-value elt `(Array:ref ,arr ,ind) doit))))))]
      ;; Tuples are written simply as the concatenation of their fields.
      [#(,fld* ...)
       (let ([tup (unique-name "tup")]
	     [len (length fld*)])
	 `(let ([,tup ,ty ,expr])
	    ,(make-begin
	      (mapi (lambda (i fldty)
		      (traverse-value fldty `(tupref ,i ,len ,tup) doit)) ;; I allow code duplication for tuprefs.
		    fld*))))]

      ;; :String, Sum
      [,oth (error 'generate-marshal-code:traverse-value "Unhandled type: ~s" oth)]))

  ;(define (SIZEOF ty) `(assert-type Int ',(type->width ty)))
  (define (SIZEOF ty) `',(type->width ty))
  (define (determine-size ty expr)
    (define sum (unique-name 'sum))
    `(let ([,sum (Ref Int) (Mutable:ref (assert-type Int '0))])
       ,(make-begin
	 (list 
	  (traverse-value ty expr
			  (lambda (xp elt)
			    `(set! ,sum (_+_ ,(SIZEOF elt) (deref ,sum)))))
	  `(deref ,sum)))))
  (define (build-marshal ty expr)
    (define buf    (unique-name "buf"))
    (define len    (unique-name "len"))
    (define offset (unique-name "offset"))
    (make-nested-lets 
     `([,len    Int           ,(determine-size ty expr)]
       [,buf    (Array Uint8) (assert-type (Array Uint8) (Array:makeUNSAFE ,len))]
       [,offset (Ref Int)     (Mutable:ref (assert-type Int '0))])
     (make-begin
      (list 
       (traverse-value ty expr
		       (lambda (xp elt)
			 (define size (SIZEOF elt))
			 `(begin
			    (__type_unsafe_write (assert-type ,elt ,xp) ,buf (deref ,offset))
			    (set! ,offset (_+_ (deref ,offset) ,size)))))
       buf))))

  (define (reconstruct-value ty expr initoffset)   
    (define buf    (unique-name "buf"))
    (define off    (unique-name "offset"))
    (define (read-and-bump ty)
      (let ([size (SIZEOF ty)])
	`(begin (set! ,off (_+_ (deref ,off) (assert-type Int ,size)))
		(assert-type ,ty (__type_unsafe_read ,buf (_-_ (deref ,off) (assert-type Int ,size)))))))
    ;; This loops over the type and returns code that returns the unpacked value.
    (define (unpack-loop ty)
      (match ty
	[,ty (guard (scalar-type? ty)) (read-and-bump ty)]

	;; Arrays have an 'Int' length, followed by the elements in order.
	[(Array ,elt)
	 (let* ([len (unique-name "len")]
		[arr (unique-name "arr")]
		[ind (unique-name "ind")])
	   `(let ([,len Int ,(read-and-bump 'Int)])
	      (let ([,arr (Array ,elt) (assert-type (Array ,elt) (Array:makeUNSAFE ,len))])
		(begin 
		  (for (,ind '0 (_-_ ,len '1))
		      (Array:set ,arr ,ind ,(unpack-loop elt)))
		  ,arr))))]

	;; Annoying - here we need to do a list reversal.  Again would be nice to have this as a primitive.
	;; Especially a destructive list reversal as a primitive.  I should do some timings of list reversal...
	[(List ,elt)
	 (let* ([acc      (unique-name "acc")]
		[flipped  (unique-name "flipped")]
		[nextbyte (unique-name "nextbyte")])
	   `(let ([,acc (Ref (List ,elt)) (Mutable:ref (assert-type (List ,elt) '()))])
	      (begin
		;; Until the CDR byte is a zero.
		(while (not (wsequal? ,(read-and-bump 'Uint8) (assert-type Uint8 '0)))
		       (set! ,acc (cons ,(unpack-loop elt) (deref ,acc)))) ;; Unpack the CAR
		;; Now reverse it.
		(let ([,flipped (Ref (List ,elt)) (Mutable:ref (assert-type (List ,elt) '()))])		  
		  (begin
		    (while (not (wsequal? (deref ,acc) (assert-type (List ,elt) '())))
			 (begin 
			   (set! ,flipped (cons (car (deref ,acc)) (deref ,flipped)))
			   (set! ,acc (cdr (deref ,acc)))))
		    (deref ,flipped))))))]
	
	;; This introduces a bunch of temporaries because it needs nested lets to order the evaluation.	
	[#(,fld* ...)
	 (let ([tmps (map (lambda (_) (unique-name "tupfld")) fld*)])
	   (make-nested-lets
	    (map (lambda (name ty) `[,name ,ty ,(unpack-loop ty)])
	      tmps fld*)
	    `(tuple ,@tmps)))]
		
	;; :String, Sum
	[,oth (error 'generate-marshal-code:reconstruct-value "Unhandled type: ~s" oth)]))
    (make-nested-lets 
     `([,buf (Array Uint8) ,expr] ;; should maybe-let
       [,off (Ref Int)     (Mutable:ref ,initoffset)])
     (unpack-loop ty)))

    [Expr 
     (lambda (x fallthru)
       (match x
	 [(marshal (assert-type ,ty ,[exp])) (build-marshal ty exp)]	 
	 [(assert-type ,ty (unmarshal ,[exp] ,[ind])) (reconstruct-value ty exp ind)]
	 [(,marshal . ,_) (guard (eq-any? marshal 'marshal 'unmarshal))
	  (error 'generate-marshal-code "missing type annotation: ~s" (cons marshal _) )]
	 [,oth (fallthru oth)]
	 ))])


;; Note: this mixes up the marshal insertion with the comm insertion.  Should separate these.
;; Currently this straps on a hackish communication system that will be used with ssh to communicate to a client using stdin and stdout.
;; It sends the binary data on stderr so that text output may still come across stdout.
(define (insert-marshal-and-comm prog cutstreams)
  (define readers 0)
  (define writers 0)
  (define marshal-and-comm 
    (core-generic-traverse/types
     (lambda  (xp tenv fallthru)
	 (let loop ([xp xp] [tenv tenv])	   
	 (match xp
	   ;; FIXME: This will perform unmarshaling SEPARATELY for each subscriber to a stream.
	   ;; This should only be done ONCE. FIXME  FIXME FIXME FIXME 
	   ;; A variable reference to one of the cut streams:
	   [,var (guard (symbol? var))
		 (cond
		  [(assq var cutstreams) =>  
		   (lambda (entry)
		     (unless (zero? readers) (error 'insert-marshal-and-comm:readers "This hack only supports one cut stream presently."))
		     (set! readers (add1 readers))		     
		     (match (tenv-lookup tenv var) ;(cdr entry)
		       [(Stream ,elt)
			(define Server_bytes (unique-name "Server_bytes"))
			(define Server_vals  (unique-name "Server_vals"))
			`(let ([,Server_bytes 
				   (Stream (Array Uint8))
				   ;,var
				   (iterate (annotations (name ,Server_bytes))
					    (let ([myin (Pointer "FILE*")
							(foreign-app '"ws_get_stdin"
								     (assert-type ( -> (Pointer "FILE*"))
										  (foreign '"ws_get_stdin" '())))]
						  [myfread ((Array Uint8) Int Int (Pointer "FILE*") -> Int)
							   (assert-type
							    ((Array Uint8) Int Int (Pointer "FILE*") -> Int)
							    (foreign '"fread" '("stdio.h")))])
					      (lambda (x vq) 
						(#() (VQueue (Array Uint8)))
						(let ([count_buf (Array Uint8) (Array:make '4 (assert-type Uint8 '0))])
						  (begin 
						    ;; WARNING: this doesn't check the error condition:
						    ;; First read the length field:
						    (foreign-app '"fread" myfread	count_buf '4 '1 myin)
						    (let ([count Int (assert-type Int (unmarshal count_buf '0))])
						      (let ([buf (Array Uint8) (Array:makeUNSAFE count)])
							(begin
							  ;(print '"Got msg w/ length ")
							  ;(print count)
							  ;(print '"\n")
							  (foreign-app '"fread" myfread buf '1 count myin)
							  (emit (assert-type (VQueue (Array Uint8)) vq)
								buf))))
						    vq))))
					    ;; This is arbitrary, should be infinity I suppose:
					    ;,var
					    (let ([mytimer (Stream #()) (timer (annotations) (assert-type Float '1000.0))])
					      (_merge (annotations) ,var mytimer))
					    )])
			   (let ([,Server_vals (Stream ,elt)
					       (iterate (annotations (name ,Server_vals))
							(let ()
							  (lambda (x vq) ((Array Uint8) (VQueue ,elt))
								  (begin (emit (assert-type (VQueue ,elt) vq)
									       (assert-type ,elt (unmarshal x '0)))
									 vq)))
							,Server_bytes)])
			     ,Server_vals
			    ))]))]
		  [else var])]

	   ;; TODO: this is overly general: since we now sit after remove-letrec we don't need to handle letrec:
	   [(,lett ([,lhs* ,ty* ,_rhs*] ...) ,_bod) (guard (memq lett '(letrec let)))
	    ;; Note, this use of newenv is technically INCORRECT:
	    ;; But if this is a let rather than a letrec, it shouldn't matter because of unique naming.
	    (define newenv (tenv-extend tenv lhs* ty*))
	    (define rhs* (map (lambda (x) (loop x newenv)) _rhs*))
	    (define bod  (loop _bod newenv))
	    ;; Danger, making ASSUMPTIONS about the naming conventions here:
	    (define Node_bytes   (unique-name "Node_bytes")) 
	    (define Node_writer  (unique-name "Node_writer"))
	    `(,lett 
		 ,(map (lambda (lhs ty rhs)
			 (if (assq lhs cutstreams)
			     (begin 
			      (unless (zero? writers) (error 'insert-marshal-and-comm:writers
							     "This hack only supports one cut stream presently."))
			      (set! writers (add1 writers))
			      (list lhs 
			       '(Stream #())
			       ;'(Stream (Array Uint8))
			       ;; Insert marshal operator:
			       (match ty
				 [(Stream ,elt)
				   ;; We'll want to replace this with different communication code down the road:
				   ;; For now, we write to stdout, but we can't just use 'print', because of null characters.
				  `(let ([,Node_bytes 
					     (Stream (Array Uint8))
					     (iterate (annotations ) ; (name ,lhs)
						      (let () 
							(lambda (x vq) (,elt (VQueue (Array Uint8)))
								(let ([arr (Array Uint8) (marshal (assert-type ,elt x))])
								  ;; A marshaled stream contains a length header for each object:
								  (begin (emit (assert-type (VQueue (Array Uint8)) vq)
									       (marshal (assert-type Int (Array:length arr))))
									 (emit (assert-type (VQueue (Array Uint8)) vq)
									       arr)
									 vq)
								  )))
						      ,rhs)])
				     (let ([,Node_writer
					     (Stream #())
					     (iterate (annotations)
						      (let ([myout (Pointer "FILE*")
								   (foreign-app '"ws_get_stderr"
										(assert-type ( -> (Pointer "FILE*"))
											     (foreign '"ws_get_stderr" '())))])
							(lambda (x vq) ((Array Uint8) (VQueue #()))
								(begin 	  
								  (foreign-app '"fwrite" 
									       (assert-type
										((Array Uint8) Int Int (Pointer "FILE*") -> Int)
										(foreign '"fwrite" '("stdio.h")))
									       x '1 (Array:length x) myout)
								  (foreign-app '"fflush" 
									       (assert-type ((Pointer "FILE*") -> #()) 
											    (foreign '"fflush" '("stdio.h")))
									       myout)
								  vq)))
						      ,Node_bytes)])
				       ,Node_writer
				     ;,Node_bytes
				       ))])))
			     (list lhs ty rhs)))
		    lhs* ty* rhs*)
	       ,bod)]
	  
	   [,oth (fallthru oth tenv)])))))
  (apply-to-program-body marshal-and-comm prog))


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
					(emit (assert-type (VQueue #()) ,vq) (tuple))
					,vq)))
			     ,bod)
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


;; Is the value represented by this scheme representation polymorphic?
(define (polymorphic-const? x) 
  (polymorphic-type? (export-type (type-const x)))
  #;
  (match x
    [,n (guard (integer? n)) #t] ;; Integer constants themselves are polymorphic.
    [()     #t]
    [#()    #t]
    [(,[x*] ...)  (ormap id x*)]
    [#(,[x*] ...) (ormap id x*)]
    [,else   #f]))

;; Does the expression represent a polymorphic constant?
(define (polymorphic-const-expr? x) 
  (match x
    [',n (guard (polymorphic-const? n)) #t]
    [nullseg #t]
    [Array:null #t]
    [(Array:makeUNSAFE ,n) #t]
    [(construct-data ,name . ,_) #t]
    [,else   #f]))

;; [2007.03.17] Including Array:makeUNSAFE here even though it's not "constant"
;; [2007.10.11] Adjusting this to apply to data constructors/destructors as well.
(define-pass lift-polymorphic-constant
    [Expr (lambda (x fallthru)
	    (define (f x) 
	      (let ([tmp (unique-name 'tmp)]
		    [t   (unique-name 'alpha)])
		`(let ([,tmp (quote ,t) ,x]) ,tmp)))
	    (match x

	      [,pc (guard (polymorphic-const-expr? pc)) (f pc)]

	      ;; [2008.01.28] Plain integers are also polymorphic:	      
	      ;[',n (guard (integer? n)) (f x)]
	      ;[nullseg (f x)]
	      ;[Array:null (f x)]
	      ;['()  (f x)]
	      
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
    ;; If you can't say anything nice, don't say anything at all.
    ;; (Any polymorphism left is irrelevant, and will be stripped away.)
    (define (make-assert t x)
      (if (polymorphic-type? t) x `(assert-type ,t ,x)))
  [Expr (lambda (x fallthru)
	  (match x
	    ;; Don't touch these:
	    [(foreign        ,x ,y) `(foreign        ,x ,y)]
	    [(foreign_source ,x ,y) `(foreign_source ,x ,y)]
	   
	    [(let ([,v1 ,t ,c]) ,v2)
	     (guard (eq? v1 v2) (polymorphic-const-expr? c))
;; [2007.07.08] Removing this assert because we clean up below:
;	       (ASSERT (lambda (t) (not (polymorphic-type? t))) t)
	     (make-assert t c)]
	   
	    [(wscase (let ([,v1 ,t ,[x]]) ,v2) (,tag* ,[fun*]) ...)
	     (guard (eq? v1 v2))
	     (ASSERT "unlift-polymorphic-constant: no polyorphism in case dispatch"
		     (compose not polymorphic-type?) t)
	     `(wscase (assert-type ,t ,x) ,@(map list tag* fun*))]

	    [',c (guard (polymorphic-const? c))
		 (error 'unlift-polymorphic-constant "missed polymorphic const: ~s" c)]

	    ;; Don't touch these:
	    [(foreign ,x ,y) `(foreign ,x ,y)]
	    [,other (fallthru other)]))])

;; Insert a dummy type in place of polymorphic types:
(define (strip-polymorphic-type t)
  ;(define dummy-type #()) ;; Type to insert.    
  (define dummy-type 'Int) ;; Type to insert.  
  (type-replace-polymorphic t dummy-type))

;; [2007.07.08]
;; Remaining polymorphism at this phase of the compiler is
;; "irrelevent" in the sense that it describes only uninspected values.
;; Thus it is equivalent to insert Unit in all such places.
(define-pass strip-irrelevant-polymorphism
    (define (data-source? e)
      (let ([expr (peel-annotations e)])
	(and (pair? expr) (memq (car expr) '(readFile dataFile)))))
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
	    [(assert-type ,[strip-polymorphic-type -> ty] ,[e])
	     `(assert-type ,ty ,e)]

	    [,oth (fallthru oth)])))
  [Expr Expr]
  [Bindings 
   (lambda (vars types exprs reconstr exprfun)
     (reconstr vars (map strip-polymorphic-type types) (map exprfun exprs)))]
  [Program
   (lambda (pr Expr)
     (match pr
       [(,lang '(program ,[Expr -> bod] ,meta* ... ,[strip-polymorphic-type -> topty]))
	`(,lang '(program ,bod ,@meta* ,topty))]))])


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
;; It makes sure that iterate always has a (let () ...) surrounding the work function.
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

;; UNUSED
#;
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

    ;; First, to apply aliases, we must resolve any aliases on the
    ;; right hand sides of aliases themselves!!
    (define (normalize-aliases aliases)
      ;; This is a bit ugly... we could topologically sort the aliases.
      ;; Instead we do something quite inefficient (but expedient).  
      ;; We simply repeatedly dealias all RHS's until we reach a fixed point.
      (let ([new (map (lambda (alias)
			(let-values ([(v a* rhs)
			  (match alias
			    [(,v ,rhs)           (values v '() rhs)]
			    [(,v (,a* ...) ,rhs) (values v a* rhs)])])
			  (list v a* (dealias-type aliases union-types rhs))
			  ))
		   aliases)])
	(if (equal? new aliases)
	    aliases
	    (normalize-aliases new))))

    ;(define Type (lambda (t) (dealias-type aliases t)))
    [Bindings (lambda (v* t* e* reconst Expr)
		(reconst v* (map Type t*) (map Expr e*)))]
    [Expr (lambda (x fallthru)
	    (match x [(assert-type ,[Type -> t] ,[e]) `(assert-type ,t ,e)]
		   [,oth (fallthru oth)]))]
    [Program (lambda(prog Expr)	  
	       (match prog
		 [(,inputlang '(program ,bod ,meta* ... ,type))
		  (fluid-let ([union-types (or (assq 'union-types meta*) '(union-types))])
		    (fluid-let ([aliases 
				 ;; [2008.07.07] Reverse these so that more later-defined aliases take precedence:
				 (reverse
				  (normalize-aliases
				   (cdr (or (assq 'type-aliases meta*) 
					    '(type-aliases)))))])
		    `(resolve-type-aliases-language
		      '(program ,(Expr bod) 
			        (type-aliases ,@aliases)
			        ,@(remq (assq 'type-aliases meta*) meta*)
				,type))))]))]
    ;; Now we're free of sugars and can use the initial grammar.
    [OutputGrammar initial_regiment_grammar])


; --mic

(define-pass propagate-copies
    (define (substitutable? bind) 
      (match bind
	[(,lhs ,ty ,[peel-annotations -> rhs])
	 (or (symbol? rhs) (constant-expr? rhs))]))
    ; FIXME: count all var. lookup stats
  [Expr (letrec ((do-expr
                    ; substs is an association list of variable substitions
                    (lambda (x fallthru substs)
                      (match x
                        [(let ([,lhs* ,ty* ,[rhs*]] ...) ,body)
			 (let-values ([(tosubst newbinds) 
				       (partition substitutable? (map list lhs* ty* rhs*))])
			   (define newsubst (append tosubst substs))
			   (define (newdriver x f) (do-expr x f newsubst))
			   (if (null? newbinds)
			       (fallthru body newdriver)
			       `(let ,newbinds ,(fallthru body newdriver))))]
			;; Have to make sure we leave a let behind:
			[(iterate ,annot ,[fn] ,[strm])
			 `(iterate ,annot
				   ,(match fn [(let . ,_) fn] [,oth `(let () ,oth)])
				   ,strm)]
                        [,var (guard (symbol? var))
			 ;;(printf "Testing var: ~s\n" var)
                         (let ((subst-binding (assq var substs)))
                           (if subst-binding
                               (begin
				 ;(printf "    OPT: copy propagate ~s -> ~s\n" var (caddr subst-binding))
				 (caddr subst-binding))
                               var))]
                        [,oth (fallthru oth)] ;(lambda (x f) (do-expr x f subst))
			))))
            (lambda (x f) (do-expr x f '())))])

;; This is so the new C backend (emit-c2) can avoid handling strings.
;; A string is mapped onto an array of characters, augmented with a null terminator character.
;; ASSUMPTIONS: assumes it happens before remove-complex-opera*, because it generates complex opera*.
(define-pass embed-strings-as-arrays
    (define (Type ty) 
      (match ty
	[String `(Array Char)]
	[,s (guard (symbol? s)) s]
	[(,qt ,tvar) (guard (memq qt '(NUM quote))) `(,qt ,tvar)]
	[#(,[t*] ...) (list->vector t*)]
	[(,[arg*] ... -> ,[res]) `(,@arg* -> ,res)]
	[,s (guard (string? s)) s] ;; Allowing strings for uninterpreted C types.
	[(,C ,[t*] ...) (guard (symbol? C)) (cons C t*)] ; Type constructor
	[,other (error 'embed-strings-as-arrays "malformed type: ~a" ty)]))
    (define (Const cn) ;; Convert string constants.
      (cond
       [(number? cn) cn]
       [(char?   cn) cn]
       [(boolean? cn) cn]
       [(string? cn) 
	;; make a constant vector.
	(list->vector (append ;(make-list 8 #\nul)
			  (string->list cn)
			  (list (integer->char 0))))]
       [(list? cn)   (map Const cn)]
       [(vector? cn) (vector-map Const cn)]
       [(tuple? cn)  (make-tuple (map Const (tuple-fields cn)))]
       [(symbol? cn)   cn] ;; Just here for compiler-internal purposes.
       [(timebase? cn) cn]
       [(sigseg? cn)   cn]
       [(double? cn) cn]
       [else (error 'embed-strings-as-arrays:Const "unmatched ~s" cn)]))
    (define (quoted-constant? x)
      (match x [(quote ,_) #t] [,else #f]))
    [Expr (lambda (xp fallthru)
	    (match xp
	      [',c `',(Const c)]
	      [(string-append ,[s1] ,[s2])
	       (let ([a1 (unique-name "strarr1")]
		     [a2 (unique-name "strarr2")]
		     [i1 (unique-name "i")]
		     [i2 (unique-name "i")]
		     [len1 (unique-name "len")]
		     [result (unique-name "appendresult")])
		 `(let ([,a1 (Array Char) ,s1] 
			[,a2 (Array Char) ,s2])
		    (let ([,len1 Int (_-_ (Array:length ,a1) '1)]) ; Number of non-null characters.
		      (let ([,result (Array Char) 
				   (assert-type (Array Char)
						(Array:makeUNSAFE (_+_ ,len1 (Array:length ,a2))))])
			(begin 
			  ;(print '"APPENDING\n");
			  (for (,i1 '0 (_-_ ,len1 '1)) ; Don't copy null char.
			      (Array:set ,result ,i1 (Array:ref ,a1 ,i1)))
			  (for (,i2 '0 (_-_ (Array:length ,a2) '1))
			      (Array:set ,result (_+_ ,len1 ,i2) (Array:ref ,a2 ,i2)))
			;(app Array:blit ,result '0 ,a1 '0 (_-_ (Array:length ,a1) '1))
			;(app Array:blit ,result (_-_ (Array:length ,a1) '1) ,a2 '0 (Array:length ,a2))
			,result)
			))))]

	      ;; Should not include null character: FIXME: Actually,
	      ;; this should scan the string until it hits a null
	      ;; character, not assume that the first null is at the
	      ;; end!
	      [(String:length ,[str])   `(_-_ (Array:length ,str) '1)]
	      [(String:ref ,[str] ,[i]) `(Array:ref ,str ,i)]

	      ;; Here we are inconsistent with the MLton backend wrt
	      ;; to whether the null terminator is included:
	      [(String:toArray ,[str]) str]
	      [(String:fromArray ,[str]) str]
#;
	      [(String:make ,[len] ,[init])
	       (define tmp    (unique-name "tmpmakestr"))
	       (define lentmp (unique-name "lentmp"))
	       `(let ([,lentmp Int ,len])
		  (let ([,tmp (Array Char) (Array:make (_+_ ,lentmp '1) ,init)])
		    (begin
		      (Array:set ,tmp ,lentmp (intToChar '0))
		      ,tmp)))]
	      
	      [(show ,[x]) `(__show_ARRAY ,x)]
	      [(wserror ,[x]) `(__wserror_ARRAY ,x)]
	      
	      [(readFile ,annot ',str1 ',str2 ,[strm])
	       (ASSERT string? str1) (ASSERT string? str2)
	       `(readFile ,annot ',str1 ',str2 ,strm)]
	      [(readFile ,annot ,[str1] ,[str2] ,[strm])
	       (error 'embed-strings-as-arrays "For now readfiles must have explicit quoted string arguments at this point.")
	       ;`(readFile ,annot (__backtoSTR ,str1) (__backtoSTR ,str2) ,strm)
	       ]

	      ;; Similarly:
	      [(,totally_ignored ,args ...)	       
	       ;; After metaprog eval the operands to foreign should also be quoted constants:
	       (guard (memq totally_ignored '(inline_C inline_TOS foreign foreign_source)))
	       (ASSERT (andmap quoted-constant? args))
	       (cons totally_ignored args)]
	      ;; 
	      #;
	      [(,ignore_first ,_ ,[second])
	       (guard (memq ignore_first '(foreign foreign_source)))
	       `(,ignore_first ,_ ,second)]
	      [(,safety . ,_) 
	       (guard (memq safety '(inline_C inline_TOS foreign foreign_source)))
	       (error 'embed-strings-as-arrays "missed this: ~s" (cons safety _))]

	      
	      [(stringToInt ,[x])     `(__stringToInt_ARRAY ,x)]
	      [(stringToFloat ,[x])   `(__stringToFloat_ARRAY ,x)]
	      [(stringToDouble ,[x])  `(__stringToDouble_ARRAY  ,x)]
	      [(stringToComplex ,[x]) `(__stringToComplex_ARRAY ,x)]

	      ;[(show ,[x]) `(__Hack:fromOldString (show ,x))]
	      ;[(wserror ,[x]) `(wserror (__Hack:backToString ,x))]
	      ;[(readFile ...) ????]
	      ;[(__readFile ...) ????]

	      ;; FIXME!!! THIS SHOULD OMIT THE NULL CHARACTER:
	      ;[(String:explode ,[x]) `(Array:toList ,x)]
	      ;[(String:implode ,[x]) `(List:toArray ,x)]

	      ;; Annoyingly, this also generates code to do a
	      ;; list-reverse, because List:reverse is a library prim
	      ;; and its gone already.
	      [(String:explode ,[x]) 
	       (let ([tmp (unique-name "tmplft")]
		     [acc  (unique-name "explodeacc")]
		     [acc2 (unique-name "explodeout")]
		     [len (unique-name "strlen")]
		     [arr (unique-name "explodearr")]
		     [accinit '((Ref (List Char)) (Mutable:ref (assert-type (List Char) '())))]
		     [i (unique-name "i")])
		 `(let ([,arr (Array Char) ,x])
		    (let ([,len Int (_-_ (Array:length ,arr) (assert-type Int '1))])
		      (let ([,acc ,@accinit])
			(begin 
			  ;; First, fill up the accumulator in reverse:
			  (for (,i (assert-type Int '0) (_-_ ,len (assert-type Int '1)))
			    (set! ,acc (cons (Array:ref ,arr ,i) (deref ,acc))))
			  ;; Next reverse it:
			  (let ([,acc2 ,@accinit])
			    (begin (while (not (List:is_null (deref ,acc)))
					  (begin 
					    (set! ,acc2 (cons (car (deref ,acc)) (deref ,acc2)))
					    (set! ,acc (cdr (deref ,acc)))))
				   (deref ,acc2))))))))]
	      [(String:implode ,[x]) 
	       (let ([tmp (unique-name "tmplft")]
		     [ptr (unique-name "implodeptr")]
		     [len (unique-name "strlen")]
		     [arr (unique-name "implodearr")]
		     [i (unique-name "i")])
		 `(let ([,len (Ref Int) (Mutable:ref (assert-type Int '0))])
		    (let ([,tmp (List Char) ,x]) ;; This will get copy-propagated 
		      (let ([,ptr (Ref (List Char)) (Mutable:ref ,tmp)])
			(begin 
			  ;; Get the length:
			  (while (not (List:is_null (deref ,ptr))) 
				 (begin 
				   (set! ,len (_+_ (deref ,len) (assert-type Int '1)))
				   (set! ,ptr (cdr (deref ,ptr)))))
			  (let ([,arr (Array Char) (Array:makeUNSAFE (_+_ (deref ,len) (assert-type Int '1)))])
			    (begin 
			      (set! ,ptr ,tmp) ;; start back at the beginning
			      (Array:set ,arr (deref ,len) '#\nul)
			      (for (,i (assert-type Int '0) (_-_ (deref ,len) (assert-type Int '1)))
				  (begin (Array:set ,arr ,i (car (deref ,ptr)))
					 (set! ,ptr (cdr (deref ,ptr)))))
			      ,arr)))))))]

	      [(assert-type ,[Type -> ty] ,[e])  `(assert-type ,ty ,e)]

	      [,oth (fallthru oth)]
	      )
	    )]  
    [Bindings
     (lambda (vars types exprs reconstr exprfun)
       (reconstr vars (map Type types) (map exprfun exprs)))])


;; [2008.08.11]
;; This is similar to lift-immutable-constants, but happens later.
;(define-pass lift-out-lambdas )

;; Any complex (immutable) constants are lifted to the top of the program.
;; Assumes unique variable names (a la rename-vars).
#;
(define-pass lift-immutable-constants
    (define acc '()) ;; Accumulates constant bindings.
  ;;[OutputProps (not single-bind-let)] ;; [2008.08.11] Made them nested
    [Expr 
     (lambda (xp fallthru)
       (match xp ;; No recursion!!
	 ;; This is troublesome, readFile should really be a special
	 ;; syntax at this point, not masquerading as a primitive application.
	 [(readFile ,annot ',str1 ',str2 ,[strm])
	  (ASSERT string? str1) (ASSERT string? str2)
	  `(readFile ,annot ',str1 ',str2 ,strm)]
	 [(,totally_ignored (quote ,args) ...)
	  (guard (memq totally_ignored '(inline_C inline_TOS foreign foreign_source)))
	  xp;(cons totally_ignored _)
	  ]
	 #;
	 [(,ignore_first ,_ ,[second])
	  (guard (memq ignore_first '(foreign foreign_source)))
	  `(,ignore_first ,_ ,second)]
	 [(,safety . ,_) 
	  (guard (memq safety '(inline_C inline_TOS foreign foreign_source readFile)))
	  (error 'lift-immutable-constants "missed this: ~s" (cons safety _))]

	 [',const (guard (or (string? const) (not (simple-constant? const))) ;; Allowing strings!
			 (not (type-containing-mutable? (export-type (type-const const))))
			 (not (symbol? const))
			 )
	  ;(printf "  LIFTING CONSTANT: ~s\n" const)
	  (let ([tmp (unique-name "tmpconstlift")])
	    (set! acc (cons `(,tmp ,(type-const const) ',const) acc))
	    tmp)]
	 
	 [,oth (fallthru oth)]))]
    [Program (lambda (prog Expr)
	       (fluid-let ([acc '()])
		 (match prog 
		   [(,lang '(program ,[Expr -> bod] ,meta* ...))
		   `(,lang '(program ,(make-nested-lets acc bod) ,@meta*))])))])

;; [2008.08.11] Extending this to lift out lambdas also.  Modifying it
;; to catch immutable constants (including first-order functions) at
;; let-binding sites.  This pass assumes that the lambdas will have no
;; freevars, but does not verify it.  Note, this will lift all lambdas
;; up to the top, even those that are already there, or in the
;; state-fields of an iterate.
(define lift-immutable-constants
  (let ()
    (define acc '()) ;; Accumulates constant bindings.
    (define (Expr inside-iter?)
      (core-generic-traverse
       (lambda (xp fallthru)
	 (match xp ;; No recursion!!
	   ;; This is troublesome, readFile should really be a special
	   ;; syntax at this point, not masquerading as a primitive application.
	   [(readFile ,annot ',str1 ',str2 ,[strm])
	    (ASSERT string? str1) (ASSERT string? str2)
	    `(readFile ,annot ',str1 ',str2 ,strm)]	   
	   [(,totally_ignored (quote ,args) ...)
	    (guard (memq totally_ignored '(inline_C inline_TOS foreign foreign_source)))
	    xp]
	   [(iterate ,annot (let ([,lhs* ,ty* ,[rhs*]] ...) ,bod) ,[strm])
	    `(iterate ,annot (let ,(map list lhs* ty* rhs*) ,((Expr #t) bod)) ,strm)]
	   
	   ;; Catch them at their binding sites if we can:
	   [(let ([,lhs ,ty ,[rhs]]) ,[bod])
	    (match rhs
	      #;
	      [(lambda . ,_)   (set! acc (cons (list lhs ty rhs) acc))     bod]
	      [',const (guard (or (string? const) (not (simple-constant? const))) ;; Allowing strings!
			      ;(not (type-containing-mutable? (export-type (type-const const))))
			      (not (type-containing-mutable? ty))
			      (not (symbol? const)))
	       (set! acc (cons (list lhs ty rhs) acc))
	       bod]
	      [,oth `(let ([,lhs ,ty ,rhs]) ,bod)])]

	   ;; If we don't catch them at the binding sites, we introduce a temporary:
	   [',const (guard (or (string? const) (not (simple-constant? const))) ;; Allowing strings!
			   (not (type-containing-mutable? (export-type (type-const const))))
			   (not (symbol? const))
			   )
		    (let ([tmp (unique-name "tmpconstlift")])
		      (set! acc (cons `(,tmp ,(type-const const) ',const) acc))
		      tmp)]

	   #;
	   [(lambda ,args ,argty* ,[bod]) (guard inside-iter?)
	    (let ([tmp (unique-name "tmpfunlift")])
	      (set! acc (cons `(,tmp ,??? (lambda ,args ,argty* ,bod)) acc))
	      tmp)]

	   ;; Strings might be found outside of the let-RHS position:
#|
	   [',const (guard (string? const))
		    (let ([tmp (unique-name "tmpconstlift")])
		      (set! acc (cons `(,tmp String ',const) acc))
		      tmp)]
	   [',const (guard (not (simple-constant? const))
			   (not (type-containing-mutable? (export-type (type-const const))))
			   (not (symbol? const)))
		    (error 'lift-immutable-constants "came across non-simple constant in non let-rhs position: '~s" const)]
|#	   

	   [(,safety . ,_) 
	    (guard (memq safety '(inline_C inline_TOS foreign foreign_source readFile let)))
	    (error 'lift-immutable-constants "missed this: ~s" (cons safety _))]	   
	   [,oth (fallthru oth)]))))
   (lambda (prog)
     (fluid-let ([acc '()])       
       (apply-to-program-body (lambda (bod)
				(let ([newbod ((Expr #f) bod)])
				  (make-nested-lets acc newbod))) 
			      prog)))))

(define lift-closed-monolambdas
  (let ()
    (define acc '()) ;; Accumulates lambda bindings.
    (define (Expr inside-iter?)
      (core-generic-traverse
       (lambda (xp fallthru)
	 (match xp ;; No recursion!!	 
	   [(iterate ,annot (let ([,lhs* ,ty* ,[rhs*]] ...) ,bod) ,[strm])
	    `(iterate ,annot (let ,(map list lhs* ty* rhs*) ,((Expr #t) bod)) ,strm)]	   
	   [(let ([,lhs ,ty ,[rhs]]) ,[bod]) ;; Catch them at their binding sites
	    (match rhs
	      [(lambda . ,_)
	       (set! acc (cons (list lhs ty rhs) acc))
	       bod]
	      [,oth `(let ([,lhs ,ty ,rhs]) ,bod)])]
	   [,oth (fallthru oth)]))))
   (lambda (prog)
     (fluid-let ([acc '()])       
       (apply-to-program-body (lambda (bod)
				(let ([newbod ((Expr #f) bod)])
				  (make-nested-lets acc newbod))) 
			      prog)))))



;(define (is-in-Node-namespace? name))

) ;; End module
