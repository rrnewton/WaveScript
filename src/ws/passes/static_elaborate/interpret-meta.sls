#!r6rs

;; TODO: Change some of the asserts below to DEBUGASSERTS

;; TODO: FIXME: When we were on deployment I made some hacks here to
;; deal with numeric constants.  The problem is that we don't yet know
;; what type we're dealing with (int, int16, int64).  And we can't
;; tell from the value's representation.  So I hackishly inserted some
;; "gints", all integer metavalues become gints...
;;
;; This should be *ok* because the program already passed the more
;; conservative type checking before getting here.  Besides, we should
;; eventually make all integer constants "gint".

(library (ws passes static_elaborate interpret-meta)
  (export Eval Marshal Marshal-Closure  interpret-meta
	  test-interpret-meta
;;	   marshal-cache
   )
  (import (except (rnrs (6)) error) ;(except (except (rnrs (6)) error) +)
	  (ws common)
	  (ws passes static_elaborate static-elaborate)
	  (ws passes wavescope_bkend nominalize-types)
	  (ws passes normalize_query ws-remove-letrec)
	  (ws langs lang_wavescript)
	  (ws testing lang_wavescript_tests)
	  )

; ================================================================================ ;
;;; Type defs and helpers

;; We use data types to separate different kinds of values.

;; Contains a datum: number, list, array, (tuples separate)
(reg:define-struct (plain val type))
;(reg:define-struct (tuple fields)) ;; To distinguish tuples from vectors.

;; There are two kinds of closures.  Native and foreign.
;; Foreign closures have #f in place of formals and env.
;;   "name" is optional -- a symbol or #f
(reg:define-struct (closure name formals argty* code env type))

;; A stream operator can be a leaf or an intermediate node.
;; The "parents" field contains a list of streamops, params are
;; regular values that parameterize the streamop.  We also need a
;; place to record the type.  At least for readFile and foreign_source...
(reg:define-struct (streamop name op params parents type))

;; This is not a pretty aspect of the system, but there are some
;; things that we should just "freeze" as code (apps) during metaprogram eval.
;; In particular, we should not evaluate foreign functions at metaprog time.
;;
;; [2007.09.06] Hash tables are another sticky part, we can't expect
;; the hash for an object to remain the same during compile
;; vs. runtime.  We might be able to make this work, as long as we
;; don't give the user direct access to the hash, but at the moment
;; we're going to just not evaluate hash tables at meta-eval time.
(reg:define-struct (suspension operator argvals))

(reg:define-struct (ref contents type))
;; We must distinguish these because they share the same physical
;; representatino as other numbers.
;(reg:define-struct (int64 num))
;(reg:define-struct (double num))

;; NOTE: also uses 'uniontype' and 'tuple' defined in constants.ss

;; This is used to turn scheme procedures back into closures.
(define reflect-msg (gensym "reflect"))

;; This is mutated by the interpreter to track source positions as we
;; descend into the AST.  I hate to see the new interpret-meta pass
;; take on this kind of ugliness.  It's an artifact of the fact that
;; annotations are not properly attached to expression variants (such
;; as primitive applications).
(define current-src-pos #f)

(define dictionary #f) ;; Set below:

; ================================================================================ ;

;; NOTE: a tuple or uniontype will go inside a "plain" wrapper (?):
(define (wrapped? x) 
  (or (plain? x) (streamop? x) (closure? x) (ref? x) (suspension? x)
      ;(tuple? x) ;; Is this right?  what about sigseg?
      ))

(define (stream-type? ty) (and (pair? ty) (eq? (car ty) 'Stream)))
(define (lambda? x) (let ([x (peel-annotations x)])
		      (and (pair? x) (eq? (car x) 'lambda))))
(define (foreign-closure? cl) (and (closure? cl) (not (closure-formals cl))))
(define (make-foreign-closure name includes ty)
  (DEBUGASSERT string? name) 
  (DEBUGASSERT (andmap string? includes))
  (make-closure #f #f #f `(assert-type ,ty (foreign ',name ',includes)) #f ty))

;; Unwraps plain only:
(define (unwrap-plain x) 
  (DEBUGASSERT wrapped? x)
  (if (plain? x) (plain-val x) x))

(define (maybe-wrap x . optional-type)
  (cond 
   [(wrapped? x) x]
   [(procedure? x) (x reflect-msg)]
   [else  
    ;(ASSERT (not (tuple? x))) ;; Can't allow these to not have a type!
    ;(make-plain x #f)
    (if (null? optional-type)
	(make-plain x (unknown-type))
	(make-plain x (car optional-type)))]))

(define (unknown-type . _) `',(unique-name 'ty))

;; Are two meta-values equal?
(define (values-equal? a b)
  (or (and (plain? a) (plain? b)
	   (equal? (plain-val a) (plain-val b)))
      (eq? a b)
      #;
      (and (streamop? a) (streamop? b)
	   ???????????
	   )))

;; This tags a value with a type.
;; This is necessary for not losing type information for constants.
(define set-value-type! 
  (case-lambda 
    [(val type) (set-value-type! val type #f)]
    [(val type poly-binding?)
     ;; Overwrite #f, or merge with existing type:
    (define (fold-in ty1 ty2)  
      (if ty2 
	  (or (types-compat? ty1 ty2) 
	      (error 'set-value-type! " Types were not compatible:\n     ~s ~s\n Value:\n     ~a\n" ty1 ty2 val))
	  ty1))
    (ASSERT type) ;; No point in setting to #f
    ;; Either us, or the caller of this function, must freshen the type variables.
    ;; Currently we handle this right here.
    (set! type (export-type (instantiate-type type)))
    (cond
     [(plain? val) 
      ;; This doesn't respect polymorphic constants:
      (set-plain-type! val (fold-in type (plain-type val)))
      ;; Set once behavior:      
      ;(unless (plain-type val) (set-plain-type! val type))
      ]

     ;; Must recursively handle the insides of the ref also:
     [(ref? val) (set-ref-type! val (fold-in type (ref-type val)))
      (match type
	[(Ref ,elt) (set-value-type! (ref-contents val) elt)])]
     [(closure? val)
      ;; Here we must make the typing behavior of the interpreter
      ;; follow the static type checker.
      
      ;; If a closure value has a "polymorphic binding" (i.e. letrec
      ;; binding to a value expression), then it's type is set once at
      ;; the binding site and "sealed" against further unions.

      ;; [2008.08.07] FIXME: WHAT ARE WE DOING WITH LEFT LEFT LAMBDA?            
      
      ;; Switching back to a "set-once" behavior for all closures:
      (if (arrow-type? (closure-type val))
          ;(and poly-binding? (arrow-type? (closure-type val)))
	  (void);(printf "Rejecting closure type! args ~s ~s\n" (closure-formals val) type)	  
	  (set-closure-type! val (fold-in type (closure-type val))))
      ]
     [(suspension? val)  (void)]
     [(streamop? val) (set-streamop-type! val (fold-in type (streamop-type val)))]
     [else (error 'set-value-type! "not handled yet: ~s" val)]
     )]))

;; Returns the type of the wrapped object, or #f
(define (get-value-type val)
    (cond
     [(plain? val)      (plain-type val)]
     [(ref? val)        (ref-type val)]
     [(closure? val)    (closure-type val)]
     [(suspension? val) #f]
     [(streamop? val)   (streamop-type val)]
     [else (error 'get-value-type "not handled yet: ~s" val)]))


; ================================================================================ ;
;;; Environments

(define (apply-env env v) 
  (ASSERT symbol? v)
  (let* ([x (cadr (ASSERT (assq v env)))]
	 [result (if (box? x) (unbox x) x)])
    (DEBUGASSERT wrapped? result)
    result))
(define (extend-env id* val* env)
  (DEBUGASSERT (curry andmap (lambda (x) (or (wrapped? x) (box? x)))) val*)
  (append (map list id* val*) env))



; ================================================================================ ;
;;; Interpreter

;; This is a hack for manually pinning CPUs.
(define (set-cpu! int strm)
  (set-streamop-params! strm
	    (match (streamop-params strm)
	      [((annotations ,alist ...) . ,rest)
	       `((annotations (cpu-pin . ,(plain-val int)) . ,alist)
		 . ,rest)])))


(define __cast_num (wavescript-language '__cast_num))

;; NOTE: this does not track everything in regiment-primitives, as
;; that may change over time.
(define (build-dictionary!)
  (set! dictionary (make-default-hash-table 500))
  (let ([prims (difference 
		(map car (append regiment-basic-primitives 
				 ;regiment-distributed-primitives
				 wavescript-primitives
				 meta-only-primitives
				 higher-order-primitives
				 regiment-constants
				 ))
		;; Leave out things that aren't real primitives or won't appear at this point:
		(append '()
 		       ;'(foreign foreign_source ptrToArray locdiff locdiff)
			lang_wavescript_prim-exceptions
			)
	       )])
    (let ([vals (wavescript-language `(list ,@prims))])
      (for-each (lambda (sym prim)
		  (hashtab-set! dictionary sym prim))	   
	prims vals)))
  ;(printf "BUILT DICTIONARY!\n")
  )




;; Make a *real* procedure that evaluates a closure.  Thus we can pass
;; it to one of the higher-order WS primitives that's implemented in a
;; separate library (wavescript_sim_library_push.ss).
(define (reify-closure c)
  (ASSERT closure? c)
  (ASSERT (not (foreign-closure? c)))
  ;; This is called within wavescript_sim_library_push.ss:
  (lambda args
    (if (eq? (car args) reflect-msg)
	;; This means that we want to convert back to the transparent closure object.
	c ;(begin (printf "REFLECTING ~a ~a\n" (closure-name c) (closure-type c)) c)
	(begin 
	  (DEBUGASSERT (curry andmap (compose not procedure?)) args)
	  ;; We need to tag the types onto values that we store in the environment.
	  (unwrap-plain
	      (Eval (closure-code c) 
		    (extend-env (closure-formals c) 
				(map (lambda (arg argty)
				       ;; Let's say you map(streamtransformer, list)...
				       ;; That requires passing closures that handle streams.
				       #;
				       (when (streamop? x)
					 (error 'reify-closure "shouldn't try to reify this stream-operator: ~s" c))
				       (let ([wrapped (maybe-wrap arg)])
					 (set-value-type! wrapped argty)
					 wrapped))
				  args (closure-argty* c))
				(closure-env c))
		    #f)))
	))
)

(define uninitialized-variable 'uninitialized-recursively-bound-variable)


;; This evaluates the meta program.  The result is a *value*
;;
;; .param pretty-name -- uses this to hang onto names for the closures
(define (Eval x env pretty-name)
  ;; This will be replaced by something more meaningful.
  (define (streamop-new-name)
;    (printf "STREAMOP NAME, pretty name was: ~s\n" pretty-name)
    (unique-name (or pretty-name 'anonstreamop))
    ;(and pretty-name (unique-name pretty-name))    
    )

  ;; Is name A worthier than B?
  (define (better-name? a b)
    ;; Node: namespace hack!! 
    ;; If we're going to stick with this node, namespace hack... we have to respect that here:
    (define (node-name? str)
      (and (> (string-length str) 5)
	   (eq? (string-ref str 0) #\N)
	   (eq? (string-ref str 1) #\o)
	   (eq? (string-ref str 2) #\d)
	   (eq? (string-ref str 3) #\e)
	   ;(eq? (string-ref str 0) #\_)
	   ))
    (define a-node (node-name? (symbol->string a)))
    (define b-node (node-name? (symbol->string b)))
    (cond
     [(and a-node (not b-node)) #t]
     [(and b-node (not a-node)) #f]
     [(eq? 'anonstreamop b) #t]
     [(= 1 (string-length (symbol->string b))) #t]
     [else #f]))
  (define (prettify-names! names vals)
    (for-each (lambda (name val)
		       (cond
			[(closure? val)			
			 (unless (closure-name val)
			   (set-closure-name! val (unique-name name)))]
			[(streamop? val)
			 ;; override the streamop name from its creation site?
			 ;; [2007.11.12] Trying just overwriting it always... 
			 ;; Is the *last* name a good name?
			 (unless (and (streamop-name val)
				      (not (better-name? name (deunique-name (streamop-name val)))))
;			   (printf "                         NAMING STREAMOP!!!!!! ~s \n"name)			   
			   (set-streamop-name! val (unique-name name)))]

			[else (void)] ;; We don't do anything with other objects.
			;[else (error 'prettify-names! "unhandled case: ~s" val)]
			))
      names vals))

  (unless dictionary (build-dictionary!))
  (let EvalLoop ([x x])
   (match x
    [,v (guard (symbol? v)) 
        (if (regiment-primitive? v)
            (make-plain (hashtab-get dictionary v) (get-primitive-type v))
            (apply-env env v))]

    ;; FIXME: This is incomplete, data structures may also contain poly constants!
    ;; Polymorphic constants: nullseg Array:null '()
    ;['()        (make-plain '() '(List 'a))]
    ;['#()       (make-plain (make-vector) '(Array 'a))]
    ;[Array:null (make-plain (make-vector) '(Array 'a))]
    ;[nullseg (make-plain )]
    [',c (ASSERT (not (tuple? c))) (make-plain c #f)]
    
    [(tuple ,[x*] ...) 
     (make-plain (make-tuple (map unwrap-plain x*))
		 (list->vector (map (lambda (x) (or (get-value-type x) (unknown-type))) x*)))]
    [(tupref ,ind ,len ,[tup])
     (ASSERT (fixnum? ind)) (ASSERT (fixnum? len))
     (ASSERT (plain? tup))
     (maybe-wrap (list-ref (tuple-fields  (plain-val tup)) ind)
		 (if (plain-type tup)
		     (vector-ref (plain-type tup) ind)
		     #f))]

    ;; As with tuples, we don't store the type metadata in the value
    ;; itself, (including in the values within the data structure),
    ;; instead we put the type info in the wrapper.
    ;[(empty-wsrecord) (make-plain (empty-wsrecord) #f)]
    [(wsrecord-extend ',nm ,[val] ,[rec]) 
     (define newty
       (let ([valty (or (get-value-type val) `',(make-tvar))]
	     [recty (get-value-type rec)])
	 ;(unless recty (printf "extend: No recty! ~a\n" rec))	 
	 (match recty
	   [(Record ,origrow) `(Record (Row ,nm ,valty ,origrow))]
	   [#f                `(Record (Row ,nm ,valty ',(make-tvar)))])))
     (make-plain (wsrecord-extend nm (unwrap-plain val) (plain-val rec)) newty)]
    [(wsrecord-select ',nm ,[rec])
     ;; This is very expensive:
     (define recty (get-value-type rec))     
     ;(unless recty (printf "select: No recty! ~a\n" rec))
     (maybe-wrap (wsrecord-select nm (plain-val rec))
		 (if recty
		     (let ([cell (make-tcell)]
			   [cell2 (make-tcell)])
		       (tcell-set! cell2 `(Row ,nm ,cell ,(make-tcell)))
		       (types-equal! (instantiate-type recty) `(Record ,cell2) "" "")
		       (export-type (tcell-get cell)))))]
    [(wsrecord-restrict ',nm ,[rec])
     (define recty (get-value-type rec))
     ;(unless recty (printf "restrict: No recty! ~a\n" rec))
     ;(warning 'rec "wsrecord-restrict should probably propagate this type info")     
     (make-plain (wsrecord-restrict nm (plain-val rec))
		 (if recty
		     (match recty
		       [(Record ,row)
			`(Record 
			  ,(let loop ([row row])
			     (match row
			       [(Row ,name ,ty ,tail)
				(if (eq? name nm) tail
				    `(Row ,name ,ty ,(loop tail)))]
			       [,oth (error 'interpret-meta:Eval "tried to restrict record, but it had a type which was missing label ~a: ~a"
					    nm recty)])))])
		     #f))]

    ;; UGLINESS
    ;; FIXME: THE BELOW SHOULD BE ABLE TO REPLACE THESE TWO CASES:
    ;; ----------------------------------------
    ;; Here's a hack to keep those type assertions on the readFiles ......
    [(assert-type ,ty ,e)
     (guard (let ([x (peel-annotations e)])
	      (and (pair? x) (memq (car x) '(readFile)))))
     (let ([op (Eval e env pretty-name)])
       (set-streamop-type! op ty)
       op)]
    ;; This duplicates the above case for foreign entries.
    [(assert-type ,ty ,e)
     (guard (let ([x (peel-annotations e)])
	      (and (pair? x) (memq (car x) '(foreign foreign_source)))))
     (match (peel-annotations e)
       [(,op ,name ,includes)
	(let ([name     (Eval name env #f)]
	      [includes (Eval includes env #f)])
	  (ASSERT (plain? name))
	  (ASSERT (plain? includes))
	  (match op
	    ;; This is a closure representing a foreign function.
	    [foreign
	     (make-foreign-closure (plain-val name) 
				   (plain-val includes) ty)]
	    ;; This is a streamop representing a foreign source.
	    [foreign_source
	     (make-streamop 
	      (string->symbol (string-append "foreign_" (unwrap-plain name)))
	      'foreign_source (list name includes) '() ty)]
	    )
	  )])]

    ;; FIXME: THIS SHOULD MAKE THE CASES ABOVE REDUNDANT:
    ;; [2008.04.05] This needs to replicate the hack for assert-type/cast_num:
    [(assert-type ,ty ,[val])      
     (if (and (plain? val) (memq ty num-types))
	 (make-plain (__cast_num #f ty (plain-val val)) ty)
	 (begin (set-value-type! val ty) val))]

    [(,frgn . ,_) (guard (memq frgn '(foreign foreign_source)))
     (error 'interpret-meta:Eval "foreign entry without type assertion: ~s" (cons frgn _))]
    ;; We leave this alone:
;    [(foreign ,[x*] ...) (cons 'foreign x*)]
    ;; ----------------------------------------

    
    ;; [2007.11.07] HACKS for manually pinning to different CPUs:
    [(SETCPU ,[int] ,[strm])
     (ASSERT streamop? strm)
     (ASSERT fixnum? (plain-val int))
     (set-cpu! int strm)
     strm]

    ;; Assumes no cycles!!
    [(SETCPUDEEP ,[int] ,[strm])
     (ASSERT streamop? strm)
     (ASSERT fixnum? (plain-val int))
     (let loop ((x strm))
       (set-cpu! int x)
       (for-each loop (streamop-parents x)))
     strm]

    ;; Unionlist is a tad different because it takes a list of streams:
    [(unionList ,[ls])      
     (ASSERT (plain? ls))
     (ASSERT (andmap streamop? (plain-val ls)))
     ;(Eval `(unionN ,@(plain-val ls)) env pretty-name)
     (make-streamop (streamop-new-name) 'unionN `((annotations)) (plain-val ls) #f)]
    [(unionN ,annot ,[args] ...) (make-streamop (streamop-new-name) 'unionN `(,annot) args #f)]


    [(_merge ,annot ,[s1] ,[s2])
     (make-streamop (streamop-new-name) '_merge `(,annot) `(,s1 ,s2) #f)]
    
    [(readFile ,annot ,[f] ,[m] ,[s])
     (make-streamop (streamop-new-name) 'readFile `(,annot ,f ,m) `(,s) #f)]

    [(timer ,annot ,[t])
     (make-streamop (streamop-new-name) 'timer `(,annot ,t) '() #f)]

    
    [(iterate ,annot ,[f] ,[s])
     (match (regiment-primitive? 'iterate)
       [((,annotty ,fty ,sty) (Stream ,return))
        (for-each set-value-type! `(,f ,s) `(,fty ,sty))
        (let ([parents (apply append (map (lambda (x t) (if (stream-type? t) (list x) '())) `(,f ,s) `(,fty ,sty)))]
              [params  (apply append (map (lambda (x t) (if (stream-type? t) '() (list x))) `(,f ,s) `(,fty ,sty)))])
          (ASSERT (curry andmap streamop?) parents)
          (ASSERT (curry andmap (compose not streamop?)) params)
          (make-streamop (streamop-new-name) 'iterate (cons annot params) parents #f))])]

    [(,streamprim ,[x*] ...)
     ;(guard (assq streamprim wavescript-stream-primitives))
     (guard (stream-primitive? streamprim))
     (match (regiment-primitive? streamprim)
       [(,argty* (Stream ,return))
	(for-each set-value-type! x* argty*) ;; Set all the types of arguments (some may be integers).
	;; This splits the stream from the non-stream components.
	(let ([parents (apply append (map (lambda (x t) (if (stream-type? t) (list x) '())) x* argty*))]
	      [params  (apply append (map (lambda (x t) (if (stream-type? t) '() (list x))) x* argty*))])
	  (ASSERT (curry andmap streamop?) parents)
	  (ASSERT (curry andmap (compose not streamop?)) params)
	  (make-streamop (streamop-new-name) streamprim params  parents #f))])]

    [(if ,[t] ,c ,a) 
     (ASSERT (plain? t))
     (Eval (if (plain-val t) c a) env pretty-name)]

    ;; [2007.09.19] Is this necessary if we've converted to left-left lambda?
    ;; [2007.10.02] Actually, we should handle left-left lambda for efficiency.
    ;; TEMPTOGGLE
    #;
    [(let ([,lhs* ,ty* ,[rhs*]] ...) ,bod)
     (for-each (lambda (lhs val)
		 (when (closure? val)
		   (prinft "   YEEEEEEEEEEEEEAAAAAAAAAAH: ~s\n" lhs)		   
		   )		 
		 )
       lhs* rhs*)
     (Eval bod (extend-env lhs* rhs* env) pretty-name)]

    ;; This is a letrec* w/ let-'n-set semantics 
    [(letrec ([,lhs* ,ty* ,rhs*] ...) ,bod)
     ;(printf "Letrec bind ~s ~s\n" lhs* ty*)
     (let* ([cells (map (lambda (_) (box uninitialized-variable)) rhs*)]
	    [newenv (extend-env lhs* cells env)])
       (for-each (lambda (cell lhs ty rhs)
		   (set-box! cell (Eval rhs newenv pretty-name))
		   (when (closure? (unbox cell)) 
		     (prettify-names! (list lhs) (list (unbox cell))))
		   ;; This is only a polymorphic binding if it meets the value restriction:
		   (set-value-type! (unbox cell) ty (value-expression? rhs))
		   )
	 cells lhs* ty* rhs*)
       (Eval bod newenv pretty-name))]

    [(lambda ,formal* ,ty* ,bod)      
     ;; FIXME: WE SHOULD NOT BE THIS SHORT ON TYPE INFO FOR OUR CLOSURES!!!
     ;(make-closure #f formal* bod env `(,@ty* -> ',(unique-name "anytype")))
     (make-closure #f formal* ty* bod env (unknown-type))
     
     ]
 
    [(Mutable:ref ,[x]) (make-ref x #f)]
    [(deref ,[x])       (ref-contents x)]
    [(set! ,[v] ,[rhs]) 
     (set-ref-contents! v rhs)
     (make-plain (void) '#())]
    
#;
    [(Array:makeUNSAFE ,_) 
     (error 'interpret-meta:Eval 
	    "Don't use Array:makeUNSAFE at meta-evaluation! ~s"
	    `(Array:makeUNSAFE ,_))]

    ;; TEMP HACK
    [(floatToInt ,[x])
     (ASSERT (plain? x))
     (make-plain (exact (floor (plain-val x))) 'Int)]

    ;; HACK: need to finish treating hash tables:
    [(HashTable:make ,[len]) 
     (set-value-type! len 'Int)
     (make-suspension 'HashTable:make (list len))]

    ;; Also, no compile-time representation for *pointers*:
    [(ptrMakeNull) (make-suspension 'ptrMakeNull '())]

    ;; This requires a bit of sketchiness to reuse the existing
    ;; implementation of this functionality within wavescript_sim_library_push
    [(,prim ,arg* ...) (guard (regiment-primitive? prim))
     (define x* (map EvalLoop arg*))
     (DEBUGASSERT (not (assq prim wavescript-stream-primitives)))     


     ;; It's very unclear how far we want to take this "suspension"
     ;; business, and we need to figure out the semantics.  If we want
     ;; to allow frozen foreign apps at metaprogram time then what do
     ;; we do when the metaprogram tries to use the value?  Add to that suspension?
     ;(if (ormap suspension? x*) (make-suspension ))
     (match (regiment-primitive? prim)
       [((,argty* ...) ,retty)
	;(for-each set-value-type! x* argty*) ;; Necessary?
	;; This is probably also rather slow.
	(let ([raw 
	       ;; Can't write/read because it will contain procedures and streamops.
	       (parameterize ([simulator-write-sims-to-disk #f])
		 ;; First, I applied wavescript-language to just the prim
		 ;; name, rather than the whole app.  That actually made
		 ;; performance a bit worse.

		 ;; This assumes it's a function and not syntax:
		 (apply (ASSERT (hashtab-get dictionary prim))
					;(wavescript-language prim)
			;; We're willing to give it "plain" vals.
			;; Refs should not be passed first class.
			;; And closures/streams remain opaque:					
			(map (lambda (x arg)
			       (ASSERT (not (ref? x)))				   
			       (ASSERT (not (suspension? x)))
			       (cond 
				[(plain? x)   (plain-val x)]
				[(closure? x) (reify-closure x)]
				[(streamop? x) x] ;; This shouldn't be touched.
				[(eq? x uninitialized-variable)
				 (error 'Eval "attempt to pass unitialized recursively bound variable to primitive ~a: ~a" prim 
					arg)]
				[else (error 'Eval "unexpected argument to primiitive ~a: ~s" prim x)]))			  
			  x* arg*)))])
	  
	  ;(ASSERT (not (eq? raw (void))))
	  ;(ASSERT (not (procedure? raw)))
	  (let ([final (maybe-wrap raw)])
	    (set-value-type! final retty) ;; Necessary?
	    final))])]
    ;; [2007.09.14] Supporting sums:
    [(construct-data ,tag ,[val]) (make-plain (make-uniontype tag val) #f)]

    [(app ,[f] ,[e*] ...)
     (ASSERT closure? f)
     (if (foreign-closure? f)
	 ;; Foreign app is suspended for later:
	 (begin
	   (when (>= (regiment-verbosity) 2)
	     (printf "EXPERIMENTAL: making meta-suspension for foreign app: ~s\n" f))
	   (make-suspension f e*))

	 ;; Native closure:
	 (begin 
	   ;; Do PRETTY naming:
	   (prettify-names! (closure-formals f) e*)
	   ;; Assert that the args have the right type:	   
	   (for-each set-value-type! e* (closure-argty* f))
	   (let ([result		     
		     (Eval (closure-code f)
			   (extend-env (closure-formals f) e*
				       (closure-env f))
			   (or (closure-name f) pretty-name))])
		;;;; [2008.08.06] ;(set-value-type! result retty) 
		result
		)))]
    
    [(for (,indvar ,[st] ,[en]) ,bod)
     (ASSERT (plain? st))
     (ASSERT (plain? en))
     (let ([end (plain-val en)])       
       (do ([i (plain-val st) (fx+ i 1)])
	   ((> i end) (make-plain '#() '#()))
	 (Eval bod (extend-env (list indvar) (list (make-plain i 'Int)) env) pretty-name)))]
    [(while ,tst ,bod)     
     (let loop ()
       (let ([test (Eval tst env pretty-name)])
	 (ASSERT (plain? test))
	 (when (plain-val test)
	   (Eval bod env pretty-name)
	   (loop))))
     (make-plain '#() '#())]
    [(begin ,x* ... ,last) 
     ;; Side effects are modeled by... actual side effects!
     (begin (for-each (lambda (x) (Eval x env pretty-name)) x*)
	    (Eval last env pretty-name))]

    ;; TODO: Need to add the data *constructors*... this is incomplete currently.
    [(wscase ,[x] (,pat* ,[rhs*]) ...) 
     (let* ([alts (map list pat* rhs*)]
	    [case (assq (uniontype-tag (plain-val x)) alts)]
	    [clos (cadr case)])
       (Eval (closure-code clos) 
	     (extend-env (closure-formals clos) 
			 (list (uniontype-val (plain-val x)))
			 (closure-env clos))
	     pretty-name))]

    ;; Handles assert-type, src-pos...
    ;; FIXME: Should attach source info to closures:
    [(src-pos ,p ,e) (fluid-let ([current-src-pos p]) (Eval e env pretty-name))]
    [(,annot ,_ ,[e]) (guard (annotation? annot)) e]
  )))





; ================================================================================ ;
;;; Marshaling Stream Values

;; We may hit the same value repeatedly.  This memoizes marshal:
;; FIXME: UNFINISHED: DON'T ADD TO IT YET:
(define marshal-cache (make-parameter 'mc-uninit))

;; This marshals the resulting stream-operators.
;; The result is a code for a stream-graph.
(define (Marshal val)
;  (display-constrained "  MARSHALLING: " `[,val 100] "\n")
  (cond

#;
   [(hashtab-get (marshal-cache) val)
    => (match-lambda ((,name . ,code))
	 (printf "\n  Marshal: HAD MARSHALLED VALUE IN CACHE!!!\n")
	 code)]

   ;; EXPERIMENTAL:
   [(suspension? val) 
    (cond
     [(foreign-closure? (suspension-operator val))
      (match (closure-code (suspension-operator val))
        [(assert-type ,ty (foreign ',name ',includes))
	 (when (string=? name "")
	   (error 'interpret-meta "cannot have application of foreign function with no name! ~s" val))	 
         `(foreign-app ',name
                       ,(Marshal-Foreign-Closure (suspension-operator val))
                       ,@(map Marshal (ASSERT (suspension-argvals val))))
         ])]
     [(regiment-primitive? (suspension-operator val))
      `(,(suspension-operator val) ,@(map Marshal (ASSERT (suspension-argvals val))))]
     [else (error 'interpret-meta:Marshal "unhandled suspended op: ~s" (suspension-operator val))]
     )]

   [(plain? val)   (Marshal-Plain val)]
   [(closure? val) (Marshal-Closure val)]
   [(streamop? val)
    (let (;[covered (make-default-hash-table 100)]
          [acc '()])
    ;; Here we trace back through the stream graph:
    ;; Could possibly replace this with a judicious use of memoization (marshal-cache).
    ;; (Plus maybe a topological sort of the bindinggs at the end.)
    ;;    
    (define allops
      (let loop ([ops (list val)] [acc '()] [covered '()])
	;; covered is a list of streamop-names that we've already included
	;; FIXME: USE A HASH TABLE!
        (cond
         [(null? ops) acc] ;; Return all the streamops encountered.
         [(memq (streamop-name (car ops)) covered)
          (loop (cdr ops) acc covered)]
         [else 
          (loop (append (cdr ops)
                        (filter (lambda (op) (not (memq (streamop-name op) covered)))
                          (streamop-parents (car ops))))
                (cons (car ops) acc)
                (cons (streamop-name (car ops)) covered)
                )])))
    (DEBUGASSERT list-is-set? (map streamop-name allops))
    (ASSERT allops)

    ;; Have to topo-sort the letrec bindings to assure it runs in a call-by-value evaluation:       
    (let ([binds (topo-sort-bindings (map streamop-name allops) 
				     (map unknown-type allops)
				     (map Marshal-Streamop allops))])      
      `(letrec ,binds ,(streamop-name val)))
    )]
   [else (error 'Marshal "cannot marshal: ~s" val)]))

(define (Marshal-Streamop op)
  (define arglist
    ;; This is more than a bit silly, I have to recombine the argument list from params/parents.
    ;; I shouldn't split params/parents in the first place.
    (let loop (
               ; FIXME: should probably move this somewhere better
               [argty*
                (case (streamop-op op)
                  [(unionN)
                   (cons '(List Annotation)
			 ;; All stream types. Contents unimportant:
                         (map (lambda (_) `(Stream ',(unique-name "anytype"))) (streamop-parents op)))]
                  [else
                   (car (ASSERT (regiment-primitive? (streamop-op op))))])]                                          
               [params  (streamop-params op)]
               [parents (streamop-parents op)])
      (cond
       [(null? argty*) '()]
       [(equal? (car argty*) '(List Annotation))
        (cons (car params) (loop (cdr argty*) (cdr params) parents))] ;; a bit of a hack, for boxes with annotations
       [(stream-type? (car argty*))
        (cons (streamop-name (car parents))
              (loop (cdr argty*) params (cdr parents)))]
       [else 
        (let ()
          (DEBUGASSERT (or (plain? (car params)) (closure? (car params))))
          (cons (Marshal (car params)) 
                (loop (cdr argty*) (cdr params) parents)))]))) ;; End Silliness
  ;; If available, let's add the name to the annotation list.
  (define nameadded
    (match arglist
      ;; This isn't that safe:
      [((annotations ,alist ...) ,otherargs ...)
       ;; Don't replace a name that's already there:
       (if (and (streamop-name op) (not (assq 'name alist)))
	   `((annotations (name ,(streamop-name op)) ,@alist) ,@otherargs)
	   arglist)]
      [,_ arglist]
      ))
  ;; Produce primitive application syntax:
  (define default (cons (streamop-op op) nameadded))
;  (display-constrained "   MARSHALLING STREAMOP: " `[,op 100] "\n")
  (if (streamop-type op)
      `(assert-type ,(streamop-type op) ,default)
      default))

(define (Marshal-Plain p) 
  (ASSERT (plain? p))
  (Marshal-Value (plain-val p) (plain-type p)))

(define (Marshal-Value val ty) 
  (define (box-doubles val ty) 
    (match ty
      [Double (if (double? val) val (make-double val))]
      [(List ,elt) (map (lambda (x) (box-doubles x elt)) val)]
      [(Array ,elt) (vector-map (lambda (x) (box-doubles x elt)) val)]
      [#(,fld* ...) (make-tuple (map (lambda (x ty) (box-doubles x ty)) (tuple-fields val) fld*))]
      [(HashTable . ,_) (error 'Marshal-Plain:box-doubles "Can't handle hash tables yet")]
      [,else val]))
    (cond
     [(hashtab? val) (error 'Marshal-Plain "hash table marshalling unimplemented")]
     ;; Going to wait and get rid of sigseg constants in remove-complex-constants:
#;
     [(sigseg? val) (if (fx= 0 (vector-length (sigseg-vec val)))
			'nullseg
			(error 'Marshal-Plain "non-null sigseg marshalling unimplemented"))]
;     [(int64? val)  (int64-num val)]
;     [(double? val) (double-num val)]

     [(tuple? val)
      `(tuple . ,(map (lambda (x ty) (if (wrapped? x) (Marshal x) (Marshal-Value x ty)))
		   (ASSERT (tuple-fields val))
		   ;; FIXME FIXME FIXME HMM: NOT SURE HOW THIS GETS TO #F:
		   ;(match ty [#(,t* ...) t*])
		   (vector->list (ASSERT ty))
		   ))]

     [(wsrecord? val)
      (match ty
	[(Record ,rows)
	 (define typairs
	   (match rows
	     [#() '()]
	     [(Row ,nm ,ty ,[rest]) (cons (cons nm ty) rest)]))
	 (match (wsrecord-pairs val)
	   [() '(empty-wsrecord)]
	   [((,nm . ,vl) . ,[rest])
	    `(wsrecord-extend ',nm 
			      ,(Marshal-Value vl (assq nm typairs))
			      ,rest)])])]

     [(uniontype? val)
      `(construct-data ,(uniontype-tag val)
		       ,(Marshal (uniontype-val val)))]

     [(timebase? val) `(Secret:newTimebase ',(timebase-num val))]

     ;; HACK: all ints become gints!!!
     ;; Subsequent type checking should clean it up...
     ;[(and (integer? val) (exact? val)) `(gint ',val)]
     [;(number? val)
      (and (integer? val) (exact? val))
      ;(ASSERT (plain-type p))
      (unless ty (error 'Marshal-Plain "No type for this int ~s" val))
      ;(ASSERT (compose not polymorphic-type?) ty)
      #;
      (if (polymorphic-type? ty)
	  `(gint ',val)
	  `(assert-type ,ty (gint ',val)))
	  
      `(gint ',val)
      ]

     ;; ANNOYING, FIXME: We don't have a direct syntax for double
     ;; constants, as a stopgap we could cast them from float
     ;; constants (unless they're too big).
     ;[(eq? ty 'Double) `(assert-type Double ',val)]
     ;[(eq? ty 'Double) `(assert-type Double (cast_num ',val))]
     [(eq? ty 'Double) `',(box-doubles val 'Double)]
     ;[(eq? ty 'Double) `(__cast_num Float Double ',val)]

     [(double? val) (error 'Marshal-Plain "Inconsistent value and type: ~s with type ~a" val ty)]

     [(list? val)
      ;; This value is thrown away:
      ;; TODO, do a proper check to make sure there are no streamops/closures
      (DEBUGMODE
       (match (or ty (type-const val))
	 ;; Going through just to make sure there are no errors:
	 [(List ,elt) (for-each (lambda (x) (Marshal-Value x elt)) val)]))
      ;`(assert-type ,ty ',val)
      `',(box-doubles val ty)
      ]

     [(vector? val)
      ;; FIXME, should COMPRESS here if possible.
      ;`(assert-type ,ty ',val)
      `',(box-doubles val ty)
      ]

     ;; FIXME: Reorganize so this isn't necessary:
     ;; We are currently hitting this case when our tuples and records contain closures.
     [(closure? val) (Marshal-Closure val)]

     ;; FIXME: GET RID OF THIS FALLTHROUGH!
     [else
;      (ASSERT (not (streamop? val))) (ASSERT (not (closure? val))) 
;      (ASSERT (not (ref? val)))      (ASSERT (not (suspension? val)))
      
      (unless (or (string? val) (char? val) (flonum? val) (cflonum? val) (boolean? val)
		  (sigseg? val)
		  (eq? val (void)))
	(error 'Marshal-Plain "invalid value: ~s" val))
      ;(DEBUGASSERT complex-constant? val)
      `',val]
     ))


;; Foreign closures are simple... they become foreign entries.
(define (Marshal-Foreign-Closure fcl)
  (ASSERT (foreign-closure? fcl))
  (closure-code fcl)
  )


(define (print-return fun)
  (lambda args
    (let ([result (apply fun args)])
      (printf "Result: ~s\n" result)
      result)))


(define Marshal-Closure
  (lambda (cl)    
    (ASSERT (not (foreign-closure? cl)))

  ;; This loop accumulates a bunch of bindings that cover all the
  ;; free variables of the closure (and, transitively, any
  ;; closures reachable from the input closure).  These bindings
  ;; fall into two categories.  Mutable bindings are only allowed
  ;; to be accessed from a single closure.  Immutable bindings may
  ;; float up to become global bindings.  (And thus not duplicate
  ;; code between multiple iterates.)
  (let marshloop ([code (closure-code cl)]       ;; Code to process
		  [fv   (closure-free-vars cl)]  ;; free vars to marshal
		  [state '()]                    ;; mutable state for this closure
		  [globals '()]                  ;; immutable global bindings
		  [env (closure-env cl)]         ;; environment from which to marshal
		  )
    (DEBUGASSERT list-is-set? fv)
    (if (null? fv)
	;; We're done processing the environment, produce some code:
	(let* ([bod `(lambda ,(closure-formals cl) 
		       ,(map unknown-type (ASSERT (closure-formals cl))) ,code)]
	       [newbod bod]
	       ;; TODO: we don't discover sharing yet...
	       [binds (append globals state)])
	  (DEBUGASSERT list-is-set? (map car binds))

	  (if (null? binds) newbod `(letrec ,binds ,newbod)))
	(let ([val (apply-env env (car fv))])
	  (cond
#;
	   [(hashtab-get (marshal-cache) val)
	    => (match-lambda ((,name . ,code))
		 (printf "\n  Marshal-Closure: HAD MARSHALLED VALUE IN CACHE!!!\n")
		 code)]

	   [(plain? val) 
	    (marshloop code (cdr fv) state
		  (cons (list (car fv) (unknown-type) (Marshal-Plain val)) globals)
		  env )]

	   ;; We redirect these to 'Marshal' which will give us some kind of app syntax.
	   [(suspension? val)
	    (marshloop code (cdr fv) state
		       (cons (list (car fv) (unknown-type) (Marshal val)) globals)
		       env)]

	   ;; This is definitely part of the state:
	   [(ref? val)
	    ;; FIXME: Need to scan for shared mutable values.
	    (marshloop code (cdr fv)
		  (cons (list (car fv) (unknown-type) 
			      `(Mutable:ref ,(Marshal (ref-contents val))))
			state)
		  globals env)]

	   [(foreign-closure? val)
	    (match (closure-code val)
	      [(assert-type ,ty (foreign ',name ',includes))
	       (match ty
		 [(,argty* ... -> ,res)
		  (let ([formals (list-build (length argty*) (lambda (_) (unique-name 'arg)) )])
		    (define newname (unique-name (car fv)))
		    (when (string=? name "")
		      (error 'interpret-meta "cannot have foreign declaration with no name! ~s" val))
		    ;; This just turns into the '(foreign name includes)' expression.
		    (marshloop code (cdr fv) state
			  ;; The foreign binding just turns into the '(foreign name includes)' expression:
			       (cons* `(,newname ,(unknown-type) ,(closure-code val))
				      ;; [2008.08.21] Actually, we don't really need the "foreign" entries
					;; so much anymore, except that they store the includes?
				      `(,(car fv) ,(closure-type val)
					;; Change any references to be foreign-apps...
					(lambda ,formals ,argty*
						(foreign-app ',name ,newname ,@formals)))
				      globals)
			       env
			       ))])])]

	   ;; FIXME:
	   ;; Free variables bound to closures need to be turned back into code and inlined.
	   ;; We can either stick them in the code now, or leave them up top as globals.
	   ;; We used to do the former, but now [2008.08.06] do the latter.
	   [(closure? val)
	    ;; First, a freshness consideration:
	    ;; FIXME: We could be better here about catching duplicated bindings.
	    ;; (Instead, by renaming we will give them distinct bindings.)
	    (let-values ([(newcode newfree env-slice) (dissect-and-rename val)])	      
	      (DEBUGASSERT null? (intersection (map car env-slice) (map car env)))
	      (marshloop code 		 
		    ;; We also merge the relevent parts of the closure's environment with our environment:
		    (union newfree (cdr fv))
		    state 
		    ;globals
		    ;; [2008.08.06] Now putting the lambdas in the globals:
		    (cons (list (car fv) 
			    (closure-type val)
			   `(lambda ,(closure-formals val)
			      ,(match (closure-type val) [(,argty* ... -> ,_) argty*])
			      ,newcode))
			  globals)
		    (append env-slice env)))]
	   
	   [(streamop? val) 
	    (error 'Marshal-Closure "cannot have stream value occuring free in a marshaled closure: ~s" val)]

	   [else (error 'Marshal-Closure "unhandled-case, could not process: ~s" val)]
	   ))))))

(define (closure-free-vars cl) 
  ;(ASSERT (not (foreign-closure? cl)))
  (if (foreign-closure? cl) '()     
      (list-rem-dups 
	(difference (core-free-vars (closure-code cl)) 
		    (closure-formals cl)))))

;; Renames the free vars for a closure.
;; Returns three value:
;;   1: A new body.
;;   2: The newly named free variables.
;;   3: A new environment containing just the bindings for the (renamed) free vars.
(define (dissect-and-rename cl)
  (DEBUGASSERT (not (foreign-closure? cl)))
  (let* ([fv    (closure-free-vars cl)]
	 [newfv (map unique-name fv)]
	 [newcode (core-substitute fv newfv (closure-code cl))]
	 [oldenv (closure-env cl)]
	 [oldslice (map (lambda (v) (apply-env oldenv v)) fv)]
	 [newslice (map list newfv oldslice)])
    ;(printf "OLDFREE ~s NEWFREE ~s\n" fv newfv )
    
    #;
    (unless (set-equal? (list->set newfv)
			(list->set (difference (core-free-vars newcode) (closure-formals cl))))
      (printf "HAVING ISSUES WITH DISSECT-AND-RENAME\n")
      (inspect newfv)
      (inspect (core-free-vars newcode))
      (inspect (closure-formals cl))
      (inspect (list-rem-dups (difference (core-free-vars newcode) (closure-formals cl))))
      )
    (DEBUGASSERT (set-equal? (list->set newfv)
			     (list->set (difference (core-free-vars newcode) (closure-formals cl)))))
    (values newcode newfv newslice)))








; ================================================================================ ;
;;; Basic inlining inside stream kernels.

;; After meta-program evaluation, what normalization do we want to do to the stream kernels?
;; Currently, we'd like to get rid of applications.  This function exhaustively inlines.
;;
;; [2008.01.22] Currently, expanding this to inline *simple* constants also.
(define (do-inlining e)
  (define (Expr e subst)
    (core-generic-traverse
     (lambda (x fallthru)
       (let loop ([x x])
	 (match x 
	 ;; If the variable is bound to a lambda, here we inline it.
	 [,v (guard (symbol? v))
	     (let ([ent (assq v subst)])
	       (if ent 
		   ;; We must continue processing the inlined function:
		   (loop (caddr ent))
		   v))]
	
	 ;; [2008.03.27] We process the rhs first.  Once those
	 ;; substitutions are finished, we see if we can carry them
	 ;; along further through the new bindings.
	 [(,lett ([,lhs* ,ty* ,[rhs*]] ...) ,bod) (guard (memq lett '(let letrec lazy-letrec)))
	  (define binds (map list lhs* ty* rhs*))
	  (define (side-effect-free? x)
	    (match x
	      [,var (guard (symbol? var)) #t]
	      [,const (guard (simple-constant? const)) #t] ;; for tupref etc
	      [(lambda . ,_) #t]
	      [(,lett ([,lhs* ,ty* ,rhs*] ...) ,[bod]) (guard (eq-any? lett 'let 'letrec))
	       (and bod (andmap side-effect-free? rhs*))]
	      [(begin . ,e*) (andmap side-effect-free? e*)]
	      [(if ,a ,b ,c) (and (side-effect-free? b) (side-effect-free? c) (side-effect-free? a))]
	      [(quote ,datum) #t]
	      [(,prim ,[args] ...) (guard (regiment-primitive? prim))
	       (if (assq prim wavescript-effectful-primitives)
		   #f
		   (andmap (lambda (x) x) args))]
	      [(,annot ,a ,[e]) (guard (annotation? annot)) e]
	      [(set! . ,_) #f]
	      ;; This is assumed to side-effect:
	      [(foreign-app . ,_) #f]
	      ;; These had better not be side efffect free!!:
	      [(while ,test ,[bod]) (and (side-effect-free? bod) test)]
	      [(for (,_ ,[st] ,[en]) ,[bod]) (and st en bod)]

	      ;; TODO: if we have the code for the function, we could perhaps answer this question.
	      ;; But for now we conservatively say no:
	      [(app ,rator ,rands ...) #f]	      

	      [,oth (error 'side-effect-free? "we forgot to handle this: ~s" oth)]))
	  
	  ;(define (lambind? b)  (lambda? (caddr b)))
	  ;; [2008.07.10] Expanding this to allow non-side effecting let's:
	  
	  (define (inlinable-rhs? ty rhs)
	    (match (peel-annotations rhs)
	      [',c (simple-constant? c)] 
	      [(cast_num ,[e]) e] ;; [2008.07.10] Allowing this to inline doubles also
	      [(lambda . ,_)
	       ;; [2008.08.06] Let's back away and try to NOT inline
	       ;; monomorphic closed, functions.	      	       

	       ;; The next step would be to split polymorphic
	       ;; functions into monomorphic ones without complete
	       ;; inlining.
	       (or (ws-full-inline)
		   (match ty
		     [(,argty* ... -> ,retty)		  
		      #;
		      (printf "Considering lambda: poly ~a higher ~a freevars ~a    ~a \n"			      
			      (polymorphic-type? ty)
			      (or (type-containing-arrow? retty)
				  (ormap type-containing-arrow? argty*))
			      (not (null? (core-free-vars (cons 'lambda _))))
			      ty)
		      (or (polymorphic-type? ty)
			  (type-containing-arrow? retty)
			  (ormap type-containing-arrow? argty*)
			  (not (null? (core-free-vars (cons 'lambda _)))))]))]

	      [(let ([,lhs* ,ty* ,rhs*] ...) ,[bod]) (guard (andmap side-effect-free? rhs*)) 
	       bod]
	      [(,annot ,_ ,[e]) (guard (annotation? annot)) e]
	      [,_ #f]))
	  (define (inlinable-bind? b)  (inlinable-rhs? (cadr b) (caddr b)))
	  (define-values (tosubst remainder) (partition inlinable-bind? binds))
	  ;; This is a hack that depends on unique naming.  That's
	  ;; how we handle let in the same way as letrec.  The lhs*
	  ;; simply won't occur in the rhs* for a let..
	  (if (null? tosubst)
	      (begin 
		;(printf "   (not subst) ~s\n" (map deunique-name (map car remainder)))
		;(if (memq 'pred (map deunique-name (map car remainder))) (inspect remainder))
		;(fallthru `(,lett ,remainder ,bod))
		`(,lett ,remainder ,(loop bod))
		)
	      ;; This is an inefficent hack, but we loop through again just to change the subst.
	      (begin
		;(printf " Substituting ~s ~s\n" (map car tosubst) (map car remainder))
		(Expr `(,lett ,remainder ,bod) (append tosubst subst))))]

	 ;; [2007.08.30] Adding basic eta-reduction also.
	 ;; FIXME: Make sure this can't break an iterate's special syntactic structure.
	 #;
	 [(lambda ,args (app ,[rator] ,args2 ...))
	  (guard (equal? args args2))
	  rator]

	 ;; "Left left lambda"
	 ;; Evaluate the rator first so it has the chance to turn into a lambda.
	 [(app ,[rator] ,[rands] ...)	  
	  (let loop ([rator (peel-annotations rator)])
	    (match rator
	      [(lambda ,formals ,types ,bod) ;[do-inlining -> bod]
	     ;; Convert to a let:
	     ;; Then we must put it back through the inliner.
	     ;; Those let RHS's might have some more inlining to do.
	     (do-inlining		  
	      `(let ,(map list formals types rands) ,bod))]
	    ;; [2008.07.10] Considering:
	      [(let ,binds ,lamapp)  ; ([,lhs* ,ty* ,rhs*] ...)
	     ;(guard (andmap side-effect-free? rhs*))
	       `(let ,binds ,(loop (peel-annotations lamapp)))]
	      [,_ 
	     ;		(printf "FAILED TO EVAL RATOR TO LAMBDA: ~s\n" `(app ,rator ,@rands))
	       `(app ,rator ,@rands)]
	      ))]

	 ;; Let's do some DCE (dead code elimination) while we're at it:
	 [(if ,[test] ,a ,b)
	  (match (peel-annotations test)
	    ['#f (loop b)]
	    ['#t (loop a)]
	    [,else `(if ,test ,(loop a) ,(loop b))])]

	 [,oth (fallthru oth)])))
     (lambda (ls k) (apply k ls))
     e))
  (Expr e '()))

; ================================================================================ ;
;;; TODO: Lift constants and check sharing.









; ================================================================================ ;
;;; Entrypoint and Unit Tests

(define-pass interpret-meta 
    [OutputGrammar static-elaborate-grammar]
    [Expr (lambda (x fallthru) 
	    (define-syntax maybtime
	      (syntax-rules ()
		[(_ e) 
		 ;(if (<= (regiment-verbosity) 0) e (time e))
		 (if (>= (regiment-verbosity) 2) (time e) e)
		 ]))
	    (define (main-work) 
	      (parameterize ([marshal-cache (make-default-hash-table 1000)])
		(print-graph #t)
		(let* (
		       [evaled    (maybtime (Eval x '() #f))]		       		       		       
		       [marshaled (maybtime (Marshal evaled))]
		       )

		  (unless (streamop? evaled)
		    (when (> (regiment-verbosity) 0)
		      (printf "------------------------------------------------------------\n")
		      (printf "  Non-stream value returned from metaprogram: \n")
		      ;(pretty-print evaled)
		      ;(printf "  Marshaled:\n")
		      (pretty-print marshaled)
		      (printf "  Aborting compilation.\n")		      
		      (ASSERT (abort-compiler-continuation))
		      ((abort-compiler-continuation) evaled)
		      ))
		 		 
		  ;(pretty-print (strip-annotations marshaled 'src-pos))
		  (do-inlining 
		   (id;inspect/continue
		    marshaled))
		  )
		))
	    ;; We set up the error handler to give some source info.
;	    (main-work)
	    (with-exception-handler
	     (lambda (x) 
	       (printf "\n========================================\n")
	       (printf "Ran into a Scheme error during meta program evaluation.\n")
	       (printf "  Probably encountered when evaluating a primitive using the Scheme embedding.\n")
	       (if current-src-pos
		   (begin (printf "  The approximate source location was:\n\n   ~a\n\n" 
				  (src-pos->string current-src-pos))
			  ;(display (get-snippet `(src-pos ,current-src-pos ignored)))
			  )
		   (printf "  The source location is unknown.\n\n"))
	       
	       (printf "  The Scheme error was:\n")
	       ;(apply inspector-error-handler args)
	        ;(display-condition x)
	       (raise x))
	     main-work)
)])

; ================================================================================ ;

(define-testing test-interpret-meta
  (default-unit-tester
    " Interpret-Meta: to evaluate the first stage of computation"
  `([(',plain-val (',Eval '(_+_ '1 '2) '() #f)) 3]
    [(',plain-val (',Eval '(app (lambda (x) (Int) x) '3) '()  #f)) 3]
    [(',plain-val (',Eval '(car (cons '39 '())) '() #f)) 39]
    [(',Eval '(timer (annotations) '3) '() #f) ,streamop?]
    [(',Eval '(car (cons (iterate (annotations) (lambda (x vq) ('a 'b) '99) (timer (annotations) '3)) '())) '() #f) ,streamop?]
    [(',plain-val (',Eval '(letrec ([x Int '3]) x) '() #f)) 3]
    [(',plain-val (',Eval '(letrec ([x Int '3]) (wsequal? x '3)) '() #f)) #t]
    [(',plain-val (',Eval 
     '(letrec ([fact 'a (lambda (n) (Int) 
        (if (wsequal? '1 n) '1 (*_ n (app fact (_-_ n '1)))))])
	(app fact '6)) '() #f)) 
     720]
    [(',plain-val (',Eval 
     '(letrec ([v (Ref 'a) (Mutable:ref '99)])
	(begin (set! v '89)
	       (deref v))) '() #f))    89]
    [(',plain-val (',Eval 
     '(letrec ([v (Ref 'a) (Mutable:ref '0)])
	(begin (for (i '1 '10) (set! v (_+_ (deref v) '1)))
	       (deref v))) '() #f))    10]
    [(',plain-val (',Eval 
     '(letrec ([v (Ref 'a) (Mutable:ref '0)])
	(begin (while (< (deref v) '10) (set! v (_+_ (deref v) '1)))
	       (deref v))) '() #f))    10]
    
    [(parameterize ([',marshal-cache (make-default-hash-table 1000)])
      (deep-assq 'letrec		
        (cdr (',Marshal (',Eval '(car (cons 
	(letrec ([x 'a '100]) (iterate (annotations) (lambda (x vq) ('a 'b) x) (timer (annotations) '3.0)))
	'())) '() #f)))))
     #f]
    [(parameterize ([',marshal-cache (make-default-hash-table 1000)])
     (and (deep-assq 'letrec
     (cdr (',Marshal (',Eval '(car (cons 
       (letrec ([y Int '100]) (iterate (annotations) (lambda (x vq) ('a 'b) y) (timer (annotations) '3.0))) '())) '() #f))))
	  #t))
     #t]
    ["With this approach, we can bind the mutable state outside of the iterate construct"
     (parameterize ([',marshal-cache (make-default-hash-table 1000)])
       (not 
     (deep-assq 'Mutable:ref
     (deep-assq 'letrec
      (',Marshal (',Eval '(letrec ([y (Ref Int) (Mutable:ref '100)]) 
	      (iterate (annotations) (lambda (x vq) ('a 'b) (deref y)) 
				(timer (annotations) '3.0))) '() #f))))))
     #f]
    ["inline a function successfully"
     ;; [2008.08.07] Making this polymorphic so that it inlines:
     (deep-assq 'f
     (interpret-meta '(lang '(program 
       (letrec ([f ('a -> 'a) (lambda (x) ('a) x)])
	 (iterate (annotations) (lambda (_ vq) ('a 'vq) (app f '9)) (timer (annotations) '3.0))) Int))))
     #f]


    ["Reduce away fact of 6." 
     (interpret-meta '(foo '(program 
      (letrec ([fact (Int -> Int)
		     (lambda (n) (Int)
			     (if (= '0 n) '1 (*_ n (app fact (_-_ n '1)))))])
	(app fact '6))
      (union-types) 'notype)))
     ;(unspecified '(program (gint '720) (union-types) 'notype))
     (unspecified
      ;; No longer decorating output ints' types:
      ;'(program (assert-type Int (gint '720)) (union-types) 'notype))
      '(program (gint '720) (union-types) 'notype))
     ]

    )))

) ;; End module
