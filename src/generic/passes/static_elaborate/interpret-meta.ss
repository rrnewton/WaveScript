
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

(module interpret-meta mzscheme
  (require (all-except "../../../plt/common.ss" )
	   (all-except "static-elaborate.ss" these-tests)
	   (all-except "../wavescope_bkend/nominalize-types.ss" these-tests test-this)
           "../../langs/lang_wavescript.ss"
           "../../testing/lang_wavescript_tests.ss"	   
           "../../../plt/hashtab.ss"
	   )
  (provide Eval Marshal Marshal-Closure  interpret-meta
	   test-interpret-meta
;	   marshal-cache
)
  (chezimports)




; ================================================================================ ;
;;; Type defs and helpers

;; We use data types to separate different kinds of values.

;; Contains a datum: number, list, array, (tuples separate)
(reg:define-struct (plain val ))
;(reg:define-struct (tuple fields)) ;; To distinguish tuples from vectors.

;; There are two kinds of closures.  Native and foreign.
;; Foreign closures have #f in place of formals and env.
;;   "name" is optional -- a symbol or #f
(reg:define-struct (closure name formals code env))

;; A stream operator can be a leaf or an intermediate node.
;; The "parents" field contains a list of streamops, params are
;; regular values that parameterize the streamop.  We also need a
;; place to record the type.  At least for readFile and foreign_source...
(reg:define-struct (streamop name op params parents type))

;; This is not a pretty aspect of the system, but there are some
;; things that we should just "freeze" as code during metaprogram eval.
;; In particular, we should not evaluate foreign functions at metaprog time.
;;
;; [2007.09.06] Hash tables are another sticky part, we can't expect
;; the hash for an object to remain the same during compile
;; vs. runtime.  We might be able to make this work, as long as we
;; don't give the user direct access to the hash, but or the moment
;; we're going to just not evaluate hash tables at meta-eval time.
(reg:define-struct (suspension operator argvals))

(reg:define-struct (ref contents))
;; We must distinguish these because they share the same physical
;; representatino as other numbers.
;(reg:define-struct (int64 num))
;(reg:define-struct (double num))

;; NOTE: also uses 'uniontype' and 'tuple' defined in constants.ss


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
  (make-closure #f #f `(assert-type ,ty (foreign ',name ',includes)) #f))

;; Unwraps plain only:
(define (unwrap-plain x) 
  (DEBUGASSERT wrapped? x)
  (if (plain? x) (plain-val x) x))

(define (maybe-wrap x)
  (if (wrapped? x) x (make-plain x)))

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

  (define (prettify-names! names vals)
    (for-each (lambda (name val)
		       (cond
			[(closure? val)			 
			 (unless (closure-name val)
			   (set-closure-name! val (unique-name name)))]
			[(streamop? val)
			 ;; override the streamop name from its creation site?
			 (unless (and (streamop-name val) 
				      (not (eq? 'anonstreamop (deunique-name (streamop-name val)))))
;			   (printf "                         NAMING STREAMOP!!!!!! ~s \n"name)
			   
			   (set-streamop-name! val (unique-name name)))]))
      names vals))

  (unless dictionary (build-dictionary!))
  (match x
    [,v (guard (symbol? v)) 
	(if (regiment-primitive? v)
	    (make-plain (hashtab-get dictionary v))
	    (apply-env env v))]
    [',c (make-plain c)]

    [(tuple ,[x*] ...) (make-plain (make-tuple (map unwrap-plain x*)))]
    [(tupref ,ind ,len ,[tup])
     (ASSERT (fixnum? ind)) (ASSERT (fixnum? len))
     (ASSERT (plain? tup))
     (maybe-wrap (list-ref (tuple-fields  (plain-val tup)) ind))
     ]

    ;; UGLINESS
    ;; ----------------------------------------
    ;; Here's a hack to keep those type assertions on the readFiles and foreign entries......
    [(assert-type ,ty ,e)
     (guard (let ([x (peel-annotations e)])
	      (and (pair? x) (memq (car x) '(readFile dataFile)))))
     (let ([op (Eval e env pretty-name)])
       (set-streamop-type! op ty)
       op)]
    ;; This duplicates the above case forforeign entries.
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
	      'foreign_source (list name includes) () ty)]
	    )
	  )])]
    [(,frgn . ,_) (guard (memq frgn '(foreign foreign_source)))
     (error 'interpret-meta:Eval "foreign entry without type assertion: ~s" (cons frgn _))]
    ;; We leave this alone:
;    [(foreign ,[x*] ...) (cons 'foreign x*)]
    ;; ----------------------------------------


    ;; Unionlist is a tad different because it takes a list of streams:
    [(unionList ,[ls])      
     (ASSERT (plain? ls))
     (ASSERT (andmap streamop? (plain-val ls)))
     (make-streamop (streamop-new-name) 'unionN () (plain-val ls) #f)]
    [(,streamprim ,[x*] ...) (guard (assq streamprim wavescript-stream-primitives))
     (match (regiment-primitive? streamprim)
       [(,argty* (Stream ,return))
	;; This splits the stream from the non-stream components.
	(let ([parents (apply append (map (lambda (x t) (if (stream-type? t) (list x) ())) x* argty*))]
	      [params  (apply append (map (lambda (x t) (if (stream-type? t) () (list x))) x* argty*))])
	  (ASSERT (curry andmap streamop?) parents)
	  (ASSERT (curry andmap (compose not streamop?)) params)
	  (make-streamop (streamop-new-name) streamprim params  parents #f))])]   

    [(if ,[t] ,c ,a) 
     (ASSERT (plain? t))
     (Eval (if (plain-val t) c a) env pretty-name)]

    ;; [2007.09.19] Is this necessary if we've converted to left-left lambda?
#; ;; TEMPTOGGLE
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
     (let* ([cells (map (lambda (_) (box 'letrec-var-not-bound-yet)) rhs*)]
	    [newenv (extend-env lhs* cells env)])
       (for-each (lambda (cell lhs rhs)
		   (set-box! cell (Eval rhs newenv pretty-name))
		   ;; PRETTY NAMES:

		   (when (closure? (unbox cell)) 
;		     (printf "   WWOOOOOOOOOOOOOOOOOOOOT: ~s\n" lhs)		     
		     (prettify-names! (list lhs) (list (unbox cell))))
		   )
	 cells lhs* rhs*)
       (Eval bod newenv pretty-name))]

    [(lambda ,formal* ,ty* ,bod) 
     (make-closure #f formal* bod env)]
 
    [(Mutable:ref ,[x]) (make-ref x)]
    [(deref ,[x])       (ref-contents x)]
    [(set! ,[v] ,[rhs]) 
     (set-ref-contents! v rhs)
     ]
    
    [(Array:makeUNSAFE ,_) (error 'interpret-meta:Eval 
				  "Don't use Array:makeUNSAFE at meta-evaluation! ~s"
				  `(Array:makeUNSAFE ,_))]

    ;; TEMP HACK
    [(floatToInt ,[x])
     (ASSERT (plain? x))

     (make-plain (inexact->exact (floor (plain-val x))))]

    ;; HACK: need to finish treating hash tables:
    [(HashTable:make ,[len]) 
     (make-suspension 'HashTable:make (list len))]

    ;; This requires a bit of sketchiness to reuse the existing
    ;; implementation of this functionality within wavescript_sim_library_push
    [(,prim ,[x*] ...) (guard (regiment-primitive? prim))
     (ASSERT (not (assq prim wavescript-stream-primitives)))
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
		     (map (lambda (x)				   
		 	    (ASSERT (not (ref? x)))				   
			    (cond 
			     [(plain? x)   (plain-val x)]
			     [(closure? x) (reify-closure x)]
			     [(streamop? x) x] ;; This shouldn't be touched.
			     [else (error 'Eval "unexpected argument to primiitive: ~s" x)]))
		       x*)))])
;       (display-constrained "  RAW RESULT from prim " `[,prim 100] " " `[,raw 100] "\n")
       (if (wrapped? raw) raw (make-plain raw)))]
    
    ;; [2007.09.14] Supporting sums:
    [(construct-data ,tag ,[val]) (make-plain (make-uniontype tag val))]

    [(app ,[f] ,[e*] ...)
     (if (foreign-closure? f)
	 ;; Foreign app is suspended for later:
	 (begin
	   (printf "EXPERIMENTAL: making meta-suspension for foreign app: ~s" f)
	   (make-suspension f e*))

	 ;; Native closure:
	 (begin 
	   ;; Do PRETTY naming:
	   (prettify-names! (closure-formals f) e*)
	   (Eval (closure-code f)
	       (extend-env (closure-formals f) e*
			   (closure-env f))
	       (or (closure-name f) pretty-name))))]

    [(for (,i ,[st] ,[en]) ,bod)
     (ASSERT (plain? st))
     (ASSERT (plain? en))
     (let ([end (plain-val en)])       
       (do ([i (plain-val st) (fx+ i 1)])
	   ((> i end) (make-plain #()))
	 (Eval bod (extend-env '(i) (list (make-plain i)) env) pretty-name)))]
    [(while ,tst ,bod)     
     (let loop ()
       (let ([test (Eval tst env pretty-name)])
	 (ASSERT (plain? test))
	 (when (plain-val test)
	   (Eval bod env pretty-name)
	   (loop))))
     (make-plain #())]
    [(begin ,x* ... ,last) 
     ;; Side effects are modeled by... actual side effects!
     (begin (for-each (lambda (x) (Eval x env pretty-name)) x*)
	    (Eval last env pretty-name))]

    ;; TODO: Need to add the data *constructors*... this is incomplete currently.
    [(wscase ,[x] (,pat* ,[rhs*]) ...) 
     (let* ([alts (map list pat* rhs*)]
	    [case (assq (uniontype-tag x) alts)]
	    [clos (cadr case)])
       (inspect case)
       (Eval (closure-code clos) 
	     (extend-env (closure-formals clos) (list (uniontype-val x))
			 (closure-env clos)) 
	     pretty-name))]

    ;; FIXME: Should attach source info to closures:
    [(,annot ,_ ,[e]) (guard (annotation? annot)) e]
  ))

(define dictionary #f) ;; Set below:
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
		(append '()
			lang_wavescript_prim-exceptions)
	       )])
    (let ([vals (wavescript-language `(list ,@prims))])
      (for-each (lambda (sym prim)
		  (hashtab-set! dictionary sym prim))	   
	prims vals)))
  (printf "BUILT DICTIONARY!\n")
;  (inspect dictionary)
  )

;; Make a *real* procedure that evaluates a closure.  Thus we can pass
;; it to one of the higher-order WS primitives that's implemented in a
;; separate library (wavescript_sim_library_push.ss).
(define (reify-closure c)
  (ASSERT (not (foreign-closure? c)))
  (lambda args
    (DEBUGASSERT (curry andmap (compose not procedure?)) args)
    (unwrap-plain
     (Eval (closure-code c) 
	   (extend-env (closure-formals c) 
		       ;;args 
		       ;;(map (lambda (x) (if (wrapped? x) x (make-plain x))) args)
		       (map (lambda (x) 
			      ;; Let's say you map(streamtransformer, list)...
			      ;; That requires passing closures that handle streams.
			      #;
			      (when (streamop? x)
				(error 'reify-closure "shouldn't try to reify this stream-operator: ~s" c))
			      ;(ASSERT (compose not wrapped?) x)
			      ;(make-plain x)
			      (if (wrapped? x) x (make-plain x))
			      )
			 args)
		       (closure-env c))
	   #f))))

#;
;; This adds code around a closure that wraps/unwraps the arguments
;; and result values, thus allowing the closure to be passed to
;; wavescript-language to operate on unwrapped values.
(define (wrap/unwrap-closure clos)
  (lambda args
    ;; Anything that gets passed to one of these wavescript
    ;; primitives should return only plain data.
    (plain-val (apply clos (map (lambda (x) (if (wrapped? x) x (make-plain x))) args)))
    ))








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
	 `(foreign-app ',name
		       ,(Marshal-Foreign-Closure (suspension-operator val))
		       ,@(map Marshal (ASSERT (suspension-argvals val))))
	 ])]
     [(regiment-primitive? (suspension-operator val))
      `(,(suspension-operator val) ,@(map Marshal (ASSERT (suspension-argvals val))))]
     [else (error 'interpret-meta:Marshal "unhandled suspended op: ~s" (suspension-operator val))]
     )]

   [(plain? val) (Marshal-Plain val)]
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
    (DEBUGASSERT set? (map streamop-name allops))

    ;; Build a let expression binding all streamops:    
    #;
    (let loop ([ops allops])
      (if (null? ops) (streamop-name val)
	  `(letrec ([,(streamop-name (car ops)) ,(unknown-type) ,(Marshal-Streamop (car ops))])
	     ,(loop (cdr ops)))))

    ;; No, doing letrec instead:
    (ASSERT allops)
    `(letrec ,(map list (map streamop-name allops) (map unknown-type allops) (map Marshal-Streamop allops))
       ,(streamop-name val)))]
   [else (error 'Marshal "cannot marshal: ~s" val)]))

(define (Marshal-Streamop op)
  (define arglist
    ;; This is more than a bit silly, I have to recombine the argument list from params/parents.
    ;; I shouldn't split params/parents in the first place.
    (let loop ([argty* (if (eq? 'unionN (streamop-op op)) ;; special case, not a prim
			   (map (lambda (_) '(Stream 'any)) (streamop-parents op)) ;; All stream types.  Contents unimportant.
			   (car (ASSERT (regiment-primitive? (streamop-op op))))
			   )]
	       [params  (streamop-params op)]
	       [parents (streamop-parents op)])
      (cond
       [(null? argty*) '()]
       [(stream-type? (car argty*))
	(cons (streamop-name (car parents))
	      (loop (cdr argty*) params (cdr parents)))]
       [else 
	(let ()
	  (DEBUGASSERT (or (plain? (car params)) (closure? (car params))))
	  (cons (Marshal (car params)) 
		(loop (cdr argty*) (cdr params) parents)))]))) ;; End Silliness
  ;; Produce primitive application syntax:
  (define default (cons (streamop-op op) arglist))
;  (display-constrained "   MARSHALLING STREAMOP: " `[,op 100] "\n")
  (if (streamop-type op)
      `(assert-type ,(streamop-type op) ,default)
      default))



;; We should maybe maintain types...
(define (Marshal-Plain p) 
;  (display-constrained "    MARSHALLING plain " `[,p 100] "\n")
  (ASSERT (plain? p))
  
  (let loop ([val (plain-val p)])
    (cond
     [(hash-table? val) (error 'Marshal-Plain "hash table marshalling unimplemented")]
     ;; Going to wait and get rid of sigseg constants in remove-complex-constants:
#;
     [(sigseg? val) (if (fx= 0 (vector-length (sigseg-vec val)))
			'nullseg
			(error 'Marshal-Plain "non-null sigseg marshalling unimplemented"))]
;     [(int64? val)  (int64-num val)]
;     [(double? val) (double-num val)]

     [(tuple? val) `(tuple . ,(map (lambda (x) (if (wrapped? x) (Marshal x) (loop x)))
				(ASSERT (tuple-fields val) )))]

     [(uniontype? val)
      `(construct-data ,(uniontype-tag val)
		       ,(Marshal (uniontype-val val)))]

     [(timebase? val) `(Secret:newTimebase ',(timebase-num val))]
     [(and (integer? val) (exact? val)) `(gint ',val)]
     ;; No double's in meta program currently!!!
     ;; Need to wrap them!!
;
     ;; FIXME:
     [(list? val)
      ;; This value is thrown away:
      ;; TODO, do a proper check to make sure there are no streamops/closures
      (for-each loop val)
      `',val]    

     [else 
      (ASSERT (not (streamop? val)))
      (ASSERT (not (closure? val)))
      (ASSERT (not (ref? val)))
      (ASSERT (not (suspension? val)))

      ;(DEBUGASSERT complex-constant? val)
      `',val]
     )))


;; Foreign closures are simple... they become foreign entries.
(define (Marshal-Foreign-Closure fcl)
  (ASSERT (foreign-closure? fcl))
  (closure-code fcl)
  )


;; FIXME: TODO: to make this more efficient, we should build up a
;; single substitution, then apply it, rather than repeatedly
;; traversing the closure's code.
(define (Marshal-Closure cl)
  ;; Below we accumulate a cumulative substitution list.
  ;; We must apply the substitions to the expressions being substituted.
  ;; This requires picking an order.
  (define (subst-the-substs subst)
    (match subst
      [() ()]
      [((,v ,expr) . ,[rest])
       (cons (list v (core-substitute rest expr))
	     rest)]))

;  (display-constrained "    MARSHALLING CLOSURE: " `[,cl 100] "\n")
  (ASSERT (not (foreign-closure? cl)))


  ;; This loop accumulates a bunch of bindings that cover all the
  ;; free variables of the closure (and, transitively, any
  ;; closures reachable from the input closure).  These bindings
  ;; fall into two categories.  Mutable bindings are only allowed
  ;; to be accessed from a single closure.  Immutable bindings may
  ;; float up to become global bindings.  (And thus not duplicate
  ;; code between multiple iterates.)
  (let marshloop ([code (closure-code cl)]                   ;; Code to process
		  [fv   (list->set (closure-free-vars cl))]  ;; free vars to marshal
		  [state '()]                                ;; mutable state for this closure
		  [globals '()]                              ;; immutable global bindings
		  [env (closure-env cl)]                     ;; environment from which to marshal
		  [substitution '()]                         ;; lambda's to inline
		  )
    (DEBUGASSERT set? fv)
    (if (null? fv)
	;; We're done processing the environment, produce some code:
	(let* ([bod `(lambda ,(closure-formals cl) 
		       ,(map unknown-type (ASSERT (closure-formals cl))) ,code)]
	       [newbod (let ([subst (subst-the-substs (reverse substitution))])
			 (core-substitute (map car subst) (map cadr subst)
					  bod))]
	       [binds (append globals state)])
	  (DEBUGASSERT set? (map car binds))

					;(if (null? state) bod `(letrec ,state ,bod))
					;	    (unless (null? globals) (inspect globals))
;	  (printf "FINALLY DOING SUBSTITUTION: ~s\n" (map car (reverse substitution)))
	  (if (null? binds) newbod `(letrec ,binds ,newbod)))
	  ;`(letrec ,binds ,newbod))
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
		  env substitution )]
	   ;; This is definitely part of the state:
	   [(ref? val)
	    ;; FIXME: Need to scan for shared mutable values.
	    (marshloop code (cdr fv)
		  (cons (list (car fv) (unknown-type) 
			      `(Mutable:ref ,(Marshal (ref-contents val))))
			state)
		  globals env substitution)]

	   [(foreign-closure? val)
  	    ;	      (printf " ....  FREE VAR IS FOREIGN CLOSURE ~s...\n" (car fv))
	    (match (closure-code val)
	      [(assert-type ,ty (foreign ',name ',includes))
	       (match ty
		 [(,argty* ... -> ,res)
		  (let ([formals (list-build (length argty*) (lambda (_) (unique-name 'arg)) )])
		    ;; This just turns into the '(foreign name includes)' expression.
		    (marshloop code (cdr fv) state
			  ;; The foreign binding just turns into the '(foreign name includes)' expression:
			  (cons `(,(car fv) ,(unknown-type) ,(closure-code val)) globals)
			  env
			  ;; Change any references to be foreign-apps...
			  (cons (list (car fv)
				      `(lambda ,formals ,argty*
					       (foreign-app ',name ,(car fv) ,@formals)))
				substitution)))])])]

	   ;; FIXME:
	   ;; Free variables bound to closures need to be turned back into code and inlined.
	   ;; This is a little ugly... we stick the lambda in but inline later.  We could proably 
	   ;; also not treat it specially at all, and leave it outside.  But we do want to rip
	   ;; the environment out of these closures, regardless of where we stick the lambda code.
	   [(closure? val)
	    ;; First, a freshness consideration:
	    ;; FIXME: We could be better here about catching duplicated bindings.
	    ;; (Instead, by renaming we will give them distinct bindings.)
	    (let-values ([(newcode newfree env-slice) (dissect-and-rename val)])
	      (DEBUGASSERT (null? (intersection (map car env-slice) (map car env))))
	      (marshloop code 		 
		    ;; We also merge the relevent parts of the closure's environment with our environment:
		    (union newfree (cdr fv))
		    state globals
		    (append env-slice env)
		    (cons 		 
		     ;; For now, don't do any inlining.  Do that later:
		     ;; Here we simply stick those lambdas into the code. (by adding them to the subst)
		     (list (car fv) 
			   `(lambda ,(closure-formals val) 
			      ,(map unknown-type (closure-formals val))
			      ,newcode))
		     substitution)))]
	   
	   [(streamop? val) 
	    (error 'Marshal-Closure "cannot have stream value occuring free in a marshaled closure: ~s" val)])))))

(define (closure-free-vars cl) 
  ;(ASSERT (not (foreign-closure? cl)))
  (if (foreign-closure? cl) ()
      (difference (core-free-vars (closure-code cl)) (closure-formals cl))))

;; Renames the free vars for a closure.
(define (dissect-and-rename cl)
  (DEBUGASSERT (not (foreign-closure? cl)))
  (let* ([fv    (list->set (closure-free-vars cl))]
	 [newfv (map unique-name fv)]
	 [newcode (core-substitute fv newfv (closure-code cl))]
	 [oldenv (closure-env cl)]
	 [oldslice (map (lambda (v) (apply-env oldenv v)) fv)]
	 [newslice (map list newfv oldslice)])
#;
    (unless (set-equal? newfv
			     (list->set (difference (core-free-vars newcode) (closure-formals cl))))
      (inspect newfv)
      (inspect (core-free-vars newcode))
      (inspect (closure-formals cl))
      (inspect (list->set (difference (core-free-vars newcode) (closure-formals cl))))
      )
    (DEBUGASSERT (set-equal? newfv
			     (list->set (difference (core-free-vars newcode) (closure-formals cl)))))
    (values newcode newfv newslice)))








; ================================================================================ ;
;;; Basic inlining inside stream kernels.

;; After meta-program evaluation, what normalization do we want to do to the stream kernels?
;; Currently, we'd like to get rid of applications.  This function exhaustively inlines.
(define (do-basic-inlining e)
  (define (Expr e subst)
    (core-generic-traverse
     (lambda (x fallthru)
       (match x 
	 ;; If the variable is bound to a lambda, here we inline it.
	 [,v (guard (symbol? v))
	     (let ([ent (assq v subst)])
	       (if ent (caddr ent) v))]

	 [(,lett ,binds ,bod) (guard (memq lett '(let letrec lazy-letrec)))
	  (let* (;[binds (map list lhs* ty* rhs*)]
		 [lambind? (lambda (b) (lambda? (caddr b)))]
		 [newbinds (filter (compose not lambind?) binds)]
		 [lambs    (filter lambind? binds)])
	    ;; This is a hack that depends on unique naming.  That's
	    ;; how we handle let in the same way as letrec.  The lhs*
	    ;; simply won't occur in the rhs* for a let..
	    (if (null? lambs)
		(fallthru `(,lett ,newbinds ,bod))
		;; This is an inefficent hack, but we loop through again just to change the subst.
		(Expr `(,lett ,newbinds ,bod) (append lambs subst))))]

	 ;; [2007.08.30] Adding basic eta-reduction also.
	 ;; FIXME: Make sure this can't break an iterate's special syntactic structure.
	 #;
	 [(lambda ,args (app ,[rator] ,args2 ...))
	  (guard (equal? args args2))
	  rator]


	 ;; "Left left lambda"
	 ;; Evaluate the rator first so it has the chance to turn into a lambda.
	 [(app ,[rator] ,[rands] ...)
	  (if (lambda?  rator)
	      (match (peel-annotations rator)
		[(lambda ,formals ,types ,bod) ;[do-basic-inlining -> bod]
		 ;; Convert to a let:
		 ;; Then we must put it back through the inliner.
		 ;; Those let RHS's might have some more inlining to do.
		 (do-basic-inlining		  
		  `(let ,(map list formals types rands) ,bod))])
	      (begin
;		(printf "FAILED TO EVAL RATOR TO LAMBDA: ~s\n" `(app ,rator ,@rands))
		`(app ,rator ,@rands))
	      )]

	 [,oth (fallthru oth)]))
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
	    (parameterize ([marshal-cache (make-default-hash-table 1000)])

	      (let* (
		     [evaled (time (Eval x '() #f))]
		     [marshaled (time (Marshal evaled))]		     
		     )
	      (do-basic-inlining 
	       (id;inspect/continue
		marshaled))
		)
	      ))])

; ================================================================================ ;

(define-testing these-tests
  `([(,plain-val (,Eval '(+_ '1 '2) '() #f)) 3]
    [(,plain-val (,Eval '(app (lambda (x) (Int) x) '3) '()  #f)) 3]
    [(,plain-val (,Eval '(car (cons '39 '())) '() #f)) 39]
    [(,Eval '(timer '3) '() #f) ,streamop?]
    [(,Eval '(car (cons (iterate (lambda (x vq) (a b) '99) (timer '3)) '())) '() #f) ,streamop?]
    [(,plain-val (,Eval '(letrec ([x Int '3]) x) '() #f)) 3]
    [(,plain-val (,Eval '(letrec ([x Int '3]) (wsequal? x '3)) '() #f)) #t]
    [(,plain-val (,Eval 
     '(letrec ([fact 'a (lambda (n) (Int) 
        (if (wsequal? '1 n) '1 (*_ n (app fact (-_ n '1)))))])
	(app fact '6)) '() #f)) 
     720]
    [(,plain-val (,Eval 
     '(letrec ([v 'a (Mutable:ref '99)])
	(begin (set! v '89)
	       (deref v))) '() #f))    89]
    [(,plain-val (,Eval 
     '(letrec ([v 'a (Mutable:ref '0)])
	(begin (for (i '1 '10) (set! v (+_ (deref v) '1)))
	       (deref v))) '() #f))    10]
    [(,plain-val (,Eval 
     '(letrec ([v 'a (Mutable:ref '0)])
	(begin (while (< (deref v) '10) (set! v (+_ (deref v) '1)))
	       (deref v))) '() #f))    10]
    
    [(parameterize ([,marshal-cache (make-default-hash-table 1000)])
      (deep-assq 'letrec		
        (cdr (,Marshal (,Eval '(car (cons 
	(letrec ([x 'a '100]) (iterate (lambda (x vq) (a b) x) (timer '3)))
	'())) '() #f)))))
     #f]
    [(parameterize ([,marshal-cache (make-default-hash-table 1000)])
     (and (deep-assq 'letrec
     (cdr (,Marshal (,Eval '(car (cons 
       (letrec ([y 'a '100]) (iterate (lambda (x vq) (a b) y) (timer '3))) '())) '() #f))))
	  #t))
     #t]
    ["With this approach, we can bind the mutable state outside of the iterate construct"
     (parameterize ([,marshal-cache (make-default-hash-table 1000)])
       (not 
     (deep-assq 'Mutable:ref
     (deep-assq 'letrec
      (,Marshal (,Eval '(letrec ([y 'a (Mutable:ref '100)]) 
	      (iterate (lambda (x vq) (a b) (deref y)) 
				(timer '3))) '() #f))))))
     #f]
    ["inline a function successfully"
     (deep-assq 'f
     (interpret-meta '(lang '(program 
       (letrec ([f 'b (lambda (x) (Int) (+_ x x))])
	 (iterate (lambda (_) ('a) (app f '9))(timer '3))) Int))))
     #f]


    ["Reduce away fact of 6." 
     (interpret-meta '(foo '(program 
      (letrec ([fact _ (lambda (n) (_) 
			       (if (= '0 n) '1 (*_ n (app fact (-_ n '1)))))])
	(app fact '6))
      (union-types) 'notype)))
     (unspecified '(program (gint '720) (union-types) 'notype))]

    ))

(define-testing test-interpret-meta
  (default-unit-tester
    " Interpret-Meta: to evaluate the first stage of computation"
    these-tests))


) ;; End module
