

(module interpret-meta mzscheme
  (require (all-except "../../../plt/common.ss" )
	   (all-except "static-elaborate.ss" these-tests)
           "../../langs/lang_wavescript.ss")
  (provide Eval Marshal Marshal-Closure  interpret-meta
	   test-interpret-meta)
  (chezimports)

; ================================================================================ ;
;;; Type defs and helpers

(reg:define-struct (plain val)) ;; Contains a datum: number, list, array, (tuples separate)
;(reg:define-struct (tuple fields)) ;; To distinguish tuples from vectors.
(reg:define-struct (closure formals code env))

;; TODO: Merge with "uniontype" in wavescript_sim_library_push
(reg:define-struct (datatype tag payload)) 

;; Parents are streamops, params are regular values that parameterize the streamop.
;; We also need a place to record the type.  At least for readFile...
(reg:define-struct (streamop name op params parents type))

(reg:define-struct (ref contents))

(define (wrapped? x) (or (plain? x) (streamop? x) (closure? x) (ref? x)))
(define (annotation? s) (memq s '(assert-type src-pos)))
(define (stream-type? ty) (and (pair? ty) (eq? (car ty) 'Stream)))
(define (lambda? x) (let ([x (peel-annotations x)])
		      (and (pair? x) (eq? (car x) 'lambda))))
(define (unknown-type . _) `',(unique-name 'ty))


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

#;
;; Could explicitly use a store...
(define (mutate-env! env v x)
  (let ([entry (apply-env env v)])
    (cond
     [(plain? entry) (set-plain-val! entry (plain-val x))]
     [else (error 'mutate-env! "unhandled environment entry: ~s" entry)])))


; ================================================================================ ;
;;; Interpreter

;; This evaluates the meta program.  The result is a *value*
(define (Eval x env)
  ;; This will be replaced by something more meaningful.
  (define (streamop-new-name) (unique-name 'streamop))
  (match x
    [,v (guard (symbol? v)) 
	(if (regiment-primitive? v)
	    (make-plain (wavescript-language v))
	    (apply-env env v))]
    [',c (make-plain c)]

    [(tuple ,[x*] ...) (make-plain (list->vector x*))]

    ;; Here's a hack to keep those type assertions on the readFiles...
    [(assert-type ,ty ,e)
     (guard (let ([x (peel-annotations e)])
	      (and (pair? x) (memq (car x) '(readFile dataFile)))))
     (let ([op (Eval e env)])
       (set-streamop-type! op ty)
       op)]

    ;; Unionlist is a tad different because it takes a list of streams:
    [(unionList ,[ls])      
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

    [(if ,[t] ,c ,a) (Eval (if (plain-val t) c a) env)]
    [(let ([,lhs* ,ty* ,[rhs*]] ...) ,bod)
     (Eval bod (extend-env lhs* rhs* env))]

    ;; This is a letrec* w/ let-'n-set semantics 
    [(letrec ([,lhs* ,ty* ,rhs*] ...) ,bod)
     (let* ([cells (map (lambda (_) (box 'letrec-var-not-bound-yet)) rhs*)]
	    [newenv (extend-env lhs* cells env)])
       (for-each (lambda (cell rhs)
		   (set-box! cell (Eval rhs newenv)))
	 cells rhs*)
       (Eval bod newenv))]

    [(lambda ,formal* ,ty* ,bod) (make-closure formal* bod env)]
 
    [(Mutable:ref ,[x]) (make-ref x)]
    [(deref ,[x])       (ref-contents x)]
    [(set! ,[v] ,[rhs]) (set-ref-contents! v rhs)]
    
    ;; This requires a bit of sketchiness to reuse the existing
    ;; implementation of this functionality within wavescript_sim_library_push
    [(,prim ,[x*] ...) (guard (regiment-primitive? prim))
     (ASSERT (not (assq prim wavescript-stream-primitives)))
					;   (printf "RUNNING ~s ~s\n" prim x*)
     ;; This is probably also rather slow.
     (let ([raw 
	    ;; Can't write/read because it will contain procedures and streamops.
	    (parameterize ([simulator-write-sims-to-disk #f])
	      (wavescript-language 
	       ;; We're willing to give it "plain" vals.
	       ;; Refs should not be passed first class.
	       ;; And closures/streams remain opaque:
	       (cons prim (map (lambda (x)				   
				 (ASSERT (not (ref? x)))				   
				 (cond 
				  [(plain? x) `',(plain-val x)]
				  [(closure? x) (reify-closure x)]				    
				  [(streamop? x) x] ;; This shouldn't be touched.
				  [else (error 'Eval "unexpected argument to primiitive: ~s" x)]))
			    x*))))])
       (if (wrapped? raw) raw (make-plain raw)))]

    [(app ,[f] ,[e*] ...)
					;   (printf "APPLYING CLOSURE to args ~s\n" e*)
     (Eval (closure-code f) 
	   (extend-env (closure-formals f) e*
		       (closure-env f)))]
    [(for (,i ,[st] ,[en]) ,bod)
     (let ([end (plain-val en)])       
       (do ([i (plain-val st) (fx+ i 1)])
	   ((> i end) (make-plain #()))
	 (Eval bod (extend-env '(i) (list (make-plain i)) env))))]
    [(while ,tst ,bod)
     (begin
       (let loop ()
	 (when (plain-val (Eval tst env))
	   (Eval bod env)
	   (loop)))
       (make-plain #()))]
    [(begin ,x* ... ,last) 
     (begin (for-each (lambda (x) (Eval x env)) x*)
	    (Eval last env))]

#;
    ;; TODO: Need to add the data *constructors*... this is incomplete currently.
    [(wscase ,[x] (,pat* ,[rhs*]) ...) 
     (let* ([alts (map list pat* rhs*)]
	    [case (assq (datatype-tag x) alts)]
	    [clos (cadr case)])
       (inspect case)
       (Eval (closure-code clos) 
	     (extend-env (closure-formals clos) (list (datatype-payload x))
			 (closure-env clos))))]

    ;; FIXME: Should attach source info to closures:
    [(,annot ,_ ,[e]) (guard (annotation? annot)) e]
  ))

;; Make a *real* procedure that evaluates a closure.
(define (reify-closure c)
  (lambda args
    (DEBUGASSERT (curry andmap (compose not procedure?)) args)
    (Eval (closure-code c) 
	  (extend-env (closure-formals c) 
		      (map (lambda (x) (if (wrapped? x) x (make-plain x))) args)
		      (closure-env c)))))


; ================================================================================ ;
;;; Marshaling Stream Values

;; This marshals the resulting stream-operators.
;; The result is a code for a stream-graph.
(define (Marshal val)
  (cond
   [(plain? val) (Marshal-Plain val)]
   [(closure? val) (Marshal-Closure val)]
   [(streamop? val)
    (let (;[covered (make-default-hash-table 100)]
	[acc '()])
    ;; Here we trace back through the stream graph:
    (define allops
      (let loop ([ops (list val)] [acc '()] [covered '()])
	(cond
	 [(null? ops) acc]
	 [(memq (streamop-name (car ops)) covered)
	  (loop (cdr ops) acc covered)]
	 [else 
	  (loop (append (cdr ops)
			(filter (lambda (op) (not (memq (streamop-name op) covered)))
			  (streamop-parents (car ops))))
		(cons (car ops) acc)
		(cons (streamop-name (car ops)) covered)
		)])))
    ;; Build a let expression:
    
    #;
    (let loop ([ops allops])
      (if (null? ops) (streamop-name val)
	  `(letrec ([,(streamop-name (car ops)) ,(unknown-type) ,(Marshal-Streamop (car ops))])
	     ,(loop (cdr ops)))))
    ;; No, doing letrec instead:
    `(letrec ,(map list (map streamop-name allops) (map unknown-type allops) (map Marshal-Streamop allops))
       ,(streamop-name val))
    )]
   [else (error 'Marshal "cannot marshal: ~s" val)]))

(define (Marshal-Streamop op)
  (define default 
    (cons (streamop-op op)
	  (if (eq? 'unionN (streamop-op op))
	      (map streamop-name (streamop-parents op))
	      ;; This is more than a bit silly, I shouldn't split params/parents in the first place.
	      (let loop ([argty* (car (ASSERT (regiment-primitive? (streamop-op op))))]
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
			  (loop (cdr argty*) (cdr params) parents)))]))
	      )))
  (if (streamop-type op)
      `(assert-type ,(streamop-type op) ,default)
      default))



;; FIXME: Uh, this should do something different for tuples.
;; We should mayb maintain types:
(define (Marshal-Plain p) 
  (let ([val (plain-val p)])
    (if (hash-table? val)
	(error 'Marshal-Plain "hash table marshalling unimplemented")
	`',val
	)))

(define (Marshal-Closure cl)
    (let loop ([code (closure-code cl)]
	       [fv   (closure-free-vars cl)]
	       [state '()]
	       [env (closure-env cl)])
      (if (null? fv)
	  ;; We're done processing the environment, produce some code:
	  (let ([bod `(lambda ,(closure-formals cl) 
		       ,(map unknown-type (closure-formals cl)) ,code)])
	    (if (null? state) bod
		`(letrec ,state ,bod)))
	  (let ([val (apply-env env (car fv))])
	    (cond
	     ;; FIXME: Inline simple constants:
	     [(plain? val) 
	      (loop code (cdr fv) 
		    (cons (list (car fv) (unknown-type) (Marshal-Plain val)) state)
		    env)]
	     ;; This is definitely part of the state:
	     [(ref? val)
	      ;; FIXME: Need to scan for shared mutable values.
	      (loop code (cdr fv)
		    (cons (list (car fv) (unknown-type) 
				`(Mutable:ref ,(Marshal-Plain (ref-contents val))))
			  state)
		    env)]
	     ;; Here we inline:	  
	     [(closure? val)  
	      ;; Freshness consideration:
	      (let-values ([(newcode newfree env-slice) (dissect-and-rename val)])
		(let ([newclosure (make-closure (closure-formals val) newcode env-slice)])
		  (loop 
		   ;; For now, don't do any inlining.  Do that later:
		   ;; Here we simply stick those lambdas into the code.
		   (substitute (list (list (car fv) `(lambda ,(closure-formals val) 
						       ,(map unknown-type (closure-formals val))
						       ,newcode))) 
			       code)
		   ;; We also merge the relevent parts of the closure's environment with our environment:
		   (union newfree (cdr fv))
		   state 
		   (append env-slice env))))]
	     
	     [(streamop? val) 
	      (error 'Marshal-Closure "cannot have stream value occuring free in a marshaled closure: ~s" val)])))))

(define (closure-free-vars cl) (difference (core-free-vars (closure-code cl)) (closure-formals cl)))

(define (dissect-and-rename cl)
  (let* ([fv    (closure-free-vars cl)]
	 [newfv (map unique-name fv)]
	 [newcode (substitute (map list fv newfv) (closure-code cl))]
	 [oldenv (closure-env cl)]
	 [oldslice (map (lambda (v) (apply-env oldenv v)) fv)]
	 [newslice (map list newfv oldslice)])
    (values newcode newfv newslice)    
    ))


; ================================================================================ ;
;;; Basic inlining for stream kernels.


;; After meta-program evaluation, what normalization do we want to do to the stream kernels?
;; Currently, we'd like to get rid of applications.  This function does some very simpl 
(define (do-basic-inlining e)
  (define (Expr e subst)
    (core-generic-traverse
     (lambda (x fallthru)
       (match x 
	 ;; Inline a simple left-left lambda:
	 ;; Sigh, we should be doing our annotation differently...
#;
	 [(app ,rator ,[arg*] ...)
	  (guard (lambda? rator))
	  (match (peel-annotations rator)
	    [(lambda ,formals ,types ,[do-basic-inlining -> bod])
	     ;; Convert to a let:
	     `(let ,(map list formals types arg*) ,bod)])]

	 ;; If the variable is bound to a lambda, here we inline it.
	 [,v (guard (symbol? v))
	     (let ([ent (assq v subst)])
;	       (when ent (printf "  INLINING LAMBDA VARREF ~s\n" v))
	       (if ent (caddr ent) v))]

	 [(,lett ,binds ,bod) (guard (memq lett '(let letrec lazy-letrec)))
	  (let* (;[binds (map list lhs* ty* rhs*)]
		 [lambind? (lambda (b) (lambda? (caddr b)))]
		 [newbinds (filter (compose not lambind?) binds)]
		 [lambs    (filter lambind? binds)])

;	    (unless (null? lambs) (printf "  GOT LAMB BINDS EXTENDING SUBST: ~s\n" (map car lambs)))

	    ;; This is a hack that depends on unique naming.  That's
	    ;; how we handle let in the same way as letrec.  The lhs*
	    ;; simply won't occur in the rhs* for a let..
	    (if (null? lambs)
		(fallthru `(,lett ,newbinds ,bod))
		;; This is an inefficent hack, but we loop through again just to change the subst.
		(Expr `(,lett ,newbinds ,bod) (append lambs subst))))]

	 ;; "Left left lambda"
	 ;; Evaluate the rator first so it has the chance to turn into a lambda.
	 [(app ,[rator] ,[rands] ...)
	  (if (lambda?  rator)
	      (match (peel-annotations rator)
		[(lambda ,formals ,types ,[do-basic-inlining -> bod])
;		 (printf "  CONVERTING LEFT-LEFT-LAMBDA ~s\n" formals)
		 ;; Convert to a let:
		 `(let ,(map list formals types rands) ,bod)])
	      `(app ,rator ,@rands))]

	 [,oth (fallthru oth)]))
     (lambda (ls k) (apply k ls))
     e))
  (Expr e '()))

;; NOTE!  Assumes unique variable names and so ignores binding forms.
(define substitute-and-beta
  (lambda (name fun exp)
    (core-generic-traverse
     (lambda (x fallthru)
       (match x
         ;; We've hit it in the wrong place:
	 [,v (guard (eq? v name)) (error 'substitute-and-beta "operand position reference")]
	 ;; Here we do the inlining.
	 [(app ,v ,[x*] ...)
	  (guard (eq? v name))
	  (substitute (map list (closure-formals fun) x*)
		      (closure-code fun))]
	 [,oth (fallthru oth)])
       )
     (lambda (ls k) (apply k ls))
     exp)))

;; [2007.04.16] NOT USED RIGHT NOW, DISABLING    
;; FIXME: REWRITE WITH GENERIC TRAVERSE!
(define substitute
  (lambda (mapping expr)
    (match expr
      [(quote ,datum) `(quote ,datum)]
      [,var (guard (symbol? var)) 
	    (let ((entry (assq var mapping)))
	      (if entry (cadr entry) var))]
      [(,ann ,_ ,[e]) (guard (annotation? ann)) `(,ann ,_ ,e)]
      [(lambda ,formals ,types ,expr)
       `(lambda ,formals ,types
		,(substitute
		  (filter (lambda (x)
			    (not (memq (car x) formals)))
		    mapping)
		  expr))]
      [(for (,i ,[st] ,[en]) ,bod)
       `(for (,i ,st ,en)		
	    ,(substitute
	      (filter (lambda (x) (not (eq? (car x) i))) mapping)
	      bod))]
      [(while ,[e1] ,[e2]) `(while ,e1 ,e2)]
      [(begin ,[arg] ...) `(begin ,arg ...)]
      [(set! ,v ,[rhs])
       (if (memq v (map car mapping))
	   (error 'interpret-meta:substitute "shouldn't be substituting against a mutated var: ~s" v))
       `(set! ,v ,rhs)]

      [(tupref ,n ,m ,[x]) `(tupref ,n ,m ,x)]
      [(tuple ,[args] ...) `(tuple ,args ...)]
      [(vector ,[args] ...) `(vector ,args ...)]
      [(unionN ,[args] ...) `(unionN ,args ...)]

      [(if ,[test] ,[conseq] ,[altern])  `(if ,test ,conseq ,altern)]
      [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
       (let ((newmap (filter (lambda (x)
			       (not (memq (car x) lhs*)))
		       mapping)))	     
	 `(letrec ([,lhs* ,type* ,(map (lambda (x) (substitute newmap x)) rhs*)] ...)
	    ,(substitute newmap expr)))]
      [(,prim ,[rands] ...) (guard (regiment-primitive? prim))
       `(,prim ,rands ...)]
      [(,app ,[rator] ,[rands] ...) (guard (memq app '(app construct-data))) 
       `(,app ,rator ,rands ...)]

      [(wscase ,[x] (,pat* ,[rhs*]) ...) `(wscase ,x ,@(map list pat* rhs*))]

      [,unmatched
       (error 'interpret-meta:substitute "invalid syntax ~s" unmatched)])))


; ================================================================================ ;
;;; TODO: Lift constants and check sharing.


; ================================================================================ ;
;;; Entrypoint and Unit Tests

(define-pass interpret-meta 
    [OutputGrammar static-elaborate-grammar]
    [Expr (lambda (x fallthru)  
	    (do-basic-inlining (time (Marshal (time (Eval x '()))))))])


(define-testing these-tests
  `([(,plain-val (,Eval '(+_ '1 '2) '())) 3]
    [(,plain-val (,Eval '(app (lambda (x) (Int) x) '3) '())) 3]
    [(,plain-val (,Eval '(car (cons '39 '())) '())) 39]
    [(,Eval '(timer '3) '()) ,streamop?]
    [(,Eval '(car (cons (iterate (lambda (x vq) (a b) '99) (timer '3)) '())) '()) ,streamop?]
    [(,plain-val (,Eval '(letrec ([x Int '3]) x) '())) 3]
    [(,plain-val (,Eval '(let ([x Int '3]) (wsequal? x '3)) '())) #t]
    [(,plain-val (,Eval 
     '(letrec ([fact 'a (lambda (n) (Int) 
        (if (wsequal? '1 n) '1 (*_ n (app fact (-_ n '1)))))])
	(app fact '6)) '())) 
     720]
    [(,plain-val (,Eval 
     '(let ([v 'a (Mutable:ref '99)])
	(begin (set! v '89)
	       (deref v))) '()))    89]
    [(,plain-val (,Eval 
     '(let ([v 'a (Mutable:ref '0)])
	(begin (for (i '1 '10) (set! v (+_ (deref v) '1)))
	       (deref v))) '()))    10]
    [(,plain-val (,Eval 
     '(let ([v 'a (Mutable:ref '0)])
	(begin (while (< (deref v) '10) (set! v (+_ (deref v) '1)))
	       (deref v))) '()))    10]
    
    [(deep-assq 'letrec
      (cdr (,Marshal (,Eval '(car (cons 
	(let ([x 'a '100]) (iterate (lambda (x vq) (a b) x) (timer '3)))
	'())) '()))))
     #f]
    [(and (deep-assq 'letrec
     (cdr (,Marshal (,Eval '(car (cons 
       (let ([y 'a '100]) (iterate (lambda (x vq) (a b) y) (timer '3))) '())) '()))))
	  #t)
     #t]
    ["With this approach, we can bind the mutable state outside of the iterate construct"
     (not 
     (deep-assq 'Mutable:ref
     (deep-assq 'letrec
      (,Marshal (,Eval '(let ([y 'a (Mutable:ref '100)]) 
	      (iterate (lambda (x vq) (a b) (deref y)) 
				(timer '3))) '())))))
     #f]
    ["inline a function successfully"
     (deep-assq 'f
     (interpret-meta '(lang '(program 
       (let ([f 'b (lambda (x) (Int) (+_ x x))])
	 (iterate (lambda (_) ('a) (app f '9))(timer '3))) Int))))
     #f]


    ["Reduce away fact of 6." 
     (interpret-meta '(foo '(program 
      (letrec ([fact _ (lambda (n) (_) 
			       (if (= '0 n) '1 (*_ n (app fact (-_ n '1)))))])
	(app fact '6))
      (union-types) 'notype)))
     (unspecified '(program '720 (union-types) 'notype))]

    ))

(define-testing test-interpret-meta
  (default-unit-tester
    " Interpret-Meta: to evaluate the first stage of computation"
    these-tests))


) ;; End module
