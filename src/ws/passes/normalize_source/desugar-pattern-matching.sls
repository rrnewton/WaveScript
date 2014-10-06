#!r6rs

;;;; .title       Pass: Desugar Pattern Matching
;;;; .author      Ryan Newton

;;;; This desugars pattern matching in binding forms.
;;;; Once that's done, it can run the type inferencer for the first time.

;;;; NOTE: this pass also desugars type assertions within the formals
;;;; list.  It pulls these outside the lambda.

;;;; NOTE: This is messier than I would like, but this pass also
;;;; desugars + into g+  The earlier this happens, the better, and
;;;; this was about as early as it could happen.

;;;; [2007.09.21] One more thing, now it desugars integer constants into gint()s

;;;; TODO: RENAME THIS PASS.

(library (ws passes normalize_source desugar-pattern-matching)
  (export pass_desugar-pattern-matching test_desugar-patterns break-pattern)
  (import (except (rnrs (6)) error) (ws common))

  ;; This generates projection code in place of patterns as formal arguments.
  (define (break-pattern pat)
      (match pat
	[(assert-type ,typ ,[form binds _]) (values form binds typ)]

	;; The special _ formal argument should never be referenced.  We rename them here.
	[_ (values (unique-name "_") '() #f)]

	[,s (guard (symbol? s)) 
	    (values s '() #f)]
	[#(,[pv* binds* type-assertion*] ...)
	 (ASSERT "Not currently allowed to make assertions on sub-parts of the pattern."
		 (lambda (ls) (for-all not ls)) type-assertion*)
	 (let ([v (unique-name 'pattmp)]
	       [len (length pv*)])
	   (let ([newbinds 
		  `([,pv* 'type (tupref ,(iota len) ,(make-list len len) ,(make-list len v))] ...)])
	     (values v
		     `( ,@(filter (lambda (b) (not (eq? (car b) '_))) newbinds)
			,@(apply append binds*)
			)
		     #f)))]))

  (define (make-let* binds bod)
    (match  binds
      [() bod]
      [(,bind . ,[rest]) `(letrec (,bind) ,rest)]))

  (define (build-assert t*)
    `(,@(map (lambda (t) (or t `(quote ,(unique-name 'alpha)))) t*)
      -> ',(unique-name 'beta)))

  (define (mangle-projector var fld)
    (string->symbol (format ":~a:~a" var fld)))

  (define (notype) `',(unique-name 'notypeyet))

  (define process-expr
    (lambda (expr fallthrough)
      (match expr 


;; TEMPTOGGLE:
;; Make plain integer constants "gints" by default:
	[(quote ,n) (guard (integer? n) (exact? n))
	 ;(inspect n)
	 `(gint (quote ,n))
	 ]

	;; Miscellaneous desugaring -- that must be done early!
	;;======================================================================
	;; Unadorned arithmetic symbols match onto their generic counterparts:
 	[+ 'g+] [- 'g-] [* 'g*] [/ 'g/] [^ 'g^]	
 	[(+ ,[a] ,[b]) `(g+ ,a ,b)]
 	[(- ,[a] ,[b]) `(g- ,a ,b)]
 	[(* ,[a] ,[b]) `(g* ,a ,b)]
 	[(/ ,[a] ,[b]) `(g/ ,a ,b)]
 	[(^ ,[a] ,[b]) `(g^ ,a ,b)]


	;;; THESE HACKS BREAK OVERRIDING OF THE PRIMITIVE NAMES:

	;; This is a work-around for a name conflict:
   [merge '(lambda (x y) ('noty 'noty) (_merge (annotations) x y))]
   [(app merge ,[s1] ,[s2]) `(_merge (annotations) ,s1 ,s2)]
   ; FIXME: is this dangerous?
   [(app (src-pos ,sp1 merge) ,[s1] ,[s2])
    `(_merge (annotations) ,s1 ,s2)]

   [(app readFile ,[args] ...) `(readFile (annotations) ,@args)]
   ; FIXME: is this dangerous?
   [(app (src-pos ,sp1 readFile) ,[args] ...)
    `(readFile (annotations) ,@args)]
   [(readFile ,[args] ...) `(readFile (annotations) ,@args)]

   ;; THIS IS A HACK ON A HACK:
   [(app ,timer ,[args] ...) 
    (guard (eq? (peel-annotations timer) 'timer)
	   (eq? (compiler-invocation-mode) 'wavescript-compiler-nesc)
	   ;; Only when we're NOT splitting into server/node do we do this:
	   ;(not (memq 'split (ws-optimizations-enabled)))
	   ;; NAH, doing it all the time... don't yet support server side timers in a split program.
	   )
    `(app TOS:timer ,@args)]
   ;; And this is an auxillary hack:
   [(app ,timer ,[args] ...) 
    (guard (eq? (peel-annotations timer) 'Server:timer))
    `(timer (annotations) ,@args)]

   [(app ,timer ,[args] ...) (guard (eq? (peel-annotations timer) 'timer))
    `(timer (annotations) ,@args)]
   [(timer ,[args] ...) `(timer (annotations) ,@args)]


	;[ref (inspect "HMRM Ref in desugar pat match")'Mutable:ref]

	;;======================================================================	
	
	[(lambda (,[break-pattern -> formal* binds* type-assertion*] ...) ,types ,[bod])
	 (let ([lam (if (null? binds*)
			`(lambda (,formal* ...) ,types ,bod)
			`(lambda (,formal* ...) ,types 
				 ,(make-let* (apply append binds*) bod)))])
	   (if (ormap id type-assertion*)
	       `(assert-type ,(build-assert type-assertion*) ,lam)
	       lam))]
	[(,let ((,[break-pattern -> lhs* binds* type-assertion*] ,type* ,[rhs*]) ...) ,[bod])
	 (guard (memq let '(let letrec)))
	 ;; Shouldn't have assertions on the variable names here:
	 (ASSERT "Shouldn't have assertions on the variable names here:"
		 (lambda (ls) (for-all not ls)) type-assertion*)
	 `(,let ([,lhs* ,type* ,rhs*] ...  )
	    ,(make-let* (apply append binds*) bod))]

	;; This isn't "pattern-matching" but we desugar it here so
	;; that the type checker doesn't need to deal with it.	 
	[(let* ,binds ,bod) (process-expr (make-let* binds bod) fallthrough)]


#;       
	;; Only handles one-armed matches right now:
	;;    [(match ,[x] [ ,[break-pattern -> var* binds*]  ,[rhs*] ] ...)
	[(match ,[x] (,[break-pattern -> var binds type-assertion] ,[rhs]))
	 ;; Shouldn't have assertions on the variable names here for now:
	 (ASSERT (not type-assertion))
	 `(letrec ([,var ,(notype) ,x] ,binds ...)
	    ,rhs)]

	;; I would like to convert this to a form that uses lambdas on the RHS to do the binding.
	;; This keeps wscase from being yet another binding form.
	;; Unfortunately, we use two different translations of wscase
	;; based on what backend we're ultimitely going to be using.
	;; For the C++ backend we just treat sums as tuples and
	;; minimize the extra machinery required.
	;;
	;; We only use case for dispatching on the tag of a sum type:
	[(wscase ,[x] (,pat* ,[rhs*])  ...)
	 (let ([newclause*
		(map (lambda (pat rhs)
		       (match pat
			 [_ `(,default-case-symbol ,rhs)]
			 [(data-constructor ,tc ,v* ...) (guard (andmap symbol? v*))
			  (list tc 
				`(lambda ,v* ,(map (lambda (_) (notype)) v*) ,rhs)
#;
			    (let ([mode (compiler-invocation-mode)])
			      (cond 
			       [(memq (compiler-invocation-mode) '(wavescript-compiler-caml))
				`(lambda ,v* ,(map (lambda (_) (notype)) v*) ,rhs)]
			       [(memq (compiler-invocation-mode) '(wavescript-compiler-xstream wavescript-simulator))
				;; Feed it back through to break up that tuple pattern:
				(process-expr `(lambda (#(,(unique-name 'tag) ,@v*)) (,(notype)) ,rhs)
					      fallthrough)]
			       [else (error 'desugar-pattern-matching 
					    "don't know what to do with a case construct in this compiler-invocation-mode: ~s"
					    (compiler-invocation-mode))]
			       )))]
			 [,oth (error 'desugar-pattern-matching 
				      "not supporting this kind of pattern yet: ~s" oth)]))
		  pat* rhs*)])
	   `(wscase ,x ,@newclause*))]
	[(wscase ,_ ...)
	 (error 'desugar-pattern-matching "don't support this form of case yet: ~s" 
		`(wscase . ,_))]


	;; [2006.11.15] Going to add special stream-of-tuples field-naming syntax.
	;; TODO: make it work for general patterns, not just for flat tuples!
	[(let-as (,v (,fldname* ...) ,[rhs]) ,[body])
	 (guard (symbol? v) (andmap symbol? fldname*))
	 (let ([len (length fldname*)])
	 ;; Each field-name gets bound to a projection function.
	 `(letrec ([,v ,(notype) ,rhs])
	    (letrec ([,(map (curry mangle-projector v) fldname*)
		      ,(map (lambda (_) (notype)) fldname*)
		      ,(map (lambda (i) 
			    #;
			    `(lambda (s) (iterate (annotations) (lambda (x vq) 
						    (begin (emit vq (tupref ,i ,len x)) vq))
						  s))
			    ;; Simple tuple projector:
			    `(lambda (x) (,(notype)) (tupref ,i ,len x))
			    )
                          (iota len))]
		  ...)
	      ,body))
	    )]
	
	;; This is let-as's counterpart for projecting out stream values.
	;;
	;; For NOW this only works with variables as the projections.
	;; This is because of name mangling... can't put general
	;; expressions here.
	[(dot-project (,projector* ...) ,[src])
	 (ASSERT (curry andmap symbol?) projector*)
	 ;; For the time being, we only use this syntax on the original variable:
	 (ASSERT symbol? src)
	 ;; THIS DOES NOT GUARANTEE HYGIENE:
	 (let ([tmp (unique-name '___tmp___)]
	       [vq (unique-name '___vq___)]
	       [make-tuple (lambda (args) (if (= 1 (length args)) 
					      (car args) (cons 'tuple args)))])
           `(iterate (annotations) (lambda (,tmp ,vq) (,(notype) ,(notype))
		       (begin (emit ,vq ,(make-tuple 
					  (map (lambda (proj) 
						 `(app ,(mangle-projector src proj) ,tmp)) 
					    projector*)))
			      ,vq))
		     ,src))]


	;; Likewise, we desugar this construct here as well:
	[(dot-record-project (,fld* ...) ,[src])
	 (let ([body (lambda (var)
		       (let loop ((fld* fld*))
			 (if (null? fld*)
			     '(empty-wsrecord)
			     `(wsrecord-extend ',(car fld*)
					       (wsrecord-select ',(car fld*) ,var)
					       ,(loop (cdr fld*))))))])
	   (if (symbol? (peel-annotations src))
		(body src)
		(let ([var (unique-name 'dotrecprojtmp)])	      
		  `(let ([,var ,(notype) ,src])
		     ,(body var))
		  )))]

	;; We don't desugar this here, it lives for one more pass:
	[(using ,M ,[e]) `(using ,M ,e)]


	;; [2008.01.27] Here's a bit of extra desugaring for wsc2:
	[(assert-type (Stream (Sigseg ,elt)) ,bod)
	 (guard (wsc2-variant-mode? (compiler-invocation-mode))
		(match (peel-annotations bod)
		  [(app ,rator . ,_)
		   (eq? (peel-annotations rator) 'readFile)]
		  [,_ #f]))
	 (define x (unique-name "x"))
	 (define vq (unique-name "vq"))
	 `(iterate (annotations) 
		   (let ()
		     (lambda (,x ,vq) ((Array ,elt) 'vqty)
			     (begin (emit ,vq (app toSigseg ,x (gint '0) nulltimebase))
				    ,vq)))
		   ,(process-expr `(assert-type (Stream (Array ,elt)) ,bod) fallthrough))]
	
	[,other (fallthrough other)])))

;; Desugar pattern matching within lambda's, letrecs, and "match" statements. <br>
;; TODO: This should really not go in the source_loader.
(define-pass pass_desugar-pattern-matching     

  ;; We're not quite ready to leave the "sugared" grammar yet.  The
  ;; next pass removes the 'using' construct.
  [OutputGrammar  sugared_wavescript_grammar]
  ;[OutputGrammar  initial_wavescript_grammar]

;; TODO: When it works, could redo this with Expr/ExtraArg
  [Expr process-expr]
  
  ;; After desugaring pattern matching, then we can typecheck the prog for the first time:
  [Program (lambda (prog Expr)	  
	     (match prog
	       [(,inputlang '(program ,bod ,other ... ,type))
		`(desugar-pattern-matching-language 
		  '(program ,(Expr bod) ,other ... ,type))]))]
  )

; ================================================================================

(define-testing test_desugar-patterns
  (default-unit-tester "desugar-pattern-matching.ss: For reading wavescript source files." 
    `(["Run a basic test of the pattern match expander."
     (cadr (cadadr
	    (reunique-names 
	     (strip-binding-types 
	      (',pass_desugar-pattern-matching 
	       '(foo '(program (lambda (#(foo #(bar baz)) x) ('t1 't2) foo) UncheckedType)))))))
     (lambda (pattmp x)
       (letrec ([foo (tupref 0 2 pattmp)])
	 (letrec ([pattmp_1 (tupref 1 2 pattmp)])
	   (letrec ([bar (tupref 0 2 pattmp_1)])
	     (letrec ([baz (tupref 1 2 pattmp_1)]) 
	       foo)))))]

    [(',reunique-names (values->list (',break-pattern '#(x y))))
     (pattmp
      ((x 'type (tupref 0 2 pattmp))
       (y 'type (tupref 1 2 pattmp)))
      #f)]

#;
    [(',pass_desugar-pattern-matching '(foo '(program (case 3 [x x]) Int)))
     (desugar-pattern-matching-language
      '(program (letrec ([x unspecified 3]) x) unspecified))]
#;
    ;; [2007.01.30] BUG: Different behavior in petite and chez.
    [(cadr (deep-assq 'aggr
		(pass_desugar-pattern-matching 
		 '(verify-wavescript-language
		   '(program
			(letrec ([readings 'type_13 (rmap
						     (lambda (n)
						       ('type_8)
						       (cons (sense "temp" n) (cons 1 '())))
						     world)]
				 [aggr 'type_12 (lambda (x y)
						  ('type_7 'type_6)
						  (cons
						   (g+ (car x) (car y))
						   (cons
						    (g+ (car (cdr x)) (car (cdr y)))
						    '())))]
				 [div 'type_11 (lambda (v)
						 ('type_5)
						 (if (= (car (cdr v)) 0)
						     0
						     (/ (car v) (car (cdr v)))))]
				 [sums 'type_10 (rfold aggr (cons 0 (cons 0 '())) readings)]
				 [result 'type_9 (smap div sums)])
			  result)
		      'toptype)))))
     ,(lambda (x)
	(match x 
	  [((List (NUM ,v)) (List (NUM ,v2)) -> (List (NUM ,v3))) #t]
	  [,else #f]))]

#;
(lang '(program (letrec ([test 'typefoo (lambda (x y)
                               ('type_7 'type_6)
                               (cons
                                 (g+ (car x) (car y))
                                 (cons
                                   (g+ (car (cdr x)) (car (cdr y)))
                                   '())))])
		  (app test 3))
	 'toptype))

    )))

)
