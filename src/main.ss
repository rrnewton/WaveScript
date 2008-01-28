
;;;; .title Compiler Core (main.ss)

;;;; This contains the core compiler entry points. 
;;;; Loaded by both the Chez and PLT versions.
;;;; (Expects to be loaded from the directory that contains it.)

;======================================  
;(display "Loading main compiler module.  RegionStreams Demo.")
;(newline)


(define-regiment-parameter regiment-version "0.9?")


;; This is the global parameter that determines which transformations
;; (passes) the compiler applies and in what order.  We set it here.
(IFWAVESCOPE (begin)
(pass-list
  (list
    ;; (1) Type checking comes first, but that happens before these passes
    ;; are run.  Maybe should make it one of the "passes".
    
    ;; (2) Next we verify our input language.
    verify-regiment
    
    ;; (3) Then we do a little massaging/normalization.
    pass_desugar-pattern-matching
    retypecheck ;; This is actually the first typecheck.

    eta-primitives
    desugar-misc
    rename-vars
    remove-unquoted-constant
    retypecheck

    ;; (4) Then -- here comes the metaprogramming -- we evaluate as much
    ;; of the program as can be evaluated.  The residual had better follow our
    ;; restrictions on implementable Regiment programs.
    static-elaborate    
    degeneralize-arithmetic
    rename-vars ;; We run again after elaborator.
    retypecheck

;    verify-elaborated

    ;; (5) Now we normalize the residual in a number of ways to
    ;; produce the core query language, then we verify that core.
    reduce-primitives    
    remove-complex-constant  
    retypecheck

    uncover-free             

    lift-letrec              
    lift-letrec-body         
    remove-complex-opera*
    verify-core
    retypecheck

    ;; (6) Analysis: these passes analyze the query circuit and
    ;; annotate it with various information which may be used in
    ;; "deglobalize" further down the road.  Currently, most of these
    ;; analyses are underdeveloped; more work is warranted.
    classify-names ;; Remove this pass..
    add-heartbeats
    add-control-flow
    add-data-flow
    resolve-fold-trees
    add-places      ;; UNNECESSARY CURRENTLY
;    add-routing
    analyze-places

    ;; (7) Finally, the core of the Regiment compiler.  Convert a
    ;; Regiment query into a (albeit high level) node-level
    ;; Token Machine program.
    deglobalize
    ;; Alternatively:
    ;; flatten-comm deglobalize2 streams-to-tm

    ;; (8) There's a large gap from the high-level (human readable) TM
    ;; language and the low-level (actually implemented) TM language.
    ;; Cleanup-token-machine does a lot of the work of desugaring.
    cleanup-token-machine 
    desugar-macros		
;    cleanup-token-machine   ;; TEMP: FIXME

    ;; (9) The next major step is desugaring the gradient
    ;; communication constructs used in TML.  
    find-emittoks
    desugar-gradients
    cleanup-token-machine   ;; Rerun to expand out some stuff.
    ;    analyze-tokmac-recursion    

    ;; (10) Then we desugar another construct: "let-stored".
    desugar-let-stored
    rename-stored


    ;; (11) CPS: we CPS the program to get rid of all non-tail calls.

    ;; This is because Token handlers may only schedule more tokens,
    ;; not wait for "child" handlers to return!  This is also the
    ;; trick we use to implement synchronous sensor reading, which is
    ;; really split-phased in the underlying NesC.
;    cps-tokmac
;    sever-cont-state 
;    closure-convert
;    cleanup-token-machine ;; Trying this.. [2005.09.27]

    ;; (12) Optimization: (unfinished) there's a bunch of low-hanging
    ;; fruit optimizations on Token Machines that we should be doing.
;    inline-tokens

    ;; (*) OLD: The compiler back-end used to be in Haskell.  Thus we
    ;; would export the intermediate language to a haskell readable
    ;; form for the back-end to work with it.
    ;haskellize-tokmac 
    
    ;; (13) This is the current back-end.  
    ;; Uncomment to make the compiler generate NesC code.
;    flatten-tokmac
;    emit-nesc

    )))

; ==================================================================
;; Functions for input/output to filesystem and for invoking compiler.

(define (dump-tokenmachine-to-file prog fn)
  (match prog
    [(haskellize-tokmac-language ,str)
     (with-output-to-file fn
       (lambda () (display str) (newline))
       'replace)]
    ;; If it's an earlier file, pretty print it:
    [(,lang ,prog ,rest ...)
     (guard (list? prog))
     (with-output-to-file fn
       (lambda () (pretty-print `(,lang ,prog)))
       'replace)]
    [,other (error 'dump-tokenmachine-to-file "invalid input: ~S" prog)]))

(IFWAVESCOPE
  (begin )
  (begin

;; This dumps to file only when provided the optional string filename argument:
;; The symbolic options are:  'barely-tokens 'almost-tokens 'full-tokens
;; Also: use 'verbose to print the output of every pass.
(define (run-compiler p . args )                              ;; Entrypoint.
  (parameterize ([compiler-invocation-mode 'regiment-simulator])
    (if (memq 'deglobalize2 args)
        (apply run-compiler2 p (remq 'deglobalize2 args))

  (let ([filename #f]
	[passes (pass-list)]
	[verbose #f]
	;; If there are still values that need to be evaluated and filled in in the TM, do it.
	[p (match p
	     [(quasiquote ,_) (eval p)]
	     [,_ p])])
    (for-each (lambda (arg)
		(cond
                  [(string? arg) ;; It's an output filename.
                   (set! filename arg)]
		  [(eq? arg 'verbose) (set! verbose #t)]
		  ;; The pass-list may have already been restricted to be just the TML passes:
                  [(eq? arg 'almost-tokens)
                   (set! passes (list-remove-first deglobalize ;; <- might fizzle
				  (list-remove-first cleanup-token-machine
                                     (list-remove-after cleanup-token-machine (pass-list)))))]
                  [(eq? arg 'barely-tokens)
                   (set! passes (list-remove-first cleanup-token-machine
				  (list-remove-after cleanup-token-machine (pass-list))))]
		  ;; Out dated... don't haskellize any more:
#;
                  [(eq? arg 'full-tokens)
                   (set! passes (remq haskellize-tokmac (pass-list)))
		   ]
                  ;[(eq? arg 'haskell-tokens) (void)]
		  ;; Otherwise... do nothing.
		  [else (warning 'run-compiler "ignored flag: ~s" arg)]
		  ))
	      args)
    (when verbose
	  (printf "Running compiler with pass-list: \n")
	  (pretty-print passes))
    (let ((funs (map eval passes)))
      (let loop ([p p] [funs funs] [names passes])
        (if (null? funs) 
            (begin (if filename (dump-tokenmachine-to-file p filename)
                       p))
	    (begin 
	      (if verbose
		  (begin
		    (printf ";===============================================================================\n")
		    (printf "~a:\n\n" (regiment-pass->name (car names)))))
	      (let ((result (if verbose 
				(time ((car funs) p))
				((car funs) p))))
		(when verbose
		  (pretty-print result) (newline))
		(loop result (cdr funs) (cdr names))))))))
      )))


;; This is a front-end to run-compiler that reads source from a file.
;; Either returns compiled program or writes it to a file.
;; Flags:
;;  'write-file : write output to file.
;; 
;;  'to-typed
;;  'almost-tokens
;;  'barely-tokens
;;  'full-tokens 
;;  'to-simcode
(define regiment-compile-file
  (let ()
    (define compile-target-flags '(to-typed almost-tokens barely-tokens full-tokens to-simcode))
    (define (do-the-compilation fn flags)
      (let ([type (extract-file-extension fn)])
	;; Do a little sanity checking between our options and our input file:
	(cond
	 ;; [2007.05.16] symargs is unbound... clean this junk up at some point.
#;
	 [(equal? type "tm") 
	  (if (or (memq '-l0 symargs) (memq '-l1 symargs))
	      (error 'regiment_command_line_compiler
		     "Cannot use -l0/-l1 with an input that's already a TM!: ~s" fn))]
	 [(equal? type "rs") (void)]
	 [(equal? type "ws") (void)]
	 [else (error 'regiment_command_line_compiler "invalid input file extension: ~s" fn)])
	;; ----------------------------------------
	(parameterize ([pass-list
			(cond
			 [(equal? type "rs") (pass-list)]
			 ;; We treat these as normal regiment files:
			 [(equal? type "ws") (pass-list)]
			 [(equal? type "tm") (list-remove-before cleanup-token-machine
								 (pass-list))]
			 [else (error 'regiment "unknown input file extension: ~s" type)]
			 )])

	  (mvlet ([(prog params) 
		   (parameterize ([current-directory (if (top-level-bound? 'start-dir)
							 (top-level-value 'start-dir)
							 (current-directory))])
		     (read-regiment-source-file fn))])
	    (let ((comped 
		   (if (memq 'to-simcode flags)
		       ;; This goes all the way to a sim-file and bakes the parameters right in:
		       (let ([comped1 (apply run-compiler prog flags)])
			 (apply compile-simulate-alpha comped1 params))
		       (apply run-compiler prog flags))))
	      (values comped params)
	      )))))
    ;----------------------------------------
    (lambda (fn . flags)      
      (ASSERT (andmap symbol? flags))
      (unless (= 1 (length (intersection flags compile-target-flags)))
	(error 'regiment-compile-file "expects exactly one flag to indicate compilation target"))
      (mvlet ([(comped params) (do-the-compilation fn flags)])
	(let ([compile-target (car (intersection flags compile-target-flags))]	    )
	  (if (not (memq 'write-file flags))
	      comped
	      (let* ([extension (case compile-target 
				  [(to-typed) ".sexp0"]
				  [(almost-tokens) ".sexp"]
				  [(barely-tokens) ".tm0"]
				  [(full-tokens) ".tm"]
				  [(to-simcode) ".sim.alpha"]
				  )]
		     [out_file (string-append (remove-file-extension fn) extension)])
		;; Don't overwrite our own input file!
		(if (equal? fn out_file) (set! out_file (string-append "out." extension)))
		(printf "~n  Writing compilation-result to: ~s ~n" out_file)
		(delete-file out_file)		
		(parameterize ([print-graph #t] 
			       [print-level #f] 
			       [print-length #f]
			       [pretty-maximum-lines #f])
		  (with-output-to-file out_file
		    (lambda ()
		      ;; Otherwise we need to propogate the params to the output file:
		      (if (memq 'to-simcode flags)
			  (pretty-print `(parameters ,@params)))
		      (pretty-print comped))))
		;'compiled-result-written-to-file
		;; Return the name of the output file:
		out_file
		)))
	))))

;; This is the second version of the compiler, uses deglobalize2
(define (run-compiler2 p . args)                              ;; Entrypoint.
  (parameterize ([pass-list (snoc deglobalize2 
				  (rdc(list-remove-after deglobalize (pass-list))))])
    (apply run-compiler p args)
    ))



;; This one just stops after deglobalize:
(define (compile-to-tokens p . args)                          ;; Entrypoint.
  (apply run-compiler p 'barely-tokens args))
(define (compile-almost-to-tokens p . args)                   ;; Entrypoint.
  (apply run-compiler p 'almost-tokens args))

;; This finishes off the compilation of scheme-format token machine.
;; It's just a front-end to run-compiler that restricts the passes we run over.
(define (assemble-tokmac tm . args)                           ;; Entrypoint.
  (printf "assem tokmac...\n" )
  (let ([starting-place 
	 (match tm
	   [(,lang ,prog ...)
	    (case lang
	      [(add-places-language) analyze-places] ;; Not strictly correct.
	      [(deglobalize-lang) deglobalize]
	      [(cleanup-token-machine-lang) cleanup-token-machine]
	      [(cps-tokmac-lang) cps-tokmac]
	      
					;[(haskellize-tokmac-lang) (error...
	      [else deglobalize])]
	   [,else deglobalize])
	 ])
  (let ((passes (cdr (list-remove-before starting-place (pass-list)))))
    (disp "Assembling tokmac with passes: " passes)
;    (lambda (tm)
      (parameterize ([pass-list passes])
	(apply run-compiler tm args)))))


(define (reg:printlog file)
  (let ((stream (reg:read-log file 'stream)))
    (let loop ((s (reg:read-log file 'stream)))
      (unless (null? s)
	(display (log-line->human-readable 0 (stream-car s) ()))
	(loop (stream-cdr s))))))

    ))







;; ====================================================================================================
;; WaveScope Compiler:

(define ws-pass-optional-stop 
  (lambda (x)
    (if (regiment-verbose)
	(IFDEBUG
	 (begin (parameterize ([pretty-line-length 160]
			       [print-length 300]
			       [print-level 60])
		    (newline)
		    ;(pretty-print (strip-annotations x))
		    (pretty-print x)
		    )
		  (printf "================================================================================\n\n")
		  x)
	   x)
	x)))
(define-syntax ws-run-pass
  (syntax-rules ()
    [(_ v pass)
     ;;(time (set! p (optional-stop (pass p))))
     (parameterize ([regiment-current-pass 'pass])
       (unless (regiment-quiet)
	 (printf "Running Pass: ~s\n" 'pass)(flush-output-port))
       (if (regiment-verbose)
	   (time (set! v (ws-pass-optional-stop (pass v))))
	   (set! v (ws-pass-optional-stop (pass v)))))
     ]))

;; EXPERIMENTAL:
;; Playing around with fusing passes.
;; This is *just* for optimization.
(define-syntax ws-run-fused/disjoint
  (syntax-rules ()
    [(_ v pass* ...)
     ;; [2007.06.13] For now having problems with this, so just disabling it.
     (begin (ws-run-pass v pass*) ...)
     
#;
     ;; DEBUGGING.
     ;; Asserting that the result of the fused passes is *exactly* the
     ;; same as the passes individually.
     (let ([orig v])
       (printf "  EXPERIMENTAL: Running passes fused.")
       (time (begin (ws-run-pass v pass*) ...))
       ;; Now the fused version:
       (printf "Now running fused version: ~s\n" '(pass* ...))
       (let ([fusedver (time ((fuse-passes/disjoint pass* ...) orig))])
	 ;(ASSERT (equal? v fusedver))
	 (unless (equal? (reunique-names v) (reunique-names fusedver))
	   (diff (reunique-names v) (reunique-names fusedver))
	   ;(error 'ws-run-fused/disjoint "didn't match"))
	   (inspect (reunique-names fusedver))
	 )))]))

;; This just encapsulates the first few steps of the compiler.
;; Everything up to and including type checking the program.
(define (ws-compile-until-typed p)

  (ws-run-pass p verify-regiment)

  ;; TEMPTOGGLE:
  ;(ws-run-pass p eta-primitives)
  (ws-run-pass p pass_desugar-pattern-matching)

  (ws-run-pass p resolve-varrefs)

  ;; TODO: Insert optional PRUNE-UNUSED pass to quickly prune unused code.

  (ws-run-pass p resolve-type-aliases)
  (ws-run-pass p ws-label-mutable)

  ;; This is the initial typecheck. 
  (parameterize ([inferencer-enable-LUB     #f]
		 [inferencer-let-bound-poly #t])
    (ws-run-pass p retypecheck))
  p)

;; [2006.08.27] This version executes the WaveScript version of the compiler.
;; It takes it from (parsed) source down as far as WaveScript 
;; can go right now.  But it does not invoke the simulator or the c_generator.
(define run-ws-compiler             ;; Entrypoint.
  ; FIXME: this case-lambda is probably a temporary construction
  (case-lambda
    [(p params)                               (run-ws-compiler p params () #f)]
    [(p params disabled-passes)               (run-ws-compiler p params disabled-passes #f)]
    [(p params disabled-passes already-typed)

  (define (do-typecheck lub poly)
    (parameterize ([inferencer-enable-LUB      lub]
		   [inferencer-let-bound-poly poly])
      (time-accum (ws-run-pass p retypecheck))))
  ;; There are currently two different typecheck configs that we use.
  ;; One for the meta language, one for the object.
  ;; The meta language has let-polymorphism, and doesn't bindings to LUB types.
  (define (do-early-typecheck) (do-typecheck #f #t))
  ;; The object language is the reverse:
  (define (do-late-typecheck)  (do-typecheck #t #f))

  ; already-typed is now its own param. --mic
  ;(set! already-typed (if (null? already-typed) #f (car already-typed)))
  
  ;; [2008.01.10] This is a need that pops up frequently.
  ;; When there's a stray datum that should be type annotated, this helps find it.
  (define (assure-type-annotated prog pred)
    (define asserts (deep-assq-all 'assert-type prog))
    (define hits    (deep-all-matches pred prog))
    (for-each (lambda (hit)
		(unless (ormap (match-lambda ((assert-type ,ty ,ob)) (eq? ob hit))
			       asserts)
		  (warning 'assure-type-annotated "object was not type annotated: ~s" hit)))
      hits))

  (define (run-that-compiler)
    (parameterize ()
      (ws-pass-optional-stop p)
      (unless already-typed (set! p (ws-compile-until-typed p)))
      (unless (regiment-quiet) (printf "Program verified.\n"))

      ;; FIXME FIXME FIXME [2007.09.07] It seems that a repeated typecheck
      ;; here breaks something wrt "union2" and sum types... 
      ;; Perhaps the LUB typing isn't being implemented correctly.
      
      ;;(DEBUGMODE (do-early-typecheck) (void))
      ;;(do-early-typecheck)

  ;; [2007.07.06] Moving this back where it belongs... after typechecking
  ;; The only reason it was moved earlier was to accomodate using a hash table for type environments
  ;; ... which didn't work anyway.
  (ws-run-pass p rename-vars)

;  (DEBUGMODE (do-early-typecheck) (void))
  (ws-run-pass p eta-primitives)
  (ws-run-pass p desugar-misc)
  (ws-run-pass p remove-unquoted-constant)
 
  ;; Run this twice!!!
  ;;;;(ws-run-pass p degeneralize-arithmetic)

;; FIXME Broken here too:
;  (DEBUGMODE (do-early-typecheck) (void))
  ;; This doesn't work either:
;  (do-typecheck #t #t)


  
  ;; <OPTIMIZATION>: REWRITE RULES
  ;; -----------------------------------------
  (IFCHEZ
   (when (memq 'rewrites (ws-optimizations-enabled))
    (ws-run-pass p hide-special-libfuns)
    (dump-compiler-intermediate (strip-annotations p) ".__hidden.ss")
    (parameterize ([regiment-primitives
		    (append (map (match-lambda ([,lhs ,ty ,rhs]) 
				   (match ty
				     [(,args ... -> ,res) `(,lhs ,args ,res)]))
			      (cdr (ASSERT (project-metadata 'special-libfuns p))))
			    (regiment-primitives))])
      (ws-run-pass p eta-primitives)
      (ws-run-pass p interpret-meta) (do-early-typecheck)
      (ws-run-pass p rename-vars)
      (dump-compiler-intermediate (strip-annotations p) ".__presmooshed.ss")
      (ws-run-pass p smoosh-together)      
      (dump-compiler-intermediate (strip-annotations p) ".__smooshed.ss")

      ;(inspect (match p [(,lang '(program ,bod . ,_)) bod]))
      (ws-run-pass p rewrite-rules)
      (dump-compiler-intermediate (strip-annotations p) ".__rewritten.ss")
      ;(inspect (match p [(,lang '(program ,bod . ,_)) bod]))
      (ws-run-pass p reveal-special-libfuns)))
   (void))
  ;; -----------------------------------------

;  (ws-run-pass p interpret-meta) (do-early-typecheck)  ;; Testing idempotentcy 
;  (DEBUGMODE (dump-compiler-intermediate p ".__elaborated_first.ss"))


  ;; <OPTIMIZATION>: FUSE BOXES
  ;; -----------------------------------------
  ;; RRN: Will enable merge-iterates as soon as the backend can handle (app _) constructs...
  ;(unless (memq 'merge-iterates disabled-passes)
  ;  ;(pretty-print p)
  ;  (ws-run-pass p merge-iterates)
  ;  ;(pretty-print p)
  ;  ) ;; <Optimization>
  #;
  (when (or (memq 'fuse (cdr (or (assoc 'optimizations params) '(_ . ()))))
            (memq 'fuse (ws-optimizations-enabled))
            (memq 'merge-iterates (ws-optimizations-enabled)))
    (ws-run-pass p simple-merge-policy:always)
    (ws-run-pass p simple-merge-iterates)
    ;; Inline those function defs again.
    ;; This really is terrible code bloat... need to simply support
    ;; first order functions in the backends.  Might not be time for
    ;; that right this second though.
    ;(ws-run-pass p interpret-meta) (do-early-typecheck)
    )
  ;; -----------------------------------------


  ;; <METAPROGRAM-EVAL>: 
  ;; -----------------------------------------
;  (inspect p)  
  (printf "  PROGSIZE: ~s\n" (count-nodes p))
  ;(dump-compiler-intermediate (strip-annotations p) ".__preelab.ss")
  (if (regiment-quiet) (ws-run-pass p interpret-meta) (time (ws-run-pass p interpret-meta)))
;  (time (ws-run-pass p static-elaborate))
  (printf "  PROGSIZE: ~s\n" (count-nodes p))
  ;(dump-compiler-intermediate (strip-annotations p) ".__elaborated.ss")

  ;;;;;;;;;;;;;; POST ELABORATION: ;;;;;;;;;;;;;;;;;;;;;

  (when (or (regiment-verbose) (IFDEBUG #t #f)) (dump-compiler-intermediate p ".__elaborated.ss"))
;  (inspect (let-spine 1 p))
;  (inspect (let-spine 4 p))

#;
  (begin 
    (with-output-to-file "./pdump_new"  (lambda () (fasl-write (profile-dump)))  'replace)
    (dump-compiler-intermediate p ".__elaborated.ss")
    (inspect (let-spine 4 p))
    (exit 0)
    )

  ;; We want to immediately get our uniqueness property back.
  (ws-run-pass p rename-vars)

;  (DEBUGMODE (do-late-typecheck))
;  (do-late-typecheck)

  ;; NOTE: SHOULD BE SAFE TO TURN OFF LET-BOUND-POLYMORPHISM HERE:
  (ws-run-pass p degeneralize-arithmetic)

  ;; We MUST typecheck before verify-elaborated.
  ;; This might kill lingering polymorphic types ;)
  (do-late-typecheck)

  (IFDEBUG 
   (unless (regiment-quiet)
     (printf "Post elaboration types: \n")
     (print-var-types p +inf.0)) 
   (void))
  
  ;; This just fills polymorphic types with unit.  These should be
  ;; things that don't matter.  We typecheck afterwards to make sure
  ;; things still make sense.
  ;(ws-run-pass p kill-polymorphic-types)
  (ws-run-pass p strip-unnecessary-ascription)  
  (ws-run-pass p verify-elaborated)

  ;; [2007.10.27] For MLton we might should remove at least part of this pass:
  (ws-run-pass p anihilate-higher-order)  ;; Of a kind with "reduce-primitives"

  (IFDEBUG (do-late-typecheck) (void))

  ;; NOTE: wavescript-language won't work until we've removed complex constants.
  ;; Quoted arrays work differently in WS than in Scheme.
  ;; (WS has a freshness guarantee.)
  (ws-run-pass p remove-complex-constant)

  ;; Now fill in some types that were left blank in the above:
  ;; Shouldn't need to redo LUB because the types are already restrictive???
  (do-late-typecheck)

  ;; Trying this *before* unlift.  
  ;; The function here is to strip all but the essential type annotations.
;  (ws-run-pass p strip-irrelevant-polymorphism)

  ;; This three-step process is inefficient, but easy:
  ;; This is a hack, but a pretty cool hack.
  (ws-run-pass p lift-polymorphic-constant)
  (do-late-typecheck)

  (ws-run-pass p unlift-polymorphic-constant)
  (ws-run-pass p split-union-types) ;; monomorphize sum types (not necessary for MLton)

  ;; [2007.10.11] Right now this messes up demo3f:
  (ws-run-pass p strip-irrelevant-polymorphism)

  ;; <-------- NOTE: Old location for merge-iterates. [2007.11.01]

  (IFDEBUG (do-late-typecheck) (void))

  ;; (5) Now we normalize the residual in a number of ways to
  ;; produce the core query language, then we verify that core.
  (ws-run-pass p reduce-primitives) ; w/g 
 
  (IFDEBUG (do-late-typecheck) (void))

  (ws-run-pass p type-annotate-misc)
  (assure-type-annotated p (lambda (x) (equal? x ''())))

  (when (eq? (compiler-invocation-mode) 'wavescript-compiler-c)
    (ws-run-pass p explicit-toplevel-print))

  (ws-run-pass p optimize-print-and-show) ;; Should be optional.
  (ws-run-pass p generate-printing-code)

  (when (eq-any? (compiler-invocation-mode) 'wavescript-compiler-c)  ;'wavescript-simulator
    (ws-run-pass p embed-strings-as-arrays)

    (DEBUGMODE 
     (let ([tmp (deep-assq-all 'String p)])
       (unless (null? tmp) 
	 (warning 'embed-strings-as-arrays "The symbol String occured in the output.  Here's a snippet:")
	 (inspect tmp))))
    ;(ws-run-pass p remove-complex-constant) ;; Should we leave those array constants?
    )
  
  (when (eq-any? (compiler-invocation-mode) 
	     'wavescript-compiler-c
	     ;'wavescript-compiler-xstream
	     )
    (ws-run-pass p type-annotate-misc)
    (ws-run-pass p generate-comparison-code)
    )

  ;; Should also generate printing code:
  ;(ws-run-pass p generate-printing-code)

;  (ws-run-pass p uncover-free)

  ;(ws-run-pass p purify-letrec)
  ;; This is what we need to do.

  ;; Now that we're done with elaboration we should take the stream
  ;; processing spine, convert it to let.
  ;; For the time-being we don't even need letrec in the object code
  ;; because functions have all been inlined.
  (ws-run-pass p remove-letrec) ;; This is a bit redundant with interpret-meta, which already sorts the bindings.
  (IFDEBUG (do-late-typecheck) (void)) ;; Do a typecheck to make sure it works without letrec.

  (ws-run-pass p standardize-iterate) ;; no fuse


  ;; <OPTIMIZATION>: FUSE BOXES
  ;; -----------------------------------------
  ;; mic: for now, simple-merge-iterates does manual inlining
  ;; (interpret-meta doesn't work this far down in the compiler, yet)
  (when (or (memq 'fuse (cdr (or (assoc 'optimizations params) '(_ . ()))))
            (memq 'fuse (ws-optimizations-enabled))
            (memq 'merge-iterates (ws-optimizations-enabled)))
    (ws-run-pass p smoosh-together)
    (ws-run-pass p simple-merge-policy:always)
    (ws-run-pass p simple-merge-iterates)
    (ws-run-pass p rename-vars)
    (ws-run-pass p standardize-iterate)
    )


;  (ws-run-pass p introduce-lazy-letrec)
;  (ws-run-pass p lift-letrec)
;  (ws-run-pass p lift-letrec-body)

;  (profile-clear)
  (ws-run-pass p ws-remove-complex-opera*)
  ;; Don't do this yet!!  (At least make it debug only.)
  ;; Remove-complex-opera* added new type variables, but delay a
  ;; couple more passses before type checking.
  (IFDEBUG (do-late-typecheck) (void))
;  (with-output-to-file "./pdump_new"  (lambda () (fasl-write (profile-dump)))  'replace)

  ;; This is an example of the experimental pass-fusion mechanism.
  ;(ws-run-fused/disjoint p ws-normalize-context ws-lift-let)
  (ws-run-pass p ws-normalize-context)
  (ws-run-pass p ws-lift-let)
  
  ; --mic, <OPTIMIZATION>
  (unless (memq 'propagate-copies disabled-passes)
    (ws-run-pass p propagate-copies)
    )

  ;; Mandatory re-typecheck.  Needed to clear out some polymorphic
  ;; types that might have snuck in from lifting.
  ;(do-typecheck #f #f)
  (do-late-typecheck)

  ;; Replacing remove-complex-opera* with a simpler pass:
  ;(ws-run-pass p flatten-iterate-spine) ;; Not anymore....
  (DEBUGMODE (dump-compiler-intermediate p ".__nocomplexopera.ss"))

  (ws-run-pass p type-annotate-misc)
  (assure-type-annotated p (lambda (x) (equal? x ''())))
  (ws-run-pass p reify-certain-types)

  ;; for analysis of data rates between boxes
  ;; uncomment to enable
  (IFCHEZ   
   (when (memq 'profile (ws-optimizations-enabled))
     (unless  (memq 'annotate-with-data-rates disabled-passes)
       (ws-run-pass p annotate-with-data-rates)))
   (void))


;   (set! prog (ws-add-return-statements prog))
  ;(ws-run-pass p ws-add-return-statements)

  (unless (regiment-quiet)
    (printf "Total typechecker time used:\n")
    (time-accum-report)(newline))
;  (with-output-to-file "./pdump_new"  (lambda () (fasl-write (profile-dump)))  'replace)
;  (exit)

  ;; Here we dump it to a .dot file for graphviz.
  ;; Wasted work if we're going to apply explicit-stream-wiring again later.
  (IFCHEZ
   (when (dump-graphviz-output)
    (string->file (output-graphviz (explicit-stream-wiring p)) "query.dot")
    ;; If this fails, oh well:
    (system "rm -f query.png")
    (time (system "dot -Tpng query.dot -oquery.png")))
   (void))
  

  p)) ;; End run-that-compiler

  (ASSERT (memq (compiler-invocation-mode)  
    '(wavescript-simulator wavescript-compiler-c wavescript-compiler-xstream wavescript-compiler-caml)))
  
  (run-that-compiler)
  ;(if (regiment-quiet) (run-that-compiler) (time (run-that-compiler)))

]))




;; ================================================================================
;; For usability, the below ws* procedures (wsint wscomp wscaml wsmlton...) can 
;; each take their input as a filename, a parsed program, or a port.
;; This is the coercion function they use to coerce their input to a parseed program.
(define (coerce-to-ws-prog x input-params)
  (let ((prog
         (cond  [(input-port? x)
                 (unless (regiment-quiet) (printf "WSCOMP: Loading WS source from port: ~s\n" x))
                 ;; We assume this is parsed but not post-processed:
                 (wsparse-postprocess (read x))]
                [(string? x) 
                 (unless (regiment-quiet) (printf "WSCOMP: Loading WS source from file: ~s\n" x))
                 (read-wavescript-source-file x)]
                [(list? x)   
                 (unless (regiment-quiet) (printf "WSCOMP: Evaluating WS source: \n \n"))
                 x]
                [else (error 'wsint "bad input: ~s" x)])))
    
    ;; add in the input-parameters
    (set! prog
          (match prog
            [(,lang '(program ,s* ... ,types))
             `(,lang '(program ,@s* (input-parameters ,input-params) ,types))]
            
            ;; do nothing in this case
            [,oth prog]))

    prog))


;; Dump output of intermediate stages in copmiler.
;; (This sets a variety of parameters controlling printing)
(define (dump-compiler-intermediate prog fn)
  (with-output-to-file fn
    (lambda () 
      (parameterize ([pretty-line-length 200]
		     [pretty-maximum-lines #f]
		     [print-level #f]
		     [print-length #f]
		     [print-graph #f])
	(parameterize-IFCHEZ ([pretty-one-line-limit 100])
			     (pretty-print ;(strip-annotations prog)
			      prog)))
      (flush-output-port))
    'replace))

(define ws-disabled-by-default '(merge-iterates ))

(define ws-default-wavescope-scheduler 'default-scheduler)



;; ================================================================================
;; The WaveScript "interpreter".  (Really a wavescript embedding.)
;; It loads, compiles, and evaluates a wavescript query.
;; .param x - can be an input port, a filename, or a wavescript AST (list)
;; .returns - A stream of results
(define-values (wsint wsint-early)
  (let ()

    (define (cleanup-compiler-tmpfiles)
      (let ([please-delete-file 
	     (lambda (f) (if (file-exists? f) (delete-file f)))])
	;; Delete these files so that we don't get mixed up.		  
	(please-delete-file ".__types.txt")
	(please-delete-file ".__inputprog.ss")
	(please-delete-file ".__compiledprog.ss")
	(please-delete-file ".__elaborated.ss")
	(please-delete-file ".__nocomplexopera.ss")))

    (define (wsint-parameterize th)
      (parameterize ([compiler-invocation-mode 'wavescript-simulator]
		     ;;[regiment-compile-sums-as-tuples ]
		     ;;		 [included-var-bindings '()]
		     [regiment-primitives
		      ;; Remove those regiment-only primitives.
		      (difference (regiment-primitives) regiment-distributed-primitives)])
	(th)))

    (define (early-part x input-params . flags)
      (define prog (coerce-to-ws-prog x input-params))
      (define _ (begin (unless (regiment-quiet)
			 (printf "Evaluating program: ~a\n\n"
				 (IFDEBUG "(original program stored in .__inputprog.ss)" "")))
		       (DEBUGMODE 
			(cleanup-compiler-tmpfiles)
			(dump-compiler-intermediate prog ".__inputprog.ss")
			)))
      (define typed (ws-compile-until-typed prog))
      (define __ 
	(begin 
	  (unless (regiment-quiet)
	    (printf "Program verified, type-checked. (Also dumped to \".__parsed.ss\".)")
	    (printf "\nProgram types: (also dumped to \".__types.txt\")\n\n")
	    (if (regiment-verbose)
		(print-var-types typed +inf.0)
		(print-var-types typed 1))
	    (flush-output-port))
	  (DEBUGMODE
	   (with-output-to-file ".__types.txt"
	     (lambda () (print-var-types typed +inf.0)(flush-output-port))
	     'replace))))
      typed)

    (define (make-uniontype-defs x)
      (match x
	[#f '(void)]
	[(union-types ((,name* . ,_) [,fld** ,ty**] ...) ...)
	 (cons 'begin
	   (map (lambda (fld ty) 
		  `(define ,fld (lambda (val) (make-uniontype ',fld val))))
	     (apply append fld**)
	     (apply append ty**)))]))

  (define (wsint x input-params . flags)                                             ;; Entrypoint.      
    (wsint-parameterize
     (lambda ()    
       (define typed (early-part x input-params))
       (define disabled-passes (append (map cadr (find-in-flags 'disable 1 flags)) ws-disabled-by-default))
       (define compiled (run-ws-compiler typed input-params disabled-passes #t))
       (unless (regiment-quiet) (printf "WaveScript compilation completed.\n"))
       (DEBUGMODE (dump-compiler-intermediate compiled ".__compiledprog.ss"))
       (run-wavescript-sim compiled))))
  
  (define (wsint-early x input-params . flags)
    (wsint-parameterize
     (lambda ()
       (define p x)
       (time (begin 
	       (set! p (early-part p input-params))
	       (ws-run-pass p eta-primitives)
	       ;;       (ws-run-pass p desugar-misc)

	       ;; Need to convert readFile to __readFile
	       ;(ws-run-pass p type-annotate-misc)
	       (ws-run-pass p strip-src-pos)
	       (ws-run-pass p reify-certain-types)
	       (ws-run-pass p strip-annotations)
	       ))
       (printf "Running program EARLY:\n")
       (run-wavescript-sim p))))

  (values wsint wsint-early)))

;; When invoking "ws" from the command line, this is the function that
;; determines where the stream goes.  I.e. to an interactive browser
;; or to a file.
(define (wsint:direct-stream strm)
  (IFCHEZ (import streams) (begin))
  
  (define (run-to-tuplimit) (first-value (stream-take (wsint-tuple-limit) strm)))
  (define (run) (if (wsint-time-query) (time (run-to-tuplimit)) (run-to-tuplimit)))

  (cond
   [(not (stream? strm))
    (eprintf  "\nWS query returned a non-stream value:\n  ~s\n" strm)]
   [(and (wsint-tuple-limit) (wsint-output-file))
    ;; This could be more efficient, but for now we just take it all
    ;; into memory and then dump it all to disk.
    (slist->file  (run)  (wsint-output-file) 'display)]
   [(wsint-tuple-limit)
    ;; TODO, use proper WS printing:
    (for-each pretty-print (run))]

   [(wsint-output-file)
    (eprintf "Dumping output to file: ~s\n" (wsint-output-file))
    (stream-dump strm (wsint-output-file))]
   [else
    ;; Otherwise, browse it interactively:
    (parameterize ([print-vector-length #t])
      (browse-stream strm))]))

;; This functionality should probably be included in the wavescript language itself.
;; But due to limitations in the language-mechanism that I've been usng, that doesn't work presently.
(define run-wavescript-sim 
  (lambda (p)
    ;; New Streams:

    ;; [2007.02.06] Now we wrap it with a little extra to run
    ;; the query.  This is needed as a result of switching over
    ;; to imperative streams.
    
    ;; [2007.07.05] TODO: This means that the "wavescript-language" isn't really complete.
    ;; It SHOULD be self contained, even if that means discarding the existing "language-mechanism.ss"
    (wavescript-language
     (match (strip-types p)
       [(,lang '(program ,body ,_ ...))
        ;; If strip-types worked there shouldn't be any VQueue symbols!
        (DEBUGASSERT (not (deep-assq 'VQueue (list body _))))
        `(begin (reset-wssim-state!)
                (run-stream-query ,body))
        ]))
))

;; ================================================================================
;; WaveScript Compiler Entrypoint:

(define (wscomp x input-params . flags)                                 ;; Entrypoint.  
  (define new-version? (not (null? (find-in-flags 'wsc2 0 flags))))
 (parameterize ([compiler-invocation-mode 
		 (if new-version? 'wavescript-compiler-c 'wavescript-compiler-xstream)]
		[regiment-primitives
		 ;; Remove those regiment-only primitives.
		 (difference (regiment-primitives) regiment-distributed-primitives)])
   (define prog (coerce-to-ws-prog x input-params))
   (define typed (ws-compile-until-typed prog))
   (define disabled-passes (append (map cadr (find-in-flags 'disable 1 flags)) ws-disabled-by-default))
   (define wavescope-scheduler (car (append (map cadr (find-in-flags 'scheduler 1 flags))
                                            `(,ws-default-wavescope-scheduler))))
   ;; [2007.11.23] Are we running the new C backend?
   
   (define outfile (if new-version? "./query.c" "./query.cpp"))  

   (define (run-wscomp)
     
     ;;(ASSERT (andmap symbol? flags)) ;; [2007.11.06] Not true after Michael added (scheduler _) flags.
     
     ;;(unless (regiment-quiet) (printf "Compiling program for C++/XStream backend\n\n"))
     ;;;(pretty-print prog)

     (unless (regiment-quiet)
       (printf "\nTypecheck complete, program types:\n\n")
       (if (regiment-verbose) 
	   (print-var-types typed +inf.0)
	   (print-var-types typed 1))
       (flush-output-port))
     
     ;; Run the main body of the compiler.
     (set! prog (run-ws-compiler typed input-params disabled-passes #t))
     
     (unless (regiment-quiet) (printf "\nFinished normal compilation, now emitting C++ code.\n"))

     ;;(inspect (deep-assq-all 'wsequal? prog))
     ;;(ws-run-pass prog generate-comparison-code) ;; RUNNING AGAIN
     ;;(ws-run-pass prog ws-lift-let)
     ;;(inspect (deep-assq-all 'wsequal? prog))

     (ws-run-pass prog convert-sums-to-tuples)
     #;
     ;; Retypecheck to bring back some types that were lost:
     (parameterize ([inferencer-enable-LUB #t]
		    [inferencer-let-bound-poly #f])
       (ws-run-pass p retypecheck))
     
     (when (regiment-verbose)
       (pretty-print prog)
       ;;   (printf "================================================================================\n")
       (printf "\nNow emitting C code:\n"))


      (if new-version?
	  (begin 
	    (ws-run-pass prog nominalize-types)
	    (ws-run-pass prog gather-heap-types)

	    (dump-compiler-intermediate prog ".__beforeexplicitwiring.ss")
	    
	    (ws-run-pass prog explicit-stream-wiring)
	    (dump-compiler-intermediate prog ".__afterexplicitwiring.ss")

	    (printf "  PROGSIZE: ~s\n" (count-nodes prog))
	    (time (ws-run-pass prog insert-refcounts))
	    
	    ;; It's painful, but we need to typecheck again.
	    ;; HACK: Let's only retypecheck if there were any unknown result types:
	    (let ([len (length (deep-assq-all 'unknown_result_ty prog))])
	      (unless (zero? len)
		(printf "*** GOT AN UNKNOWN RESULT TYPE ***\n")
		;(inspect prog)		
		(parameterize ([inferencer-enable-LUB #t]
			       [inferencer-let-bound-poly #f])
		  (time (ws-run-pass prog retypecheck)))))

	    (dump-compiler-intermediate prog ".__after_refcounts.ss")
	    ;(print-graph #f)  (inspect prog)

	    (printf "  PROGSIZE: ~s\n" (count-nodes prog))	 	    

	    (time (ws-run-pass prog emit-c2))
	    
	    (string->file (text->string prog) outfile)
	    (unless (regiment-quiet)
	      (printf "\nGenerated C output to ~s.\n" outfile))
	    )
       (begin 
	 (ws-run-pass prog nominalize-types)

	 (DEBUGASSERT (dump-compiler-intermediate prog ".__almostC.ss"))   
	 (string->file 
	  (text->string 
	   (wsquery->text prog wavescope-scheduler))
	  outfile)   
	 (unless (regiment-quiet)
	   (printf "\nGenerated C++/XStream output to ~s.\n" outfile))
	 )))
   
   (if (regiment-quiet) (run-wscomp) (time (run-wscomp)))
   )
 ) ; End wscomp






;; ================================================================================
;; WaveScript OCAML Compiler Entrypoint:

(define (wscaml x input-params . flags)                                 ;; Entrypoint.  
  (parameterize ([compiler-invocation-mode 'wavescript-compiler-caml]
		 [regiment-primitives ;; Remove those regiment-only primitives.
		  (difference (regiment-primitives) regiment-distributed-primitives)])
    (define disabled-passes (append (map cadr (find-in-flags 'disable 1 flags)) ws-disabled-by-default))
    (define outfile "./query.ml")
    (define prog (begin (ASSERT list? x) x))

    (ASSERT (andmap symbol? flags))
    (set! prog (run-ws-compiler prog input-params disabled-passes #f))
    (set! prog (explicit-stream-wiring prog))
    (string->file (text->string (emit-caml-wsquery prog)) outfile)
    (printf "\nGenerated OCaml output to ~s.\n" outfile)
    ))

;; ================================================================================
;; WaveScript MLTON Compiler Entrypoint:

(define (wsmlton x input-params . flags)                                 ;; Entrypoint.  
  (parameterize ([compiler-invocation-mode 'wavescript-compiler-caml]
		 [regiment-primitives ;; Remove those regiment-only primitives.
		  (difference (regiment-primitives) regiment-distributed-primitives)])
    (define disabled-passes (append (map cadr (find-in-flags 'disable 1 flags)) ws-disabled-by-default))
    (define outfile "./query.sml")
    (define prog (coerce-to-ws-prog x input-params))
    
    (ASSERT (andmap symbol? flags))
    (set! prog (run-ws-compiler prog input-params disabled-passes #f))
    (ws-run-pass prog explicit-stream-wiring)
    
    ;(IFCHEZ (string->file (output-graphviz prog) "query.dot") (void))

;    (inspect prog)
    (printf "SIZE BEFORE MLTON CODEGEN: ~s\n" (count-nodes prog))

    (time (ws-run-pass prog emit-mlton-wsquery))

;    (inspect 'made-it-this-far)
    (string->file (text->string prog) outfile)
    (printf "\nGenerated MLton output to ~s.\n" outfile)
    ))



;===============================================================================
;;; These functions are used for command-line invocation of the whole system:

(define (print-help)
  (printf "Regiment system, version ~s (rev ~s) (loaded from ~a)\n" 
	  (regiment-version) 
	  (top-level-value 'svn-revision)
	  (top-level-value 'regiment-origin))
  (printf "Usage: regiment command [options] ~n")
  (printf "~n")
  (printf "Commands: ~n")
  (printf "  help          prints this message~n")
  (printf "  compile  (c)  compile Regiment source (.rs) to token machines~n")
  (printf "  simulate (s)  simulate a token machine or simulator file~n")
  (printf "  interact (i)  start up Scheme REPL with Regiment loaded~n")
  (printf "  test     (t)  run all regiment tests~n")
  (printf "  log      (l)  simulator trace manipulation mode~n")
  (printf "  wsint    (wsint)  WaveScript evaluator mode~n")
  (printf "  wscomp   (wscomp) WaveScript (C++) compiler mode~n")
  (printf "  wsc2     (wsc2)   WaveScript (C) compiler mode ver 2~n")
  (printf "  wsml     (wsml)   WaveScript compiler SML (MLton) backend~n")
  (printf "  wscaml   (wscaml) WaveScript compiler Caml backend~n")
  (printf "~n")
  (printf "General Options:  ~n")
  (printf "  -v   verbose compilation/simulation, includes warnings~n")
  (printf "  -q   suppress banners and other nonessential output~n")
  (printf "~n")
  (printf "Regiment Compiler Options: ~n")
  (printf "  -d2  use new compiler: deglobalize2 ~n")
  (printf "  -lt  type check only, print typed program to stdout           ~n")
  (printf "  -ltt type check only, print *only* top level types to stdout  ~n")
  (printf "  -l0  stop compilation just before deglobalize          (.sexp)~n")
  (printf "  -l1  compile barely to tokens (just after deglobalize) (.tm0)~n")
  (printf "  -l2  compile to just tokens (maximally lowered)        (.tm)~n")
  (printf "  -l4  output generated simulator-alpha code             (.sim.alpha)~n")
  (printf "  -l5  output generated NesC/Tossim code                 (.sim.nesc) ~n")
  (printf "  -debug       print extra info, inspect errors ~n")
  (printf "~n")
  (printf "Simulator Options: ~n")
  (printf "  -timeout <n>  timeout after n clock ticks\n")
  (printf "  -plot         when simulation finishes, gnuplot output\n")
  (printf "  -repl         when simulation finishes, run interactive REPL\n")
  (printf "~n")
  (printf "Interactive Options: ~n")
  (printf "  --script  <file>    run a scheme file as a script~n")
  (printf "  -exit-error        exit process w/ nonzero error code on a scheme error~n")
  (printf "~n")
  (printf "Log-manipulation Options: ~n")
  (printf "  -print    <file>    print any log-file in human readable format~n")
  (printf "  -examine  <file>    describe the chunking format of an existing logfile~n")
  (printf "  -reencode <f1> <f2> reencode a logfile in a compressed but fast-loading way~n")
  (printf "  -vw <worldfile>     (not really a log) if gui is loaded, view saved world~n")
  (printf "~n")
  ;(display (file->string (string-append (REGIMENTD) "bin/regiment_opts.txt")))
  (display (file->string (string-append (REGIMENTD) "/bin/ws_opts.txt")))
  )

(define (print-ws-prim-table)
  (define prims (difference (regiment-primitives) regiment-distributed-primitives))
  ;; These are prims that we don't want to publicize for whatever reason:
  (define secret-prims
    `(world anchor locdiff sense nodeid 
      HACK_O_RAMA Secret:newTimebase
      tuple tupref
      static statref
      ,@(filter (lambda (n) (define str (symbol->string n))
			(or (memq #\? (string->list str))
			    (and (> (string-length str) 3)
				 (equal? (substring str 0 2) "__"))))
	  (map car prims)))
    )
  (define (pad name)
    (define namestr (format "~a" name))
    (define padincr 6)
    (define padsize (- padincr (remainder (string-length namestr) padincr)))
    (define padding (make-string (if (= padsize padincr) 0 padsize) #\space))
    (format "~a~a" name padding))
  (for-each (lambda (entry)
	      (match entry
		[(,name ,args ,retty . ,rest)
		 (unless (null? rest) (inspect rest))
		 (when (and (list? args) (not (memq name secret-prims)))
		   (printf "~a :: ~a\n" (pad name) (show-type `(,@args -> ,retty))))]
		;; Constants:
		[(,name ,ty)
		 (unless (memq name secret-prims)		   
		   (printf "~a :: ~a\n" (pad name) (show-type ty)))
		 ])
	      ) prims))

(define (print-types-and-exit prog . opts)
  (define verbose? (memq 'verbose opts))
  (printf ";; Regiment program with infered types: \n")
  ;; Run just the verify regiment pass, it will associate types:
  (parameterize ([print-vector-length #f])
  (match (verify-regiment `(lang '(program ,prog)))
    [(,lang '(program ,p ,t))
     (match p
       [(letrec ([,id* ,t* ,rhs*] ...) ,bod)
	(for-each (lambda (id t rhs)
		    (if verbose?
			(begin (pretty-print `(define ,id : ,t ,rhs))(newline))
			(printf "~a : ~a\n" (pad-width 30 id) t)))
	  id* t* rhs*)
	(if verbose? (pretty-print bod))]
       [,p (pretty-print p)])
     ;(printf "\n;; Regiment program return type: ~a\n" t)
     (printf "  : ~a\n" t)
     (exit 0)]
    [,other (error 'print-types-and-exit "bad output from verify-regiment: ~s" other)])))

(define (regiment-exit code)
  ;; In case we're building a heap, we set this before we exit.
  ;(disp "SETTING HEAP: " regiment-origin (top-level-value 'regiment-origin))
  (set-top-level-value! 'regiment-origin "saved heap")
  ;(disp "HEAP SET: " regiment-origin (top-level-value 'regiment-origin))
  (exit code))


;; *THE* Main function for the regiment/wavescript process.
;; Takes an arbitrary number of strings (flags) as arguments.
(define main 
  (lambda args    
    (define makesimcode #f)
    (define outfile #f)
    (define plot #f)
    (define simrepl #f)
;    (disp "Main called w ARGS: " args)
    (when (null? args) (print-help) (regiment-exit 0))

    (IF_THREADS
     (begin       
       ;; No matter which arguments we're called with, let's go ahead and
       ;; initialize the thread system.  Really this should happen only
       ;; for compile-modes that may actually use it.
       (define-top-level-value 'desired-number-of-threads
	 (ASSERT (string->number (or (getenv "REGTHREADS") "1"))))
       ;; TODO: PUT IN CORRECT NUMBER OF CPUS!
       (init-par desired-number-of-threads)
)
     (void))

;    (printf "regimentc: compile regiment programs!~n")
    ;; This is a list of option flags mutated by "loop" below.
    ;; This is a bit sketchy.  The same flags are sent to run-compiler and run-simulator-alpha.
    ;; FIXME: may want to merge these two eventually; input-parameters added mostly for
    ;;        profiling information, which can have slightly more complex structure
    (let ([opts '()]
          [input-parameters '()])
      
      ;; This determines what mode we're in then calls "loop" to process the flags.
      (define (main)		
        ;; I keep disjoint options for the modes so I use the same option-processor for all modes (loop)
	(let ([symargs (map (lambda (x) (if (symbol? x) x (string->symbol x))) args)])

	  ;; [2007.01.29] Killing this:
	  ;(unless (null? (cdr symargs)) (printf "Processing options: ~s\n" (cdr symargs)))
	  (let ([mode (car symargs)] [filenames (loop (cdr symargs))])

	(define (acquire-input-prog callmode ) 
	  (match filenames
	    ;; If there's no file given read from stdin
	    [() (console-input-port)]
	    [(,fn ,rest ...) 
	     (unless (null? rest)
	       (error callmode  "bad extra filename(s) or flag(s) following filename ~s:\n ~s" fn rest))
	     ;; If it's a ws file we need to parse the file:
	     (if (equal? "ws" (extract-file-extension fn))
		 (or (read-wavescript-source-file fn)
		     (error callmode  "couldn't parse file: ~s" fn))
		 ;; Otherwise let's assume 
		 (open-input-file fn))]
	    [,else (error 'regiment "~a should take one file name as input, given: ~a" callmode else)]))

	;; AFTER, those options are processed we switch on the mode flag.
	(case mode
	  ;; Unit Test mode:
	  [(t test)
	   (define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (test-units)
	   ;(test-everything)
	   ]

	  ;; Compile mode:
	  [(c compile)
	   ;(define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (IFWAVESCOPE (void)
	     (if (null? filenames)
	       (begin
		 (printf "No input file.  Type top-level Regiment expression.~n")
		 (printf "> ")(flush-output-port)
		 (let* ([expr (read)])
		   (printf "~n Using default output file: out.tm...~n")		
		   (apply run-compiler expr "out.tm" opts)))
	       (begin 
		 (if (> (length filenames) 1)
		     (error 'regiment_command_line_compiler 
			    "can't handle more than one filename at a time currently: ~s" filenames))
		 ;; Otherwise we're good to go.
		 (cond 
		  [(memq 'type-only-verbose opts)
		   (print-types-and-exit 
		    (apply regiment-compile-file (car filenames) 'to-typed opts)
		    'verbose)]
		  [(memq 'type-only opts)
		   (print-types-and-exit 
		    (apply regiment-compile-file (car filenames) 'to-typed opts))]
		  [else 
		   (apply regiment-compile-file (car filenames) 'write-file opts)])
		 )))]

	  ;; Simulation mode (also may invoke compiler):
	  [(s simulate)
	   (IFWAVESCOPE (void)
	     (parameterize ([simulation-logger-level 1]
			  );; Only get send/receive, groundtruth, and newworld msgs.
	   (let ((fn (if (null? filenames)
			 "out.sim"
			 (car filenames))))
	     
	     (define-top-level-value 'go-sim
	       (lambda ()
		 (printf "Running simulation from file: ~a\n" fn)
		 (let ((result
			(apply load-regiment fn opts)
#;
			;; Be careful to watch for parameterization:	     
			(mvlet (([prog params] (read-regiment-source-file fn)))
			  (with-evaled-params params
					      (lambda () 
						(apply run-simulator-alpha prog 
						       'srand (current-time)
						       opts))))))
		   ;; Print simalpha stats:
		   (print-stats)
		   (if plot (gnuplot result))
		   (if simrepl (IFCHEZ (new-cafe) (read-eval-print-loop)))
		   result)))

	     (IF_GRAPHICS 
	      ;; This starts swl then evals the expression.
	      (bounce-to-swl '(go-sim))	      
	      (begin (printf "WOOT\n")
		     ((top-level-value 'go-sim))
		     ))
	     )))]

	  ;; Interactive mode.  A Scheme REPL.
	  ;; [2006.02.21] This is a better way of exposing the normal scheme startup behavior:
	  [(i interact)
	   (eprintf "Exposing Regiment through interactive read-eval-print loop:\n")
	   ;; [2006.02.28] We want to reset global bindings before loading any more source:
	   ;(eval '(import scheme))
	   ;; Can't trust new code to not mutate primitive names:
	   (IFCHEZ (optimize-level 1) (void))	   

	   (cond
	    [(null? filenames) 

;	     (printf "GOING INTO REPL ~s\n" main)
	     ;(eval '(require main_plt))
	     ;(eval '(require regiment_pltscript))
;	     (printf "GOT MODULE INTO TOP LEVEL ~s\n" (eval 'main))
	     (IFCHEZ (call-with-values new-cafe (lambda ls (apply exit ls)))
		     (read-eval-print-loop))]
	    ;; To run a script through "regiment"
	    ;; --script must be the first argument after "regiment i"
	    ;;
	    ;; This won't occur, chez grabs the --script parameter
	    ;; directly.  Code should go in the scheme-script parameter.
	    ;;
	    ;; Note, if we're doing it this way we pass the raw
	    ;; arguments, not those processed by "loop" above.
	    [(equal? (cadr args) "--script")
	     ;(printf "Using Regiment to invoke script: ~a\n" args)
	     ;(error 'regiment.ss "this shouldn't happen.")

	     ;; --script implies --exit-error: add that setting:
	     (loop '(-exit-error))
	     (IFCHEZ (apply orig-scheme-script (cddr args))
		     ;(error 'interact-mode "cannot currently run scripts through regiment in PLT Scheme")
		     (parameterize ([current-command-line-arguments (list->vector (cdddr args))])
		       (load (caddr args)))
		     )]
	    [else 
	     ;(inspect (list->vector args))
	     (IFCHEZ (apply orig-scheme-start (cdr args))
		     (error 'interact-mode "cannot currently run scripts through regiment in PLT Scheme")
		     )
	     ])]

	  ;; Printing SExp log files.
	  [(l log)
	   (IFWAVESCOPE (void)
	   (match (map string->symbol (cdr args))
	     [() (if (file-exists? "__temp.log") (reg:printlog "__temp.log")
		     (if (file-exists? "__temp.log.gz") (reg:printlog "__temp.log.gz")
			 (error 'regiment:log:print "no log file supplied or found")))]
	     [(-print ,file) 
	      (let loop ([s (reg:read-log (symbol->string file) 'stream)])
		(unless (stream-empty? s) 
		  (printf "~a\n" (stream-car s))
		  (loop (stream-cdr s))))]
	     [(-print ,_ ...) (error 'regiment:log:print "only can print exactly one logfile at a time: ~a" args)]
	     [(-reencode ,in ,out)
	      ;; Do not replace output file if it's there:
	      (let ((out (open-output-file (symbol->string out) '(compressed)))
		    (block-size 1000)  ;; 1000 lines of log chunked at a time.
		    (count 0))
		(progress-dots 
		 (lambda ()
		   (let loop ((in (reg:read-log (symbol->string in) 'stream))
			      (n block-size) (acc '()))
		     (cond
		      [(stream-empty? in) 
		       (fasl-write (list->vector (reverse! acc)) out)
		       (close-output-port out)]
		      [(fxzero? n)
		       (fasl-write (list->vector (reverse! acc)) out)
		       (loop in block-size '())]
		      [else 
		       (set! count (add1 count))
		       (loop (stream-cdr in) (fx- n 1) (cons (stream-car in) acc))]))
		   )))]
	     [(-reencode ,_ ...)
	      (error 'regiment:log:reencode 
		     "bad arguments for log reencoding, expects input and output file: ~a" args)]
	     [(-examine ,file)
	      (newline)
	      (let* ([file (symbol->string file)]
		     [in (lambda ()
			   (if (equal? (extract-file-extension file) "gz")
			       (open-input-file file 'compressed)
			       (open-input-file file)))]
		     [first (read (in))])
		;; First classify contents.
		(cond
		 [(vector? first)
		  (printf "Log file batched into vectors, first vector is size: ~a\n" (vector-length first))]
		 [(list? first)
		  (printf "Log file contains raw, unbatched log entries.\n")]
		 [else (printf "WARNING: Log file contains the following unrecognized object: ~a" first)])
		;; Next classify fasl/plaintext.
		(let* ([in (in)]
		       [c1 (read-char in)]
		       [c2 (read-char in)])
		  ;; TODO: THIS DOESN'T WORK!!! COULD HAVE COMMENTS AT THE BEGINNING OF THE FILE.
		  (if (equal? "#@" (list->string (list c1 c2)))
		      (printf "First expression in file is FASL encoded.  (Binary fast-loading format.)\n")
		      (printf "First characters in file appears to be non-FASL plaintext (might be wrong) ~a.\n"
			      (if (equal? (extract-file-extension file) "gz")
				  " (except for being gzipped)" ""))
		      )))]
	     [(-examine ,_ ...)
	      (error 'regiment:log:examine "-examine expects exactly one file name argument: ~a" args)]

	     [(-vw ,worldfile)
	      (let ([file (symbol->string worldfile)])
		(IF_GRAPHICS 			     
		 (begin 
		   (unless (file-exists? file)
		     (error 'regiment:log:view-world "this worldfile doesn't exist: ~s" file)
		     (exit -1))
		   ;; Read only the first entry, set up as global world.
		   (simalpha-current-simworld (read (open-input-file file)))
		   (animate-world! (simalpha-current-simworld))
		   (printf "Animated world from world file ~s.\n" file)
		   ;(clean-simalpha-counters!)
		   ;(inspect (simalpha-current-simworld))
		   (bounce-to-swl '(begin 
				     ;(clean-simworld! (simalpha-current-simworld))
				     (init-graphics)
				     (simalpha-draw-world (simalpha-current-simworld))
					;(printf "HMM: ~s\n" (simalpha-current-simworld))
				     ;; Run a stupidly simple query just to initialize things. 
					;(run-simulator-alpha (run-compiler '3) 'use-stale-world)
				     )
				  )
		   )
		 (begin (printf "view-world (-vw) mode doesn't really make sense without GUI loaded!")
			(exit -1)))
		)]

	     [,other (warning 'regiment:log "unsupported options or missing arguments: ~a" other)
		     (exit -1)]
	     ))]

	  ;; Interpreting (preparsed) wavescript code.
	  [(wsint)
	   (let ()
	   (define prog (acquire-input-prog 'wsint))
	   (wsint:direct-stream (apply wsint (cons prog (cons input-parameters opts)))))]

	  ;; Same as wsint but only runs the compiler up to typechecking.
	  [(wsearly)
	   (let ()
	     (define prog (acquire-input-prog 'wsearly))
	     (wsint:direct-stream (apply wsint-early (cons prog (cons input-parameters opts)))))]
	  
	  [(wscomp)
	   ;(define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (let ()
	     (define port (acquire-input-prog 'wscomp))
	     (apply wscomp port input-parameters opts))]

	  [(wsc2)
	   ;; HACK: need to set compiler-invocation-mode EARLY for wsc2 (for parsing):
	   (parameterize ([compiler-invocation-mode 'wavescript-compiler-c])
	     (let ((port (acquire-input-prog 'wscomp)))
	       (apply wscomp port input-parameters 'wsc2 opts)))]

	  [(wscaml)
	   (let ()
	     (define exp (acquire-input-prog 'wscaml))
	     (apply wscaml exp input-parameters opts))]

	  ;; Copy/pasted from above:
	  [(wsml)
	   (let ()
	     (define exp (acquire-input-prog 'wsmlton))
	     (apply wsmlton exp input-parameters opts))]
	  
	  ))))

      ;; Loop goes through the arguments, processing them accordingly:
      ;; Anything not matched by this is presumed to be a file name.
      (define (loop args)
        (match args
          [() '()]

	  [(-v ,rest ...) 
	   (set! opts (cons 'verbose opts))
	   (regiment-verbose #t)
	   (loop rest)
	   ]

	  [(-n ,limit ,rest ...)
	   (wsint-tuple-limit (ASSERT integer? (string->number (symbol->string limit))))
	   (loop rest)]
	  ;; Goes with -n... Time query for wsint:
	  [(-t ,rest ...)
	   (wsint-time-query #t)
	   (loop rest)]

	  ;; This SHOULD also switch on some optimization passes with ws-optimizations-enabled:
	  [(-O3 ,rest ...) (ws-optimization-level 3) (loop rest)]
	  [(-O2 ,rest ...) (ws-optimization-level 2) (loop rest)]
	  
	  [(-o ,outfile ,rest ...)
	   (wsint-output-file (symbol->string outfile))
	   (loop rest)]

	  [(-opt ,name ,rest ...)
	   (unless (symbol? name) (error 'main "bad option to -opt flag: ~s" name))
	   (unless (memq name '(rewrites fuse merge-iterates profile))
	     (error 'main "unsupported name for optimization passed to -opt flag: ~s" name))
	   (unless (regiment-quiet) (printf "  Optimization enabled: ~s\n" name))
	   (ws-optimizations-enabled (cons name (ws-optimizations-enabled )))
	   (loop rest)]

	  ;; This signals that we include whatever debugging info we can in the output code.
	  ;; [2007.10.26] Currently it's just used by the wsmlton script, and doees nothing here.
	  [(-dbg ,rest ...) (loop rest)]


	  [(.h ,rest ...) (print-help) (regiment-exit 0)]

	  [(-plot ,rest ...) (set! plot #t) (loop rest)]
	  [(-repl ,rest ...) (set! simrepl #t) (loop rest)]

	  [(-d2 ,rest ...) (set! opts (cons 'deglobalize2 opts)) (loop rest)]

	  [(-lt ,rest ...) (set! opts (cons 'type-only-verbose opts)) (loop rest)]
	  [(-ltt ,rest ...) (set! opts (cons 'type-only opts)) (loop rest)]

	  [(-l0 ,rest ...) (set! opts (cons 'almost-tokens opts))   (loop rest)]
	  [(-l1 ,rest ...) (set! opts (cons 'barely-tokens opts))   (loop rest)]
	  [(-l2 ,rest ...) (set! opts (cons 'full-tokens opts))  (loop rest)]

	  [(-l4 ,rest ...) 
	   (IFWAVESCOPE (begin) 
	     (begin (set! makesimcode #t)
		    (set! opts (cons 'to-simcode opts))))
	   (loop rest)]

	  [(-l5 ,rest ...)
	   ;; [2006.11.11] Not handled right now:
	   (IFWAVESCOPE (begin) 
	     (begin (set! opts (cons 'to-nesc opts))
		    (pass-list 
		     (snoc emit-nesc (snoc flatten-tokmac
					   (remq flatten-tokmac (remq emit-nesc (pass-list))))))))
	   (loop rest)]

	  [(-exit-error ,rest ...)
	   (eprintf "SETTING BATCH MODE\n")
	   (define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (loop rest)]

	  [(-dot ,rest ...) (dump-graphviz-output #t) (loop rest)]
	  [(-ret ,name ,rest ...) (ws-alternate-return-stream name) (loop rest)]
	  
					;		    [(--script ,rest ...) (set! opts (cons 'script opts))  (loop rest)]
	  [(-debug ,rest ...)		     
	   (define-top-level-value 'REGIMENT-BATCH-MODE #f)
	   (regiment-emit-debug #t)
	   (loop rest)]

	  [(-quiet ,rest ...)
	   (regiment-quiet #t)
	   (loop rest)]

	  [(-dump ,file ,rest ...)
	   (set! outfile file)
	   (loop rest)]

	  [(-c0 ,rest ...) (set! opts (cons 'stop-at-c++ opts)) (loop rest)]

	  [(-timeout ,n ,rest ...)
	   (let ((n (read (open-input-string (format "~a" n)))))
	     (set! opts (cons 'timeout (cons n opts))))]

	  ;; --mic
	  ;; FIXME: add to print-help (or automate print-help)
	  [(--disable-pass ,pass-name ,rest ...)
	   (set! opts (append `(disable ,pass-name) opts))
	   (loop rest)]

     ;; --mic
     ;; FIXME: add to print-help (or automate print-help)
     ;; FIXME: get rid of this; put it into input-parameters
     [(--scheduler ,sched-name ,rest ...)
      (set! opts (append `(scheduler ,sched-name) opts))
      (loop rest)]

     ;; --mic
     ;; FIXME: add to print-help (or automate print-help)
     [(--param-file ,param-file-name ,rest ...)
      (let ((param-file (open-input-file (symbol->string param-file-name))))
        (set! input-parameters (read param-file))
        (close-input-port param-file))
      (loop rest)]
	  
	  ;; otherwise a file to compile that we add to the list
	  [(,fn ,rest ...)
					;(regiment-compile-file fn)
	   (cons (symbol->string fn) (loop rest))]

	  [,_ (error "Bad command line arguments to regimentc: ~a~n" args)]
	  ))

      (main) ;; Call the entrypoint above.      
      )))

;; ENTRYPOINTS:  These mimic the shell commands but are callable from within Scheme.
(define (ws prog . args)   (apply main "wsint"  prog args))
(define (wsc2 prog . args) 
  (apply main "wsc2"   prog args)
  (system "wsc2-gcc"))
;(define (wsc prog . args)  (apply main "wscomp" prog args))

;===============================================================================
;;; TESTING:

(define tm-to-list ;; This is boilerplate, many of these tests just run the following:
	 (lambda (tm . extraparams)
	   `(parameterize ([unique-name-counter 0] 
			   [simalpha-dbg-on #f]
			   ,@extraparams
			   )
		 (let ([prog (assemble-tokmac ',tm
					   ;'verbose
					   )])
		   (let ((prt (open-output-string)))
		     (display "(" prt)
		     (let ((result (run-simulator-alpha prog 
					;'timeout 10000
					;'outport prt
							)
				   ))
		     (display ")" prt)
		     (read (open-input-string (get-output-string prt)))
		     result ;; Returns the soc-returned values rather than the output list.
		     ))))
	   ))

;; These are some of our system tests.  They test the compiler and the simulator as a whole.
;; The rest of the system tests are in the files named tests_*.ss
;; But some of the below tests may also be miscellaneous unit tests that require more than one module.
;;
;; NOTE: in WaveScript only mode these are mostly invalid, disabling them:
(IFWAVESCOPE 
(begin)
(begin 
  (define-testing maintests 
   (let ([tm-to-list ;; This is boilerplate, many of these tests just run the following:
	 (lambda (tm . extraparams)
	   `(parameterize ([unique-name-counter 0]
			   [simalpha-dbg-on #f]
			   ,@extraparams
			   )
		 (let ([prog (assemble-tokmac ',tm
					   ;'verbose
					   )])
		   (let ((prt (open-output-string)))
		     (display "(" prt)
		     (let ((result (run-simulator-alpha prog 
					;'timeout 10000
					'outport prt
							)
				   ))
		       (display ")" prt)
		       (read (open-input-string (get-output-string prt)))
		     ))))
	   )]
	
	;; This one returns soc-return'd vals rather than the printed data.
	[tm-to-socvals
	 (lambda (tm . extraparams)
	   `(parameterize ([unique-name-counter 0] 
			   [simalpha-dbg-on #f]
			   ,@extraparams
			   )
		 (let ([prog (assemble-tokmac ',tm 
					   )])
		   ;(profile-clear) ;; Temp: profiling the simulator:
		   (let ((result (run-simulator-alpha prog 
					;'timeout 10000
						      )))
		     result ;; Returns the soc-returned values rather than the output list.
		     ))))]
	)

    ;; I put them in another file because they were simply taking up too many LOC:
     (include "generic/testing/system_tests.ss")  
    ))
  (define-testing maintest (default-unit-tester "Main compiler units + system tests." maintests))
  ))










