
;;;; .title Compiler Core (main.ss)

;;;; This contains the core compiler entry points. 
;;;; Loaded by both the Chez and PLT versions.
;;;; (Expects to be loaded from the directory that contains it.)

;======================================  
;(display "Loading main compiler module.  RegionStreams Demo.")
;(newline)


(define-regiment-parameter regiment-version "0.9")


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
       (printf "Running Pass: ~s\n" 'pass)(flush-output-port)
       (if (regiment-verbose)
	   (time (set! v (ws-pass-optional-stop (pass v))))
	   (set! v (ws-pass-optional-stop (pass v)))))
     ]))


;; This just encapsulates the first few steps of the compiler.
;; Everything up to and including type checking the program.
(define (ws-compile-until-typed p)
  (ws-run-pass p verify-regiment)
  (ws-run-pass p pass_desugar-pattern-matching)
  (ws-run-pass p resolve-varrefs)
  ;; TODO: Insert optional PRUNE-UNUSED pass to quickly prune unused code.
  (ws-run-pass p resolve-type-aliases)
  (ws-run-pass p ws-label-mutable)

  (ws-run-pass p rename-vars)

  ;; This is the initial typecheck. 
  (parameterize ([inferencer-enable-LUB #f]
		 [inferencer-let-bound-poly #t])
    (ws-run-pass p retypecheck))
  p)

;; [2006.08.27] This version executes the WaveScript version of the compiler.
;; It takes it from (parsed) source down as far as WaveScript 
;; can go right now.  But it does not invoke the simulator or the c_generator.
(define (run-ws-compiler p . already-typed)                                   ;; Entrypoint.

  (define (do-typecheck lub poly)
    (parameterize ([inferencer-enable-LUB      lub]
		   [inferencer-let-bound-poly poly])
      (time-accum (ws-run-pass p retypecheck))))
  ;; There are currently two different typecheck configs that we use.
  ;; One for the meta language, one for the object.
  (define (do-early-typecheck) (do-typecheck #f #t))
  (define (do-late-typecheck)  (do-typecheck #t #f))

  (set! already-typed (if (null? already-typed) #f (car already-typed)))

  (ASSERT (memq (compiler-invocation-mode)  '(wavescript-simulator wavescript-compiler-cpp wavescript-compiler-caml)))
(time 
  (parameterize ()
    
  (ws-pass-optional-stop p)
  
  (unless already-typed (set! p (ws-compile-until-typed p)))

  (unless (regiment-quiet) (printf "Program verified.\n"))

;  (ws-run-pass p rename-vars)
  (DEBUGMODE (do-early-typecheck) (void))
  (ws-run-pass p eta-primitives)
  (ws-run-pass p desugar-misc)
  (ws-run-pass p remove-unquoted-constant)
  ;; Run this twice!!!
  ;(ws-run-pass p degeneralize-arithmetic)
  (time (ws-run-pass p static-elaborate))

  (DEBUGMODE
   (with-output-to-file ".__elaborated.ss"
     (lambda () 
       (parameterize ([pretty-line-length 200]
		      [pretty-maximum-lines #f]
		      [print-level #f]
		      [print-length #f]
		      [print-graph #f])
	 (pretty-print p))
       (flush-output-port))
     'replace))

  ;; We want to immediately get our uniqueness property back.
  (ws-run-pass p rename-vars)

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
  ;(ws-run-pass p retypecheck)
 
  (ws-run-pass p verify-elaborated)

  (ws-run-pass p anihilate-higher-order)  ;; Of a kind with "reduce-primitives"

  ;; Now fill in some types that were left blank in the above:
  ;; Shouldn't need to redo LUB because the types are already restrictive???
  (do-late-typecheck)

  ;; This three-step process is inefficient, but easy:
  ;; This is a hack, but a pretty cool hack.
  (ws-run-pass p lift-polymorphic-constant)
  (do-late-typecheck)
  (ws-run-pass p unlift-polymorphic-constant)

;  (ws-run-pass p merge-iterates) ;; <Optimization>
  (IFDEBUG (do-late-typecheck) (void))

  ;; (5) Now we normalize the residual in a number of ways to
  ;; produce the core query language, then we verify that core.
  (ws-run-pass p reduce-primitives) ; w/g 
  (ws-run-pass p remove-complex-constant)
  (IFDEBUG (do-late-typecheck) (void))

;  (ws-run-pass p uncover-free)

  ;(ws-run-pass p purify-letrec)
  ;; This is what we need to do.

  ;; Now that we're done with elaboration we should take the stream
  ;; processing spine, convert it to let.

  ;; For the time-being we don't even need letrec in the object code
  ;; because functions have all been inlined.

  (ws-run-pass p remove-letrec)
  (ws-run-pass p standardize-iterate)

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
;  (exit)

  (ws-run-pass p ws-normalize-context)
  (ws-run-pass p ws-lift-let)

  ;; Mandatory re-typecheck.  Needed to clear out some polymorphic
  ;; types that might have snuck in from lifting.
  ;(do-typecheck #f #f)
  (do-late-typecheck)

  ;; Replacing remove-complex-opera* with a simpler pass:
  ;(ws-run-pass p flatten-iterate-spine)
   
  (DEBUGMODE
   (with-output-to-file ".__nocomplexopera.ss"
     (lambda () 
       (parameterize ([pretty-line-length 200]
		      [pretty-maximum-lines #f]
		      [print-level #f]
		      [print-length #f]
		      [print-graph #f])
	 (pretty-print p))
       (flush-output-port))
     'replace))

  (ws-run-pass p type-annotate-misc) 

;   (set! prog (ws-add-return-statements prog))
  ;(ws-run-pass p ws-add-return-statements)

  (printf "Total typechecker time used:\n")
  (time-accum-report)(newline)
;  (with-output-to-file "./pdump_new"  (lambda () (fasl-write (profile-dump)))  'replace)
;  (exit)

  p))
)






;; ================================================================================
;; The WaveScript "interpreter".  (Really a wavescript embedding.)
;; It loads, compiles, and evaluates a wavescript query.
;; .param x - can be an input port, a filename, or a wavescript AST (list)
(define (wsint x)                                             ;; Entrypoint.  
  (parameterize ([compiler-invocation-mode 'wavescript-simulator]
		 ;[regiment-compile-sums-as-tuples ]
;		 [included-var-bindings '()]
		 [regiment-primitives
		  ;; Remove those regiment-only primitives.
		  (difference (regiment-primitives) regiment-distributed-primitives)])
    (define prog
    (cond  [(input-port? x)
	    (unless (regiment-quiet) (printf "WSINT: Loading WS source from port: ~s\n" x))
	     ;; We assume this is parsed but not post-processed:
	     (ws-postprocess (read x))]
	    [(string? x) 
	     (unless (regiment-quiet) (printf "WSINT: Loading WS source from file: ~s\n" x))
	     (or (read-wavescript-source-file x)
		 (error 'wsint "file did not parse: ~a" x))]
	    [(list? x)   
	     (unless (regiment-quiet) (printf "WSINT: Evaluating WS source: \n \n"))
	     x]
	    [else (error 'wsint "bad input: ~s" x)]))

  (define _ (begin (unless (regiment-quiet)
		     (printf "Evaluating program: ~a\n\n"
			     (IFDEBUG "(original program stored in .__inputprog.ss)" "")))
		   (DEBUGMODE 
		    (let ([please-delete-file 
			   (lambda (f) (if (file-exists? f) (delete-file f)))])
		      ;; Delete these files so that we don't get mixed up.		  
		      (please-delete-file ".__types.txt")
		      (please-delete-file ".__inputprog.ss")
		      (please-delete-file ".__compiledprog.ss")
		      (please-delete-file ".__elaborated.ss")
		      (please-delete-file ".__nocomplexopera.ss"))
		    (with-output-to-file ".__inputprog.ss"
		      (lambda () 
		       (parameterize ([pretty-line-length 200]
				      [pretty-maximum-lines #f]
				      [print-level #f]
				      [print-length #f]
				      [print-graph #f])
			 (pretty-print ;(strip-annotations prog)
			  prog))
		       (flush-output-port))
		     'replace))))

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

  (define compiled (let ([x (run-ws-compiler typed #t)])
		     (unless (regiment-quiet) (printf "WaveScript compilation completed.\n"))
		     (parameterize-IFCHEZ ([pretty-line-length 150]
					   [pretty-one-line-limit 100]
					   [print-level #f]
					   [print-length #f]
					   [print-graph #f])
		       (with-output-to-file ".__compiledprog.ss"
			 (lambda () (pretty-print x)(flush-output-port))
			 'replace))
		     x))
  (define stripped (strip-types compiled))
  (define stream 
    (begin 
      ;; If strip-types worked there shouldn't be any VQueue symbols!
      (DEBUGASSERT (not (deep-assq 'VQueue stripped)))

      ;; New Streams:
      ;; [2007.02.06] Now we wrap it with a little extra to run the query:
      (wavescript-language
       (match stripped
	 [(,lang '(program ,body ,_ ...))
	  `(begin (reset-state!) 
		  (run-stream-query ,body))
	  ]))
      ))

  stream)
  ) ; End wsint


;; For debugging
(define run-wavescript-sim 
  (lambda (p)
    (wavescript-language
     (match (strip-types p)
       [(,lang '(program ,body ,_ ...))
	`(begin (reset-state!) 
		(run-stream-query ,body))
	]))))


;; ================================================================================
;; WaveScript Compiler Entrypoint:
(define (wscomp x . flags)                                 ;; Entrypoint.  
 (parameterize ([compiler-invocation-mode 'wavescript-compiler-cpp]
;		[included-var-bindings '()]
		[regiment-primitives
		 ;; Remove those regiment-only primitives.
		 (difference (regiment-primitives) regiment-distributed-primitives)])
   (define outfile "./query.cpp")
   (define prog
     (cond  [(input-port? x)
	     (unless (regiment-quiet) (printf "WSCOMP: Loading WS source from port: ~s\n" x))
	     ;; We assume this is parsed but not post-processed:
	     (ws-postprocess (read x))]
	    [(string? x) 
	     (unless (regiment-quiet) (printf "WSCOMP: Loading WS source from file: ~s\n" x))
	     (read-wavescript-source-file x)]
	    [(list? x)   
	     (unless (regiment-quiet) (printf "WSCOMP: Evaluating WS source: \n \n"))
	     x]
	    [else (error 'wsint "bad input: ~s" x)]))
   (define typed (ws-compile-until-typed prog))

   (ASSERT (andmap symbol? flags))

   (printf "Compiling program. \n\n")
   ;;(pretty-print prog)
   
   (printf "\nTypecheck complete, program types:\n\n")
   (if (regiment-verbose) 
       (print-var-types typed +inf.0)
       (print-var-types typed 1))
   (flush-output-port)
   
   (set! prog (run-ws-compiler typed #t))
   
   (printf "\nFinished normal compilation, now emitting C++ code.\n")
   (printf "Running pass: convert-sums-to-tuples\n")
   (time (set! prog (convert-sums-to-tuples prog)))

   (inspect `(CONVERTED ,prog))

   ;; A final typecheck will get rid of any polymorphic tuples resulting from the conversion.
#; #;
   (when (regiment-verbose)
    (printf "\n Type checking one last time."))
   ;; Retypecheck to bring back some types that were lost:
   (parameterize ([inferencer-enable-LUB #t]
		  [inferencer-let-bound-poly #f])
     (ws-run-pass p retypecheck))

   (printf "Running pass: nominalize-types.\n")
   (time (set! prog (nominalize-types prog)))

;   (inspect `(NOMINALIZED ,prog))

   (when (regiment-verbose)
    (pretty-print prog)
    ;;   (printf "================================================================================\n")
    (printf "\nNow emitting C code:\n"))

   (DEBUGASSERT
    (with-output-to-file ".__almostC.ss"
      (lambda () 
	(parameterize ([pretty-line-length 200]
		       [pretty-maximum-lines #f]
		       [print-level #f]
		       [print-length #f]
		       [print-graph #f])
	  (pretty-print prog))
	(flush-output-port))
      'replace))
   
   (string->file 
    (text->string 
     (wsquery->text
      prog))
    outfile)
   
   (printf "\nGenerated C++ output to ~s.\n" outfile))
 ) ; End wscomp






;; ================================================================================
;; WaveScript OCAML Compiler Entrypoint:

(IFCHEZ
 (define (wscaml x . flags)                                 ;; Entrypoint.  
 (parameterize ([compiler-invocation-mode 'wavescript-compiler-caml]
		[regiment-primitives ;; Remove those regiment-only primitives.
		 (difference (regiment-primitives) regiment-distributed-primitives)])
   (define outfile "./query.ml")
   (define prog (begin (ASSERT list? x) x))

   (ASSERT (andmap symbol? flags))
   (set! prog (run-ws-compiler prog))
   (set! prog (explicit-stream-wiring prog))
         
   (string->file (text->string (emit-caml-wsquery prog)) outfile)
   (printf "\nGenerated OCaml output to ~s.\n" outfile)
   ))
 (void))




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
  (printf "  wscomp   (wscomp) WaveScript compiler mode~n")
  (printf "  wscaml   (wscaml) WaveScript compiler Caml backend~n")
  (printf "~n")
  (printf "General Options:  ~n")
  (printf "  -v   verbose compilation/simulation, includes warnings~n")
  (printf "  -q   suppress banners and other nonessential output~n")
  (printf "~n")
  (printf "Compiler Options: ~n")
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
  (printf "WSINT options: ~n")
  (printf "  -dump <file>  don't go into stream browser, dump output stream to file~n")
  (printf "WSCOMP options: ~n")
  (printf "  -c0           only run the WaveScript compiler, stop at C++~n")
  )

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

(define (reg:printlog file)
  (let ((stream (reg:read-log file 'stream)))
    (let loop ((s (reg:read-log file 'stream)))
      (unless (null? s)
	(display (log-line->human-readable 0 (stream-car s) ()))
	(loop (stream-cdr s))))))

(define main 
  (lambda args    
    (define makesimcode #f)
    (define outfile #f)
    (define plot #f)
    (define simrepl #f)
;    (disp "Main called w ARGS: " args)
    (when (null? args) (print-help) (regiment-exit 0))
    
;    (printf "regimentc: compile regiment programs!~n")
    (let ([opts '()] ;; This is a bit sketchy.  The same flags are sent to run-compiler and run-simulator-alpha.
	  )
      ;; Loop goes through the arguments, processing them accordingly:
      ;; Anything not matched by this is presumed to be a file name.
      (letrec ([loop 
	      (lambda (args)
		(match args
		    [() '()]

		    [(-v ,rest ...) 
		     (set! opts (cons 'verbose opts))
		     (regiment-verbose #t)
		     (loop rest)
		     ]
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
		     (set! makesimcode #t)
		     (set! opts (cons 'to-simcode opts)) (loop rest)]

		    [(-l5 ,rest ...)
		     ;; [2006.11.11] Not handled right now:
		     (set! opts (cons 'to-nesc opts))
		     (pass-list 
		      (snoc emit-nesc (snoc flatten-tokmac
			     (remq flatten-tokmac (remq emit-nesc (pass-list))))))
		     (loop rest)]

		    [(-exit-error ,rest ...)
		     (printf "SETTING BATCH MODE\n")
		     (define-top-level-value 'REGIMENT-BATCH-MODE #t)
		     (loop rest)]
		    
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

		    ;; otherwise a file to compile that we add to the list
		    [(,fn ,rest ...)
		     ;(regiment-compile-file fn)
		     (cons (symbol->string fn) (loop rest))]

		    [,_ (error "Bad command line arguments to regimentc: ~a~n" args)]
		    ))])

        ;; I keep disjoint options for the modes so I use the same option-processor for all modes (loop)
	(let ([symargs (map string->symbol args)])

	  ;; [2007.01.29] Killing this:
	  ;(unless (null? (cdr symargs)) (printf "Processing options: ~s\n" (cdr symargs)))
	  (let ([mode (car symargs)] [filenames (loop (cdr symargs))])

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
		 ))]

	  ;; Simulation mode (also may invoke compiler):
	  [(s simulate)

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
	     ))]

	  ;; Interactive mode.  A Scheme REPL.
	  ;; [2006.02.21] This is a better way of exposing the normal scheme startup behavior:
	  [(i interact)
	   (printf "Exposing Regiment through interactive read-eval-print loop:\n")
	   ;; [2006.02.28] We want to reset global bindings before loading any more source:
	   ;(eval '(import scheme))
	   ;; Can't trust new code to not mutate primitive names:
	   (IFCHEZ (optimize-level 1) (void))

	   (cond
	    [(null? filenames) 

	     (printf "GOING INTO REPL ~s\n" main)
	     ;(eval '(require main_plt))
	     ;(eval '(require regiment_pltscript))
	     (printf "GOT MODULE INTO TOP LEVEL ~s\n" (eval 'main))
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
	     (IFCHEZ (apply orig-scheme-start (cdr args))
		     (error 'interact-mode "cannot currently run scripts through regiment in PLT Scheme")
		     )
	     ])]

	  ;; Printing SExp log files.
	  [(l log)
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
	     )]

	  ;; Interpreting (preparsed) wavescript code.
	  [(wsint)
	   (let ()
	   (define prog (match filenames
			  ;; If there's no file given read from stdin
			  [() (console-input-port)]
			  [(,fn ,rest ...) 
			   (unless (null? rest)
			     (error 'wsint "bad filename(s) or flag(s): ~s" rest))
			   ;; If it's a ws file we need to parse the file:
			   (if (equal? "ws" (extract-file-extension fn))
			       (or (read-wavescript-source-file fn)
				   (error 'wsint "couldn't parse file: ~s" fn))
			       ;; Otherwise let's assume 
			       (open-input-file fn))]

			  [,else (error 'regiment:wsint "should take one file name as input, given: ~a" else)]))
	   (let ([return (wsint prog)])
	     (IFCHEZ (import streams) (void))
	     ;(import imperative_streams)
	     (if (stream? return)
		 (if outfile
		     (begin
		       (printf "Dumping output to file: ~s\n" outfile)
		       (stream-dump return outfile))
		     ;; Otherwise, browse it interactively:
		     (parameterize ([print-vector-length #t])
		       ;(browse-stream (stream-map ws-show return))
		       (browse-stream return)
		       ))
		 (printf "\nWS query returned a non-stream value:\n  ~s\n" return))))]
	  
	  [(wscomp)
	   ;(define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (let ()
	     (define port (match filenames
			  ;; If there's no file given read from stdout
			  [() (console-input-port)]
			  [(,fn ,rest ...) 
			   (if (equal? "ws" (extract-file-extension fn))
			       (or (read-wavescript-source-file fn)
				   (error 'wsint "couldn't parse file: ~s" fn))
			       ;; Otherwise let's assume 
			       (open-input-file fn))]
			  ;[,else (error 'regiment:wscomp "should take one file name as input, given: ~a" else)]
			  ))
	     (apply wscomp port opts)
	   )]

	  [(wscaml)
	   ;(define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (IFCHEZ
	    (let ()
	     (define exp (match filenames
			  ;; If there's no file given read from stdout
			  [() (console-input-port)]
			  [(,fn ,rest ...) 
			   (if (equal? "ws" (extract-file-extension fn))
			       (or (read-wavescript-source-file fn)
				   (error 'wsint "couldn't parse file: ~s" fn))
			       ;; Otherwise let's assume 
			       (open-input-file fn))]
			  ;[,else (error 'regiment:wscomp "should take one file name as input, given: ~a" else)]
			  ))
	     (apply wscaml exp opts)
	   )
	    (error 'wavescript-compiler "OCaml output not currently available when running through PLT Scheme."))]

	  
	  )))))))

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


(define bugprog
  '(letrec ([sync2 (lambda (ctrl s1 s2)
                  (letrec ([_ctrl (iterate
                                    (lambda (#(b s e) ___VIRTQUEUE___)
                                      (begin
                                        (emit
                                          ___VIRTQUEUE___
                                          (tuple b s e nullseg))
                                        ___VIRTQUEUE___))
                                    ctrl)])
                    (letrec ([_s1 (iterate
                                    (lambda (win ___VIRTQUEUE___)
                                      (begin
                                        (emit
                                          ___VIRTQUEUE___
                                          (tuple '#f 0 0 win))
                                        ___VIRTQUEUE___))
                                    s1)])
                      (letrec ([_s2 (iterate
                                      (lambda (win ___VIRTQUEUE___)
                                        (begin
                                          (emit
                                            ___VIRTQUEUE___
                                            (tuple '#f 0 0 win))
                                          ___VIRTQUEUE___))
                                      s2)])
                        (letrec ([slist (cons
                                          _ctrl
                                          (cons _s1 (cons _s2 '())))])
                          (iterate
                            (letrec ([acc1 nullseg]
                                     [acc2 nullseg]
                                     [requests '()])
                              (lambda (#(ind tup) ___VIRTQUEUE___)
                                (begin
                                  (letrec ([#(flag strt en seg) tup])
                                    (begin
                                      (if (equal? ind 0)
                                          (set! requests
                                            (app append
                                                 requests
                                                 (cons
                                                   (tuple flag strt en)
                                                   '())))
                                          (if (equal? ind 1)
                                              (set! acc1
                                                (app joinsegs acc1 seg))
                                              (set! acc2
                                                (app joinsegs acc2 seg))))
                                      (if (not (equal? acc1 nullseg))
                                          (app print
                                               (string-append
                                                 "  Acc1: "
                                                 (string-append
                                                   (app show
                                                        (app start acc1))
                                                   (string-append
                                                     ":"
                                                     (string-append
                                                       (app show
                                                            (app end acc1))
                                                       "\n")))))
                                          .
                                          #0=((tuple)))
                                      (if (not (equal? acc2 nullseg))
                                          (app print
                                               (string-append
                                                 "  Acc2: "
                                                 (string-append
                                                   (app show
                                                        (app start acc2))
                                                   (string-append
                                                     ":"
                                                     (string-append
                                                       (app show
                                                            (app end acc2))
                                                       "\n")))))
                                          .
                                          #0#)
                                      (if (equal? requests '())
                                          (tuple)
                                          (letrec ([#(fl st en) (app head
                                                                     requests)])
                                            (if (and (not (equal?
                                                            acc1
                                                            nullseg))
                                                     (and (not (equal?
                                                                 acc2
                                                                 nullseg))
                                                          (and (<= (app start
                                                                        acc1)
                                                                   st)
                                                               (and (<= (app start
                                                                             acc2)
                                                                        st)
                                                                    (and (>= (app end
                                                                                  acc1)
                                                                             en)
                                                                         (>= (app end
                                                                                  acc2)
                                                                             en))))))
                                                (begin
                                                  (app print
                                                       (string-append
                                                         "  Spit out segment!! "
                                                         (string-append
                                                           (app show st)
                                                           (string-append
                                                             ":"
                                                             (string-append
                                                               (app show
                                                                    en)
                                                               "\n")))))
                                                  (letrec ([size (+ (- en
                                                                       st)
                                                                    1)])
                                                    (begin
                                                      (emit
                                                        ___VIRTQUEUE___
                                                        (tuple
                                                          (app subseg
                                                               acc1
                                                               st
                                                               size)
                                                          (app subseg
                                                               acc2
                                                               st
                                                               size)))
                                                      (set! acc1
                                                        (app subseg
                                                             acc1
                                                             (+ st size)
                                                             (- (app width
                                                                     acc1)
                                                                size)))
                                                      (set! acc2
                                                        (app subseg
                                                             acc2
                                                             (+ st size)
                                                             (- (app width
                                                                     acc2)
                                                                size)))
                                                      (set! requests
                                                        (app tail
                                                             requests)))))
                                                .
                                                #0#)))))
                                  ___VIRTQUEUE___)))
                            (app unionList slist)))))))])
  (letrec ([ch1 (app audio 0 128 0)])
    (letrec ([ch2 (app audio 1 128 0)])
      (letrec ([outwidth 100])
        (letrec ([ctrl (iterate
                         (letrec ([pos 0])
                           (lambda (w ___VIRTQUEUE___)
                             (begin
                               (emit
                                 ___VIRTQUEUE___
                                 (tuple '#t pos (- (+ pos outwidth) 1)))
                               (set! pos (+ pos outwidth))
                               ___VIRTQUEUE___)))
                         ch1)])
          (app sync2 ctrl ch1 ch2)))))))










