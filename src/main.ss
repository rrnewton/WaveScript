
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
                  [(eq? arg 'full-tokens)
                   (set! passes (remq haskellize-tokmac (pass-list)))]
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
	 [(equal? type "tm") (if (or (memq '-l0 symargs) (memq '-l1 symargs))
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
		   (parameterize ([current-directory start-dir])
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
		    (pretty-print x))
		  (printf "================================================================================\n\n")
		  ;(read-line)
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

  (ASSERT (memq (compiler-invocation-mode)  '(wavescript-simulator wavescript-compiler)))
(time 
  (parameterize ()
    
  (ws-pass-optional-stop p)
  
  (unless already-typed
    (ws-run-pass p verify-regiment)
    (ws-run-pass p pass_desugar-pattern-matching)
    (ws-run-pass p resolve-varrefs)
    ;; TODO: Insert optional PRUNE-UNUSED pass to quickly prune unused code.
    (ws-run-pass p resolve-type-aliases)
    (ws-run-pass p ws-label-mutable)

    ;; This is the initial typecheck. 
    (do-early-typecheck)
    )

  (unless (regiment-quiet) (printf "Program verified.\n"))

  (ws-run-pass p rename-vars)
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

  ;; NOTE: SHOULD BE SAFE TO TURN OFF LET-BOUND-POLYMORPHISM HERE:
  (ws-run-pass p degeneralize-arithmetic)

  ;; We MUST typecheck before verify-elaborated.
  ;; This might kill lingering polymorphic types ;)
  (do-late-typecheck)
  (ws-run-pass p rename-vars)

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

;  (inspect (count-nodes p))
;  (profile-clear)
  (ws-run-pass p ws-remove-complex-opera*)
  (do-late-typecheck)
;  (inspect (count-nodes p))
;  (with-output-to-file "./pdump_new"  (lambda () (fasl-write (profile-dump)))  'replace)
;  (exit)

  (ws-run-pass p ws-normalize-context)


  (ws-run-pass p ws-lift-let)

  ;; Mandatory re-typecheck.  Needed to clear out some polymorphic
  ;; types that might have snuck in from lifting.

  ;; NOTE: MIGHT combine this with the previous typecheck!!
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

  p))
)






;; ================================================================================
;; The WaveScript "interpreter".  (Really a wavescript embedding.)
;; It loads, compiles, and evaluates a wavescript query.
;; .param x - can be an input port, a filename, or a wavescript AST (list)
(define (wsint x)                                             ;; Entrypoint.  
  (parameterize ([compiler-invocation-mode 'wavescript-simulator]
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
			 (pretty-print prog))
		       (flush-output-port))
		     'replace))))

  (define typed 
    (let ([p prog])
      (ws-run-pass p verify-regiment)
      (ws-run-pass p pass_desugar-pattern-matching)
      (ws-run-pass p resolve-varrefs)
      (ws-run-pass p resolve-type-aliases)
      (ws-run-pass p ws-label-mutable)
      (parameterize ([inferencer-enable-LUB #f]
		     [inferencer-let-bound-poly #t])
	(time-accum (ws-run-pass p retypecheck))) ;; This is the initial typecheck.
      p))

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
		     (unless (regiment-quiet) (printf "Compilation completed.\n"))
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
 (parameterize ([compiler-invocation-mode 'wavescript-compiler]
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
   (define typed 
    (let ([p prog])
      (ws-run-pass p verify-regiment)
      (ws-run-pass p pass_desugar-pattern-matching)
      (ws-run-pass p resolve-varrefs)
      (ws-run-pass p resolve-type-aliases)
      (ws-run-pass p ws-label-mutable)
      (parameterize ([inferencer-enable-LUB #f]
		     [inferencer-let-bound-poly #t])
	(ws-run-pass p retypecheck)) ;; This is the initial typecheck.
      p))

   (ASSERT (andmap symbol? flags))

   (printf "Compiling program. \n\n")
   ;;(pretty-print prog)
   
   (printf "\nTypecheck complete, program types:\n\n")
   (if (regiment-verbose) 
       (print-var-types typed +inf.0)
       (print-var-types typed 1))
   (flush-output-port)
   
   (set! prog (run-ws-compiler typed #t))

   (when (regiment-verbose)
    (printf "================================================================================\n")
    (printf "\nNow nominalizing types.\n"))
   (set! prog (nominalize-types prog))
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

(define (wscaml x . flags)                                 ;; Entrypoint.  
 (parameterize ([compiler-invocation-mode 'wavescript-compiler]
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
  (define-testing these-tests 
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
  (define-testing test-this (default-unit-tester "Main compiler units + system tests." these-tests))
  (define maintests these-tests)
  (define maintest test-this)))


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










