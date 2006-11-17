;;;; .title source_loader.ss 

;;;; [2005.11.11] This module loads regiment source from a ".rs" file
;;;; and desugars it according to a few simple conventions that I've
;;;; followed.  It can also load token machine ".tm" files.

; [2006.02.27] Refactored to use new common module syntax.

; [2006.10.25] This now contains some of the code to parse&load ".ws" files as well.
; Note, .rs files have a mechanism for including simulator parameter settings in the file.
; .ws files have nothing comparable at the moment.

(module source_loader mzscheme 
  (require "../plt/iu-match.ss"
           "../generic/constants.ss"
	   "../plt/simulator_alpha_datatypes.ss"
           (all-except "../plt/pass21_cleanup-token-machine.ss" test-this these-tests)
           (all-except "../plt/helpers.ss" test-this these-tests)
           (all-except "../plt/regiment_helpers.ss" test-this these-tests)
           (all-except "../generic/simulator_alpha.ss" test-this these-tests id)
           )
  ;; Module exports:
  (provide     	
   read-regiment-source-file ;; Read a file to SEXP syntax
   load-regiment reg:load    ;; Read and execute a file using simulator.

   expand-include            ;; Expand an include statement into a set of definitions.

   read-wavescript-source-file ;; Read a WS file, invoking "wsparse", also doing post-processing
   ws-postprocess              ;; Take parsed decls and turn into a single top level expression.

   ;test_sourceloader
   ) ;; End provide

  (chezimports ;constants
               (except helpers           test-this these-tests)
	       (except regiment_helpers  test-this these-tests)
	       (except simulator_alpha   test-this these-tests)
	       ;; FIXME: Why doesn't this work: [2006.10.18]
	       (except desugar-pattern-matching test-this these-tests)
	       )
; ================================================================================

;; Read the file from disk, desugar the concrete syntax appropriately.
;; .param fn File name.
;; .returns Two values: desugared program, and parameters.
(define read-regiment-source-file
  (lambda (fn)
    (define (desugar-token e)      
      (match e
	[((,name . ,subtok) (,arg ...) ,bods ...)
	 `[,name ,subtok (,arg ...) ,bods ...]]
	[(,name (,arg ...) ,bods ...)
	 `[,name (,arg ...) ,bods ...]]))
    (define (desugar exps)
      (let loop ((ls exps))
	(match ls
	  ;[() (error 'read-regiment-source-file "file has no return expression.")]
	  [() ()]

	  ;; Macro expansion:
	  ;==================================================
	  ;; First of all, we allow some load-time evaluation for syntax preprocessing:
	  [((quasiquote ,form) ,rest ...)
	   ;; Just evaluat it and loop:
	   (desugar (cons (eval `(quasiquote ,form)) rest))]

	  ;; Single expression file:
	  ;==================================================
	  ;; These first forms are the "single construct" style:
	  [((,lang '(program ,stuff ...)))
	   (car ls)]
	  [((tokens ,tok* ...))
	   (car ls)]

	  ;; Multiple expression file:
	  ;==================================================
	  ;; These two are in the more natural style, in which
	  ;; many seperate "define" or "token" clauses are allowed.
	  [((token ,stuff ...) . ,rest)
	   (match (desugar rest)
	     [() `(tokens ,(desugar-token stuff))]
	     [(tokens . ,others)
	      `(tokens ,(desugar-token stuff) ,@others)])]

	  [((define ,x* ,y*) ... ,main)
	   `(letrec (,(map desugar-define x* y*) ...) ,main)]

	  [(,other ,rest ...)
	   (error 'read-regiment-source-file
		  "invalid expression in definition context: ~s" `(,other ,rest ...))])))

    (if (equal? "ws" (extract-file-extension fn))
	(begin 
	  (unless (zero? (system "which wsparse")) 
	    (error 'wsint "couldn't find wsparse executable"))
	  (let* ([port (car (process (++ "wsparse " fn)))]
		 [decls (read port)])
	    (close-input-port port)
	    (printf "POSTPROCESSING: ~s\n" decls)
	    (match (ws-postprocess decls)
	      [(let* ,decls ,bod)
	       (values `(base-lang '(program (letrec ,decls ,bod))) ())])))
	(match (file->slist fn)
	  [((parameters ,p ...) ,exps ...)
	   (values (desugar exps) p)]
	  [(,prog ...) 
	   (values (desugar prog) ())]))))

;; This loads (e.g. reads, compiles, and simulates) a regiment program:  <br><br>
;;
;; [2005.11.17] Currently redundant with code in regiment.ss:            <br>
;; [2006.02.15] NOTE: This currently passes the flags to BOTH the       
;; compiler and the simulator.  They both follow the bad practice of
;; ignoring flags they don't understand.
;;
;; [2006.03.02] To make the argument conventions even more confusing,
;; I just added the option to include a simworld argument before the file-name.
;;
;; [2006.11.08] It looks like there are two conditions in which a
;; stale world is used.  Either when 'use-stale-world is passed
;; explicitely, or when (simalpha-current-simworld) is non-false.
;
;; [2006.11.13] Adding params to compile phase as well.  Compiler
;; reads default-slow-pulse, for example.
(define (load-regiment . args)
  (match args    
    [(,world ,fn . ,opts) (guard (simworld? world) (string? fn))
     ;; We pass the world by putting it in the global parameter and
     ;; telling the simulator to use the existing "stale" simworld.
     (simalpha-current-simworld world)
     (apply load-regiment fn 'use-stale-world opts)]
    [(,fn . ,opts) (guard (string? fn))
     (ASSERT (list? opts))
     ;; Flags are things like 'verbose, params are '[sim-timeout 1000]
     ;; Flags get passed to run-compiler and compile-simulate-alpha.
     (let ([flags (filter (lambda (x) (not (list? x))) opts)]
	   [userparams (filter list? opts)])
       (ASSERT (andmap symbol? flags))
       (ASSERT (andmap (lambda (l) (= 2 (length l))) userparams))
       (ASSERT (andmap symbol? (map car userparams)))
       (mvlet ([(prog codeparams passes)
		  (let ([type (string->symbol (extract-file-extension fn))])
		    (mvlet (((prg params) (read-regiment-source-file fn)))
		      (case type
			[(rs) (values prg params (pass-list))]
			[(tm) (values prg params 
				      (list-remove-before cleanup-token-machine (pass-list)))]
			[(sim alpha) (values prg params ())]
			[else (error 'load-regiment "can't handle file with this extension: ~s" fn)]
			)))])

	 ;; Prioriy: User params override those set in the file:	 
	 (define params 
	   (if (memq 'ignore-file-params flags) 
	       (begin (set! flags (remq 'ignore-file-params flags))
		      userparams)
	       (append (filter (lambda (pr) (not (assq (car pr) userparams))) codeparams) userparams)
	       ))

	 (define compiled 
	   (with-evaled-params params
	       (lambda ()
		 (parameterize ([pass-list passes])
		   (apply (top-level-value 'run-compiler) prog flags))))	   
	   )

	   ;; Set all the params before running things:
	   ;; [2005.12.02] Changing this so the params stick after the run.  Better for re-running.
	   (HACK (for-each eval params))
	   ;;; [2006.11.11] HACK HACK -- loading params twice for now!!!
;	   (inspect compiled)
;	   (inspect params)
	   
	   (let ((result 
		  (with-evaled-params params
		      (lambda ()
			(apply run-simulator-alpha compiled  flags
					;'srand (current-time)
			       )))))
	     (print-stats)
	     result)))]
    [,other (error 'load-regiment "bad arguments: ~s\n" other)]))

(define reg:load load-regiment) ;; shorthand


;----------------------------------------
;;; Some syntactic sugar.

;; Private helper:
;; Desugar define statements found within files into letrec statements.
(define (desugar-define lhs rhs)  
  (match lhs
    [,s (guard (symbol? s))
	`[,s ,rhs]]
    [(,f ,x* ...) (guard (symbol? f))
     `[,f (lambda ,x* ,rhs)]]
    [,_  (error 'source_loader:desugar-define
		"invalid define expression: ~a" `(define ,lhs ,rhs))]))


;================================================================================

;; Do all the post-processing to turn a set of bindings into a single valid expression.
(define (ws-postprocess ws)
  ;; First we expand includes:
  (let ([ws (apply append 
              (map (lambda (form)
		   (match form
		     [(include ,file) (expand-include file)]
		     [,other (list other)]))
	      ws))])
  (define (f1 x) (eq? (car x) '::))
  (define (f2 x) (eq? (car x) 'define))
  (define (f3 x) (eq? (car x) '<-))
  (let ([types (map cdr (filter f1 ws))]
        [defs (map cdr (filter f2 ws))]
        [routes (map cdr (filter f3 ws))])
    (define other (filter (lambda (x) (and (not (f1 x)) (not (f2 x)) (not (f3 x)))) ws))
    (unless (null? other) (error 'ws-postprocess "unknown forms: ~s" other))
    (let ([typevs (map car types)]
          [defvs (map car defs)])
      ;(pretty-print ws)
      (unless (= 1 (length routes))
        (if (zero? (length routes))
	    (begin (warning 'ws-postprocess "No stream-wiring expression, defaulting \"BASE <- ()\"")
		   (set! routes `((BASE (tuple)))))
	    (error 'ws-postprocess "Must have exactly one stream-wiring (<-) expression! ~a" routes)))
      (unless (eq? 'BASE (caar routes))
        (error 'ws-postprocess "BASE is the only allowed destination for (<-) currently!  Not: ~s" (car routes)))
      (unless (subset? typevs defvs)
        (error 'ws-postprocess "type declarations for unbound variables! ~a" (difference typevs defvs)))
      (let ([binds (map (lambda (def)
		     (match def
		       [(,v ,e) (if (memq v typevs)
				    `[,v ,(cadr (assq v types)) ,e]
				    `[,v ,e])]))
		defs)]
	    [body (cadar routes)])
	;; [2006.09.19] Using let* rather than letrec for now.  Type checker isn't ready for letrec.

	;; [2006.11.16] Ok, well now we need recursion.  Going back to
	;; letrec.  Problem was (I think) that we didn't have
	;; letrec*... I'm just making nested letrecs for now.

	;;`(let* ,binds ,body)
	;;`(letrec ,binds ,body)
	(let loop ([binds binds])
	  (if (null? binds) body
	      `(letrec (,(car binds))
		 ,(loop (cdr binds))
		 )))
	
	)))))

;; Expand an include statement into a set of definitions.
;; Optionally takes the absolute path of the file in which the
;; (include _) statement is located.
(define expand-include
  (case-lambda
    [(fn) (expand-include fn #f)]
    [(inclfile fromfile)
     (unless (file-exists? inclfile)
       (error 'parser "Included file not found: ~s\n" inclfile))
     (match (ws-parse-file inclfile)
       [(letrec ,binds ,ignored)
	`((define . ,binds) ...)])]))


(define (read-wavescript-source-file fn)
  (ASSERT (string? fn))
  (unless (equal? (extract-file-extension fn) "ws")
    (error 'read-wavescript-source-file 
	   "should only be used on a .ws file, not: ~s" fn))

  ;; HACK: WON'T WORK IN WINDOWS:)
  (unless (zero? (system "which wsparse")) 
    (error 'wsint "couldn't find wsparse executable"))
  (let* ([port (car (process (++ "wsparse " fn)))]
	 [decls (read port)])
    (close-input-port port)
    (printf "POSTPROCESSING: ~s\n" decls)
    (ws-postprocess decls)))

) ;; End Module.


#|	      
      (when (equal? type "rs")
	(runloop 'compile (list fn))
	(set! fn out_file))) 

	     (printf "Running simulation from file: ~a\n" fn)
	     (let ((result
		    ;; Be careful to watch for parameterization:	     
		    (mvlet (([prog params] (read-regiment-source-file fn)))
		      (with-evaled-params params
					  (lambda () 
					    (apply run-simulator-alpha prog opts))))))
	       ;; Print simalpha stats:
	       (print-stats)
	       (if plot (gnuplot result))
	       (if simrepl (new-cafe))
	       result))





		    (parameterize ([pass-list
				 (cond
				  [(equal? type "rs") (pass-list)]
				  [(equal? type "tm") (list-remove-before cleanup-token-machine
									  (pass-list))]
				  [else (error 'regiment "unknown input file extension: ~s" type)]
				  )])
		      (set! out_file (string-append (remove-file-extension fn) extension))
		      ;; Don't overwrite one of our own input files!
		      (if (member out_file filenames) (set! out (string-append "out." extension)))

		      (printf "~n  Writing token machine to: ~s ~n" out_file)
		      (mvlet ([(prog params) (read-regiment-source-file fn)])
		       
			(delete-file out_file)
			(let ((comped 
			       (if makesimcode 
				   ;; This bakes the parameters right into the sim code:
				   (apply compile-simulate-alpha
					  (apply run-compiler prog opts)
					  params)
				   (apply run-compiler prog opts))))			  
			  (parameterize ([print-graph #t] [print-level #f] [print-length #f])
			    (with-output-to-file out_file
			      (lambda ()
				;; Otherwise we need to propogate the params to the output file:
				(if (not makesimcode)
				    (pretty-print `(parameters ,@params)))
				(pretty-print comped))))))



|#
