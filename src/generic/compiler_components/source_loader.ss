;;;; .title source_loader.ss 

;;;; [2005.11.11] This module loads regiment source from a ".rs" file
;;;; and desugars it according to a few simple conventions that I've
;;;; followed.  It can also load token machine ".tm" files.

; [2006.02.27] Refactored to use new common module syntax.

; [2006.10.25] This now contains some of the code to parse&load ".ws" files as well.
; Note, .rs files have a mechanism for including simulator parameter settings in the file.
; .ws files have nothing comparable at the moment.

(module source_loader mzscheme 
  (require 
   (all-except "regiment_helpers.ss" test-this these-tests)
   "../constants.ss"
   "../../plt/iu-match.ss"   
   (all-except "../util/helpers.ss" test-this these-tests)
   (all-except "../passes/tokmac_bkend/cleanup-token-machine.ss" test-this these-tests)   
   "../sim/simulator_alpha_datatypes.ss"
   (all-except "../sim/simulator_alpha.ss" test-this these-tests id)

   (only "../../plt/regiment_parser.ss" ws-parse-file)
   )
  ;; Module exports:
  (provide     	
   read-regiment-source-file ;; Read a file to SEXP syntax
   load-regiment reg:load    ;; Read and execute a file using simulator.

   ws-parse-file
   ws-relative-path?

   read-wavescript-source-file ;; Read a WS file, invoking "wsparse", also doing post-processing
   wsparse-postprocess              ;; Take parsed decls and turn into a single top level expression.

   ;test_sourceloader
   ) ;; End provide

  (chezimports ;constants
               (except helpers           test-this these-tests)
	       (except regiment_helpers  test-this these-tests)
	       
	       ;; FIXME: Why doesn't this work: [2006.10.18]
	       (except desugar-pattern-matching test-this these-tests)
	       )

  (IFCHEZ (IFWAVESCOPE (begin) (import (except simulator_alpha   test-this these-tests))) (begin))
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
	(let ([decls (ws-parse-file fn)])
;	  (printf "POSTPROCESSING: ~s\n" decls)
	  (match (wsparse-postprocess decls)
	    [(let* ,decls ,bod)
	     (values `(base-lang '(program (letrec ,decls ,bod))) ())]))
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
(define load-regiment
  (IFWAVESCOPE
      'undefined-in-wavescope
   (lambda args
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
	   
	   (let ((result 
		  (with-evaled-params params
		      (lambda ()
			(apply run-simulator-alpha compiled  flags
					;'srand (current-time)
			       )))))
	     (print-stats)
	     result)))]
    [,other (error 'load-regiment "bad arguments: ~s\n" other)]))))

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

;; Extract the file and path portions of 
;(define (basename pathstr) )
(define (dirname pathstr)
  ;; Everything up until the last "#/"
  (define chars (string->list pathstr))
  (define dir (reverse! (or (memq #\/ (reverse! chars)) '(#\.))))
  (list->string dir))

;; Would like to do a bit better job of this:
(IFCHEZ
 (define (ws-relative-path? p)
  (case (machine-type)
    [(i3nt) 
     (if (> (string-length p) 2)
	 ;; A drive letter:
	 (eqv? ":" (string-ref p 1))
	 #f)]
    [else (ASSERT (not (zero? (string-length p))))
	  (not (eqv? #\/ (string-ref p 0)))
     ]))
 (define (ws-relative-path? p)
   ;; Actually this is built-in... should use it:
   ;(error 'ws-relative-path? "not implemented in PLT yet")
   (let ([v (relative-path? (build-path p))])
     ;(if v (path->string v) #f)
     v
     )
   ))

;; Resolve whether a path refers to a .ws file in the library or
;; somewhere else.  Returns an absolute path in either case.  Might be
;; better to have a different syntax for including library code.
(define (resolve-lib-path file)
  (cond 
   ;; Absolute path, not a library:
   [(not (ws-relative-path? file)) file]
   ;; Safety: Can't use ".." wrt to lib directory:
   [(and (not (substring? ".." file)) 
	 (file-exists? (** (REGIMENTD) "/lib/" file)))
    (** (REGIMENTD) "/lib/" file)]
   [else (** (path->string (current-directory)) "/" file)]
   ))

;================================================================================

;; Do all the post-processing to turn a set of bindings into a single valid expression.
;;
;; [2007.03.19] Note: top level type declarations don't have to occur
;; in any particular position in the file.
(define (wsparse-postprocess origws)
  ;; First we expand includes:  
  ;; NOTE: Should be tail recursive.  The number of forms could grow large.
  (define (process* forms all-includes current-file)
    (if (null? forms) 
        (values '() all-includes)
	(let-values ([(fst includes)   (process  (car forms) all-includes current-file)])
	  (let-values ([(snd includes) (process* (cdr forms) includes current-file)])
	    (values (append fst snd) 
		    includes)))))
  (define (process form all-includes current-file)
    (define (names-defined forms)
      (match forms
	[() ()]
	[((define ,v ,_) . ,[rest])      (cons v rest)]
	[((define-as ,v . ,_) . ,[rest]) (cons v rest)]
	[(,_ . ,[rest])                  rest]))
    (match form
      [(include ,file) 

       ;; We have to go to the directory containing the included file to resolve further includes.
       (parameterize ([current-directory (if current-file (dirname current-file) (current-directory))])
	 ;(printf "    CURDIR IS: ~s\n" (current-directory))
	 (let ([path (resolve-lib-path file)])
	 ;; FIXME: Currently ../ can defeat this method of tracking files loaded:
	 ;; We need to at least see through that:
	   (unless (file-exists? path)
	     (error 'parser "Included file not found: ~s\n" path))
	   
	   (if (member path all-includes)
	       (begin
		 (eprintf "  Suppressing repeated include of file!: ~s\n" path)
		 (values '() all-includes))
	       (begin 
		 ;; This is usually a relative file path!
		 (let-values ([(imported new-includes)
			       (process*			      				
				(parameterize () ;([current-directory (dirname path)])
				  ;(printf "    CURDIR IS: ~s\n" (current-directory))
				  (ws-parse-file path) ;; Expand out the includes within this included file.
				  )
				(cons path all-includes)
				path ;; Now the current file becomes the included file.
				)])
		   ;; Record that these symbols were pulled from an include:
		   ;; This is *just* cosmetic:
		   
					;(printf "ADDING INCLUDES FROM ~s :\n   ~s\n" path (names-defined imported))
		   (included-var-bindings (append (names-defined imported) (included-var-bindings)))
		   (values imported new-includes)
		   )))))]

      [(using ,M) (values `((using ,M)) all-includes)]
      ;; This just renames all defs within a "namespace".  There's
      ;; some question, though, as to how they may refer to eachother.
      ;; 
      [(namespace ,Space . ,rest)
       (let-values ([(defs new-includes) (process* rest all-includes current-file)])
	 (values
	  (map (lambda (def)
		 (define (mangle v) (string->symbol (format "~a:~a" Space v)))
		 
		 ;; We also inject a bunch of 'using' constructs so that
		 ;; we can use the namespace's bindings from *within* the namespace:
		 (define (wrap-rhs e) `(using ,Space ,e))
		 ;; SCRATCH THAT!
		 ;; Because of certain limitations in the current implementation
		 ;; of letrec (value restriction), for the moment definitions
		 ;; within the namespace still have to use the FULL NAMES of
		 ;; their peers.
		 ;(define (wrap-rhs e) e)
		 ;; FIXME: This doesn't handle "using" within a namespace.
		 (match def
		   [(define ,v ,e)         `(define ,(mangle v) ,(wrap-rhs e))]
		   [(define-as ,v ,pat ,e) `(define-as ,(mangle v) ,pat ,(wrap-rhs e))]
		   [(:: ,v ,t)             `(:: ,(mangle v) ,t)]
		   [(<- ,sink ,e)          `(<- ,sink ,(wrap-rhs e))]))
	    defs)
	  new-includes)
	 )]

      [(uniondef ,ty ,def) (values `((uniondef ,ty ,def)) all-includes)]

      [(namespace . ,other)
       (error 'wsparse-postprocess "bad namespace form: ~s" (cons 'namespace other))]
      
      [,other  (values (list other) all-includes)]))

  ;; First we use "process" to handle includes and namespaces.
  ;; We automatially inject an include of "internal.ws"
  (define  ws (first-value (process* (cons '(include "internal.ws") origws) '() #f)))
  (define (f1 x) (eq? (car x) '::))
  ;; We're lumping 'using' declarations with defines.  Order must be maintained.
  (define (f2 x) (or (memq (car x) '(define using define-as)) ))
  (define (f3 x) (eq? (car x) '<-))
  (define (f4 x) (eq? (car x) 'typedef))
  (define (f5 x) (eq? (car x) 'uniondef))

  (define types       (map cdr (filter f1 ws)))
  (define defs        (map cdr (filter f2 ws)))
  (define routes      (map cdr (filter f3 ws)))
  (define typealiases (map cdr (filter f4 ws)))
  (define uniondefs   (map cdr (filter f5 ws)))
  (define other (filter (lambda (x) (and (not (f1 x)) (not (f2 x)) (not (f3 x)) (not (f4 x)) (not (f5 x)))) ws))

  (unless (null? other) (error 'wsparse-postprocess "unknown forms: ~s" other))
  (let ([typevs (map car types)]
	[defvs  (map car defs)])

    (unless (list-subset? typevs defvs)
      (error 'wsparse-postprocess "type declarations for unbound variables! ~a" (difference typevs defvs)))
    (unless (list-is-set? (map car typealiases))
      (error 'wsparse-postprocess 
	     "Got two type aliases with the same name!\nAll aliases: ~s"
	     typealiases))

    ;; Now let's build that expression:
    (let ()
      ;; Pull out the "BASE <-" returned expression:
      (define body 
	(begin 
	  ;; DEFENSE:
	  (if (= 1 (length routes))
	      (if (eq? 'BASE (caar routes)) (cadar routes) ;; Here's our body.
		  (error 'wsparse-postprocess "BASE is the only allowed destination for (<-) currently!  Not: ~s" (car routes)))
	      (if (zero? (length routes))
		  ;; [2007.12.01] Allowing a new convention.  Return the stream named "main".
		  (let ([return-name (or (ws-alternate-return-stream) 'main)])
		    (if (assq return-name defs)
			return-name ;; The returned stream is whatever the most recent binding of the return name is.
			(begin (warning 'wsparse-postprocess "Return stream unbound, defaulting to \"timer(1)\"")
			       ;(set! routes `((BASE (timer '1.0))))
			       '(timer '1.0))))
		  (error 'wsparse-postprocess "Must have only one stream-wiring (<-) expression for now! ~a" routes)))))
      (define final-expression
	(match defs
	  [() body]
	  ;; A 'using' statement:
	  [((,M) . ,[rest]) `(using ,M ,rest)]
	  ;; A define statement:
	  [((,v ,e) . ,[rest])
	   ;; If there's a type decl for this binding, use it:
	   `(let (,(if (memq v typevs)
		       `[,v ,(cadr (assq v types)) ,e]
		       `[,v ,e]))
	      ,rest)]

	  ;; A define-as statement:
	  [((,v ,pat ,e) . ,[rest])
	   ;; If there's a type decl for this binding, use it:
	   `(let-as ,(if (memq v typevs)
			  `[(assert-type ,(cadr (assq v types)) ,v) ,pat ,e]
			  `[,v ,pat ,e])
		    ,rest)]))
;      (inspect final-expression)
      
      `(wsparse-postprocess-language
	'(program ,final-expression
	   (type-aliases . ,typealiases)
	   (union-types . ,uniondefs)
	   'notype))
      )))


(define (de-cygwin-path fn)
  (if (< (string-length fn) 13)
      ;(error 'de-cygwin-path "this can't be a cygwin path: ~s" fn)
      fn
      (if (equal? (substring fn 0 10) "/cygdrive/")
	  (string-append 
	   (substring fn 10 11)  ":"
	   (substring fn 11 (string-length fn)))
	  fn)
      ))

(IFCHEZ
 ;; Chez can't run the parser right now, so we call a separate executable.
 ;; .returns A parsed file, or #f for failure.
 ;;
 ;; Expects absolute path!!
 ;;
 ;; [2007.07.19] Removing code to call the old server (through pipes).
 (define ws-parse-file
   (let ([outport #f] [inport #f]) ;; Ports to parser process.
     (lambda (fn)
       ;; FIXME: DELETE THIS.  It's obsoleted by the vastly more reliable TCP based server.

       ;; We don't even track source locations in ws.opt
       (define dont-track (or (not (regiment-track-source-locations)) (= 3 (REGOPTLVL))))
       (define extra-opts (if dont-track " --nopos" ""))

       (define (try-command)
	 ;; HACK: WON'T WORK IN WINDOWS:
	 (if (zero? (system "which wsparse")) 
	     ;; Use pre-compiled executable:
	     (begin 
	       (eprintf "  Falling back to wsparse executable to parse file: ~a\n" fn)
	       (system-to-str 
		(string-append "wsparse " fn  " --nopretty" extra-opts)))
	     #f))

       (define (try-from-source)
	 (eprintf
	  (** "  Falling back to wsparse.ss from source, but you probably"
	      " want to do 'make wsparse' or run 'wsparse_server_tcp' for speed.\n"))
	 ;; We want only the stdout not the stderr.
	 ;; UNFORTUNATELY, this 
	 (system-to-str 
	  (format "mzscheme -mqt ~a/src/plt/wsparse.ss ~a --nopretty ~a" 
		  (REGIMENTD) fn extra-opts)))

       (define (try-client/server)
	 (if (file-exists? "/tmp/wsparse_server_tcp_running")
	     (begin 
	       (eprintf "Calling wsparse_client.ss to parse file: ~a\n" fn)
	       ;; Ideally, we want only the stdout not the stderr, but can't do that.
	       ;; I don't want this command line to be bash-dependent if possible.
	       (let ([str (system-to-str
			   (format "mzscheme -mqt ~a/src/plt/wsparse_client.ss ~a ~a " (REGIMENTD) fn extra-opts))])
		 (if (or (equal? str "#f") (equal? str "#f\n")) 
		     #f
		     str)))
	     #f))

       ;; [2008.01.12] Adding this method.  Probably almost always good enough.
       (define (try-command-persist)     
	 ;; HACK: WON'T WORK IN WINDOWS:
	 (if (zero? (system "which wsparse")) 
	     ;; Use pre-compiled executable:
	     (begin 
	       (unless outport
		 (let-match ([(,in ,out ,id) (process (string-append "wsparse --persist --nopretty" extra-opts))])
		   (printf "  wsparse process started.\n")
		   (set! inport in)
		   (set! outport out)))
	       (eprintf "  Using wsparse process on file: ~a\n" fn)
	       (write fn outport)(newline outport)
	       (read inport))
	     #f))

       ;; ========================================   
       (when (eq? 'i3nt (machine-type))  (set! fn (de-cygwin-path fn)))

       (let* ([result 
	       (or (try-client/server)
		   (try-command-persist)
		   (try-command)       
		   (try-from-source))]
	      [decls (if (string? result) (read (open-input-string result)) result)])

	 ;; This is very hackish:
	 (when (or (eq? decls 'ERROR) (eq? decls 'PARSE)) ;; From "PARSE ERROR"
	   (error 'ws-parse-file "wsparse returned error when parsing ~s" fn))
	 
	 decls
	 ))))

 ;; The PLT version is imported above: (from regiment_parser.ss)
 (begin)
 )

;; Returns #f if parsing/reading fails.
;; Expects absolute path!!
(define (read-wavescript-source-file fn)
  (ASSERT (string? fn))
  (unless (equal? (extract-file-extension fn) "ws")
    (error 'read-wavescript-source-file 
	   "should only be used on a .ws file, not: ~s" fn))
  (let ([parsed (ws-parse-file fn)])
    (if parsed 
	(wsparse-postprocess parsed)
	#f)))

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
