
;; SET SECKET OPTION TO SS_REUSE_ADDR... so it can reuse something previously bound...

;;====================================================================================================
;: Runtime configuration manager for Reuters distributed processing project.

;; This is Chez Scheme specific, meant to be loaded after the ws/regiment compiler is loaded.

;; Note that this Scheme code is not on the critical path for actual
;; (high rate) data processing.  This code runs at "configuration
;; time" -- i.e. whenever we want to change the running system.

;; It must keep track of what code (query subgraphs) we've compiled
;; and loaded.  It must call the WaveScope compiler to compile new
;; code and dlopen to bring it into the running system.

;;====================================================================================================

;;====================================================================================================
;; Global Variables:

;; This is gross but we use global state to keep track of the
;; interactions with the control module.

(define curtransaction (box #f))
(define current-child-process #f)
(define query-output-file (getenv "WSQ_OUTPUTFILE")) ; #f means use stdout
(define query-app-name "wsq_query")

;; [2011.02.16] Introducing a paused/unpaused state to the stream engine.
(define runtime-paused-state #f)

;; The subgraph we're currently working on:
(define cursubgraph #f)

;; A persistent accumulation of subgraph ids.
(define subgraphs (box '()))

;; A list of the operators in the current subgraph.
(define cur_ops #f)

;; A list of extra WS includes for the current transaction
(define extra_includes #f)

;; This table tracks all the operators created.
;; It stores the *GENERATED* WS code (bindings) to instantiate each operator.
(define op_table (make-eq-hashtable))

;; This should store the types of edges when we can conveniently extract them.
;; For now [2010.01.15] it stores undefined values.
(define edge_table (make-eq-hashtable))

;; This tracks the operator ids in each subgraph.  Binds subgraph ids
;; to a list of numbers.
(define subgraph_table (make-eq-hashtable))

(define stream-engine-paused #f)

;; HACK
;; Here we build up an list of op ids in the order they were added:
(define op_log (box '()))

(define print-state void)
#;
(define (print-state)  ;; super verbose debug version:
  (printf "     Subgraphs:\n       ")
  (pretty-print (unbox subgraphs))
  (printf "     Current Subgraph:\n       ")
  (pretty-print op_log))

;;==============================================================================

;; Following the convention that we try to use SQL names, here we need
;; to map them onto the names used by the WaveScript intermediate
;; representation.
(define supported-binops 
  '((+ g+) (* g*) (- g-) 
    (/ g/) (^ g^)    
    
    (== wsequal?)
    (=  wsequal?) 
    (<=) (>=) (<) (>)
    (<> ,(lambda (a b) `(not (wsequal ,a ,b))))
    
    (% moduloI)

    (AND ws:and) (OR ws:or) (NOT not)
    ))

;; For now abs is not generic, just for FLOAT:
    ;(abs absF)

;; These are the allowed aggregators over windows:
(define aggregator-prims '(SUM AVG MIN MAX FIRST LAST FIRSTWHERE LASTWHERE))
;; TODO: Add PREV

;; This dispatches on the representation of the entry in
;; supported-binops and returns a piece of WS syntax that applies the
;; primitive to its operands.
(define (apply-prim-entry entry . args)
 (match entry
   [(,op) (cons op args)]
   [(,op ,newop)
   (if (procedure? newop)
       (apply newop args)
       (cons newop args))]
   [,else (error "apply-prim-entry bad entry in supported-binops: ~s" entry)]))

;;====================================================================================================
;; Poor man's "Parsing" -- we're just assuming sexp-compatible strings.
;;====================================================================================================

;; When we write field names "SYM" rather than "A.SYM" there's an
;; implicit record (tuple) that we're operating on.  In the generated
;; WS code that variable gets a name, as follows:
(define implicit-record-name 'rec)

(define (edge-sym id) (string->symbol (format "edge_~a" id)))

;; We prefix the variables:
(define (var-sym id) (symbol-append 'var id))

(define (ununquote x) 
  (if (and (pair? x) (eq? (car x) 'unquote))
      (cadr x) x))

;; Super simple at first, assumes it is just a list of fields:
(define (parse-project slist) 
  `(lambda (orig)
     ,(foldl (lambda (x acc)
               (match x
	        [(,s) (guard (symbol? s))           `(wsrecord-extend ',s (wsrecord-select ',s orig) ,acc)]
		[(,x ... AS ,y) (guard (symbol? y)) `(wsrecord-extend ',y ,(parse-expression x 'RECORD 'orig)  ,acc)]
		[,x (error 'parse-project "invalid syntax for projection field: ~s" x)]
		))
	'(empty-wsrecord)
	(map ununquote slist))))


(define (comma-split-slist sexp)
  (let loop ((sexp sexp) (acc '()))
    (match sexp
      [()    (if (null? acc) '() (list (reverse! acc)))]
      [((,uq ,a) ,b* ...) (guard (eq? uq 'unquote))
       (cons (reverse! acc) (loop b* (list a)))]
      [(,a ,b* ...) (loop b* (cons a acc))]
      )))
;; (string->symbol "unquote")


;; Parsing of SQL-style value expressions.
;;  Takes an sexp and returns a WS expression AST.
;;  Does NOT take a string.  This operates in two modes.  VARIABLE
;;  where identifiers are interpreted as plain variables, and RECORD,
;;  where they are all implicitly field references.
(define parse-expression
  (case-lambda 
    [(expr)      (parse-expression expr 'RECORD)]
    [(expr mode) (parse-expression expr mode implicit-record-name)]
    [(expr mode rec)
     (match expr
       [,x (guard (symbol? x)) 
	   (match mode	     
	     [RECORD 
	      (match (string-split (symbol->string x) #\.)
		[(,field)      `(wsrecord-select ',x ,rec)]
		[(,rec ,field) `(wsrecord-select ',(string->symbol field) ,(string->symbol rec))]
		[,else          (error 'parse-expression "unsupported identifier: ~s" x)])]
	     [VARIABLE (var-sym x)])]
       [,x (guard (number? x)) `',x]
       [,x (guard (string? x)) `',x]
       [(,[x]) x] ;; Extra parens are ok.
       [(,[a] ,binop ,[b])  (guard (symbol? binop) (assq binop supported-binops))
	(define entry (assq binop supported-binops))
	;(unless entry  (error 'parse-expression "unsupported binary operator"))
	(apply-prim-entry entry a b)]

	[(CAST (,[expr] AS ,type))
	 ;(ASSERT "WSQ/SQL types should be simple for now." symbol? type)
	 ;; Currently this only works for NUMBERS:
	 `(assert-type ,(parse-type type) (cast_num ,expr))
	]

       ;; Difficult multiple values error here... is it a problem with -> x*?
       ;[(,AGGRPRIM ,[comma-split-slist -> x*] ...)  ]

       [(,AGGRPRIM (,_x* ...))
       	(guard (symbol? AGGRPRIM) (memq AGGRPRIM aggregator-prims))
        (define x* (comma-split-slist _x*))
	(define (app x . args)
	  (let-values ([(aggrexpr window-name) (lift-aggregate rec x)])
	    `(app ,(symbol-append 'wsq_ AGGRPRIM) ,aggrexpr ,@args ,window-name)))
	(cond 	  
	 ;; [2010.07.06] Adding these new aggregators
	 [(memq AGGRPRIM '(FIRSTWHERE LASTWHERE))	  
	  (match x* [(,x ,pred) (app x `(lambda (,implicit-record-name) ,(parse-expression pred)))])]
	 [else (match x* [(,x) (app x)])])]
	
	;; TODO: ADD COMMAS:
	#;
       [(,prim ,[rand*] ...) (guard (symbol? prim) (regiment-primitive? prim))
        `(,prim . ,rand*)]

       [,oth (error 'parse-expression "Invalid expression: ~a" oth)]
       )]))

;; Reads a single symbol from a string (eliminating whitespace)
;; This is a hackish way to do it:
; (define (extract-symbol-name str)  (read (open-string-input-port str)))

;; Trim whitespace just from the start and end:
(define (trim-whitespace str)
  (define len (string-length str))
  (define start 
    (let loop1 ((i 0))
      (cond
	[(= i len) (sub1 len)]
	[(not (char-whitespace? (string-ref str i))) i]
	[else (loop1 (add1 i))])))
  (define end
    (let loop2 ((i (sub1 len)))
      (cond
	[(< i 0) 0]
	[(not (char-whitespace? (string-ref str i))) (add1 i)]
	[else (loop2 (sub1 i))])))
  (if (> len 0)
      (substring str start end)
      ""))

;; Variables in SQL-like expressions may be qualified by what stream
;; they refer to or that may be implicit.
(define (free-rec-vars expr default-rec)
  (match expr
    [,x (guard (symbol? x)) 
	(match (string-split (symbol->string x) #\.)
	  [(,field)       (list default-rec)]
	  [(,rec ,field)  (list (string->symbol rec))])]
    [,x (guard (number? x)) '()]
    [,x (guard (string? x)) '()]
    [(,[x]) x] ;; Extra parens are ok.
    [(,[a] ,binop ,[b])  (guard (symbol? binop) (assq binop supported-binops))
     (union a b)]
    [(CAST (,[expr] AS ,type)) expr]
    [(,AGGRPRIM ,[comma-split-slist -> x*] ...) 
     (guard (symbol? AGGRPRIM) (memq AGGRPRIM aggregator-prims)) 
     (apply append x*)]
    [(,prim ,[rand*] ...) (guard (symbol? prim) (regiment-primitive? prim)) (apply union rand*)]
    [,oth (error 'free-rec-vars "Invalid expression: ~a" oth)]))


;; SQL has overloading conventions where sometime a variable that
;; refers to a window can be used as a scalar.  We need to
;; unravel that.
(define (lift-aggregate default-rec expr)
  ;; For now we assume that all variables inside the aggregation body
  ;; are WINDOWED, we need some primitive type checking to figure out
  ;; when scalars and windows are mixed.
  ;;
  ;; Here's a hack... we need to determine sometimes WHICH window it refers to.
  ;; We check the free variables of the operand to determine that.
  (define rec-name 
    (match (free-rec-vars expr default-rec)
      [(,nm) nm]
      [,oth (error 'lift-aggregate "aggregator expression contains mixed record references ~s : ~s " oth expr)]))
  (values `(lambda (,rec-name) ,(parse-expression expr 'RECORD rec-name))
	  rec-name))

(define (parse-type ty)
  (case  ty
    [(FLOAT INT DOUBLE) (capitalize-first ty)]
    [else (error 'parse-type "This type not currently supported by WSQ: ~s" ty)]))

(define (capitalize-first s_or_s)
  (define str (if (symbol? s_or_s) (symbol->string s_or_s) s_or_s))
  (define result 
    (string-append (uppercase (substring str 0 1))
		   (lowercase (substring str 1 (string-length str)))))
  (if (symbol? s_or_s) (string->symbol result) result))

;; Operates on symbols:
(define (capitalize s)
  (define str (symbol->string s))
  (string->symbol
   (string-append 
    (string-upcase (substring str 0 1))
    (substring str 1 (string-length str)))))

(define (hashtable-for-each fn tab)
  (define-values (keys vals) (hashtable-entries tab))
  (map fn (vector->list keys) (vector->list vals)))

#;
(define (downcase str)  ;; Lowercase a string
  (let ((new (string-copy str)))
    (for i = 0 to (sub1 (string-length str))
	 (string-set! new i (char-downcase (string-ref new i))))
    new))

(define (downcase-symbol s)
  ;(symbol->string (lowercase (string->symbol s)))
  (string->symbol (lowercase (symbol->string s)))
  )

;; Convert a type from the input schema (which is wannabe SQL) into a proper WaveScript type.
(define (Type t)
  (case (downcase-symbol t)
    [(int float double string)  (capitalize t)]
    [(bigint) 'Int64]
    [else (error 'Type "unhandled type: ~s" t)]))

(define (parse-types str)
  (define sexp (ASSERT (string->slist str)))
  `(Record 
    ,(match sexp
       [() `',(unique-name "_rowvar")]
       #; ;; It's inexplicable that this one doesn't work:
       [(,[ununquote -> type] ,[ununquote -> name] . ,[rest])
	`(Row ,name ,type ,rest)]
       [(,type ,name . ,[rest])
	`(Row ,(ununquote name) ,(Type (ununquote type)) ,rest)]
       )))


(define mergemagic (gensym "Merge"))

;;==============================================================================
;; Here are the C-callable functions -- the interface to the control module.
;;==============================================================================

;; Transactions.

(define-entrypoint WSQ_BeginTransaction (int) void
  (lambda (id)
    (vprintf 1 " <WSQ>  WSQ_BeginTransaction ~s \n" id)
    (when (unbox curtransaction)
      (error "Nested transactions are not allowed.  Attempted to start ~a within ~a\n" id (unbox curtransaction)))
    (set-box! curtransaction id)
    (set! extra_includes '())
    ))

;; Pick which WS backend to use, default is wsc2:
(define wsq-engine
  (string->symbol (uppercase (or (getenv "WSQ_BACKEND") "C"))))

(define-entrypoint WSQ_EndTransaction () int
  (lambda ()    
    (vprintf 1 " <WSQ>  WSQ_EndTransaction \n")
    (ASSERT "Must be in transaction to call EndTransaction." id (unbox curtransaction))

    ;; Ok, this is where we need to actually commit a change.

    ;; FIXME: need to do a topological sort.
    (let* ([binds (map (lambda (opid)
                         (ASSERT (hashtable-ref op_table opid #f)))
		    (reverse (unbox op_log)))]
	   [empties '()]
	   [bod (map (match-lambda ((,v ,e)) 
		       (let ([var (if (eq? v mergemagic)
				      (let ((new (unique-name "ignored")))
					(set! empties (cons new empties ))
					new)
				      v)])			      
			 `(define ,var ,e)))
		  binds)]
	   [prog `((include ,(string-append (getenv "REGIMENTD") "/apps/reuters/runtime/wsqlib.ws"))
	           ;; Include the user's code as well:
	           ,@(map (lambda (x) `(include ,x)) extra_includes)
		   ,@bod
		   (define main 
		     (assert-type (Stream #())
				  ,(match empties
				     [() '(app timer '0)]
				     [(,vr) vr]
				     [(,vr . ,[rest])
				      `(app merge ,vr ,rest)]))))]
	   [start-time (current-time 'time-monotonic)]
	   [print-end-time 
	    (lambda () 
	      (let ([end-time (current-time 'time-monotonic)])
		(vprintf 1 " <WSQ>   Finished compilation (~a seconds); back to control program...\n"
			 (+ (- (time-second end-time)
			       (time-second start-time))
			    (/ (- (time-nanosecond end-time)
				  (time-nanosecond start-time))
			       (* 1000.0 1000 1000))))))])

      ;;[regiment-verbosity (if (>= verbose-mode) 5 1)]
      (regiment-verbosity (- verbose-mode 1))

      (if (>= verbose-mode 2)
	  (begin
	    (printf "\n >>> ASSEMBLED PROG: \n\n") (pretty-print prog) (newline)
	    ;;(pretty-print (wsparse-postprocess prog))  
	    ;;(ws prog)
	    )
	  (begin 
	    (regiment-verbosity 0) (putenv "REGIMENT_QUIET" "1"))
	  )


      (case wsq-engine
        [(SCHEME SCHEMEQUICK)
	 (vprintf 1 " <WSQ>   JITing WS program inside Scheme process.\n")
	 ;(regiment-verbosity 5)
	 (let* ([strm 
		 ((if (eq? wsq-engine 'SCHEMEQUICK) wsint-early wsint) 
		  (wsparse-postprocess prog) '())]
		[end-time (current-time 'time-monotonic)])
	   (print-end-time)
	   ;(wsint-output-file query-output-file)
	   ;(wsint:direct-stream strm)
	   ; (browse-stream   )
	   ;; FORK A THREAD FOR THIS:
	   (stream-dump strm (or query-output-file (current-output-port)))
	   )]

        ;; ====================================================================================================
	[(C)
	 (vprintf 1 " <WSQ>   Compiling generated WS program.\n")	 
	 ;; Make it run in REALtime:
	 (when (not (getenv "WSQ_MAXSPEED"))
	   (vprintf 1 " <WSQ>   Compiling query to run timers in REAL time (using usleep)....\n")
	   (putenv "WS_LINK" (format "~a -DWS_REAL_TIMERS " (or (getenv "WS_LINK") ""))))

	 (when (getenv "WSQ_OUTEXE") (WSQ_SetQueryName-entry (getenv "WSQ_OUTEXE")))

	 ;;------------------------------------------------------------
         (set-c-output-basename! query-app-name)      
	 ;; Compile through the wsc2 backend:
	 (parameterize ([compiler-invocation-mode 'wavescript-compiler-c]

	                ;; In tests/7_MergeMonotonic I have tripped a refcount GC bug... doing Boehm for the moment:
			[wsc2-gc-mode 'boehm]
			)
	   (wscomp (wsparse-postprocess prog) '() 'wsc2)
	   ;; The problem with this is our input program is already an sexp:
	   ; (wsc2 prog "-o" (string-append (remove-file-extension query-app-name) ".exe"))
	   )
	 ;;------------------------------------------------------------

	 ;; We use the undocumented interface to wsc2-gcc
	 (putenv "CFILE" (string-append query-app-name ".c"))
	 ;; Same here:
	 (putenv "COPTFLAG" (or (getenv "WSQ_OPTLVL") "0"))
	 
	    ;(printf "SETTING LINK ~s\n" (getenv "WS_LINK"))
	    
	    ;; We go all the way back to our shell script to have it call gcc.
	    (if (>= verbose-mode 1)
	    	(system "wsc2-gcc") ;; Why is the output from this not printing?
		(system "wsc2-gcc 2> wsc2-gcc.output.log") ;; FIXME: ASSUMES UNIX
		)

	    (print-end-time)

	    ;; Begin executing the query:
	    (if stream-engine-paused
	      (vprintf 1 " <WSQ> EndTransaction NOT running query because stream engine paused.\n")
	      (begin
		(vprintf 1 "\n================================================================================\n")
		;; Kill old process:
		(when current-child-process (kill-child current-child-process))
		;; Create new child process:
		(let-values ([(to-stdin from-stdout from-stderr pid)
			      (let ((cmd 
				     (if query-output-file
					 ;; [2010.08.20] I think I was using exec to avoid TWO CHILD PROCESSES and make sure my kill went through.
					 ;; But if I kill the process can I make sure it's flushing and actually opens the output log?
					 ;; Maybe it would be better to generate WS code that opens/writes the output file.
					 (format "exec ./~a.exe > \"~a\"" query-app-name query-output-file)
					 (format "exec ./~a.exe" query-app-name))))
				(vprintf 1 " <WSQ> Executing command in child process: ~a\n" cmd)
				(if query-output-file
				    (open-process-ports cmd)
				    (open-process-ports cmd 'block (make-transcoder (latin-1-codec)))))

			    ;(open-process-ports "./query.exe &> /dev/stdout" 'block (make-transcoder (latin-1-codec)))
			    ])
		  (if current-child-process
		      (vprintf 1 " <WSQ> Created replacement child process (pid ~a).\n" pid)
		      (vprintf 1 " <WSQ> Started child process to execute query (pid ~a).\n" pid))
		  (set! current-child-process pid)
		  (vprintf 1 "================================================================================\n\n")

		  ;; We need to return from the EndTransaction call back to the control program.
		  ;; I wish there were some good way to plug the from-stdout into console-output-port 
		  ;; without having a separate thread for the express purpose of echoing input.

		  ;; (unless (threaded?) (error 'WSQ "must be run with a threaded version of Chez Scheme."))
		  (unless query-output-file
		    (fork-thread (echo-port from-stdout)))
		  ))
	    	)]
	 [else (error 'WSQ_EndTransaction "unknown WSQ_BACKEND: ~s" wsq-engine)]))
    (set-box! curtransaction #f)
    (set! extra_includes #f)
    ;; Return pid:
    (or current-child-process 0)
    ))

;; Creates a silly thread just for the purpose of relaying input to
;; output -- "plugging" an input port into an output one.
(define (echo-port port)
  (lambda ()
;    (printf "##### STARTING THREAD FOR PRINTING... \n")
    (let loop ((x (get-string-some port)))
      (unless (eof-object? x)
	(display x (current-output-port))  
	(flush-output-port (current-output-port))
	(loop (get-string-some port))))
	(when (>= verbose-mode 1)
	  (fprintf (console-error-port) "\n ### ECHO THREAD: Port closed.  Child process probably dead.  Terminating.\n"))
    ; (printf "##### ENDING ECHO THREAD... \n")
	))

;; [2010.06.26] I'm running into what appears to be a bug with system.
;; It will give me spurious "No child process" errors.  This fixes the problem:
(define (spin-system cmd)
  (let-values ([(to out err pid) (open-process-ports cmd)])
    ;; Run it dry:
    (let loop ((x 0)) (unless (eof-object? x) (loop (get-bytevector-some out))))))

(define (kill-child pid)
  ;(define killcmd (format "kill -9 ~a" pid))
  ;; If it's a shell I thought a normal kill might let it kill recursively... but no.
  ;; [2011.02.25] How about behavior with respect to flushing output ports?  Any difference?
  (define killcmd (format "kill  ~a" pid))

  (when (>= verbose-mode 1)
    (vprintf 1 " <WSQ> checking for child process:\n")(flush-output-port (current-output-port))
    ;(system (format "ps aux | grep ~a" pid))
    (system (format "ps u -p  ~a" pid))
    )

  ;; And in fact doing a normal kill will of course let the query
  ;; persist a little bit and overlap with the next executable.
  ;; Oh, wait... that happens with -9 too... darn.
  ;(printf "RIGHT BEFORE KILLCMD\n") (flush-output-port (current-output-port))
  ;(system killcmd) ;; This is the call that throws the "No child process" error.
  (spin-system killcmd)      
  ;(printf "RIGHT AFTER KILLCMD\n") (flush-output-port (current-output-port))

  (vprintf 1 " <WSQ> Killing child process ~s\n" killcmd)(flush-output-port (current-output-port))
  ;(printf "  Processes after kill:\n") (system "ps aux | grep query")
)

(define (add-op! opid binding)
  (when (hashtable-contains? op_table opid) (error 'WSQ "op with node id ~s already exists!" opid))
  (hashtable-set! op_table opid binding)
  (unless cur_ops (error 'add-op! "The system is not in a good state.  Maybe BeginSubgraph was omitted?\n"))
  (set! cur_ops (cons opid cur_ops))
  (set-box! op_log (cons opid (unbox op_log))))


;; The value stored in the edge table tracks the number of WRITERS (out edges) for that edge.
(define (add-in-edge! id)
  ;; This makes sure there is an entry but doesn't increment the counter:
  (hashtable-set! edge_table id (hashtable-ref edge_table id 0)))
(define (add-out-edge! id)
  (define old (hashtable-ref edge_table id 0))
  (unless (zero? old)
    (error 'add-in-edge 
       "More than one output to a given edge id not currently allowed.  Offending edge id: ~s." id))
  (hashtable-set! edge_table id 1))


;; Subgraphs.
;; ----------------------------------------

(define-entrypoint WSQ_BeginSubgraph (int) void 
  (lambda (id)
    (vprintf 1 " <WSQ>  WSQ_BeginSubgraph ~s \n" id)
    (when cursubgraph
       (error 'WSQ_BeginSubgraph "cannot begin subgraph ~s within another subgraph ~s!" id cursubgraph)
       ;(error 'WSQ "Subgraph with ID ~s already exists and has not been removed.\n" id)
       )
    (ASSERT (not (hashtable-contains? subgraph_table id)))
    (ASSERT not cur_ops)
    (set! cursubgraph id)
    (set! cur_ops '())
    ))

(define-entrypoint WSQ_EndSubgraph () void
  (lambda ()
    (unless cursubgraph 
      (vprintf 1 " <WSQ>  ERROR: WSQ_EndSubgraph called without a corresponding WSQ_BeginSubgraph.\n")
      (exit -1))
    (let ((id cursubgraph))
      (vprintf 1 " <WSQ>  WSQ_EndSubgraph ~s \n" id)
      ;; Currently subgraphs don't really do anything special.
      ;; But we do record the operators introduced by the subgraph so we can remove them later.
      (hashtable-set! subgraph_table id cur_ops)
      (set! cursubgraph #f)
      (set! cur_ops #f)
      )))

(define-entrypoint WSQ_RemSubgraph (int) void
  (lambda (id)
    (vprintf 1 " <WSQ>  WSQ_RemSubgraph ~s \n" id)
    (unless (hashtable-contains? subgraph_table id)
      (error 'WSQ_RemSubgraph "Tried to remove subgraph which was not installed: id ~s" id))
    (hashtable-delete! subgraph_table id)
    ))

(define-entrypoint WSQ_EdgeType (int) scheme-object
  (lambda (id)
    (define type (format "type_~a" (random 1000)))
    (vprintf 1 " <WSQ>  WSQ_EdgeType ~s : ~s \n" id type)
    ;   type
    (error 'WSQ_EdgeType "not implemented yet")
    ))

;; Adding operators.
;; ----------------------------------------

(;define-entrypoint WSQ_AddReutersSource (int int single-float string) void
 define WSQ_AddASCIIFileSource
  (lambda (opid outid frequency schema-path datafile)
    ;(printf " <WSQ>  WSQ_AddReutersSource ~s ~s \n" opid schema-path)
    (define code `(,(edge-sym outid) 
		   (app wsq_asciiFileSource ,(car (ASSERT (string->slist frequency))) ',schema-path ',datafile)))
    (add-op! opid code) (add-out-edge! outid)
    ))


(define-entrypoint WSQ_AddPrinter (int string int) void
  (lambda (opid str inid)
    ;(printf " <WSQ>  WSQ_AddPrinter ~s ~s \n" str id)
    (define code `(,mergemagic (app wsq_printer ,str ,(edge-sym inid))))
    (add-op! opid code) (add-in-edge! inid)
    ))

(define-entrypoint WSQ_AddProject (int int int string) void
  (lambda (opid in out expr)
    ;(printf " <WSQ>  WSQ_AddProject ~s ~s ~s \n" in out expr)
    (define fun (parse-project (map (lambda (s) (ASSERT (string->slist s))) (string-split expr #\,))))
    (define code `(,(edge-sym out) (app wsq_project ,fun ,(edge-sym in))))
    (add-op! opid code) (add-in-edge! in) (add-out-edge! out)
    ))


;; This assumes for now that it's a series of binary operators.
(define (parse-filter str)
  `(lambda (,implicit-record-name) 
     ,(parse-expression (ASSERT (string->slist str)))))

(define-entrypoint WSQ_AddFilter (int int int string) void
  (lambda (opid in out expr)
    ;(printf " <WSQ>  WSQ_AddFilter ~s ~s ~s \n" in out expr)
    (define code `(,(edge-sym out) (app wsq_filter ,(parse-filter expr) ,(edge-sym in))))
    (add-op! opid code) (add-in-edge! in) (add-out-edge! out)
    ))

;; UDF's may take and produce any number of stream arguments.
(define (WSQ_UDF opid in* out* file name args)

;; FIXME!!! If I DONT do this maybe-tuple thing I seem to get type
;; errors with unary tuples (which should just be aliases for
;; non-tuples!).

  (define (maybe-tuple fn ls)
    (if (> (length ls) 1)
        (fn ls)
	(car ls)))
  (define code
    `[,(maybe-tuple list->vector (map edge-sym out*)) ;; Tuple of output streams.
       (app ,name
	    ,(maybe-tuple (lambda (x) `(tuple ,@x))
	                  (map edge-sym in*))
	    ,@args)])
  (unless extra_includes (error 'WSQ_UDF "The system is not in a good state.  Maybe BeginTransaction was omitted?\n"))  
  (set! extra_includes (cons file extra_includes))

  (add-op! opid code)  
  (for-each add-in-edge! in*) 
  (for-each add-out-edge! out*)
  )

(define (WSQ_AddFilterWindows opid in out _expr)  
  (define code
    `[,(edge-sym out) (app wsq_project
			   (lambda (win)
			      (app Sigseg:filter ,(parse-filter _expr) win))
			      ,(edge-sym in))])
  (add-op! opid code)  (add-in-edge! in) (add-out-edge! out)
 )


; char* joinstr = "MONOTONIC LEFT ONLY | A B | ((FIRST(A.TIME)) <= (FIRST(B.TIME))) AND (LAST(A.TIME)) >= (LAST(B.TIME))";
(define (WSQ_AddJoin opid in1 in2 out _conf _names _predexpr)
  (define conf     (ASSERT (string->slist _conf)))
  (define predexpr (parse-expression (ASSERT (string->slist _predexpr))))  
  (match conf 
    [(MONOTONIC LEFT ONLY) (void)]
    [,oth (error 'WSQ_AddJoin "Initial prototype is very restrictive, only MONOTONIC LEFT ONLY config supported, not: ~s" oth)])
  (match (string->slist _names)
    [(,A ,B)  
     (define code `[,(edge-sym out)
		    (app wsq_join_leftonly (lambda (,A ,B) ,predexpr)
			 ,(edge-sym in1) ,(edge-sym in2))])
     (add-op! opid code)  (add-in-edge! in1) (add-in-edge! in2) (add-out-edge! out)
     ]))

(define (WSQ_AddMergeMonotonic opid in1 in2 out _labeler)
  (define labeler (parse-expression (ASSERT (string->slist _labeler))))
  (define code `[,(edge-sym out)
		 (app wsq_mergeMonotonic (lambda (,implicit-record-name) ,labeler)
		      ,(edge-sym in1) ,(edge-sym in2))])
  (add-op! opid code)  (add-in-edge! in1) (add-in-edge! in2) (add-out-edge! out)
  )



(define (WSQ_AddMatchRecognize opid in out _rows-per-match _pattern _defs)
  (define rows-per-match (car (ASSERT (string->slist _rows-per-match))))
  (define pat (ASSERT (string->slist _pattern)))
  (define defs (map (lambda (x) (ASSERT (string->slist x))) (string-split _defs #\,)))
  (define patwidth (length pat))
  (define names (map (lambda (x) (match x [(,v AS . ,e*) (guard (symbol? v)) v])) defs))
  (define exprs (map (lambda (x) (match x [(,v AS . ,e*) (guard (symbol? v)) e*])) defs))
  (define dummy_names (map (lambda (i) 
			     (symbol-append 'dummy_ (string->symbol (number->string i)))) 
			(iota patwidth)))

  ;; [2010.06.28] TYPE CHECKER ERROR THAT SEEM SPURIOUS:
  ;; Trouble matching the incomplete record type in the predicates against the closed one.
#;
  (define code 
    `(,(edge-sym out)
      (app wsq_filter
       ;; First bind predicate functions for each name:
       (let ,(map (lambda (name rhs) `[,name (lambda (,name) 
					       ;; Allow either qualified or unqualified field refs:
					       (let ([,implicit-record-name ,name]) 
						 ,(parse-expression rhs)))]) names exprs)
	(lambda (tuple)
	    ;; Second bind the elements of the window to names:
	    (let (,@(map (lambda (dummyname i) `[,dummyname (seg_get tuple ',i)]) dummy_names (iota patwidth)))
		;; Now take the conjunction of all predicates on corrensponding positions:
		,(match (map (lambda (dummyname predname) `(app ,predname ,dummyname)) dummy_names pat)
		   [() '#t]
		   [(,one) one]
		   [(,a . ,[rest]) `(ws:and ,a ,rest)]))))
	(app rewindow (app window ,(edge-sym in) ',patwidth)
	  ',patwidth ',(- (sub1 patwidth)) ))))

  ;; Workaround: doing windowing manually with a circular buffer:
  (define code 
    `(,(edge-sym out)
      (iterate (annotations)
       ;; First bind predicate functions for each name:
       (let (,@(map (lambda (name rhs) `[,name (lambda (,name) 
					       ;; Allow either qualified or unqualified field refs:
					       (let ([,implicit-record-name ,name]) 
						 ,(parse-expression rhs)))]) names exprs)
	     ;; This is the circular buffer:
	     [buffer (Array:makeUNSAFE ',patwidth)]
	     [index  (Mutable:ref '0)]
	     ;[filled (Mutable:ref '#f)]
	     [total (Mutable:ref '0)]
	     )
	(lambda (tuple vq)
	  (begin
	    ;; Store the new tuple and see if we've filled the buffer:
	    (Array:set buffer index tuple)
	    (set! index (_+_ '1 index))
	    (if (wsequal? index ',patwidth)
		(begin 
		  (set! index '0)
		   ;(if filled (begin) (set! filled '#t))
		  ;(set! filled '#t)
		  )
		  (begin))

	    (set! total (_+I64 '1 total))
	    (if (ws:and 
		 ;; filled
		      (>= total ',patwidth)
		      ;; Now take the conjunction of all predicates on corrensponding positions:
		      ,(match (map (lambda (i predname) 
		                     ;; Index has already been incremented, therefore it points to the EARLIEST tuple in 
				     ;; the buffer (first in the pattern), not the latest.
				     `(app ,predname (Array:ref buffer (moduloI (_+_ index ',i) ',patwidth))))
				(iota patwidth) pat)
			 [() '#t]
			 [(,one) one]
			 [(,a . ,[rest]) `(ws:and ,a ,rest)]))
		  ,(case rows-per-match
		    [(ALL) 
		     ;; Not really anything we can do with Arrays... turn it into a sigseg.
		     ;'(emit vq (app Array:copy buffer))
		     `(let ([inorder (Array:makeUNSAFE ',patwidth)])
			(begin
			  (app Array:blit inorder '0 buffer index (_-_ ',patwidth index)) ;; Copy first part.
			  (app Array:blit inorder (_-_ ',patwidth index) buffer '0 index) ;; Copy second part.
			  (emit vq (app Sigseg:toSigseg 
					inorder
					(_-I64 total ',(sub1 patwidth)) 
					nulltimebase))))
		     ]
		    [(ONE)
		      ;; In ONE ROW PER MATCH mode we pass on the last tuple received:
		      '(emit vq tuple)]
		    [else (error 'MatchRecognize "unsupported ROWS PER MATCH setting: ~s\n" rows-per-match)])
		  (begin))
	      vq)))
       ,(edge-sym in))))

  (add-op! opid code) (add-in-edge! in) (add-out-edge! out)
  )

;; BOILERPLATE: WE COULD GENERATE THESE SIMPLE WRAPPERS:
(define (WSQ_AddRandomSource opid outid frequency schema-path)
    (define code `(,(edge-sym outid) 
		   (app wsq_randomSource ,(car (ASSERT (string->slist frequency))) ',schema-path)))
    (add-op! opid code) (add-out-edge! outid))
(define (WSQ_AddNonRandomSource opid outid frequency schema-path)
    (define code `(,(edge-sym outid) 
		   (app wsq_nonRandomSource ,(car (ASSERT (string->slist frequency))) ',schema-path)))
    (add-op! opid code) (add-out-edge! outid))

;; This is the general version that uses a start/end predicate rather than
(define (WSQ_WindowGeneral opid in out _names _predicate)
  (define predicate (parse-expression (ASSERT (string->slist _predicate))))
  (define code 
    (let-match ([(,fst ,lst) (ASSERT (string->slist _names))])  
      `[,(edge-sym out) (app wsq_window (lambda (,fst ,lst) ,predicate) ,(edge-sym in))]))
  (add-op! opid code)  (add-in-edge! in) (add-out-edge! out))

(define (WSQ_AddWindow opid in out _field _timeexpr _slide)
  (define field (car (ASSERT (string->slist _field))))
  (define time (parse-expression (ASSERT (string->slist _timeexpr))))
  (define tmp (ASSERT (string->slist _slide)))
  (define slide 
    (match tmp
      [(,e ... TUPLE) (parse-expression e)]
      [(,e ... TUPLES) (parse-expression e)]
      [,oth (error 'WSQ_AddWindow "unhandled SLIDE specification (currently): ~s" oth)]))

  (define code 
    `[,(edge-sym out) (app wsq_window 
			   ;; An extractor:
			   (lambda (,implicit-record-name) ,(parse-expression field))
			   ,time ,slide ,(edge-sym in))]
  )

  (add-op! opid code)  (add-in-edge! in) (add-out-edge! out))






;; The generic entrypoint that can add any operator.
(define-entrypoint WSQ_AddOp (int string string string string) void
  (lambda (id optype inputs outputs _args)
    (define args (map trim-whitespace (string-split _args #\|)))
;    (define args (string-split _args #\|))
    (define __ (vprintf 1 " <WSQ>  WSQ_AddOp ~a ~s in: ~a out: ~a  args:  ~s \n" id optype inputs outputs args))
    (define in*  (ASSERT (string->slist inputs)))
    (define out* (ASSERT (string->slist outputs)))
    (define opsym (string->symbol (uppercase optype)))

    ;; Error handling:
    (define (has-inputs n)
      (ASSERT (format "~a should have exactly ~s input edge(s), not ~s." opsym n (length in*))  (curry = n) (length in*)))
    (define (has-outputs n)
      (ASSERT (format "~a should have exactly ~s output edge(s), not ~s." opsym n (length out*)) (curry = n) (length out*)))
    (define (has-args n)
      (ASSERT (format "~a should have exactly ~a extra string arguments, not ~s.  See documentation in README.txt" opsym n (length args))
              (curry = n) (length args)))
    (unless in*  (error 'WSQ_AddOp "Bad list of inputs: ~s"  inputs))
    (unless out* (error 'WSQ_AddOp "Bad list of outputs: ~s" outputs))
    
    ;; Dispatch on the type of operator.  
    (case opsym
      [;(REUTERSSOURCE) (has-inputs 0) (has-outputs 1) (has-args 2)
       (ASCIIFILESOURCE) (has-inputs 0) (has-outputs 1) (has-args 3)
        (apply WSQ_AddASCIIFileSource id (car out*) args)]
      [(RANDOMSOURCE) (has-inputs 0) (has-outputs 1) (has-args 2)
        (apply WSQ_AddRandomSource id (car out*) args)]
      [(NONRANDOMSOURCE) (has-inputs 0) (has-outputs 1) (has-args 2)
        (apply WSQ_AddNonRandomSource id (car out*) args)]

      [(UDF) 
        (match args 
	 [(,file ,funname . ,rest) (WSQ_UDF id in* out* file (string->symbol (trim-whitespace funname)) rest)]
	 [,other (error 'UDF "UDF WSQ Op expected at LEAST two arguments (file/name), got: ~s" other)])]

      [(PRINTER)    (has-inputs 1) (has-outputs 0) (has-args 1) (WSQ_AddPrinter id _args (car in*))]
      [(FILTER)     (has-inputs 1) (has-outputs 1) (has-args 1) (WSQ_AddFilter id (car in*) (car out*) _args)]
      [(PROJECT)    (has-inputs 1) (has-outputs 1) (has-args 1) (WSQ_AddProject id (car in*) (car out*) _args)]
      [(WINDOWJOIN) (has-inputs 2) (has-outputs 1) (has-args 4)
       ;; This is a horrible hack, to get both a FILTER expression,
       ;; and a number of seconds into this operator, we pack them
       ;; both in the same string.  We follow the arbitrary convention
       ;; that "|" serves as a divider for breaking up that string.
       (let-match ([(,_seconds ,recA ,recB ,filter) args])
        (define seconds (car (ASSERT (string->slist _seconds))))
	(WSQ_AddWindowJoin id (car in*) (cadr in*) (car out*) seconds recA recB filter))]

      [(CONNECTREMOTEIN)  (has-inputs 0) (has-outputs 1) (has-args 3)
       ;; This one takes three arguments in the string 'args'
       (let-match ([(,host ,_port ,fieldtypes) args])
        (define port (car (ASSERT (string->slist _port))))
	(WSQ_ConnectRemoteIn id (car out*) (trim-whitespace host) port fieldtypes))]

      [(CONNECTREMOTEOUT) (has-inputs 1) (has-outputs 0) (has-args 2)
       (let-match ([(,host ,_port) args])
        (define port (car (ASSERT (string->slist _port))))
	(WSQ_ConnectRemoteOut id (car in*) (trim-whitespace host) port))]

      [(MATCHRECOGNIZE) (has-inputs 1) (has-outputs 1) (has-args 3)  (apply WSQ_AddMatchRecognize id (car in*) (car out*) args)]
      [(WINDOW)         (has-inputs 1) (has-outputs 1) (has-args 3)  (apply WSQ_AddWindow id (car in*) (car out*) args)]
      [(JOIN)           (has-inputs 2) (has-outputs 1) (has-args 3)  (apply WSQ_AddJoin   id (car in*) (cadr in*) (car out*) args)]
      [(MERGEMONOTONIC) (has-inputs 2) (has-outputs 1) (has-args 1)  (apply WSQ_AddMergeMonotonic id (car in*) (cadr in*) (car out*) args)]
      [(FILTERWINDOWS)  (has-inputs 1) (has-outputs 1) (has-args 1)  (apply WSQ_AddFilterWindows  id (car in*) (car out*) args)]

	
      [else (error 'WSQ_AddOp "unknown op type: ~s" optype)]
    )))


(define (make-merger skel1 skel2)
  (define result )
  `(lambda (left right)
     ,(match (union skel1 skel2)
	[() '(empty-wsrecord)]
	[(,hd . ,[rest])
	 ;; For now this biases left if a field occurs in both tuples.
	 (if (memq hd skel1)
	     `(wsrecord-extend ',hd (wsrecord-select ',hd left) ,rest)
	     `(wsrecord-extend ',hd (wsrecord-select ',hd right) ,rest))])))

;; FINISHME
(define (get-type-skeleton1)  '(SYM PRICE TIME))
;(define (get-type-skeleton2)  '(VOLUME))
(define (get-type-skeleton2)  '(SYM PRICE TIME VOLUME))

;; Ok, what we need to do here is keep track of some of the edge
;; information... specifically just the set of fields.  We don't need
;; to know the whole type... just that set of field *names*.
;;
;; This will enable us to simulate ....
(define-entrypoint WSQ_AddWindowJoin (int int int int single-float string string string) void
  (lambda (opid in1 in2 out seconds recA recB expr)
    ;(printf " <WSQ>  WSQ_AddWindowJoin ~s ~s ~s ~s ~s \n" in1 in2 out seconds expr)
    (define code
      `(,(edge-sym out) (app wsq_windowJoin
			     (lambda (,(car (ASSERT (string->slist recA))) 
				      ,(car (ASSERT (string->slist recB))))
			       ,(parse-expression (ASSERT (string->slist expr))))
			     ,(make-merger (get-type-skeleton1)
					   (get-type-skeleton2))
			     ,(edge-sym in1)
			     ,(edge-sym in2)
			     ,seconds
			     )))
    (add-op! opid code)   (add-in-edge! in1) (add-in-edge! in2) (add-out-edge! out)))


;; Connecting to remote machines.

(define-entrypoint WSQ_ConnectRemoteIn (int int string int string) void
  (lambda (opid inid host port field-types)
    ;(printf " <WSQ>  WSQ_ConnectRemoteIn ~s ~s ~s ~s \n" inid host port field-types)
    (define code `(,(edge-sym inid) 
		   (assert-type (Stream ,(parse-types field-types))
				(app wsq_connect_in ,host ,port))))
    (add-op! opid code)  (add-out-edge! inid)
    )) 

(define-entrypoint WSQ_ConnectRemoteOut (int int string int) void
  (lambda (opid outid host port)
    ;(printf " <WSQ>  WSQ_ConnectRemoteOut ~s ~s ~s \n" outid host port)
    (define code `(,mergemagic (app wsq_connect_out ,host ,port ,(edge-sym outid))))
    (add-op! opid code) (add-in-edge! outid)
    ))


(define-entrypoint WSQ_Shutdown () void
  (lambda ()
    (when current-child-process (kill-child current-child-process))
    (vprintf 1 " <WSQ> Shutting down query engine.\n")

#;
    (begin 
      (printf "Subgraph table was:\n")
      (hashtable-for-each 
       (lambda (id ls)
	 (printf "  ~s: ~s\n" id ls))
       subgraph_table))
    ))

(define-entrypoint WSQ_SetOutputFile (string) void
  (lambda (path)
    (if (equal? path "")
     (if query-output-file
         (vprintf 1 " <WSQ> Query output goes to ~s based on WSQ_OUTPUTFILE environment var.\n" query-output-file)
         (vprintf 1 " <WSQ> Query output goes to stdout (not redirecting).\n"))
     (begin
       (set! query-output-file path)
       (vprintf 1 " <WSQ> Query output redirected to ~s\n" path))
       )))


;; Takes a base name.
(define-entrypoint WSQ_SetQueryName (string) void
  (lambda (path)
    (unless (equal? path "")
      (set! query-app-name path)
      (vprintf 1 " <WSQ> Query base name (used for .c/.exe) set to: ~s\n" path)
   )))

;; This can also be done from the environment variable "WSQ_BACKEND"
(define-entrypoint WSQ_SetBackend (int) void
  (lambda (mode)
    (define sym
      (case mode 
	[(1) 'C]           
	[(2) 'SCHEME]      
	[(3) 'SCHEMEQUICK] 
	[else (error 'WSQ_SetBackend "Error: unrecognized mode enum ~s" mode)]))
    (vprintf 1 " <WSQ> Setting query mode to: ~s\n" sym)
    (set! wsq-engine sym)
    ))

(define-entrypoint WSQ_Pause () void
  (lambda ()
    (vprintf 1 " <WSQ> Pausing stream engine.\n")
    (set! stream-engine-paused #t)
    ))

(define-entrypoint WSQ_Unpause () void
  (lambda ()
    (vprintf 1 " <WSQ> Unpausing stream engine.\n")
    ;; Return value:
    (error 'WSQ_Unpause "<WSQ> unpausing not supported yet")
    0
    ))

;;==============================================================================
;; TESTS:

#;
(parse-expression
   (((LAST (PRICE))
      /
      ((SUM (CAST (VOLUME AS FLOAT) * PRICE))
        /
        (CAST ((SUM (VOLUME)) AS FLOAT))))
     >=
     1.02)
   rec)


;(comma-split-slist ((PRICE ,SYM == "S&P")))

#;
(parse-expression  '(SUM (VOLUME))  'RECORD  'rec)
