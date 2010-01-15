

;: Runtime configuration manager for Reuters distributed processing project.

;; This is Chez Scheme specific.

;; Note that this Scheme code is not on the critical path for actual
;; (high rate) data processing.  This code runs at "configuration
;; time" -- i.e. whenever we want to change the running system.

;; It must keep track of what code (query subgraphs) we've compiled
;; and loaded.  It must call the WaveScope compiler to compile new
;; code and dlopen to bring it into the running system.




;;==============================================================================

;; This is gross but we use global state to keep track of the
;; interactions with the control module.

(define curtransaction (box #f))
(define current-child-process #f)

;; The subgraph we're currently working on:
(define cursubgraph #f)

;; A persistent accumulation of subgraph ids.
(define subgraphs (box '()))

;; A list of the operators in the current subgraph.
(define cur_ops #f)

;; This table tracks all the operators created.
;; It stores the *GENERATED* WS code (bindings) to instantiate each operator.
(define op_table (make-eq-hashtable))

;; This should store the types of edges when we can conveniently extract them.
;; For now [2010.01.15] it stores undefined values.
(define edge_table (make-eq-hashtable))

;; This tracks the operator ids in each subgraph.  Binds subgraph ids
;; to a list of numbers.
(define subgraph_table (make-eq-hashtable))

;; HACK
;; Here we build up an list of op ids in the order they were added:
(define op_log (box '()))

(define print-state void)
#;
(define (print-state)
  (printf "     Subgraphs:\n       ")
  (pretty-print (unbox subgraphs))
  (printf "     Current Subgraph:\n       ")
  (pretty-print op_log))

;;==============================================================================

(define (edge-sym id) (string->symbol (format "edge_~a" id)))

(define (ununquote x) 
  (if (and (pair? x) (eq? (car x) 'unquote))
      (cadr x) x))

;; Super simple at first, assumes it is just a list of fields:
(define (parse-project str) 
  `(lambda (orig)
     ,(foldl (lambda (x acc)
	       `(wsrecord-extend ',x (wsrecord-select ',x orig) ,acc))
	'(empty-wsrecord)    
	(map ununquote (ASSERT (string->slist str))))))

;; This assumes for now that it's a series of binary operators.
(define (parse-filter str . extra)
  (define args
    (match extra
      [() '(rec)]
      [(,x)    extra]
      [(,x ,y) extra]))

  `(lambda ,args
     ,(match (ASSERT (string->slist str))
    [() #t]
    [(,left ,binop ,right . ,[rest])
     (define bop 
       (case binop
	 [(== =) 'wsequal?] ;; Accept both single and double equals.
	 [(<= >= < >) binop]
	 [else (error 'WSQ "unhandled binary operator ~s" binop)]))
     (define expr
       `(,bop ,(temp-convert-rand (car args) (ununquote left)) 
	      ,(temp-convert-rand (if (not (null? (cdr args)))
				      (cadr args)
				      (car args))
				  right)))
     (if (eq? rest #t) expr
	 `(ws:and ,expr ,rest))]
    )))

(define (capitalize s)
  (define str (symbol->string s))
  (string->symbol
   (string-append 
    (string-upcase (substring str 0 1))
    (substring str 1 (string-length str)))))

(define (hashtable-for-each fn tab)
  (define-values (keys vals) (hashtable-entries tab))
  (map fn (vector->list keys) (vector->list vals)))

(define (Type t)
  (case t
    [(int float double
	  string) 
     (capitalize t)]    
    [else (error 'Type "unhandled type: ~s" t)]))

(define (parse-types str)
  `(Record 
    ,(match (ASSERT (string->slist str))
       [() `',(unique-name "_rowvar")]
       #; ;; It's inexplicable that this one doesn't work:
       [(,[ununquote -> type] ,[ununquote -> name] . ,[rest])
	`(Row ,name ,type ,rest)]
       [(,type ,name . ,[rest])
	`(Row ,(ununquote name) ,(Type (ununquote type)) ,rest)]
       )))

(define (temp-convert-rand rec x)
  (match x
    [,x (guard (symbol? x)) `(wsrecord-select ',x ,rec)]
    [,x (guard (number? x)) `',x]
    [,x (guard (string? x)) `',x]
    [(,[a] ,binop ,[b]) (guard (memq binop '(+ * - / ^)))
     `(,(symbol-append 'g binop) ,a ,b)]
    [,else (error 'temp-convert-rand "Invalid operand: ~a" x)]
    )
  )

(define mergemagic (gensym "Merge"))

;;==============================================================================

;; Here are the C-callable functions -- the interface to the control module.

;; Transactions.

(define-entrypoint WSQ_BeginTransaction (int) void
  (lambda (id)
    (printf " <WSQ>  WSQ_BeginTransaction ~s \n" id)

    (when (unbox curtransaction)
      (error "Nested transactions are not allowed.  Attempted to start ~a within ~a\n" id (unbox curtransaction)))
    (set-box! curtransaction id)
    ))

(define-entrypoint WSQ_EndTransaction () void
  (lambda ()    
    (printf " <WSQ>  WSQ_EndTransaction \n")
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
		   ,@bod
		   (define main 
		     (assert-type (Stream #())
				  ,(match empties
				     [() '(app timer '0)]
				     [(,vr) vr]
				     [(,vr . ,[rest])
				      `(app merge ,vr ,rest)]))))]
	   [start-time (current-time 'time-monotonic)])
				      
      ;(printf "\n >>> ASSEMBLED PROG: \n\n") (pretty-print prog)
      ;(pretty-print (wsparse-postprocess prog))  
      ;(ws prog)

      ;(regiment-verbosity 0) (putenv "REGIMENT_QUIET" "1")

      (printf " <WSQ>   Compiling generated WS program.\n")
      (if #f
	  ;; Execute through the "ws" scheme environment.
	  (begin
	    (browse-stream (wsint (wsparse-postprocess prog) '()))
	    )

	  ;; Compile through the wsc2 backend:
	  (begin
	    (parameterize ([compiler-invocation-mode 'wavescript-compiler-c])
	      (wscomp (wsparse-postprocess prog) '() 'wsc2))

	    (putenv "WS_LINK" (format "~a -DWS_REAL_TIMERS " (or (getenv "WS_LINK") "")))

	    ;(printf "SETTING LINK ~s\n" (getenv "WS_LINK"))
	    
	    ;; We go all the way back to our shell script to have it call gcc.
	    (system "wsc2-gcc")

	    (let ([end-time (current-time 'time-monotonic)])
	      (printf " <WSQ>   Finished compilation (~a seconds); back to control program...\n"
	        (+ (- (time-second end-time)
		      (time-second start-time))
		   (/ (- (time-nanosecond end-time)
			 (time-nanosecond start-time))
		      (* 1000.0 1000 1000)))))

	    (begin 
	      (printf "\n================================================================================\n")
	      ;; Kill old process:
	      (when current-child-process (kill-child current-child-process))
	      (let-values ([(to-stdin from-stdout from-stderr pid)
			  (open-process-ports "exec ./query.exe" 'block (make-transcoder (latin-1-codec)))
			  ;(open-process-ports "./query.exe &> /dev/stdout" 'block (make-transcoder (latin-1-codec)))
			  ])
	      (if current-child-process
		  (printf " <WSQ> Created replacement child process (pid ~a).\n" pid)
		  (printf " <WSQ> Started child process to execute query (pid ~a).\n" pid))
	      (set! current-child-process pid)
	      (printf "================================================================================\n\n")

	      ;; We need to return from the EndTransaction call back to the control program.
	      ;; I wish there were some good way to plug the from-stdout into console-output-port 
	      ;; without having a separate thread for the express purpose of echoing input.

	     ;; (unless (threaded?) (error 'WSQ "must be run with a threaded version of Chez Scheme."))
	      (fork-thread (echo-port from-stdout))
	      ))
	      
	    )))
    (set-box! curtransaction #f)
    ))

;; Creates a silly thread just for the purpose of relaying input to
;; output -- "plugging" an input port into an output one.
(define (echo-port port)
  (lambda ()
    ;(printf "##### STARTING THREAD FOR PRINTING... \n")
    (let loop ((x (get-string-some port)))
      (unless (eof-object? x)
	(display x)
	(loop (get-string-some port))))
    ;(printf "##### ENDING ECHO THREAD... \n")
	))

(define (kill-child pid)
  (define killcmd (format "kill -9 ~a" pid))
  ;; If it's a shell I thought a normal kill might let it kill recursively... but no.
  ;(define killcmd (format "kill  ~a" pid))

  ;; And in fact doing a normal kill will of course let the query
  ;; persist a little bit and overlap with the next executable.
  ;; Oh, wait... that happens with -9 too... darn.
  (system killcmd)

  (printf " <WSQ> Killing child process ~s\n" killcmd)
  ;(printf "  Processes after kill:\n") (system "ps aux | grep query")
)

(define (add-op! opid binding)
  (when (hashtable-contains? op_table opid) (error 'WSQ "op with node id ~s already exists!" opid))
  (hashtable-set! op_table opid binding)
  (ASSERT cur_ops) (set! cur_ops (cons opid cur_ops))
  ;;(hashtable-set! subgraph_table sym code)
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
    (printf " <WSQ>  WSQ_BeginSubgraph ~s \n" id)
    (when cursubgraph
       (error 'WSQ_BeginSubgraph "cannot begin subgraph ~s within another subgraph ~s!" id cursubgraph)
       ;(error 'WSQ "Subgraph with ID ~s already exists and has not been removed.\n" id)
       )
    (ASSERT (not (hashtable-contains? subgraph_table id)))
    (ASSERT not cur_ops)
    (set! cursubgraph id)
    (set! cur_ops '())
    (print-state)
    ))

(define-entrypoint WSQ_EndSubgraph () void
  (lambda ()
    (unless cursubgraph 
      (printf " <WSQ>  ERROR: WSQ_EndSubgraph called without a corresponding WSQ_BeginSubgraph.\n")
      (exit -1))
    (let ((id cursubgraph))
      (printf " <WSQ>  WSQ_EndSubgraph ~s \n" id)
      (print-state)
      ;; Currently subgraphs don't really do anything special.
      ;; But we do record the operators introduced by the subgraph so we can remove them later.
      (hashtable-set! subgraph_table id cur_ops)
      (set! cursubgraph #f)
      (set! cur_ops #f)
      )))

(define-entrypoint WSQ_RemSubgraph (int) void
  (lambda (id)
    (printf " <WSQ>  WSQ_RemSubgraph ~s \n" id)
    (unless (hashtable-contains? subgraph_table id)
      (error 'WSQ_RemSubgraph "Tried to remove subgraph which was not installed: id ~s" id))
    (hashtable-delete! subgraph_table id)
    ))

(define-entrypoint WSQ_EdgeType (int) scheme-object
  (lambda (id)
    (define type (format "type_~a" (random 1000)))
    (printf " <WSQ>  WSQ_EdgeType ~s : ~s \n" id type)
    ;   type
    (error 'WSQ_EdgeType "not implemented yet")
    ))

;; Adding operators.
;; ----------------------------------------

(define-entrypoint WSQ_AddReutersSource (int int string) void
  (lambda (opid outid schema-path)
    ;(printf " <WSQ>  WSQ_AddReutersSource ~s ~s \n" opid schema-path)
    (define code `(,(edge-sym outid) (app wsq_reuterSource ',schema-path)))
    (add-op! opid code) (add-out-edge! outid)
    (print-state)
    ))

(define-entrypoint WSQ_AddPrinter (int string int) void
  (lambda (opid str inid)
    ;(printf " <WSQ>  WSQ_AddPrinter ~s ~s \n" str id)
    (define code `(,mergemagic (app wsq_printer ,str ,(edge-sym inid))))
    (add-op! opid code) (add-in-edge! inid)
    (print-state)
    ))

(define-entrypoint WSQ_AddProject (int int int string) void
  (lambda (opid in out expr)
    ;(printf " <WSQ>  WSQ_AddProject ~s ~s ~s \n" in out expr)
    (define code `(,(edge-sym out) (app wsq_project ,(parse-project expr) ,(edge-sym in))))
    (add-op! opid code) (add-in-edge! in) (add-out-edge! out)
    (print-state)
    ))

(define-entrypoint WSQ_AddFilter (int int int string) void
  (lambda (opid in out expr)
    ;(printf " <WSQ>  WSQ_AddFilter ~s ~s ~s \n" in out expr)
    (define code `(,(edge-sym out) (app wsq_filter ,(parse-filter expr) ,(edge-sym in))))
    (add-op! opid code) (add-in-edge! in) (add-out-edge! out)
    (print-state)
    ))

(define-entrypoint WSQ_AddOp (int string string string string) void
  (lambda (id optype inputs outputs args)
    (define __ (printf " <WSQ>  WSQ_AddOp ~a ~s in: ~a out: ~a   '~a' \n" id optype inputs outputs args))
    (define in*  (string->slist inputs))
    (define out* (string->slist outputs))
    (define opsym (string->symbol optype))

    (define (kill-whitespace str)
      (list->string (filter (compose not char-whitespace?) (string->list str))))

    ;; Error handling:
    (define (has-inputs n)
      (ASSERT (format "~a should have exactly one input edge." opsym)  (curry = n) (length in*)))
    (define (has-outputs n)
      (ASSERT (format "~a should have exactly one output edge." opsym) (curry = n) (length out*)))
    (unless in*  (error 'WSQ_AddOp "Bad list of inputs: ~s"  inputs))
    (unless out* (error 'WSQ_AddOp "Bad list of outputs: ~s" outputs))

    (case opsym
      [(ReutersSource) (has-inputs 0) (has-outputs 1)  (WSQ_AddReutersSource id (car out*) args)]
      [(Printer)       (has-inputs 1) (has-outputs 0)  (WSQ_AddPrinter id args (car in*))]
      [(Filter)     (has-inputs 1) (has-outputs 1)  (WSQ_AddFilter id (car in*) (car out*) args)]
      [(Project)    (has-inputs 1) (has-outputs 1)  (WSQ_AddProject id (car in*) (car out*) args)]
      [(WindowJoin) (has-inputs 2) (has-outputs 1)
       ;; This is a horrible hack, to get both a FILTER expression,
       ;; and a number of seconds into this operator, we pack them
       ;; both in the same string.  We follow the arbitrary convention
       ;; that "|" serves as a divider for breaking up that string.
       (let-match ([(,_seconds ,filter) (string-split args #\|)])
        (define seconds (car (ASSERT (string->slist _seconds))))
	(WSQ_AddWindowJoin id (car in*) (cadr in*) (car out*) seconds filter))]

      [(ConnectRemoteIn)  (has-inputs 0) (has-outputs 1)
       ;; This one takes three arguments in the string 'args'
       (let-match ([(,_host ,_port ,fieldtypes) (string-split args #\|)])
        ;; Trim any extra whitespace.  No whitespace in hostnames.
        (define host (kill-whitespace _host))
        (define port (car (ASSERT (string->slist _port))))
	(WSQ_ConnectRemoteIn id (car out*) host port fieldtypes))]

      [(ConnectRemoteOut) (has-inputs 1) (has-outputs 0)     
       (let-match ([(,_host ,_port) (string-split args #\|)])
        (define host (kill-whitespace _host))
        (define port (car (ASSERT (string->slist _port))))
	(WSQ_ConnectRemoteOut id (car in*) host port))]

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
;; This will enable us to simulate 
(define-entrypoint WSQ_AddWindowJoin (int int int int single-float string) void
  (lambda (opid in1 in2 out seconds expr)
    ;(printf " <WSQ>  WSQ_AddWindowJoin ~s ~s ~s ~s ~s \n" in1 in2 out seconds expr)
    (define code
      `(,(edge-sym out) (app wsq_windowJoin
			     ,(parse-filter expr 'left 'right)
			     ,(make-merger (get-type-skeleton1)
					   (get-type-skeleton2))
			     ,(edge-sym in1)
			     ,(edge-sym in2)
			     ,seconds
			     )))
    (add-op! opid code)   (add-in-edge! in1) (add-in-edge! in2) (add-out-edge! out)
    (print-state)))


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
    (printf " <WSQ> Shutting down query engine.\n")

#;
    (begin 
      (printf "Subgraph table was:\n")
      (hashtable-for-each 
       (lambda (id ls)
	 (printf "  ~s: ~s\n" id ls))
       subgraph_table))
    ))


;;==============================================================================
