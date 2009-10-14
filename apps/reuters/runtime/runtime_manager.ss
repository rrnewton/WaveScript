

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

;; Subgraphs accumulated in the current transaction.
(define subgraphs (box '()))

;; Here we build up a list of bindings that will be used to build a WS program.
(define cursubgraph (box '()))

(define print-state void)
#;
(define (print-state)
  (printf "     Subgraphs:\n       ")
  (pretty-print (unbox subgraphs))
  (printf "     Current Subgraph:\n       ")
  (pretty-print cursubgraph))

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
	(map ununquote (string->slist str)))))

;; This assumes for now that it's a series of binary operators.
(define (parse-filter str) 
  (define arg 'rec)
  `(lambda (,arg)
     ,(match (string->slist str)
    [() #t]
    [(,left ,binop ,right . ,[rest])
     ;(ASSERT (memq binop '(== <= >=)))
     (define bop 
       (match binop
	 [== 'wsequal?]
	 [<= <=]
	 [>= >=]))
     (define expr
       `(,bop ,(temp-convert-rand arg (ununquote left)) 
	      ,(temp-convert-rand arg right)))
     (if (eq? rest #t) expr
	 `(ws:and ,expr ,rest))]
    )))

(define (capitalize s)
  (define str (symbol->string s))
  (string->symbol
   (string-append 
    (string-upcase (substring str 0 1))
    (substring str 1 (string-length str)))))

(define (Type t)
  (case t
    [(int float double)   (capitalize t)]
    [else (error 'Type "unhandled type: ~s" t)]))

(define (parse-types str)
  `(Record 
    ,(match (string->slist str)
       [() `',(unique-name "_rowvar")]
       #; ;; It's inexplicable that this one doesn't work:
       [(,[ununquote -> type] ,[ununquote -> name] . ,[rest])
	`(Row ,name ,type ,rest)]
       [(,type ,name . ,[rest])
	`(Row ,(ununquote name) ,(Type (ununquote type)) ,rest)]
       )))

(define (temp-convert-rand rec x)
  (cond    
    [(symbol? x) `(wsrecord-select ',x ,rec)]
    [(number? x) `',x]
    [else (error 'temp-convert-rand "Invalid operand: ~a" x)]
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
    (ASSERT "State must be clean at start of transaction" null? (unbox subgraphs))
    (ASSERT "State must be clean at start of transaction" null? (unbox cursubgraph))
    (set-box! curtransaction id)
    ))

(define-entrypoint WSQ_EndTransaction () void
  (lambda ()    
    (printf " <WSQ>  WSQ_EndTransaction \n")
    (ASSERT "Must be in transaction to call EndTransaction." id (unbox curtransaction))

    ;; Ok, this is where we need to actually commit a change.

    ;; FIXME: need to do a topological sort.
    (let* ([binds (reverse (unbox cursubgraph))]
	   [empties '()]
	   [bod (map (match-lambda ((,v ,e)) 
		       (let ([var (if (eq? v mergemagic)
				      (let ((new (unique-name "ignored")))
					(set! empties (cons new empties ))
					new)
				      v)])			      
			 `(define ,var ,e)))
		  binds)]
	   [prog `((include "wsqlib.ws")
		   ,@bod
		   (define main 
		     (assert-type (Stream #())
				  ,(match empties
				     [() '(app timer '0)]
				     [(,vr) vr]
				     [(,vr . ,[rest])
				      `(app merge ,vr ,rest)]))))])
      (printf "\n >>> ASSEMBLED PROG: \n\n") 
      (pretty-print prog)
      ;(pretty-print (wsparse-postprocess prog))  
      ;(ws prog)
      (printf "\n\n >>> COMPILING PROG: \n\n")
      (browse-stream
       (wsint (wsparse-postprocess prog) '()))
      ;(ws (wsparse-postprocess prog))
      )
    
    (set-box! subgraphs '())
    (set-box! cursubgraph '())
    (set-box! curtransaction #f)
    ))

;; Subgraphs.

(define-entrypoint WSQ_BeginSubgraph (int) void 
  (lambda (id)
    (printf " <WSQ>  WSQ_BeginSubgraph ~s \n" id)
    (set-box! subgraphs (cons id (unbox subgraphs)))
    (print-state)
    ))

(define-entrypoint WSQ_EndSubgraph () void
  (lambda ()
    (printf " <WSQ>  WSQ_EndSubgraph \n")
    (print-state)
    ))

(define-entrypoint WSQ_RemSubgraph (int) void
  (lambda (id)
    (printf " <WSQ>  WSQ_RemSubgraph ~s \n" id)
    ))

(define-entrypoint WSQ_EdgeType (int) scheme-object
  (lambda (id)
    (define type (format "type_~a" (random 1000)))
    (printf " <WSQ>  WSQ_EdgeType ~s : ~s \n" id type)
    type
    ))

;; Adding operators.

(define-entrypoint WSQ_AddReutersSource (int string) void
  (lambda (id path)
    (printf " <WSQ>  WSQ_AddReutersSource ~s ~s \n" id path)
    (let ([sym (edge-sym id)])
      (set-box! cursubgraph (cons `(,sym (app wsq_reuterSource ',path))
				  (unbox cursubgraph))))
    (print-state)
    ))

(define-entrypoint WSQ_AddPrinter (int) void
  (lambda (id)
    (printf " <WSQ>  WSQ_AddPrinter ~s \n" id)
    (let ([sym (edge-sym id)])
      (set-box! cursubgraph (cons `(,mergemagic (app wsq_printer ,(edge-sym id)))
				  (unbox cursubgraph))))
    (print-state)
    ))

(define-entrypoint WSQ_AddProject (int int string) void
  (lambda (in out expr)
    (printf " <WSQ>  WSQ_AddProject ~s ~s ~s \n" in out expr)
    (set-box! cursubgraph 
	      (cons `(,(edge-sym out) (app wsq_project ,(parse-project expr) 
						   ,(edge-sym in)))
		    (unbox cursubgraph)))
    (print-state)
    ))

(define-entrypoint WSQ_AddFilter (int int string) void
  (lambda (in out expr)
    (printf " <WSQ>  WSQ_AddFilter ~s ~s ~s \n" in out expr)
    (set-box! cursubgraph 
	      (cons `(,(edge-sym out) (app wsq_filter ,(parse-filter expr) 
						  ,(edge-sym in)))
		    (unbox cursubgraph)))
    (print-state)
    ))

;; Connecting to remote machines.

(define-entrypoint WSQ_ConnectRemoteIn (int string int string) void
  (lambda (id host port field-types)
    (printf " <WSQ>  WSQ_ConnectRemoteIn ~s ~s ~s ~s \n" id host port field-types)
    (set-box! cursubgraph 
	      (cons `(,(edge-sym id) 
		      (assert-type (Stream ,(parse-types field-types))
				   (app wsq_connect_in ,host ,port)))
		    (unbox cursubgraph)))
    ))

(define-entrypoint WSQ_ConnectRemoteOut (int string int) void
  (lambda (id host port)
    (printf " <WSQ>  WSQ_ConnectRemoteOut ~s ~s ~s \n" id host port)
    (set-box! cursubgraph 
	      (cons `(,mergemagic (app wsq_connect_out ,host ,port ,(edge-sym id)))
		    (unbox cursubgraph)))
    ))


;;==============================================================================
