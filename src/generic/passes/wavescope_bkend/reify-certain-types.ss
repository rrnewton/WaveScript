
;; [2007.07.05] Adapting this for use by ws.early.  It needs to accept
;; pre-static elaborate input.  Thus it must be versatile enough to be
;; called near the front or near the end of the compiler.

(module reify-certain-types mzscheme 
  (require  "../../../plt/common.ss" 
	    ;"../normalize_query/ws-lift-let.ss"
            )
  (provide reify-certain-types
;	   reify-certain-types-grammar
	   )
  (chezimports)

#;
;; UNFINISHED, should reflect constraints on annotated prims:
  (define reify-certain-types-grammar
    (append `(
;	      (Const ComplexConst)
	      (ComplexConst ('__foreign Const Const Const ComplexDatum)) 	      
	      )
	    type-annotate-misc-grammar))
  
  ;; [2007.07.05]
  ;; This pass reifies the types provided to certain input sources (dataFile, readFile).
  ;; This is for the Scheme backend, which needs to have a representation of the types.
  ;; This is factored it out type-annotate-misc.
  (define-pass reify-certain-types
      (define process-expr
	   (lambda (x fallthru)
	 (match x
                      
	   ;; Need to see through any src-pos annotations that are in the way:
	   ;; (This strips them in the process.)
	   [(assert-type ,t (src-pos ,_ ,[e]))
	    (process-expr `(assert-type ,t ,e) fallthru)]
	   ;; Like the src-pos case above, this is a little snip of what static-elaborate does:
	   [(app ,prim ,_ ...) (guard (regiment-primitive? prim))
	    (process-expr (cons prim _) fallthru)]

	   ;; This needs to explicitly pass the types as argument to run with wsint.
	   [(assert-type (Stream ,t) (dataFile ,[f] ,[m] ,[rate] ,[rep]))
	    (let ([types (match t [#(,t* ...)  t*]  [,t   	(list t)])])
	      `(__readFile ,f ,m ,rep ,rate '0 '0 '0 ',types)
	      )]
	   
	   ;; This needs the type tagged on also:
	   [(assert-type ,T (foreign ,[name] ,[files] ,[pointertypes]))
	    `(__foreign ,name ,files ,pointertypes ',T)]
	   
	   ;; This parses the option string to readFile.
	   [(assert-type (Stream ,t) (readFile ,[fn] ',str))
	    ;; From regiment_helpers.ss
            (parse-readFile-modestring str t fn)
            ]
           
           ;; Here's a hack for ws.early:
           ;; We don't necessarily have the string available, but we can still throw
           [(assert-type (Stream ,t) (readFile ,[fn] ,[str]))
            `(readFile-wsearly ,fn ,str ',t)]

#;
           [(assert-type ,_ (readFile ,[fn] ,[str]))
	    (inspect 'HMMM)]

#;
	   ;; Safety net:
	   [(,op ,_ ...) (guard (memq op '(foreign readFile dataFile)))
	    (error 'reify-certain-types "compiler error, missed operator: ~s" op)]
	   
	   
	   [,oth (fallthru oth)]

	   )))
    
    [Expr process-expr]
    )

) ;; End module