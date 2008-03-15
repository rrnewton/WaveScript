
;;;; Pass: Reify Certain Types
;;;; This pass passes types as explicit arguments to those primitives that need them.

;; [2007.07.05] Adapting this for use by ws.early.  It needs to accept
;; pre-static elaborate input.  Thus it must be versatile enough to be
;; called near the front or near the end of the compiler.

;; Note!  This requires stripping out src-pos info, as it breaks some
;; of the pattern matching below.

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
	      ;; Foreign_source?
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
	   
	   ;; This needs the type tagged on also:
	   ;; This is where we introduce __foreign_source and __foreign:
	   [(assert-type ,T (,frgn ,[name] ,[files]))
	    (guard (memq frgn '(foreign foreign_source)))
	    `(,(symbol-append '__ frgn) ,name ,files ',T)]
	   ;; Safety net:
	   [(,frgn . ,_) (guard (memq frgn '(foreign foreign_source)))
	    (error 'reify-certain-types "missed ~s construct, must not have type annotation: ~s" frgn (cons frgn _))]
	   
	   ;; This parses the option string to readFile.
	   [(assert-type (Stream ,t) (readFile ,annot ,[fn] ',str ,[src]))
	    ;; From regiment_helpers.ss
            (parse-readFile-modestring annot str t fn src)]
           
           ;; Here's a hack for ws.early:
           ;; We don't necessarily have the string available, but we can still throw in the types.
           [(assert-type (Stream ,t) (readFile ,annot ,[fn] ,[str] ,[src]))
            `(readFile-wsearly ,annot ,fn ,str ,src ',t)]

#;
           [(assert-type ,_ (readFile ,[fn] ,[str]))
	    (inspect 'HMMM)]

	   ;; Safety net:
	   [(,op ,_ ...) (guard (memq op '(foreign readFile)))
	    (error 'reify-certain-types "compiler error, missed operator: ~s" op)]
	   
	   
	   [,oth (fallthru oth)]

	   )))
    
    [Expr process-expr]
    )

) ;; End module