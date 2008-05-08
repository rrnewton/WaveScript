#!r6rs

;;;; Encode tagged variant types (sums) as disjoint tuple types.  They
;;;; are then contained with in a parent tuple type using a C "union"
;;;; as well as a tag.
;;;;
;;;; .author Ryan Newton

;; Introduces "cast-variant-to-parent" form.
;; Modifies wscase form.
;; Introduces "Union" type construct to distinguish from the previous "Sum".

;============================================================

(library (ws passes wavescope_bkend convert-sums-to-tuples)
  (export convert-sums-to-tuples tag-type)
  (import (rnrs) (ws common))

  (define tag-type 'Int)  
  (define sum-decls    'uninitialized-sum-decls)

  ;;;; NOTE!!! NOT HANDLING SUMS WITH TYPE ARGUMENTS YET!
;  (define sum-sizes    'uninitialized-sum-sizes)
  (define sum-variants 'uninitialized-sum-variants)
  
  (define (lookup-tag sumty TC)
    (list-find-position TC
     (cdr (ASSERT (assq (cadr sumty) sum-variants)))))

#;
  (define (lookup-sumdef name)
    (trace-let loop ([ls sum-decls])
      (ASSERT (not (null? ls)))
      (if (eq? (caaar ls) name)
	  (car ls)
	  (loop (cdr ls)))))

  ;; Todo, this should be handled by type->width!!
  #;
  (trace-define (sizeof-sum sum)
999
#;
   (let ([variants (cdr sum)])
;      (inspect variants)
      (apply max (map type->width (apply append (map cdr variants)))))
   )
  
  ;; Introducing a "Union" type constructor understood by the C++ backend.
  (define (parent-struct sumname)
    ;`#(,tag-type (Union ,@(cdr (lookup-sumdef sumname))))
    `(Union ,sumname)
    )

  (define (make-tuple-term . args)
    (cond
     [(null? args) 'UNIT]
     [(null? (cdr args)) (car args)]
     [else (cons 'tuple args)]))
  (define (make-tuple-type . args)
    (cond
     [(null? args) '#()]
     [(null? (cdr args)) (car args)]
     [else (list->vector args)]))


  ; (define (pad-tuple size tupty) ...)

  (define Expr
    (lambda (x tenv fallthru)
      (match x
	
	[(construct-data ,TC ,[arg*] ...)
	 ;; [2007.05.28] Because these are value types, we don't
	 ;; CURRENTLY need to allocate extra space to bring it up
	 ;; to the size of the largest variant:
	 (let ([ty (match (peel-outer-typevars (tenv-lookup tenv TC))
		     [(,_ ... -> ,sumty) sumty])])
	   ;; For the program to typecheck in C we need to cast it to the shared sum-type.
	   ;; This is a hack that's understood by the C++ generator:
	   `(cast-variant-to-parent ,TC ,(parent-struct (cadr ty))
	     (assert-type 
	      ,(apply make-tuple-type ;,tag-type 
		      (map (lambda (x) (recover-type x tenv)) arg*))
	      ,(apply make-tuple-term ;',(lookup-tag ty TC) 
		      arg*)
	      )))]

	;; TODO: Handle no cases or just default!
	;[(wscase )]

	[(wscase ,xx (,TC* (lambda ,v** ,ty** ,bod*)) ...)
	 (let* ([newbod* (map (lambda (v* ty* bod) (Expr bod (tenv-extend tenv v* ty*) fallthru))
			   v** ty** bod*)]
		[rhs*
		(map (lambda (v* ty* bod)
		       (let* ([formal (unique-name 'pattmp)]
			      [len    (fx+ 1 (length v*))])
			 `(lambda (,formal) 
			    (,(apply make-tuple-type ty*))
			    
			    ,(if (fx= 1 (length ty*))
				 `(let ([,(car v*) ,(car ty*) ,formal]) ,bod)
				 (let loop ([i 0] [v* v*] [ty* ty*])
				   (if (null? v*) bod
				       `(let ([,(car v*) ,(car ty*) (tupref  ,i ,len ,formal)])
					  ,(loop (fx+ 1 i) (cdr v*) (cdr ty*))))))
			    )))
		  v** ty** newbod*)]
		;[sumty (recover-type x tenv)] ;; Should be a simple expression.
	       [sumty (match (peel-outer-typevars (tenv-lookup tenv (car TC*)) )
			[(,_ ... -> ,sumty) sumty])]
	       [tag* (map (lambda (tc) (lookup-tag sumty tc)) TC*)])
	   `(wscase ,(Expr xx tenv fallthru) ,@(map list (map cons tag* TC*) rhs*)))]
	[(wscase . ,_) (error 'convert-sums-to-tuples "bad wscase: ~s" `(wscase ,@_))]
	
	[(assert-type ,[Type -> t] ,[e]) `(assert-type ,t ,e)]

	[,oth (fallthru oth tenv)])))

  (define (Type ty)
    (let l ((ty ty))
      (match ty
	[,s (guard (symbol? s))                    ty]
	[(,qt ,v) (guard (memq qt '(quote NUM)))   ty]
	[#()                                       ty]
	[,s (guard (string? s))                    ty]
	[(Sum ,TC)                                 (parent-struct TC)]
	[(,[l -> arg] ... -> ,[l -> ret])         `(,@arg -> ,ret)]
	[(,C ,[l -> t] ...) (guard (symbol? C))   `(,C ,@t)]
	[#(,[l -> t*] ...)                        `#(,@t*)]
	[,else (error 'nominalize-types:convert-type "unmatched type: ~s" else)])))

  (define Bind
    (lambda (vars types exprs reconstr exprfun) 
      (reconstr vars 
		(map Type types)
		(map exprfun exprs))))

  (define Prog
    (lambda (prog Expr)
      (match prog
	[(,lang '(program ,body ,meta* ... ,toptype)) 
	 (fluid-let ([sum-decls (cdr (or (assq 'union-types meta*) '(union-types)))])
	   (fluid-let ([sum-variants
			(map (lambda (entry) ;; No type args at this point.
			       (cons (caar entry) (map car (cdr entry))))
			  sum-decls)]
		       #;
		       [sum-sizes (map (lambda (x) (list (caar x) (sizeof-sum x)))
				    sum-decls)])	     
	     ;; Note: because this is split into two passes, the toptype is run thorugh twice:
	     `(,lang '(program ,(Expr body (grab-init-tenv meta*))
			,@meta*
			,toptype))))])))


;; ============================================================
;;; Mini-pass #1: Convert expressions

;;; INEFFECIENT!
;;;
;;; These could run together but it's a limitation of define-pass...
;;; (Can't use 'Bindings' and 'Expr/Types')

(define-pass convert-expressions
  [Expr/Types Expr]
  [Program    Prog])

;; ============================================================
;;; Mini-pass #2: Convert types

(define-pass convert-types
  [Bindings Bind]
  [Program  Prog])

;; ============================================================
;;; Mini-pass #3: Convert top level sum type declarations

(define-pass convert-sumdecls
  [Program (lambda (prog Expr)
	     (match prog
	       [(,lang '(program ,body ,meta* ... ,toptype))
		(let* ([newdecls 
			(map (lambda (entry) ;; No type args at this point.
			       (match entry
				 [((,tyname) (,tag* ,[Type -> ty**] ...) ...)
				  (cons (list tyname) (map cons tag* ty**))
				  ]))
			  (cdr (or (assq 'union-types meta*) '(union-types))))])
		  `(,lang '(program ,body
			     (union-types . ,newdecls)
			     ,@(remq (assq 'union-types meta*) meta*)
			     ,(Type toptype))))])
	     )])

;; ============================================================

(define convert-sums-to-tuples 
  (compose convert-sumdecls 	   
	   convert-expressions
	   convert-types
	   ))


) ; End module
