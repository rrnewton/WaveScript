
;;;; .title Data Representation Transforms
;;;; .author Ryan Newton

;;;; [2007.10.20] UNFINISHED, EXPERIMENTAL, UNDER DEVELOPMENT:


;; In a limited capacity we will automatically derive the mapping
;; between types.
(define (derive-type-mapping t1 t2)

  99
  )

;(define supported-fixedsize-constructors '(tuple))
;(define supported-varsize-constructors   '(array))

;; What concrete syntax should I use for this??
;; Maybe ::>  instead of :: ??
;;  (s ::>  Stream (Array (Int * Bool)))
;; Or just "coerce" with a required annotation:
;;  coerce(s) :: Stream (Array (Int * Bool)))

;; Build an expression that implements the conversion.
(define (build-conversion t1 t2)
  (lambda (inputexpr)
    ;; Walk down both types and the expression, reconstructing where
    ;; there's no change, and converting where necessary.
    (let loop ([xp inputexpr] [t1 t1] [t2 t2])
      (ASSERT symbol? xp)
      (match (vector t1 t2)

	;; This is a case where we can optimize:
	;; We currently only do a SINGLE swap... no swaps deeper inside the type.	
	[#((Array #(,elt1* ...)) #((Array ,elt2*) ...))
	 (ASSERT (equal? elt1* elt2*))
	 (let ([ind (unique-name 'i)])
	   `(Array:build (Array:length ,xp)
			 (lambda (,ind) (Int)
				 )))]

	;; How can we best express an Array-Array row/column swap???
	
	[#((List ,elt1) (List ,elt2))
	 (let ([vr (unique-name 'elm)])
	   `(List:map (lambda (,vr) (,elt1)
			      ,((build-conversion elt1 elt2) vr))
		      ,xp))]
	[#(,t1 ,t2) (guard (scalar-type? t1) (scalar-type? t2))
	 (if (eq? t1 t2) xp
	     (error 'build-conversion "scalar types do not match: ~s ~s" t1 t2))]

	))))

