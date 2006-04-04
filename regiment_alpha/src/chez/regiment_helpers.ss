



(chez:module regiment_helpers
    (

	  reg:random-int reg:random-real reg:get-random-state reg:set-random-state!
	  randomize-list randomize-vector! list-get-random

	  sense-spatial-sine-wave
	  sense-dist-from-origin 
	  sense-sine-wave
	  sense-noisy-rising
	  sense-random-1to100
	  ;sense-fast-sine-wave

	  unique-name unique-name-counter extract-suffix make-begin 
	  deep-reg:struct->list deep-reg:struct->list2 reg:very-deep-map
	  strip-illegal deunique-name  reunique-names
	  get-formals 


	  ;; Hmm, not sure what meaning immediate has here...
	  ;immediate? 
	  constant? datum? qinteger? qinteger->integer
	  formalexp? cast-formals fit-formals-to-args
	  simple-expr?

;	  token-machine-primitives token-machine-primitive? 
;	  token-machine? token-machine->program token-machine-keyword?
;	  basic-primitive? distributed-primitive?
;	  get-primitive-entry regiment-constants regiment-constant? ;get-primitive-arity
;	  get-primitive-return-type
;	  map-prim-w-types

	  ;; Token names:
	  token-name? new-token-name token-names get-names get-formation-name get-membership-name
	  token->name token->subtok
	  destructure-tokbind handler->tokname handler->formals handler->body handler->subtokid handler->stored

	  regiment-print-params

	  test-regiment_helpers

	  )

  (import prim_defs)
  
  ;;; Regiment Random Number Interface. <br>
  ;;;
  ;;;   These provide a simple random number generator interface for use
  ;;; within the Regiment codebase. <br>
  ;;;    The simulator should only use this RNG interface to maintain
  ;;; determinism.  (Currently this just uses the primitive Chez
  ;;; Scheme RNG, so there is no proper seperation which would be
  ;;; necessary for other concurrently running code to not ruin the
  ;;; simulators determininms.)

  ;;; TODO: FIXME: [2006.03.01]
  ;;; I should overhall this.  Really, what I want is a separate RNG for simulator-alpha.
  ;;; And then I'd like another seperate RNG for the sensor data-generator.

  ;; NOTE: This is currently unisolated anyways.  So it's just a
  ;; common interface into Chez/PLT's RNG's.  Need to go further than that.

  ;; A random integer. 
  (define reg:random-int
    (case-lambda 
      [() (#%random (#%most-positive-fixnum))]
      [(k) (#%random k)]))

  ;; A random real number.
  (define reg:random-real
    (case-lambda
      [() (#%random 1.0)]
      [(n) (#%random n)]))
  
  ;; Get the state of the RNG.
  (define (reg:get-random-state) (random-seed)) ;; This doesn't work!!! [2005.10.05]
  
  ;; Set the state of the RNG.
  (define (reg:set-random-state! s) (random-seed s))

  
  (include "../generic/regiment_helpers.ss")



     
;; [2006.03.01] UNFINISHED
(define (reg:very-deep-map f x)
  (let ([touched (make-default-hash-table)]) 
      (let loop ((x x))
	;(printf "MAPPING DOWN: ~a\n" (reg:struct? x))
	(cond
	 ;; Function gets applied for each instance of a repeated object:
	 [(hashtab-get touched x) => (lambda (v) ;(printf "HIT REPEAT\n")
					     (f v))]
	 [(pair? x) 
	  ;; Add the original cell to the touched list.
	  (let ([newcell (cons #f #f)])
	    ;; Associate the old cell with the new cell.
	    (hashtab-set! touched x newcell)
	    (set-car! newcell (loop (car x)))
	    (set-cdr! newcell (loop (cdr x)))
	    (f newcell))]
	 [(vector? x) 
	  (let ([newvec (make-vector (vector-length x) #f)])
	    (hashtab-set! touched x newvec)
	    (for i = 0 to (sub1 (vector-length x))
		 (vector-set! newvec i (loop (vector-ref x i))))
	    (f newvec))]
	 [(reg:struct? x)
	  ;; Copy the structure:
	  (let* ([lst (reg:struct->list x)]
		 [type (record-type-descriptor x)]
		 [new (reg:list->struct x (cdr lst))])
	    (hashtab-set! touched x new)
	    ;; CHEZ SPECIFIC.
	    ;; CURRENT BEHAVIOR IS THAT IMMUTABLE FIELDS ARE OPAQUE TO THIS MAP FUNCTION:
	    (for-each (lambda (decls val)
			;(printf "DECLS & val: ~s ~s\n" decls val)
			(unless (memq 'immutable decls)
			  ((record-field-mutator type (caddr decls))
			   new (loop val))))
	      (record-type-field-decls type)
	      (cdr lst))
	    (f new))]
	 [(atom? x) (f x)]
	 [else (error 'reg:very-deep-map "what on earth is this object? ~a" x)]))))

; (reg:very-deep-map (\\ x (if (integer? x) (add1 x) x)) 
;                       (let ((x '#0=(1 2 #(3 #0#)))) (set-car! x (make-node 99 99)) x))

;  (deep-reg:struct->list2 (make-node 1 2))
;  (deep-reg:struct->list2 (make-node 1 (make-node 3 4)))

;; THIS DOESN'T WORK RIGHT NOW:
;  (deep-reg:struct->list2 (let ((x (make-node 1 (make-node 3 4)))) (set-node-id! x x) x))

;; DOESN'T WORK YET:
(define (deep-reg:struct->list2 ob)
  (reg:very-deep-map
   (let ([touched (make-default-hash-table)])
     (lambda (x)
       (cond
	[(not (reg:struct? x)) x]
	[(hashtab-get touched x) => (lambda (v) v)]
	[else (let ([ret (reg:struct->list x)])
		(hashtab-set! touched x ret)
		ret)]
	)))
   ob))


;; UNFINISHED:
;; This is like generic-traverse, but for any list/vector/record data structure.
;; TODO: How to handle cyclic structures.
(define (reg:very-deep-traverse driver fuser x)
  (let loop ((e x))
    (driver e 
	    (lambda (x)
	      (cond
	       [(atom? x) (fuser x () (lambda () x))]
	       [(pair? x) (fuser x (list (loop (car x)) (loop (cdr x)))
				 (lambda (a d) (cons a d)))]
	       [(vector? x) (fuser x (map loop (vector->list x))
				   (lambda args (list->vector args)))]
	       [(reg:struct? x)
		(fuser x (cdr (reg:struct->list x))
		       (lambda args (reg:list->struct x args)))]
	       [else
		(error 'reg:very-deep-traverse "unknown object: ~s" otherwise)])))))



  )
