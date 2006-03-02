



(chez:module regiment_helpers
    (

	  reg:random-int reg:random-real reg:get-random-state reg:set-random-state!
	  randomize-list randomize-vector list-get-random

	  sense-spatial-sine-wave
	  sense-dist-from-origin 
	  sense-sine-wave
	  sense-noisy-rising
	  sense-random-1to100
	  ;sense-fast-sine-wave

	  unique-name unique-name-counter extract-suffix make-begin 
	  deep-reg:struct->list 
	  strip-illegal deunique-name  reunique-names
	  get-formals 


	  ;; Hmm, not sure what meaning immediate has here...
	  ;immediate? 
	  constant? datum? qinteger? qinteger->integer
	  formalexp? cast-formals fit-formals-to-args


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

  
  (include "generic/regiment_helpers.ss")

  )
