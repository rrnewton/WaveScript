#cs ;; Case Sensitivity

(module regiment_helpers mzscheme 
  (require "iu-match.ss"
           (lib "include.ss")
           (lib "date.ss")
           (lib "pretty.ss")
           ;(lib "process.ss")
;	   (lib "compat.ss")
           (all-except (lib "list.ss") filter)
           "constants.ss"
           "chez_compat.ss"
           "hashtab.ss"
           (all-except "helpers.ss" test-this these-tests)
           "engine.ss"
           "prim_defs.ss"
           (prefix swindle: (lib "misc.ss" "swindle"))
           )

 (provide     	

   reg:random-int reg:random-real reg:get-random-state reg:set-random-state!

   get-formals
   reunique-names deunique-name unique-name unique-name-counter extract-suffix make-begin 
   deep-reg:struct->list 
   strip-illegal
   randomize-list randomize-vector! list-get-random

   ;; Hmm, not sure what meaning immediate has here...
   ;immediate? 
   constant? datum? qinteger? qinteger->integer
   formalexp? cast-formals fit-formals-to-args

;   regiment-primitives regiment-primitive? 
;;   token-machine-primitives token-machine-primitive? 
;   token-machine? token-machine->program token-machine-keyword?
;   basic-primitive? distributed-primitive?
;   get-primitive-entry regiment-constants regiment-constant? ;get-primitive-arity
;   get-primitive-return-type
;   map-prim-w-types
     
   ;; Token names:
   token-name? new-token-name token-names get-names get-formation-name get-membership-name
   token->name token->subtok
   destructure-tokbind handler->tokname handler->formals handler->body handler->subtokid handler->stored

   regiment-print-params

   sense-spatial-sine-wave 
   sense-dist-from-origin 
   sense-sine-wave
   sense-noisy-rising
   sense-random-1to100
   )

;; This is a simple random number generator interface for use in this Regiment codebase:
;;
;; TODO: Seperate the reg random number generator from the generic PLT
;; one.  It needs to be sandboxed.  (However, it shouldn't matter,
;; because we're not really running any other computations concurrent
;; with the simulator.)
(define reg:random-int
  (case-lambda
   [() (reg:random-int (- (expt 2 31) 1))]
   [(k) (random k)]))
(define reg:random-real
  (case-lambda
    [() (random)]
    [(k) (* k (random))]))
  
(define (reg:get-random-state)
  (pseudo-random-generator->vector (current-pseudo-random-generator)))
(define (reg:set-random-state! s)
  (current-pseudo-random-generator (vector->pseudo-random-generator s)))  
  
; =======================================================================  

  (include (build-path "generic" "regiment_helpers.ss"))

; =======================================================================

)
