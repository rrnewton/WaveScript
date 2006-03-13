

(module hm_type_inference mzscheme
  (require "iu-match.ss"
           (lib "include.ss")
           "plt_constants.ss"
           ;"hashtab.ss"
           (all-except "helpers.ss" test-this these-tests)
	   (all-except "regiment_helpers.ss" test-this these-tests 
                       regiment-type-aliases
                       regiment-basic-primitives
                       local-node-primitives
                       regiment-constants
                       regiment-distributed-primitives
                       )
           "prim_defs.ss"
           )

  (provide 
;           regiment-type-aliases
;	   regiment-basic-primitives
;	   local-node-primitives
;	   regiment-constants
;	   regiment-distributed-primitives

	   make-tvar-generator
	   make-tvar
	   reset-tvar-generator

	   make-tcell
	   tcell->name

	   empty-tenv
	   tenv?
	   tenv-lookup
	   tenv-is-let-bound?
	   tenv-extend
	   tenv-map
	   
           instantiate-type
	   export-type
	   prim->type
	   type?
	   ;id
	   ;inject-polymorphism
	   
           type-expression
	   recover-type
	   type-const
	   type-lambda
	   type-app
	   type-letrec
           
	   annotate-expression
	   annotate-program

;	   types-equal!
;	   tvar-equal-type!
;	   no-occurrence!
;	   safe-export-type
;	   raise-type-error
;	   raise-occurrence-check
;	   raise-wrong-number-of-arguments

           
	   these-tests
	   test-this
	   test-inferencer
	   )

  (include (build-path "generic" "hm_type_inference.ss"))
  )
