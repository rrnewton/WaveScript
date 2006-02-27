

(module prim_defs mzscheme
  (require "iu-match.ss"
           (lib "include.ss")
           ;"constants.ss"
           ;"hashtab.ss"
           ;(all-except "helpers.ss" test-this these-tests)
	   ;(all-except "regiment_helpers.ss" test-this these-tests 
	   ;regiment-constants raise-type-error)
           ;"engine.ss"
           )

  (provide 
           regiment-type-aliases
	   regiment-basic-primitives
	   local-node-primitives
	   regiment-constants
	   regiment-distributed-primitives
	   ;regiment-primitives
	   ;token-machine-primitives
	   
; 	   token-machine-keyword?
; 	   get-primitive-entry
; 	   map-prim-w-types
; 	   regiment-primitive?
; 	   regiment-constant?
; 	   basic-primitive?
; 	   distributed-primitive?
; 	   token-machine-primitive?

	   )

  (include (build-path "generic" "prim_defs.ss"))
  )
