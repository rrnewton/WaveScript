
;; This just encapsulates common imports for pass modules:

(module common mzscheme
; (require-regiment "plt/iu-match.ss")
 (require (lib "include.ss")
	  ;(only "../generic/constants.ss" chezimports ASSERT)
          "iu-match.ss"
          "../generic/constants.ss"
;          (file "$REGIMENTD/src/plt/iu-match.ss")
	  "chez_compat.ss"
	  "../generic/util/helpers.ss"
          "../generic/compiler_components/prim_defs.ss"
          "../generic/compiler_components/regiment_helpers.ss"
          "../generic/compiler_components/type_environments.ss"
          "../generic/compiler_components/hm_type_inference.ss"
          "../generic/compiler_components/reg_core_generic_traverse.ss"
	  "../generic/grammars/grammar_checker.ss"
          "../generic/passes/pass-mechanism_basic.ss"                 
          "../generic/passes/pass-mechanism.ss"
          )
  (require-for-syntax "../generic/passes/pass-mechanism.ss")
  
  (provide (all-from (lib "include.ss"))
           (all-from "../generic/constants.ss")
           (all-from "iu-match.ss")
           (all-from "chez_compat.ss")
           (all-from "../generic/util/helpers.ss")
           (all-from "../generic/compiler_components/prim_defs.ss")
           (all-from "../generic/compiler_components/regiment_helpers.ss")
	   (all-from "../generic/compiler_components/type_environments.ss")
           (all-from "../generic/compiler_components/hm_type_inference.ss")
           (all-from "../generic/compiler_components/reg_core_generic_traverse.ss" )
           (all-from "../generic/grammars/grammar_checker.ss")
           (all-from "../generic/passes/pass-mechanism_basic.ss")
           (all-from "../generic/passes/pass-mechanism.ss"))
  )
