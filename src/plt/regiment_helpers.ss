#cs ;; Case Sensitivity

(module helpers mzscheme 
  (require "iu-match.ss"
           (lib "include.ss")
           (lib "date.ss")
           (lib "pretty.ss")
           (lib "process.ss")
	   (lib "compat.ss")
           (all-except (lib "list.ss") filter)
           "constants.ss"
           "hashtab.ss"
           (prefix swindle: (lib "misc.ss" "swindle"))
           )

 (provide     	


   get-formals
   reunique-names deunique-name unique-name unique-name-counter extract-suffix make-begin strip-illegal

   ;; Hmm, not sure what meaning immediate has here...
   ;immediate? 
   constant? datum? 
   formalexp? cast-formals fit-formals-to-args

   regiment-primitives regiment-primitive? 
   token-machine-primitives token-machine-primitive? 
   token-machine? token-machine->program token-machine-keyword?
   basic-primitive? distributed-primitive?
   get-primitive-entry regiment-constants regiment-constant? ;get-primitive-arity
   get-primitive-return-type
   map-prim-w-types
   

   ;; Token names:
   token-name? new-token-name token-names get-names get-formation-name get-membership-name
   token->name token->subtok
   destructure-tokbind handler->tokname handler->formals handler->body handler->subtokid handler->stored

   ) ;; End provide

  
; =======================================================================  

  (include (build-path "generic" "helpers.ss"))

; =======================================================================

)
