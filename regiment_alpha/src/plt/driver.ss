
;; This should depend on 'pass-list' being defined, but I don't know
;; how to do this correctly.

;; I've ended up giving up on the Mzscheme module compilation system
;; for this file just treating it as a normal scheme script depending
;; on and modifying the toplevel symbol table.

;(module driver mzscheme  
(require (lib "include.ss")  
	 (lib "pretty.ss")
         ;           "iu-match.ss"
         "helpers.ss"
         ;;	   pass-list)
         
;         pass-list
	   )

  (define error-handler error-display-handler)

  (include (build-path "generic" "driver.ss"))
    
;  (provide test-one test-all
;;; These are in the interface, but aren't used yet for this project:
;	   tracer game-eval analyze-all test-all-but 
;;	   print-file remaining-pass-list
;	   )
;  )

