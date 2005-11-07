#cs ;; Case Sensitiviy
(module simulator_alpha mzscheme
  (require 
   (lib "compat.ss")
   (lib "include.ss")
   (lib "pretty.ss")
   (prefix srfi1. (lib "1.ss" "srfi")) ; make-list
   "iu-match.ss"           
      
   "hashtab.ss"
   (all-except "constants.ss" test-this these-tests)
   (all-except "helpers.ss" id flush-output-port test-this these-tests)
   (all-except "tsort.ss" test-this these-tests)
   (all-except "simulator_alpha_datatypes.ss")
   ;(all-except "alpha_lib_scheduler_simple.ss")
   ;(all-except "pass21_cleanup-token-machine.ss" test-this these-tests)
   )

  ;(require (planet "copy-struct.ss" ("jacob" "copy-struct.plt" 1 0)))           
;  (require "copy-struct.ss")
  
  (IF_GRAPHICS
   (require (all-except "basic_graphics.ss" test-this these-tests)
	    (all-except "graphics_stub.ss" test-this these-tests) ;; gives us clear-buffer
	    ))
  
  ;; tests exports a whole bunch, because the simulated programs need to access this 
  ;; stuff once they are "eval"ed.
  ;; Ideally we would find a better way to do this.  For example the simulated programs 
  ;; could be generated in "prevaled" form, where the closures are stuck right in the sexp.
  (provide (all-defined-except );run-alpha-simple-simulator)
;           (all-from (planet "copy-struct.ss" ("jacob" "copy-struct.plt" 1 0)))
;           (all-from "copy-struct.ss")
;           (all-from "constants.ss")
;	   (all-from "helpers.ss")
           ;; Some Extra stuff needed by our runtime eval of simulated programs.	   
;	   yield-thread last
;           (all-from (lib "compat.ss")) ;; simulator needs flush-output-port
	   )
  
  (define vector-copy (void))
  
  ;; [2005.11.05] This fills in the implementation-specific casing for the generated code:
  ;; Could just be identity function, but wrapping in a module should give better performance.
  (define (build-genned-code-module node-code)
    `(begin (module _genned_node_code mzscheme
	      (provide node-code)
	      (require "plt/constants.ss")
	      (require "plt/hash.ss")
	      (require "plt/hashtab.ss")
	      (require (all-except "plt/helpers.ss" test-this these-tests))
	      (require (all-except "plt/simulator_alpha_datatypes.ss" test-this these-tests))
	      (require (all-except "plt/alpha_lib.ss" test-this these-tests))
	      (require "plt/alpha_lib_scheduler_simple.ss")
	      ,node-code)
	    (require _genned_node_code)))
  
  (include (build-path "generic" "simulator_nought.examples.ss"))
  (include (build-path "generic" "simulator_alpha.ss"))
    ;; The scheduler is part of the simulator rather than alpha_lib, actually:
;  (require "alpha_lib_scheduler_simple.ss")
  (include (build-path "generic" "alpha_lib_scheduler_simple.ss"))

  #;  ;; This was an old hack for simulator_nought, don't think I need it now:
  (set! structure-copy
        (lambda (s)
          (cond
            [(node? s) (copy-struct node s)]
            [(simobject? s) (copy-struct simobject s)]
            [(simworld? s) (copy-struct simworld s)]
            [else (error 'structure-copy
                         "sorry this is lame, but can't handle structure: ~s" s)]))
        )
  )


;==============================
;(require simulator_alpha)


