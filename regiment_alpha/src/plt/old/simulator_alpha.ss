#cs ;; Case Sensitiviy
(module simulator_alpha mzscheme

  ;(require (planet "copy-struct.ss" ("jacob" "copy-struct.plt" 1 0)))           
;  (require "copy-struct.ss")

  (require "../generic/constants.ss"
           (lib "include.ss"))
  
;  (IF_GRAPHICS
;   (require (all-except "basic_graphics.ss" test-this these-tests)
;	    (all-except "graphics_stub.ss" test-this these-tests) ;; gives us clear-buffer
;	    ))
  
  ;; tests exports a whole bunch, because the simulated programs need to access this 
  ;; stuff once they are "eval"ed.
  ;; Ideally we would find a better way to do this.  For example the simulated programs 
  ;; could be generated in "prevaled" form, where the closures are stuck right in the sexp.
  (provide (all-defined-except start-alpha-sim);run-alpha-simple-simulator)
;           (all-from (planet "copy-struct.ss" ("jacob" "copy-struct.plt" 1 0)))
;           (all-from "copy-struct.ss")
;           (all-from "../generic/constants.ss")
;	   (all-from "helpers.ss")
           ;; Some Extra stuff needed by our runtime eval of simulated programs.	   
;	   yield-thread last
;           (all-from (lib "compat.ss")) ;; simulator needs flush-output-port
	   )
  
  (include (build-path "generic" "simulator_nought.examples.ss"))
  (include (build-path "generic" "simalpha_rollworld.ss"))
  (include (build-path "generic" "simalpha_ui.ss"))
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


