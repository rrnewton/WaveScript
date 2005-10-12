#cs ;; Case Sensitiviy
(module simulator_alpha mzscheme
  (require 
   (all-except "constants.ss" test-this these-tests)
   (all-except "helpers.ss" id flush-output-port test-this these-tests)  
   "iu-match.ss"           
   (all-except (lib "compat.ss") define-structure) ;; gives us define-structure           
   (lib "include.ss")
   (lib "pretty.ss")
   (prefix srfi1. (lib "1.ss" "srfi")) ; make-list
;   (all-except "graphics_stub.ss" test-this these-tests) ;; gives us clear-buffer
   (all-except "tsort.ss" test-this these-tests)
  
   ;(all-except "pass21_cleanup-token-machine.ss" test-this these-tests)
   )

  ;(require (planet "copy-struct.ss" ("jacob" "copy-struct.plt" 1 0)))           
  (require "copy-struct.ss")
  
  ;; tests exports a whole bunch, because the simulated programs need to access this 
  ;; stuff once they are "eval"ed.
  ;; Ideally we would find a better way to do this.  For example the simulated programs 
  ;; could be generated in "prevaled" form, where the closures are stuck right in the sexp.
  (provide (all-defined-except );run-alpha-simple-simulator)
;           (all-from (planet "copy-struct.ss" ("jacob" "copy-struct.plt" 1 0)))
           (all-from "copy-struct.ss")
;           (all-from "constants.ss")
;	   (all-from "helpers.ss")
           ;; Some Extra stuff needed by our runtime eval of simulated programs.	   
;	   yield-thread last
;           (all-from (lib "compat.ss")) ;; simulator needs flush-output-port
	   )
  
  (define-syntax define-structure
    (syntax-rules ()
      [(_ (sname field ...))
       (define-struct sname (field ...) (make-inspector))]))  

  (define vector-copy (void))
  
  ;(define make-list srfi1.make-list)
  
  (define (write-sim-to-file sim fn)
    (with-output-to-file fn
      (lambda ()
        ;; TODO: ENSURE NO DEPTH LIMIT:
        (pretty-print `(module alpha-simulation mzscheme                         
                         (requires "alpha_lib.ss")
                         (provides (all-defined))                         
                         (start-alpha-sim ,sim))))
      'replace))
  
;  (define (make-default-hash-table) (make-hash-table 'equal))
;  (define (hashtab-get t s) (hash-table-get t s (lambda () #f)))
;  (define hashtab-set! hash-table-put!)
  
  (include (build-path "generic" "simulator_nought.examples.ss"))
  (include (build-path "generic" "simulator_alpha.ss"))
    ;; The scheduler is part of the simulator rather than alpha_lib, actually:
;  (require "alpha_lib_scheduler_simple.ss")
  (include (build-path "generic" "alpha_lib_scheduler_simple.ss"))
        
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


