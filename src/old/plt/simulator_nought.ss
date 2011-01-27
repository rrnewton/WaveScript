
(module simulator_nought mzscheme
  (require "iu-match.ss"
           "critical_section.ss"
           (lib "include.ss")
	   (lib "pretty.ss")          
           ;; NO SLIB:
           ;           (lib "load.ss" "slibinit")
           (lib "compat.ss") ;; gives us reg:define-struct  
           )
  (require 
   "../generic/constants.ss"
   (all-except "helpers.ss" id flush-output-port (current-output-port))
   (all-except "graphics_stub.ss" test-this these-tests) ;; gives us clear-buffer
   ;           (lib "9.ss" "srfi")
   ;          "engine.ss"
   (all-except "flat_threads.ss" test-this these-tests)
   (all-except "tsort.ss" test-this these-tests)
   )
  
  ;; This exports a whole bunch, because the simulated programs need to access this 
  ;; stuff once they are "eval"ed.
  (provide (all-defined)
           (all-from "../generic/constants.ss")
	   (all-from "helpers.ss")
	   (all-from "flat_threads.ss") 
	   ;; Some Extra stuff needed by our runtime eval of simulated programs.	   
           ;	   yield-thread last
           (all-from (lib "compat.ss")) ;; simulator needs flush-output-port
	   )
  
  
  (define (vector-copy v)
    (let ((newv (make-vector (vector-length v))))
      (let loop ((n (vector-length newv)))
        (if (>= n 0)
            (begin (vector-set! newv n (vector-ref v n))
                   (loop (sub1 n)))))))
  
  ;; These need to be defined for the module system to be happy.
  (define soc-return 'not-bound-yet-in-plt)
  (define soc-finished 'not-bound-yet-in-plt)
  
  (include (build-path "generic" "simulator_nought.examples.ss"))
  (include (build-path "generic" "simulator_nought.ss"))
  
  ;; RRN: This is a cludge!! But how do I copy a structure in mzscheme!!
  (set! structure-copy 
        (lambda (s)
          (cond
            [(node? s)
             (make-node (node-id s) (node-pos s))]
            [(simobject? s)
             (make-simobject (simobject-node s) 
                             (simobject-incoming s)
                             (simobject-timed-tokens s)
                             (simobject-redraw s) 
                             (simobject-gobj s)
                             (simobject-homepage s)
			     (simobject-token-cache s)
			     (simobject-local-sent-messages s)
			     (simobject-local-recv-messages s))]
            [else (error 'structure-copy
                         "sorry this is lame, but can't handle structure: ~s" s)]))
        )
  
  
;; <TODO> Make stream version for this:
;(define text-repl  (repl-builder void void run-compiler run-simulation))
'(define precomp-repl (repl-builder 
		      void  ;; Startup
		      void  ;; Cleanse
		      (lambda (x) x) ;; Compiler
		      run-simulation-stream))
;(define precomp-graphical-repl
;  (repl-builder (lambda () (init-world) (init-graphics))
;		cleanse-world
;		(lambda (x) x) ;; Compiler ;run-compiler
;		graphical-simulation))
  
  (define graphical-repl
    (repl-builder (lambda () (init-world) (init-graphics))
                  cleanse-world
                  (lambda (x)
                    (parameterize ([pass-list (list-remove-after deglobalize (pass-list))])
                      (match x
                        [(precomp ,exp) `(unknown-lang (quote ,exp))]
                        [,other (run-compiler other)])))
                  graphical-simulation))  
  (define precomp-graphical-repl
    (repl-builder (lambda () (init-world) (init-graphics))
                  cleanse-world
                  (lambda (x)
                    (parameterize ([pass-list (cleanup-token-machine)])
                      (match x
                        [(precomp ,exp) `(unknown-lang (quote ,exp))]
                        [,other (run-compiler other)])))
                  graphical-simulation))
  (define pgr precomp-graphical-repl) ;; shorthand
  )

;(require simulator_nought);
;(define (g) (eval (cadadr these-tests)))
;(go)

;(define x (cadr (list-ref testssim 2)))

