
(module simulator_alpha mzscheme
  (require "iu-match.ss"
           (lib "include.ss")
	   (lib "pretty.ss")
           (prefix srfi1. (lib "1.ss" "srfi")) ; make-list
           ;; NO SLIB:
;           (lib "load.ss" "slibinit")
           (all-except (lib "compat.ss") define-structure) ;; gives us define-structure           
           )
  (require 
   (all-except "constants.ss" test-this these-tests)
   (all-except "helpers.ss" id flush-output-port test-this these-tests)
   (all-except "graphics_stub.ss" test-this these-tests) ;; gives us clear-buffer
   (all-except "critical_section.ss" test-this these-tests)
;;           (lib "9.ss" "srfi")
 ;;          "engine.ss"
   (all-except "flat_threads.ss" test-this these-tests)
   (all-except "tsort.ss" test-this these-tests)
           )
  (require (planet "copy-struct.ss" ("jacob" "copy-struct.plt" 1 0)))           
  
  ;; tests exports a whole bunch, because the simulated programs need to access this 
  ;; stuff once they are "eval"ed.
  (provide (all-defined)
           (all-from (planet "copy-struct.ss" ("jacob" "copy-struct.plt" 1 0)))
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
  
  
  (define make-list srfi1.make-list)
  
  (define (write-sim-to-file sim fn)
    (with-output-to-file fn
      (lambda ()
        ;; TODO: ENSURE NO DEPTH LIMIT:
        (pretty-print `(module alpha-simulation mzscheme                         
                         (requires "alpha_lib.ss")
                         (provides (all-defined))                         
                         (start-alpha-sim ,sim))))
      'replace))
  
  (define (vector-copy v)
    (let ((newv (make-vector (vector-length v))))
      (let loop ((n (vector-length newv)))
        (if (>= n 0)
            (begin (vector-set! newv n (vector-ref v n))
                   (loop (sub1 n)))))))

  (define (make-default-hash-table) (make-hash-table 'equal))
  (define (hashtab-get t s) (hash-table-get t s (lambda () #f)))
  (define hashtab-set! hash-table-put!)

  ;; These need to be defined for the module system to be happy.
  (define soc-return 'not-bound-yet-in-plt)
  (define soc-finished 'not-bound-yet-in-plt)
  
  (include "../generic/simulator_nought.examples.ss")
  (include "../generic/simulator_alpha.ss")
   
 '(set! structure-copy
        (lambda (s)
          (cond
            [(node? s) (copy-struct node s)]
            [(simobject? s) (copy-struct simobject s)]
            [else (error 'structure-copy
                         "sorry this is lame, but can't handle structure: ~s" s)]))
            )
  
  ;; RRN: This is a cludge!! But how do I copy a structure in mzscheme!!
  (set! structure-copy 
        (lambda (s)
          (cond
            [(node? s)
             (make-node (node-id s) (node-pos s))]
            [(simobject? s)
             (make-simobject (simobject-node s) 
                             (simobject-incoming s)
                             (simobject-timed-token-buf s)
                             (simobject-redraw s) 
                             (simobject-gobj s)
                             (simobject-homepage s)
			     (simobject-token-store s)
			     (simobject-local-sent-messages s)
			     (simobject-local-recv-messages s))]
            [else (error 'structure-copy
                         "sorry this is lame, but can't handle structure: ~s" s)]))
              )
  
  )

(require simulator_alpha)
