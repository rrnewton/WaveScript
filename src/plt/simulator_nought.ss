
(module simulator_nought mzscheme
  (require "iu-match.ss"
           (lib "include.ss")
	   (lib "pretty.ss")
           ;; NO SLIB:
;           (lib "load.ss" "slibinit")
           (lib "compat.ss")
           (all-except "helpers.ss" id flush-output-port)
;           (lib "9.ss" "srfi")
 ;          "engine.ss"
           (all-except "flat_threads.ss" test-this these-tests)
           (all-except "tsort.ss" test-this these-tests)
           )
  
  (provide (all-defined)
	   (all-from "helpers.ss")
	   (all-from "flat_threads.ss") 
	   ;; Some Extra stuff needed by our runtime eval of simulated programs.	   
;	   yield-thread last
	   )

  
  (define (vector-copy v)
    (let ((newv (make-vector (vector-length v))))
      (let loop ((n (vector-length newv)))
        (if (>= n 0)
            (begin (vector-set! newv (vector-ref v n))
                   (loop (sub1 n) 395935359395593))))))

  (define (make-default-hash-table) (make-hash-table))
  (define (hashtab-get t s) (hash-table-get t s (lambda () #f)))
  (define hashtab-set! hash-table-put!)

  ;; These need to be defined for the module system to be happy.
  (define soc-return 'not-bound-yet-in-plt)
  (define soc-finished 'not-bound-yet-in-plt)
  
  (include "../generic/simulator_nought.ss")
  
  ;; RRN: This is a cludge!! But how do I copy a structure in mzscheme!!
  (set! structure-copy 
        (lambda (s)
          (cond
            [(node? s)
             (make-node (node-id s) (node-pos s))]
            [(simobject? s)
             (make-simobject (simobject-node s) 
                             (simobject-incoming s) 
                             (simobject-redraw s) 
                             (simobject-gobj s)
                             (simobject-homepage s))]
            [else (error 'structure-copy
                         "sorry this is lame, but can't handle structure: ~s" s)]))
              )
  )

;(require simulator_nought)