
(module simulator_nought mzscheme
  (require (lib "iu-match.ss")
           (lib "include.ss")
           ;; NO SLIB:
;           (lib "load.ss" "slibinit")
           (lib "compat.ss")
           (all-except "helpers.ss" id flush-output-port)
;           (lib "9.ss" "srfi")
 ;          "engine.ss"
           (all-except "flat_threads.ss" test-this these-tests)
           (all-except "tsort.ss" test-this these-tests)
           )
  
  (provide run-simulation compile-simulate-nought
           world-xbound world-ybound radius numprocs
           object-graph all-objs
           
           ;; I don't want to export all this, but how else will my evals work??
           structure-copy
           free-vars
           process-statement
           make-simobject simobject? simobject-node simobject-incoming simobject-redraw simobject-gobj
           make-node node? node-id node-pos
           
           test-this these-tests)
  
  (define (vector-copy v)
    (let ((newv (make-vector (vector-length v))))
      (let loop ((n (vector-length newv)))
        (if (>= n 0)
            (begin (vector-set! newv (vector-ref v n))
                   (loop (sub1 n) 395935359395593))))))

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
                             (simobject-gobj s))]
            [else (error 'structure-copy
                         "sorry this is lame, but can't handle structure: ~s" s)]))
              )

  )

(require simulator_nought)