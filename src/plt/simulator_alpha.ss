 
(module simulator_alpha mzscheme
  (require 
   (all-except "constants.ss" test-this these-tests)
   (all-except "helpers.ss" id flush-output-port test-this these-tests)  
   "iu-match.ss"           
   (all-except (lib "compat.ss") define-structure) ;; gives us define-structure           
   (lib "include.ss")
   (lib "pretty.ss")
   (prefix srfi1. (lib "1.ss" "srfi")) ; make-list
   (all-except "graphics_stub.ss" test-this these-tests) ;; gives us clear-buffer
   (all-except "tsort.ss" test-this these-tests)
   )

  ;(require (planet "copy-struct.ss" ("jacob" "copy-struct.plt" 1 0)))           
  (require "copy-struct.ss")
  
  ;; tests exports a whole bunch, because the simulated programs need to access this 
  ;; stuff once they are "eval"ed.
  (provide (all-defined)
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
  
  (define (make-default-hash-table) (make-hash-table 'equal))
  (define (hashtab-get t s) (hash-table-get t s (lambda () #f)))
  (define hashtab-set! hash-table-put!)
  
  (include (build-path "generic" "simulator_nought.examples.ss"))
  (include (build-path "generic" "simulator_alpha.ss"))
        
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

     (require simulator_alpha)

(compile-simulate-alpha 
'(cps-tokmac-lang
  '(program
     (bindings (result_50 '3))
     (nodepgm
       (tokens
         (returnhandler_53
           retid
           (destid flag val toind viaind)
           (stored (acc_55 '#f))
           (let ([oldacc acc_55])
             (begin (set! acc_55 '())
                    (if (= (my-id)
                           (ext-ref
                             (tok global-tree viaind)
                             storedgorigin_58))
                        (call (tok soc-return-handler toind)
                              (cons val oldacc))
                        (bcast
                          (tok returnhandler_53 retid)
                          (ext-ref
                            (tok global-tree viaind)
                            storedgparent_59)
                          '333
                          (cons val oldacc)
                          '0
                          '0))
                    (void))))
         (global-tree
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (storedgparent_59 #0='#f)
             (storedgorigin_58 #1='#f)
             (storedghopcount_57 #2='#f)
             (storedgversion_56 #3='#f))
           (if (if (not storedghopcount_57)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version storedgversion_56)
                           (if (= g_version storedgversion_56)
                               (< g_hopcount storedghopcount_57)
                               '#f)
                           '#f)
                       '#f))
               (begin (bcast
                        (tok global-tree subtok_ind)
                        (my-id)
                        g_origin
                        (+ '1 g_hopcount)
                        g_version)
                      (if (not (= g_hopcount '0))
                          (begin (set! storedgparent_59 g_parent)
                                 (set! storedgorigin_58 g_origin)
                                 (set! storedghopcount_57 g_hopcount)
                                 (set! storedgversion_56 g_version)
                                 (void))
                          (void))
                      (void))
               (void)))
         (spread-global
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (storedgparent_65 #0#)
             (storedgorigin_64 #1#)
             (storedghopcount_63 #2#)
             (storedgversion_62 #3#)
             (ver_61 (void))
             (storedliftoption_60 '#f))
           (if (if (not storedghopcount_63)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version storedgversion_62)
                           (if (= g_version storedgversion_62)
                               (< g_hopcount storedghopcount_63)
                               '#f)
                           '#f)
                       '#f))
               (begin (if (not storedliftoption_60)
                          (begin (set! storedliftoption_60 '#t)
                                 (set! ver_61 '0)
                                 (void))
                          (void))
                      (set! ver_61 (+ '1 ver_61))
                      (bcast (tok global-tree '0) (my-id) '1 ver_61)
                      (timed-call
                        1000
                        (tok spread-global 0)
                        '#f
                        '#f
                        '0
                        '#f)
                      (if (not (= g_hopcount '0))
                          (begin (set! storedgparent_65 g_parent)
                                 (set! storedgorigin_64 g_origin)
                                 (set! storedghopcount_63 g_hopcount)
                                 (set! storedgversion_62 g_version)
                                 (void))
                          (void))
                      (void))
               (void)))
         (node-start subtok_ind () (stored) (void))
         (soc-start
           subtok_ind
           ()
           (stored)
           (let ([aggrid_52 (+ (* '1000 '0) '0)])
             (call (tok returnhandler_53 aggrid_52)
                   '333
                   result_50
                   '0
                   '0))))))))

