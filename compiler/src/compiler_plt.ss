;; Ryan Newton
;; Started 2004.03.26
;; Here is the main file for my compiler.
;;======================================================================

#cs ;; Case Sensitivity

;; [2004.06.16] This should have been done already, but just for good measure...
(define current_interpreter 'mzscheme)

;(module compiler_plt mzscheme
 
(require (lib "include.ss")
         (all-except "plt/helpers.ss" id rec))

(require "plt/constants.ss"
         "plt/iu-match.ss"
          ;; [2004.12.06] I think I had this working:
          "plt/critical_section.ss"

(all-except "plt/pass00_verify-regiment.ss" these-tests test-this)
(all-except "plt/pass01_eta-primitives.ss" these-tests test-this)
(all-except "plt/pass02_rename-vars.ss" these-tests test-this)
(all-except "plt/pass03_remove-unquoted-constant.ss" these-tests test-this)
(all-except "plt/pass04_static-elaborate.ss" these-tests test-this)
(all-except "plt/pass05_reduce-primitives.ss" these-tests test-this)

(all-except "plt/pass06_remove-complex-constant.ss" these-tests test-this)
; pass07_verify-stage2.ss
(all-except "plt/pass08_uncover-free.ss" these-tests test-this)
(all-except "plt/pass09_lift-letrec.ss" these-tests test-this)
(all-except "plt/pass10_lift-letrec-body.ss" these-tests test-this)
(all-except "plt/pass11_remove-complex-opera.ss" these-tests test-this)
(all-except "plt/pass12_verify-core.ss" these-tests test-this)
(all-except "plt/pass13_classify-names.ss" these-tests test-this)
;(all-except "plt/pass09_separate-graph" these-tests test-this)

(all-except "plt/pass14_add-heartbeats.ss" these-tests test-this)
(all-except "plt/pass15_add-control-flow.ss" these-tests test-this)
(all-except "plt/pass16_add-places.ss" these-tests test-this)
(all-except "plt/pass17_analyze-places.ss" these-tests test-this)
;(all-except "plt/pass18_add-routing.ss" these-tests test-this
)
(all-except "plt/pass20_deglobalize.ss" these-tests test-this)
(all-except "plt/pass21_cleanup-token-machine.ss" these-tests test-this)

(all-except "plt/pass23_desugar-gradients.ss" these-tests test-this)
(all-except "plt/pass24_desugar-let-stored.ss" these-tests test-this)
(all-except "plt/pass25_rename-stored.ss" these-tests test-this)

;(all-except "plt/pass24_analyze-calls.ss" these-tests test-this)
;(all-except "plt/pass25_inline.ss" these-tests test-this)
;(all-except "plt/pass26_prune-returns.ss" these-tests test-this)

(all-except "plt/pass27_cps-tokmac.ss" these-tests test-this)
(all-except "plt/pass28_closure-convert.ss" these-tests test-this)

;;(all-except "plt/pass29_verify-token-machine.ss" these-tests test-this)
(all-except "plt/pass30_haskellize-tokmac.ss" these-tests test-this)


;          (all-except "plt/pass00_verify-regiment.ss" these-tests test-this)
;          "plt/pass01_eta-primitives.ss"
;          (all-except "plt/pass02_rename-vars.ss" these-tests test-this)
;          (all-except "plt/pass03_remove-unquoted-constant.ss")
;          (all-except "plt/pass04_reduce-primitives.ss" these-tests test-this)
;          (all-except "plt/pass04_static-elaborate.ss" these-tests test-this)
;          (all-except "plt/pass05_remove-complex-constant.ss")
;          (all-except "plt/pass06_uncover-free.ss")
;          (all-except "plt/pass07_lift-letrec.ss")
;          (all-except "plt/pass08_lift-letrec-body.ss")
;          (all-except "plt/pass09_remove-complex-opera.ss" these-tests test-this)
;          (all-except "plt/pass10_verify-core.ss" these-tests test-this)
;          (all-except "plt/pass11_classify-names.ss" these-tests test-this)
;          (all-except "plt/pass12_add-heartbeats.ss" )
;          (all-except "plt/pass13_add-control-flow.ss")
;          (all-except "plt/pass14_add-places.ss" these-tests test-this)
;          (all-except "plt/pass15_analyze-places.ss" these-tests test-this)
;          (all-except "plt/pass16_deglobalize.ss" these-tests test-this)
;          (all-except "plt/pass17_cleanup-token-machine.ss" these-tests test-this)
         
;          (all-except "plt/pass19_haskellize-tokmac.ss" test-this these-tests)

         
;    (all-except "plt/pass14_cleanup-token-machine.ss" ); these-tests test-this)
        ;          "plt/pass09_separate-graph.ss"
)

(require
;;; (all-except "plt/simulator_nought.ss" these-tests test-this)
; (all-except "plt/simulator_nought_graphics.ss" these-tests test-this wrap-def-simulate)
 )

(require
; (all-except "plt/simulator_nought.ss" these-tests test-this)
 (all-except "plt/simulator_alpha.ss" these-tests test-this)
 (all-except "plt/alpha_lib.ss" these-tests test-this)
 )
;  (require "plt/demo_display.ss")

;; Get those module bound identifiers out in the open!
(load/use-compiled "plt/tests.ss") ;(require tests)
;(define tests tests)

;(define test00 test00)
;(define test01 test01)
;(define test07 test07)

;(define tests00 tests00)
;(define tests01 tests01)
;(define tests07 tests07)

;(disp "BOUT TO LOAD COMPLIRE" default-unit-tester)
;(disp "Testing" (eval 'default-unit-tester))
;(load/use-compiled "compiler.ss")
(include "compiler.ss")

;(disp "BOUT TO LOAD DRIVER" pass-names)

(load/use-compiled "plt/driver.ss")
;; Can't get langs to work.  Just abandon evaluation:
(game-eval (lambda args 'unspecified))
(host-eval (lambda args 'unspecified))


;(require "plt/language-mechanism.ss") ;; This blows up if we try to require it up top!
;(load/use-compiled "generic/lang00.ss")
;(load/use-compiled "generic/lang05.ss")

;(require (lib "trace.ss"))
;(trace  explode-primitive process-expr process-letrec)

#;(define (test-this)
  (parameterize ((tracer #t))
    (test-one
     '(letrec ((a (anchor-at '(30 40)))
               (r (circle-at 50 a))
               (f (lambda (tot next)
                    (cons (+ (car tot) (sense next))
                          (+ (cdr tot) 1))))
               (g (lambda (tot) (/ (car tot) (cdr tot))))
               (avg (smap g (rfold f (cons 0 0) r))))
        avg))))

;(require "plt/simulator_nought.ss")


(define (g) 
  ;  (define prog (rc '(anchor-at '(30 40))))
  (init-world)
  (let ((res (run-simulation 
              (build-simulation
               (compile-simulate-nought prog)) 10.0)))
    (disp "EXEC FINISHED, HERE WAS PROG:")
    (pretty-print prog)
    res))
;(g)

(load/use-compiled "generic/repl.ss")

(define-syntax lazy-letrec
  (syntax-rules ()
    [(_ ([lhs rhs] ...) body ...)        
     (letrec ([lhs rhs] ...) body ...)]))


(define (load_loader)
  (current-directory (build-path "plt"))
  (let ([f (load-extension 
	    (build-path "compiled" "native"  (system-library-subpath) "_loader.so"))])
  (printf "Got loader: ~s~n" f)
  (let-values ([(th r) (f #t)])
	      (printf "Loaded, ~s  ~s~n" th r)
	      (th))))

;(begin (display ) (newline) (exit))

;(begin (init-graphics) (cleanse-world) (graphical-repl))
;(define (start) (begin (init-graphics) (cleanse-world) (graphical-repl))) ;; shorthand

;(t)

;(ra '(tokens (node-start () (soc-return 3))))


(define (t)
  (ra
   '


(cps-tokmac-lang
  '(program
     (bindings (result_1 '34234324324))
     (nodepgm
       (tokens
         (returnhandler_6
           retid
           (destid flag val toind viaind)
           (stored (acc_9 ()))
           (if (= flag '222)
               (let ([oldacc_5 acc_9])
                 (begin (set! acc_9 '())
                        (let ([parentpointer_7
                               (ext-ref
                                 (tok global-tree viaind)
                                 storedgparent_13)])
                          (if (not parentpointer_7)
                              (dbg '"ERROR: fell off the via tree.")
                              (if (= '0 parentpointer_7)
                                  (call (tok SOC-return-handler toind)
                                        (cons val oldacc_5))
                                  (bcast
                                    (tok returnhandler_6 retid)
                                    parentpointer_7
                                    '333
                                    (cons val oldacc_5)
                                    '0
                                    '0))))
                        #0=(void)))
               (if (not (if (= destid '0) '#t (= destid (my-id))))
                   (void)
                   (set! acc_9 (cons val acc_9)))))
         (global-tree
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (storedgparent_13 #1='#f)
             (storedgorigin_12 #2='#f)
             (storedghopcount_11 #3='#f)
             (storedgversion_10 #4='#f))
           (if (if (not storedghopcount_11)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version storedgversion_10)
                           (if (= g_version storedgversion_10)
                               (< g_hopcount storedghopcount_11)
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
                          (begin (set! storedgparent_13 g_parent)
                                 (set! storedgorigin_12 g_origin)
                                 (set! storedghopcount_11 g_hopcount)
                                 (set! storedgversion_10 g_version)
                                 #0#)
                          (void))
                      #0#)
               (void)))
         (spread-global
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (storedgparent_19 #1#)
             (storedgorigin_18 #2#)
             (storedghopcount_17 #3#)
             (storedgversion_16 #4#)
             (ver_15 (void))
             (storedliftoption_14 '#f))
           (if (if (not storedghopcount_17)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version storedgversion_16)
                           (if (= g_version storedgversion_16)
                               (< g_hopcount storedghopcount_17)
                               '#f)
                           '#f)
                       '#f))
               (begin (if storedliftoption_14
                          (void)
                          (begin (set! storedliftoption_14 '#t)
                                 (set! ver_15 '0)
                                 #0#))
                      (ext-set! (tok global-tree 0) storedgparent_13 '0)
                      (set! ver_15 (+ '1 ver_15))
                      (bcast (tok global-tree '0) (my-id) '1 ver_15)
                      (timed-call
                        1000
                        (tok spread-global 0)
                        '#f
                        '#f
                        '0
                        '#f)
                      (if (not (= g_hopcount '0))
                          (begin (set! storedgparent_19 g_parent)
                                 (set! storedgorigin_18 g_origin)
                                 (set! storedghopcount_17 g_hopcount)
                                 (set! storedgversion_16 g_version)
                                 #0#)
                          (void))
                      #0#)
               (void)))
         (node-start subtok_ind () (stored) (void))
         (SOC-start
           subtok_ind
           ()
           (stored)
           (begin (void)
                  (let ([aggrID_3 (+ (* '1000 '0) '0)])
                    (call (tok returnhandler_6 aggrID_3)
                          (my-id)
                          '222
                          result_1
                          '0
                          '0))
                  (soc-finished)
                  'multiple-bindings-for-token
                  #0#))))))


   
   ))
