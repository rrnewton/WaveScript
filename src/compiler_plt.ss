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
;(all-except "plt/pass18_add-routing.ss" these-tests test-this)

(all-except "plt/pass20_deglobalize.ss" these-tests test-this)
(all-except "plt/pass21_cleanup-token-machine.ss" these-tests test-this)

(all-except "plt/pass23_desugar-gradients.ss" these-tests test-this)
(all-except "plt/pass24_desugar-let-stored.ss" these-tests test-this)
(all-except "plt/pass25_rename-stored.ss" these-tests test-this)

;(all-except "plt/pass24_analyze-calls.ss" these-tests test-this)
;(all-except "plt/pass25_inline.ss" these-tests test-this)
;(all-except "plt/pass26_prune-returns.ss" these-tests test-this)

(all-except "plt/pass27_cps-tokmac.ss" these-tests test-this)

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
   '(cps-tokmac-lang
  '(program
     (bindings (result_2 '393939))
     (nodepgm
       (tokens
         (returnhandler_5
           retid
           (destid flag val toind viaind)
           (stored (acc_7 '#f))
           (let ([oldacc acc_7])
             (begin (set! acc_7 '())
                    (if (= (my-id)
                           (ext-ref
                             (tok global-tree viaind)
                             storedgorigin_10))
                        (call (tok SOC-return-handler toind)
                              (cons val oldacc))
                        (bcast
                          (tok returnhandler_5 retid)
                          (ext-ref
                            (tok global-tree viaind)
                            storedgparent_11)
                          '333
                          (cons val oldacc)
                          '0
                          '0))
                    (void))))
         (global-tree
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (storedgparent_11 #0='#f)
             (storedgorigin_10 #1='#f)
             (storedghopcount_9 #2='#f)
             (storedgversion_8 #3='#f))
           (if (if (not storedghopcount_9)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version storedgversion_8)
                           (if (= g_version storedgversion_8)
                               (< g_hopcount storedghopcount_9)
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
                          (begin (set! storedgparent_11 g_parent)
                                 (set! storedgorigin_10 g_origin)
                                 (set! storedghopcount_9 g_hopcount)
                                 (set! storedgversion_8 g_version)
                                 (void))
                          (void))
                      (void))
               (void)))
         (spread-global
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (storedgparent_17 #0#)
             (storedgorigin_16 #1#)
             (storedghopcount_15 #2#)
             (storedgversion_14 #3#)
             (ver_13 (void))
             (storedliftoption_12 '#f))
           (if (if (not storedghopcount_15)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version storedgversion_14)
                           (if (= g_version storedgversion_14)
                               (< g_hopcount storedghopcount_15)
                               '#f)
                           '#f)
                       '#f))
               (begin (if (not storedliftoption_12)
                          (begin (set! storedliftoption_12 '#t)
                                 (set! ver_13 '0)
                                 (void))
                          (void))
                      (set! ver_13 (+ '1 ver_13))
                      (bcast (tok global-tree '0) (my-id) '1 ver_13)
                      (timed-call
                        1000
                        (tok spread-global 0)
                        '#f
                        '#f
                        '0
                        '#f)
                      (if (not (= g_hopcount '0))
                          (begin (set! storedgparent_17 g_parent)
                                 (set! storedgorigin_16 g_origin)
                                 (set! storedghopcount_15 g_hopcount)
                                 (set! storedgversion_14 g_version)
                                 (void))
                          (void))
                      (void))
               (void)))
         (node-start subtok_ind () (stored) (void))
         (soc-start
           subtok_ind
           ()
           (stored)
           (let ([aggrid_4 (+ (* '1000 '0) '0)])
             (call (tok returnhandler_5 aggrid_4)
                   '333
                   result_2
                   '0
                   '0)))))))
 
   ))
