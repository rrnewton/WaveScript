;; Ryan Newton
;; Started 2004.03.26
;; Here is the main file for my compiler.
;;======================================================================

;; [2004.06.16] This should have been done already, but just for good measure...
(define current_interpreter 'mzscheme)

;(module compiler_plt mzscheme
 
(require (lib "include.ss")
         (all-except "plt/helpers.ss" id mvlet rec))

;(disp "yay" default-unit-tester)

         ;	 "plt/language-mechanism.ss"

 (require "plt/iu-match.ss"
         (all-except "plt/pass00_verify-regiment.ss" these-tests test-this)
         "plt/pass01_eta-primitives.ss"
         (all-except "plt/pass02_rename-vars.ss" these-tests test-this)
         (all-except "plt/pass03_remove-unquoted-constant.ss")
         (all-except "plt/pass04_reduce-primitives.ss")
         (all-except "plt/pass05_remove-complex-constant.ss")
         (all-except "plt/pass06_uncover-free.ss")
         (all-except "plt/pass07_lift-letrec.ss")
         (all-except "plt/pass08_lift-letrec-body.ss")
         (all-except "plt/pass09_remove-complex-opera.ss" these-tests test-this)
         (all-except "plt/pass10_verify-core.ss" these-tests test-this)
         (all-except "plt/pass11_classify-names.ss" these-tests test-this)
         (all-except "plt/pass12_add-heartbeats.ss" )
         (all-except "plt/pass13_add-control-flow.ss")
         (all-except "plt/pass14_add-places.ss" these-tests test-this)
         (all-except "plt/pass15_analyze-places.ss" these-tests test-this)
         (all-except "plt/pass16_deglobalize.ss" these-tests test-this)
         
;         (all-except "plt/pass14_cleanup-token-machine.ss" ); these-tests test-this)
        ;          "plt/pass09_separate-graph.ss"
)

(require
; (all-except "plt/simulator_nought.ss" these-tests test-this)
 (all-except "plt/simulator_nought_graphics.ss" these-tests test-this wrap-def-simulate)
 )

;(disp "UNION" union (union '(a b c) '(a d c)))

'(define program 
   (lambda args (car (reverse args))))

'(define base-language 
   (lambda args (eval (car (reverse args)))))
;(define base-language 
;  (lambda args
;    (for-each eval args)))

;(disp "FOOB DONE REQS")
;(require "plt/language-mechanism.ss")
;(disp "DOEN LANG")

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

(disp "BOUT TO LOAD COMPLIRE" default-unit-tester)
(disp "Testing" (eval 'default-unit-tester))
;(load/use-compiled "compiler.ss")
(include "compiler.ss")

(disp "BOUT TO LOAD DRIVER" pass-names)

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

;; <TODO> Make stream version for this:
(define text-repl  (repl-builder void void run-compiler run-simulation))
(define graphical-repl
  (repl-builder (lambda () (init-world) (init-graphics))
		cleanse-world
		(lambda (x)
                  (match x
                    [(precomp ,exp) `(unknown-lang (quote ,exp))]
                    [,other (run-compiler other)]))
		graphical-simulation))

;(define precomp-graphical-repl
;  (repl-builder (lambda () (init-world) (init-graphics))
;		cleanse-world
;		(lambda (x) x) ;; Compiler ;run-compiler
;		graphical-simulation))

;; From lang05.ss
;; Doesn't work:
#;(define-syntax lazy-letrec
  (syntax-rules ()
    [(_ ([lhs rhs] ...) body)        
     (letrec ([lhs 
               (lambda () 
                 (let-syntax ([lhs (identifier-syntax (lhs))] ...)
                   rhs))]
              ...)
       (let-syntax ([lhs (identifier-syntax (lhs))] ...)
         body))]))

(define-syntax lazy-letrec
  (syntax-rules ()
    [(_ ([lhs rhs] ...) body ...)        
     (letrec ([lhs rhs] ...) body ...)]))

(cleanse-world)
(define simulate run-simulation)
;(eval (cadr (last testssim)))


;(begin (init-graphics) (cleanse-world) (graphical-repl))

'(lambda (x) (rmap sense x))
