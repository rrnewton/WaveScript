
(printf "Loading compiler in chezscheme..~n")

(include "chez/match.ss")

;; [2004.06.04] MOVED THIS DEFINIITON to helpers.ss
;; Uncomment this to remove debugging code and possibly make the
;; system run faster.
;;(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (void)]))
;;(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (begin expr ...)]))


;; This in turn includes "../generic/helpers.ss" so we gotta load it from its dir.
(cd "chez") (include "helpers.ss") (cd "..")

(include "generic/language-mechanism.ss")
(include "generic/lang00.ss")
(include "generic/lang05.ss")
(include "generic/lang10.ss") ;; deglobalize

(include "generic/pass00_verify-regiment.ss")
(include "generic/pass01_rename-var.ss")
(include "generic/pass02_remove-unquoted-constant.ss")
(include "generic/pass03_remove-complex-constant.ss")
(include "generic/pass04_uncover-free.ss")
(include "generic/pass05_lift-letrec.ss")
(include "generic/pass06_lift-letrec-body.ss")
(include "generic/pass07_remove-complex-opera.ss")
(include "generic/pass08_verify-core.ss")
(include "generic/pass09_classify-names.ss")

;(include "generic/pass09_separate-graph")
(include "generic/pass10_deglobalize.ss")

(include "compiler.ss")

;; Driver depends on 'pass-names being defined.
(include "generic/driver.ss")
  (game-eval (lambda args 'unspecified))
  (host-eval (lambda args 'unspecified))
(include "generic/tests_noclosure.ss")
(include "generic/tests.ss")

(load "../depends/slib/chez.init")
(require 'tsort) ;; for the simulator: 

;; Basic parallel computation (engines):
(if (top-level-bound? 'SWL-ACTIVE)
    (load "chez/swl_flat_threads.ss")
    (load "chez/flat_threads.ss"))

;; LAME:
;(if (top-level-bound? 'SWL-ACTIVE) (eval '(import flat_threads)))

;; Basic simulator for the nodal language:
;(include "chez/simulator_nought.ss")
(load "chez/simulator_nought.ss")

;; If we're in SWL then load the GRAPHICS portion:
(if (top-level-bound? 'SWL-ACTIVE)
    (let ()
      (load "chez/basic_graphics.ss")
      (load "chez/graphics_stub.ss")
      (load "chez/demo_display.ss")
      (load "chez/simulator_nought_graphics.ss")))

;(trace  explode-primitive process-expr process-letrec)

(if (top-level-bound? 'SWL-ACTIVE)
    (begin
      (eval '(import basic_graphics))
      (eval '(import graphics_stub))
      (load "chez/simulator_nought_graphics.ss")
      ))

(define simulate)
(if (top-level-bound? 'SWL-ACTIVE)
    (set! simulate graphical-simulation)
    (set! simulate run-simulation))

;; Load the repl which depends on the whole compiler and simulator.
(include "generic/repl.ss")

