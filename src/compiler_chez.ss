
(printf "Loading compiler in chezscheme..~n")

(include "chez/match.ss")


;; Uncomment this to remove debugging code and possibly make the
;; system run faster.
;;(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (void)]))
(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (begin expr ...)]))


;; This in turn includes "../generic/helpers.ss" so we gotta load it from its dir.
(cd "chez") (include "helpers.ss") (cd "..")

(include "generic/language-mechanism.ss")
(include "generic/lang00.ss")
(include "generic/lang05.ss")

(include "generic/pass00_verify-regiment.ss")
(include "generic/pass01_rename-var.ss")
(include "generic/pass02_remove-unquoted-constant.ss")
(include "generic/pass03_remove-complex-constant.ss")
(include "generic/pass04_uncover-free.ss")
(include "generic/pass05_lift-letrec.ss")
(include "generic/pass06_lift-letrec-body.ss")
(include "generic/pass07_remove-complex-opera.ss")
(include "generic/pass08_verify-core.ss")

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
(include "chez/flat_threads.ss")
;; Basic simulator for the nodal language:
(include"generic/simulator_nought.ss")

;; If we're in SWL then load the graphics portion:
(if (top-level-bound? 'SWL-ACTIVE)
    (begin (load "chez/basic_graphics.ss")
	   (load "chez/graphics_stub.ss")
	   (load "generic/demo_display.ss")
	   (load "generic/simulator_nought_graphics.ss")))

; (include "chez/graphics_stub.ss")
;(include "generic/demo_display.ss")

;(trace  explode-primitive process-expr process-letrec)

