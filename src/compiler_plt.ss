;; Ryan Newton
;; Started 2004.03.26
;; Here is the main file for my compiler.
;;======================================================================

;(module compiler_plt mzscheme

(require (lib "include.ss")
         "plt/helpers.ss"
;	 "plt/language-mechanism.ss"

         (all-except "plt/pass00_verify-regiment.ss" these-tests test-this)
         (all-except "plt/pass01_rename-var.ss" these-tests test-this)
         (all-except "plt/pass07.ss" these-tests test-this)
	 "plt/tests.ss"
)

(disp "FOOB DONE REQS")
;(require "plt/language-mechanism.ss")
(disp "DOEN LANG")

;  (require "plt/demo_display.ss")

;; Get those module bound identifiers out in the open!
(load "plt/tests.ss") ;(require tests)
(define tests tests)

(define test00 test00)
(define test01 test01)
(define test07 test07)

(define tests00 tests00)
(define tests01 tests01)
(define tests07 tests07)

(disp "BOUT TO LOAD COMPLIRE" )
(load/use-compiled "compiler.ss")

(define pass-names pass-names)
(disp "BOUT TO LOAD DRIVER" pass-names)

;(load/use-compiled "plt/driver.ss")
(load "plt/driver.ss")

;(module pass-names mzscheme  (provide pass-names)







