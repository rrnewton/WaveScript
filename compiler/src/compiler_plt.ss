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

         "plt/pass02_remove-unquoted-constant.ss"
         "plt/pass03_remove-complex-constant.ss"
         "plt/pass04_uncover-free.ss"
         "plt/pass05_lift-letrec.ss"
;         "plt/pass06_lift-letrec-body.ss"
;         "plt/pass07_remove-complex-opera.ss"
;         

         ;(all-except "plt/pass08_verify-core.ss" these-tests test-this)
;         "plt/pass09_separate-graph.ss"
         
         
;         "generic/pass02_remove-unquoted-constant.ss"
;         "generic/pass03_remove-complex-constant.ss"
;         "generic/pass04_uncover-free.ss"
;         "generic/pass05_lift-letrec.ss"
         
         
;	 "plt/tests.ss"
)

(define program 
  (lambda args (car (reverse args))))

(define base-language 
  (lambda args (eval (car (reverse args)))))
;(define base-language 
;  (lambda args
;    (for-each eval args)))

;(disp "FOOB DONE REQS")
;(require "plt/language-mechanism.ss")
;(disp "DOEN LANG")

;  (require "plt/demo_display.ss")

;; Get those module bound identifiers out in the open!
(load "plt/tests.ss") ;(require tests)
;(define tests tests)

;(define test00 test00)
;(define test01 test01)
;(define test07 test07)

;(define tests00 tests00)
;(define tests01 tests01)
;(define tests07 tests07)

(disp "BOUT TO LOAD COMPLIRE" )
(load/use-compiled "compiler.ss")

(define pass-names pass-names)
(disp "BOUT TO LOAD DRIVER" pass-names)

(load/use-compiled "plt/driver.ss")
;(module pass-names mzscheme
;  (define pass-names '())
;  (provide pass-names))
;(require "plt/driver.ss")

;(module pass-names mzscheme  (provide pass-names)







