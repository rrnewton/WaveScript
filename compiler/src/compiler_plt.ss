;; Ryan Newton
;; Started 2004.03.26
;; Here is the main file for my compiler.
;;======================================================================

;(module compiler_plt mzscheme

(require (lib "include.ss")
         "plt/helpers.ss"
         (all-except "plt/pass00.ss" these-tests test-this)
         (all-except "plt/pass01.ss" these-tests test-this)
         (all-except "plt/pass07.ss" these-tests test-this)
)
;         "plt/pass01.ss"
;         "plt/pass07.ss")

;  (require "plt/demo_display.ss")

(load/use-compiled "compiler_common.ss")


