;; Ryan Newton
;; Started 2004.03.26
;; Here is the main file for my compiler.
;;======================================================================

;(module compiler_plt mzscheme

(require (lib "include.ss")
         "plt/helpers.ss"
         "plt/pass00.ss"
         "plt/pass01.ss"
         "plt/pass07.ss")

;  (require "plt/demo_display.ss")

(load/use-compiled "compiler_common.ss")


