;; Ryan Newton
;; Started 2004.03.26
;; Here is the main file for my compiler.
;;======================================================================

;(module compiler_plt mzscheme

(require (lib "include.ss")
         "plt/helpers.ss"
         "plt/pass00.ss"
         ;;  (require "plt/pass01.ss")
         "plt/pass07.ss")

;  (require "plt/demo_display.ss")

;;======================================  
(display "Loading main compiler module.  RegionStreams Demo.")
(newline)

(test00)