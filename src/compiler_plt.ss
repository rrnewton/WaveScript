;; Ryan Newton
;; Started 2004.03.26
;; Here is the main file for my compiler.
;;======================================================================

(module compiler_plt mzscheme

   (require (lib "include.ss"))

   (require "plt/pass01.ss")
   (require "plt/pass_check_simple.ss")
   (require "plt/demo_display.ss")

   (provide primitives)

   ;; Old old file of utils... 
   (define current_interpreter 'mzscheme)
   (require (lib "pretty.ss"))

   (include "utils/utility.scm")

 (display '(convert-to-simulator '(lang '(program 3))))
 (newline)
 (display (convert-to-simulator '(lang '(program 3))))
 (newline)


;;======================================
	
(display "Loading main compiler module.  RegionStreams Demo.")
(newline)

;; I need a name.

;; Words: 
;; Regions Abstract Streams...
;;======================================

;; A hook for future changes to load system.
(define rn-load load)

;;======================================


(define primitives
  '(  rmap rfold smap time-of
	   circle circle-at anchor anchor-at anchor-where k-neighborhood time
	   cluster sparsify border planarize treeize filter union intersect
	   until when when-any when-percentage
	   sense neighbors 
	   cons car cdr 
	   + - * /) )

)

;;======================================================================