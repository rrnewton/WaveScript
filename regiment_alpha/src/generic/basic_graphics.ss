
;; [2004.05.26]
;; This file contains a few constants that are used by both the chez 
;; and PLT versions of this module.

;; [2004.06.21] Adding "draw-mark" to the interface, I use this for
;; drawing 'X' marks for additional debugging info.

(define window-width 600)
(define window-height 600)

(define the-win #f)

(reg:define-struct (rgb red green blue))
(define rgb make-rgb)
