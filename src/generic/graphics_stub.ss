;; [2004.05.27]
;; This file holds some definitions common to both the plt version and the chez version.

;; REQUIRES: basic_graphics.ss

;; [2004.06.05] Adding "get-state" to this interface so that the
;; simulator can ask the graphics object, say, what color it is.

;(define processor-screen-radius 25)

(define processor-screen-radius (/ window-width 45.))


;; TODO: interface description

;; I've moved this over to a "persistent object" kind of drawing interface.
;; The "simobject" structure holds a "gobj", which is an
;; implementation independent thingy that supports "get-state" for 
;; retrieving properties and "change-color!".
;;  The simobject also has a redraw flag that can be set to refresh
;;  the processor during the periodic redraw.


;; NOTE: Right now the "redraw" flag isn't used yet... [2005.02.25]
;; Change-color redraws immediately, and it's the only way to state-change...

;; provides:
;;   draw-proc  :  (x y)      -> <void>
;;   draw-procs : ((x y) ...) -> <void>
;;   draw-edge draw-mark draw-circle
;;   change-color! 
;;   get-state  : gobj, flag  -> relevent-state
;;   init-graphics close-graphics clear-buffer
;;   these-tests test-this test-graphics-stub

