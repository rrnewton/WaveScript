;; [2004.05.27]
;; This file holds some definitions common to both the plt version and the chez version.

;; Depends on basic_graphics.ss

;; [2004.06.05] Adding "get-state" to this interface so that the
;; simulator can ask the graphics object, say, what color it is.

;(define processor-screen-radius 25)

(define processor-screen-radius (/ window-width 32.))


