
;; [2004.05.26]
;; This file contains a few constants/globals that are used by both
;; the chez and PLT versions of this module.

;; [2004.06.21] Adding "draw-mark" to the interface, I use this for
;; drawing 'X' marks for additional debugging info.

(define window-width 600)
(define window-height 600)

(define the-win #f)

;; [Chez] This simply makes a SWL rgb value out of our standard record representation.
(define (rec->rgb rec)
  (make <rgb> (rgb-red rec) (rgb-green rec) (rgb-blue rec)))
