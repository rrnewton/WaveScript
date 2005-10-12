
;; This is for code that should only be linked in if graphics are enabled.
;; OFF:
(define-syntax GRAPHICSONLY 
  (syntax-rules () 
    [(_ expr ) ()]
    ))

(require (lib "include.ss"))
(include "compiler_plt.ss")
