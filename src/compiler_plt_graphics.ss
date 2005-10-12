
;; This is for code that should only be linked in if graphics are enabled.
;; ON:
(define-syntax GRAPHICSONLY (syntax-rules () [(_ expr) expr]))

(require (lib "include.ss"))
(include "compiler_plt.ss")
