

(printf "Loading compiler in chezscheme..\n")

(include "chez/match.ss")
(include "generic/helpers.ss")

; (include "chez/graphics_stub.ss")

;(include "generic/demo_display.ss")

(include "generic/pass00.ss")
(include "generic/pass01.ss")
(include "generic/pass07.ss")



(display '(convert-to-simulator '(lang (program 3))))
(newline)
;(display (convert-to-simulator '(lang (program 3))))
(newline)

(display '(verify-core '(lang (program 3)))) (newline)


;(

