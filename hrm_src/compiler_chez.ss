

(printf "Loading compiler in chezscheme..\n")


(include "chez/graphics_stub.ss")

(include "generic/demo_display.ss")
(include "generic/pass01.ss")

(display '(convert-to-simulator '(lang (program 3))))
(newline)
(display (convert-to-simulator '(lang (program 3))))
(newline)

