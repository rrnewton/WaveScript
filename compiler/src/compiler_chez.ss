
(printf "Loading compiler in chezscheme..~n")

;(load "chez/match.ss")
(include "chez/match.ss")
(include "generic/helpers.ss")

; (include "chez/graphics_stub.ss")
;(include "generic/demo_display.ss")

(include "generic/pass00.ss")
(include "generic/pass01.ss")
(include "generic/pass07.ss")

(include "compiler.ss")


