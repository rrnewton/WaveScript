
(printf "Loading compiler in chezscheme..~n")

;(load "chez/match.ss")
(include "chez/match.ss")
(include "generic/helpers.ss")

; (include "chez/graphics_stub.ss")
;(include "generic/demo_display.ss")

(include "generic/language-mechanism.ss")
(include "generic/lang00.ss")

(include "generic/pass00_verify-regiment.ss")
(include "generic/pass01_rename-var.ss")
(include "generic/pass07.ss")

(include "compiler.ss")

(include "generic/driver.ss")
(include "generic/tests_noclosure.ss")
(include "generic/tests.ss")

