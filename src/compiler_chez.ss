
(printf "Loading compiler in chezscheme..~n")

;(load "chez/match.ss")
(include "chez/match.ss")
(include "generic/helpers.ss")

; (include "chez/graphics_stub.ss")
;(include "generic/demo_display.ss")

(include "generic/language-mechanism.ss")
(include "generic/lang00.ss")
(include "generic/lang05.ss")

(include "generic/pass00_verify-regiment.ss")
(include "generic/pass01_rename-var.ss")
(include "generic/pass02_remove-unquoted-constant.ss")
(include "generic/pass03_remove-complex-constant.ss")
(include "generic/pass04_uncover-free.ss")
(include "generic/pass05_lift-letrec.ss")

(include "generic/pass07.ss")

(include "compiler.ss")

(include "generic/driver.ss")
(include "generic/tests_noclosure.ss")
(include "generic/tests.ss")

