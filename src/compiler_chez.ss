
(printf "Loading compiler in chezscheme..~n")

(include "chez/match.ss")

;; This in turn includes "../generic/helpers.ss" so we gotta load it from its dir.
(cd "chez") (include "helpers.ss") (cd "..")

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
(include "generic/pass06_lift-letrec-body.ss")
(include "generic/pass07_remove-complex-opera.ss")
(include "generic/pass08_verify-core.ss")

;(include "generic/pass09_separate-graph")

(include "compiler.ss")

;; Driver depends on 'pass-names being defined.
(include "generic/driver.ss")
(include "generic/tests_noclosure.ss")
(include "generic/tests.ss")

