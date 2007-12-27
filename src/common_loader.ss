
;;;; 
;;;; .author Ryan Newton

;;;; [2007.12.27] This loads the bulk of the Regiment/WaveScript
;;;; source code.  It's shared between the different backends.
;;;; Hopefully this is the only place you'll find a long list of all
;;;; the source filenames in the project.

;(printf "ABOUT TO COMMON LOAD prim_defs...\n")

(common:load-source "generic/compiler_components/prim_defs.ss")
;(require (all-except "generic/compiler_components/prim_defs.ss" these-tests test-this))
