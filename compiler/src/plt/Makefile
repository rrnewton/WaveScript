#*---------------------------------------------------------------------*/
#*    flags                                                            */
#*---------------------------------------------------------------------*/

BINPATH =`mzscheme -m --eval '(begin (display (build-path "compiled" "native"  (system-library-subpath))) (newline) (exit))'`

#all: native bytecode

all: compiled/constants.zo compiled/flat_threads.zo compiled/graphics_stub.zo \
     compiled/pass00_verify-regiment.zo \
     compiled/pass01_eta-primitives.zo \
     compiled/pass02_rename-vars.zo \
     compiled/pass03_remove-unquoted-constant.zo

#     compiled/simulator_nought_graphics.zo \
#     compiled/simulator_nought.zo \

compiled/constants.zo: constants.ss ../generic/constants.ss
	mzc --auto-dir --zo constants.ss

compiled/flat_threads.zo: flat_threads.ss
	mzc --auto-dir --zo flat_threads.ss

compiled/simulator_nought.zo: simulator_nought.ss ../generic/simulator_nought.ss
	mzc --auto-dir --zo simulator_nought.ss

compiled/graphics_stub.zo: graphics_stub.ss ../generic/graphics_stub.ss
	mzc --auto-dir --zo graphics_stub.ss

compiled/simulator_nought_graphics.zo: simulator_nought_graphics.ss ../generic/simulator_nought_graphics.ss
	mzc --auto-dir --zo simulator_nought_graphics.ss

compiled/pass00_verify-regiment.zo: pass00_verify-regiment.ss ../generic/pass00_verify-regiment.ss
	mzc --auto-dir --zo pass00_verify-regiment.ss
compiled/pass01_eta-primitives.zo: pass01_eta-primitives.ss ../generic/pass01_eta-primitives.ss
	mzc --auto-dir --zo pass01_eta-primitives.ss
compiled/pass02_rename-vars.zo: pass02_rename-vars.ss ../generic/pass02_rename-vars.ss
	mzc --auto-dir --zo pass02_rename-vars.ss
compiled/pass03_remove-unquoted-constant.zo: pass03_remove-unquoted-constant.ss ../generic/pass03_remove-unquoted-constant.ss
	mzc --auto-dir --zo pass03_remove-unquoted-constant.ss

compiled/helpers.zo: helpers.ss ../generic/helpers.ss
	mzc --auto-dir --zo helpers.ss


# 
#(include "generic/pass02_rename-vars.ss")
#(include "generic/pass03_remove-unquoted-constant.ss")
#(include "generic/pass04_reduce-primitives.ss")
#(include "generic/pass05_remove-complex-constant.ss")
#(include "generic/pass06_uncover-free.ss")
#(include "generic/pass07_lift-letrec.ss")
#(include "generic/pass08_lift-letrec-body.ss")
#(include "generic/pass09_remove-complex-opera.ss")
#(include "generic/pass10_verify-core.ss")
#(include "generic/pass11_classify-names.ss")
#;(include "generic/pass09_separate-graph")
#(include "generic/pass12_deglobalize.ss")


#(require (lib "include.ss")
#         (all-except "plt/helpers.ss" id mvlet rec))
#
#(disp "yay" default-unit-tester)
#
#         ;	 "plt/language-mechanism.ss"
#         
# 
#         pass02_remove-unquoted-constant
#         "plt/pass03_remove-complex-constant.ss"
#         "plt/pass04_uncover-free.ss"
#         "plt/pass05_lift-letrec.ss"
#         "plt/pass06_lift-letrec-body.ss"
#         "plt/pass07_remove-complex-opera.ss"
#         
#         (all-except "plt/pass08_verify-core.ss" these-tests test-this)
#         (all-except "plt/pass09_classify-names.ss" these-tests test-this)
#         (all-except "plt/pass10_deglobalize.ss" these-tests test-this))
#        ;          "plt/pass09_separate-graph.ss"
#
#(require
#; (all-except "plt/simulator_nought.ss" these-tests test-this)
# (all-except "plt/simulator_nought_graphics.ss" these-tests test-this wrap-def-simulate)
# )

#
#
#          pass00_verify-regiment           
#          pass01_eta-primitives            
#          pass02_rename-var                \
#          pass03_remove-unquoted-constant  \
#          pass04_reduce-primitives         \
#          pass05_remove-complex-constant   \
#          pass06_uncover-free              \
#          pass07_lift-letrec               \
#          pass08_lift-letrec-body          \
#          pass09_remove-complex-opera      \
#          pass10_verify-core               \
#          pass11_classify-names               \
#          pass12_deglobalize               \
#          flat_threads basic_graphics      \
#          demo_display graphics_     \
#          simulator_nought simulator_nought_graphics \
#          tests driver 


#bytecode:
#	(cd compiled; $(MAKE) "SRCFILES=$(SRCFILES)")

native:
	(cd $(BINPATH); $(MAKE))

clean:
	(cd compiled; $(MAKE) "SRCFILES=$(SRCFILES)" clean)
	(cd compiled/native; $(MAKE) "SRCFILES=$(SRCFILES)" clean)


helpers:
	(cd compiled; $(MAKE) helpers)
